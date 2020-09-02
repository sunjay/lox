use std::fmt;

use crate::scanner::{TokenKind, Token};
use crate::diag::Diagnostic;
use crate::ast::*;

type Input<'a> = &'a [Token];
type IResult<'a, T> = Result<(Input<'a>, T), ParseError>;

/// Error type for converting into a diagnostic
///
/// Avoids allocating on every error. Only the final error that is propagated is converted into a
/// string.
#[derive(Debug)]
struct ParseError {
    line: usize,
    expected: Expected,
    found: TokenKind,
}

#[derive(Debug)]
enum Expected {
    Kind(TokenKind),
    Str(&'static str),
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Kind(kind) => write!(f, "{}", kind),
            Expected::Str(value) => write!(f, "{}", value),
        }
    }
}

impl ParseError {
    fn to_diag(self) -> Diagnostic {
        Diagnostic {
            line: self.line,
            message: format!("expected {}, found {}", self.expected, self.found)
        }
    }
}

pub fn parse_expr(input: &[Token]) -> anyhow::Result<Expr> {
    match expr(input) {
        Ok((input, expr)) => {
            let (input, _) = tk(input, TokenKind::Eof).map_err(|err| err.to_diag())?;
            assert!(input.is_empty(), "bug: tokens after EOF");

            Ok(expr)
        },

        Err(err) => Err(err.to_diag())?,
    }
}

// Grammar:
//
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "false" | "true" | "nil"
//                | "(" expression ")" ;
//
// Source: https://craftinginterpreters.com/parsing-expressions.html

macro_rules! match_kinds {
    (
        $input:ident {
            $($(@ $name:ident @)?$kind:ident => $kind_expr:expr,)*
        } $(else $not_found:block)?
    ) => {{
        let kinds = &[$(TokenKind::$kind),*];
        match tk_any($input, kinds) {
            $(Ok((input, token)) if token.kind == TokenKind::$kind => {
                $(let $name = token;)?
                (input, $kind_expr)
            },)*
            Ok(_) => unreachable!("bug: tk_any should only return one of the specified kinds"),
            $(Err(_) => $not_found,)?
            #[allow(unreachable_patterns)]
            Err(e) => Err(e)?,
        }
    }};
}

fn expr(input: Input) -> IResult<Expr> {
    equality(input)
}

fn equality(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = comparison(input)?;

    loop {
        let (next_input, op) = match_kinds!(input {
            EqualEqual => BinaryOp::Equal,
            BangEqual => BinaryOp::NotEqual,
        } else { // No matching tokens found
            break;
        });
        let (next_input, right) = comparison(next_input)?;

        input = next_input;

        left = Expr::Binary(Box::new(BinaryExpr {
            left,
            op,
            right,
        }));
    }

    Ok((input, left))
}

fn comparison(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = addition(input)?;

    loop {
        let (next_input, op) = match_kinds!(input {
            Greater => BinaryOp::Greater,
            GreaterEqual => BinaryOp::GreaterEqual,
            Less => BinaryOp::Less,
            LessEqual => BinaryOp::LessEqual,
        } else { // No matching tokens found
            break;
        });
        let (next_input, right) = addition(next_input)?;

        input = next_input;

        left = Expr::Binary(Box::new(BinaryExpr {
            left,
            op,
            right,
        }));
    }

    Ok((input, left))
}

fn addition(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = multiplication(input)?;

    loop {
        let (next_input, op) = match_kinds!(input {
            Plus => BinaryOp::Add,
            Minus => BinaryOp::Sub,
        } else { // No matching tokens found
            break;
        });
        let (next_input, right) = multiplication(next_input)?;

        input = next_input;

        left = Expr::Binary(Box::new(BinaryExpr {
            left,
            op,
            right,
        }));
    }

    Ok((input, left))
}

fn multiplication(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = unary(input)?;

    loop {
        let (next_input, op) = match_kinds!(input {
            Star => BinaryOp::Mul,
            Slash => BinaryOp::Div,
        } else { // No matching tokens found
            break;
        });
        let (next_input, right) = unary(next_input)?;

        input = next_input;

        left = Expr::Binary(Box::new(BinaryExpr {
            left,
            op,
            right,
        }));
    }

    Ok((input, left))
}

fn unary(input: Input) -> IResult<Expr> {
    let (input, op) = match_kinds!(input {
        Bang => UnaryOp::Not,
        Minus => UnaryOp::Neg,
    } else { // No matching tokens found
        return primary(input);
    });

    let (input, value) = unary(input)?;

    Ok((input, Expr::Unary(Box::new(UnaryExpr {op, value}))))
}

fn primary(input: Input) -> IResult<Expr> {
    group(input)
        .or_else(|_| nil(input).map(|(input, value)| (input, Expr::Nil(value))))
        .or_else(|_| bool_lit(input).map(|(input, value)| (input, Expr::Bool(value))))
        .or_else(|_| str_lit(input).map(|(input, value)| (input, Expr::String(value))))
        .or_else(|_| ident(input).map(|(input, value)| (input, Expr::Ident(value))))
        .or_else(|_| num_lit(input).map(|(input, value)| (input, Expr::Number(value))))
}

fn group(input: Input) -> IResult<Expr> {
    let (input, _) = tk(input, TokenKind::LeftParen)?;
    let (input, expr) = expr(input)?;
    let (input, _) = tk(input, TokenKind::RightParen)?;
    Ok((input, expr))
}

fn nil(input: Input) -> IResult<Nil> {
    let (input, token) = tk(input, TokenKind::Nil)?;
    Ok((input, Nil {
        line: token.line,
    }))
}

fn bool_lit(input: Input) -> IResult<BoolLit> {
    let (input, (token, value)) = match_kinds!(input {
        @token@True => (token, true),
        @token@False => (token, false),
    });

    Ok((input, BoolLit {
        value,
        line: token.line,
    }))
}

fn str_lit(input: Input) -> IResult<StrLit> {
    let token = &input[0];
    match &token.kind {
        TokenKind::String(value) => Ok((&input[1..], StrLit {
            value: value.clone(),
            line: token.line,
        })),

        _ => Err(ParseError {
            line: token.line,
            expected: Expected::Str("a string literal"),
            found: token.kind.clone(),
        })
    }
}

fn ident(input: Input) -> IResult<Ident> {
    let token = &input[0];
    match &token.kind {
        TokenKind::Identifier(value) => Ok((&input[1..], Ident {
            value: value.clone(),
            line: token.line,
        })),

        _ => Err(ParseError {
            line: token.line,
            expected: Expected::Str("an identifier"),
            found: token.kind.clone(),
        })
    }
}

fn num_lit(input: Input) -> IResult<NumLit> {
    let token = &input[0];
    match &token.kind {
        &TokenKind::Number(value) => Ok((&input[1..], NumLit {
            value,
            line: token.line,
        })),

        _ => Err(ParseError {
            line: token.line,
            expected: Expected::Str("a number literal"),
            found: token.kind.clone(),
        })
    }
}

/// Matches any of the provided token kinds
fn tk_any<'a>(input: Input<'a>, kinds: &[TokenKind]) -> IResult<'a, &'a Token> {
    let mut err = None;

    for kind in kinds {
        match tk(input, kind.clone()) {
            Ok(res) => return Ok(res),
            Err(tk_err) => {
                if err.is_none() {
                    err = Some(tk_err);
                }
            },
        }
    }

    Err(err.unwrap())
}

fn tk(input: Input, kind: TokenKind) -> IResult<&Token> {
    let token = &input[0];
    if token.kind == kind {
        Ok((&input[1..], token))
    } else {
        Err(ParseError {
            line: token.line,
            expected: Expected::Kind(kind.clone()),
            found: token.kind.clone(),
        })
    }
}
