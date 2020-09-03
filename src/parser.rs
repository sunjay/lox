use std::iter;
use std::fmt;

use crate::scanner::{TokenKind, Token};
use crate::diag::Diagnostic;
use crate::ast::*;

type Input<'a> = &'a [Token];
type IResult<'a, T> = Result<(Input<'a>, T), ParseError>;

fn map<T, U, F>(value: IResult<T>, f: F) -> IResult<U>
    where F: FnOnce(T) -> U,
{
    value.map(|(input, value)| (input, f(value)))
}

/// Error type for converting into a diagnostic
///
/// Avoids allocating on every error. Only the final error that is propagated is converted into a
/// string.
#[derive(Debug)]
enum ParseError {
    UnexpectedToken {
        line: usize,
        expected: Expected,
        found: TokenKind,
    },

    UnsupportedLValue {
        line: usize,
    },

    TooManyArguments {
        line: usize,
    },
}

impl ParseError {
    fn to_diag(self) -> Diagnostic {
        use ParseError::*;
        match self {
            UnexpectedToken {line, expected, found} => Diagnostic {
                line,
                message: format!("expected {}, found {}", expected, found),
            },

            UnsupportedLValue {line} => Diagnostic {
                line,
                message: format!("Unsupported left-hand side of assignment expression"),
            },

            TooManyArguments {line} => Diagnostic {
                line,
                message: format!("Cannot have more than 255 arguments in function or call"),
            },
        }
    }
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

pub fn parse_program(input: &[Token]) -> anyhow::Result<Program> {
    match program(input) {
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
// program     → declaration* EOF ;
//
// declaration → varDecl
//             | statement ;
//
// varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement → exprStmt
//           | forStmt
//           | ifStmt
//           | printStmt
//           | whileStmt
//           | block ;
//
// exprStmt  → expression ";" ;
// forStmt   → "for" "(" ( varDecl | exprStmt | ";" )
//                       expression? ";"
//                       expression? ")" statement ;
// ifStmt    → "if" "(" expression ")" statement ( "else" statement )? ;
// printStmt → "print" expression ";" ;
// whileStmt → "while" "(" expression ")" statement ;
// block     → "{" declaration* "}" ;
//
// expression → assignment ;
// assignment → identifier "=" assignment
//            | logic_or ;
// logic_or   → logic_and ( "or" logic_and )* ;
// logic_and  → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
// addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
// multiplication → unary ( ( "/" | "*" ) unary )* ;
// unary → ( "!" | "-" ) unary | call ;
// call  → primary ( "(" arguments? ")" )* ;
// arguments → expression ( "," expression )* ;
// primary        → NUMBER | STRING | "false" | "true" | "nil"
//                | "(" expression ")" ;
//
// Source: https://craftinginterpreters.com

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

fn program(mut input: Input) -> IResult<Program> {
    let mut decls = Vec::new();

    while input[0].kind != TokenKind::Eof {
        let (next_input, decl) = declaration(input)?;
        decls.push(decl);
        input = next_input;
    }

    Ok((input, Program {decls}))
}

fn declaration(input: Input) -> IResult<Decl> {
    match input[0].kind {
        TokenKind::Var => map(var_decl(input), Decl::VarDecl),
        _ => map(statement(input), Decl::Stmt),
    }
}

fn var_decl(input: Input) -> IResult<VarDecl> {
    let (input, _) = tk(input, TokenKind::Var)?;
    let (input, name) = ident(input)?;

    let (input, initializer) = match input[0].kind {
        TokenKind::Equal => {
            let (input, _) = tk(input, TokenKind::Equal)?;
            let (input, initializer) = expr(input)?;
            (input, Some(initializer))
        },
        _ => (input, None),
    };

    let (input, _) = tk(input, TokenKind::Semicolon)?;

    Ok((input, VarDecl {name, initializer}))
}

fn statement(input: Input) -> IResult<Stmt> {
    match input[0].kind {
        TokenKind::Print => map(print_stmt(input), Stmt::Print),
        TokenKind::LeftBrace => map(block(input), Stmt::Block),
        TokenKind::If => map(cond(input), |cond| Stmt::If(Box::new(cond))),
        TokenKind::While => map(while_loop(input), |while_loop| Stmt::While(Box::new(while_loop))),
        TokenKind::For => for_loop(input),
        _ => map(expr_stmt(input), Stmt::Expr),
    }
}

fn print_stmt(input: Input) -> IResult<PrintStmt> {
    let (input, print_token) = tk(input, TokenKind::Print)?;
    let (input, expr) = expr(input)?;
    let (input, _) = tk(input, TokenKind::Semicolon)?;

    Ok((input, PrintStmt {
        print_token_line: print_token.line,
        value: expr,
    }))
}

fn block(input: Input) -> IResult<Block> {
    let (mut input, _) = tk(input, TokenKind::LeftBrace)?;

    let mut decls = Vec::new();
    while input[0].kind != TokenKind::RightBrace {
        let (next_input, decl) = declaration(input)?;
        decls.push(decl);
        input = next_input;
    }

    let (input, _) = tk(input, TokenKind::RightBrace)?;

    Ok((input, Block {decls}))
}

fn cond(input: Input) -> IResult<Cond> {
    let (input, _) = tk(input, TokenKind::If)?;
    let (input, _) = tk(input, TokenKind::LeftParen)?;
    let (input, cond) = expr(input)?;
    let (input, _) = tk(input, TokenKind::RightParen)?;
    let (input, if_body) = statement(input)?;

    let (input, else_body) = if let Ok((input, _)) = tk(input, TokenKind::Else) {
        let (input, else_body) = statement(input)?;
        (input, Some(else_body))
    } else {
        (input, None)
    };

    Ok((input, Cond {cond, if_body, else_body}))
}

fn while_loop(input: Input) -> IResult<WhileLoop> {
    let (input, _) = tk(input, TokenKind::While)?;
    let (input, _) = tk(input, TokenKind::LeftParen)?;
    let (input, cond) = expr(input)?;
    let (input, _) = tk(input, TokenKind::RightParen)?;
    let (input, body) = statement(input)?;

    Ok((input, WhileLoop {cond, body}))
}

// for loop is desugared into block with while loop
fn for_loop(input: Input) -> IResult<Stmt> {
    let (input, _) = tk(input, TokenKind::For)?;
    let (input, _) = tk(input, TokenKind::LeftParen)?;

    let (input, initializer) = match input[0].kind {
        TokenKind::Var => map(var_decl(input), |decl| Some(Decl::VarDecl(decl)))?,

        TokenKind::Semicolon => map(tk(input, TokenKind::Semicolon), |_| None)?,

        _ => map(expr_stmt(input), |expr| Some(Decl::Stmt(Stmt::Expr(expr))))?,
    };

    let (input, cond) = if input[0].kind == TokenKind::Semicolon {
        map(tk(input, TokenKind::Semicolon), |token| Expr::Bool(BoolLit {
            value: true,
            line: token.line,
        }))?
    } else {
        let (input, cond) = expr(input)?;
        let (input, _) = tk(input, TokenKind::Semicolon)?;
        (input, cond)
    };

    let (input, increment) = if input[0].kind == TokenKind::RightParen {
        map(tk(input, TokenKind::RightParen), |_| None)?
    } else {
        let (input, increment) = expr(input)?;
        let (input, _) = tk(input, TokenKind::RightParen)?;
        (input, Some(increment))
    };

    let (input, body) = statement(input)?;

    let for_loop = Stmt::Block(Block {
        decls: initializer.into_iter()
            .chain(iter::once(Decl::Stmt(Stmt::While(Box::new(WhileLoop {
                cond,
                body: Stmt::Block(Block {
                    decls: iter::once(Decl::Stmt(body))
                        .chain(increment.map(|expr| Decl::Stmt(Stmt::Expr(expr))))
                        .collect(),
                }),
            })))))
            .collect(),
    });

    Ok((input, for_loop))
}

fn expr_stmt(input: Input) -> IResult<Expr> {
    let (input, expr) = expr(input)?;
    let (input, _) = tk(input, TokenKind::Semicolon)?;

    Ok((input, expr))
}

fn expr(input: Input) -> IResult<Expr> {
    assignment(input)
}

fn assignment(input: Input) -> IResult<Expr> {
    let (input, lhs) = logical_or(input)?;
    if let Ok((input, _)) = tk(input, TokenKind::Equal) {
        let lvalue = match lhs {
            Expr::Ident(name) => LValue::Ident(name),

            _ => Err(ParseError::UnsupportedLValue {
                line: lhs.line(),
            })?,
        };

        let (input, rhs) = assignment(input)?;

        Ok((input, Expr::Assign(Box::new(Assign {
            lvalue,
            rhs,
        }))))
    } else {
        Ok((input, lhs))
    }
}

fn logical_or(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = logical_and(input)?;

    while let Ok((next_input, _)) = tk(input, TokenKind::Or) {
        let (next_input, right) = logical_and(next_input)?;
        left = Expr::LogicalOr(Box::new(LogicalOr {left, right}));
        input = next_input;
    }

    Ok((input, left))
}

fn logical_and(input: Input) -> IResult<Expr> {
    let (mut input, mut left) = equality(input)?;

    while let Ok((next_input, _)) = tk(input, TokenKind::And) {
        let (next_input, right) = equality(next_input)?;
        left = Expr::LogicalAnd(Box::new(LogicalAnd {left, right}));
        input = next_input;
    }

    Ok((input, left))
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
        return call(input);
    });

    let (input, value) = unary(input)?;

    Ok((input, Expr::Unary(Box::new(UnaryExpr {op, value}))))
}

fn call(input: Input) -> IResult<Expr> {
    let (mut input, mut callee) = primary(input)?;

    while input[0].kind == TokenKind::LeftParen {
        let (next_input, args) = arguments(input)?;
        callee = Expr::Call(Box::new(CallExpr {callee, args}));
        input = next_input;
    }

    Ok((input, callee))
}

fn arguments(input: Input) -> IResult<Vec<Expr>> {
    let (mut input, paren_token) = tk(input, TokenKind::LeftParen)?;
    let start_line = paren_token.line;

    // This allows trailing commas because yolo
    let mut args = Vec::new();
    while input[0].kind != TokenKind::RightParen {
        let (next_input, arg) = expr(input)?;
        args.push(arg);
        input = next_input;

        match tk(input, TokenKind::Comma) {
            Ok((next_input, _)) => input = next_input,
            Err(_) => break,
        }
    }

    let (input, _) = tk(input, TokenKind::RightParen)?;

    // The limit is actually 254 because of `this`
    if args.len() >= 255 {
        return Err(ParseError::TooManyArguments {
            line: start_line,
        });
    }

    Ok((input, args))
}

fn primary(input: Input) -> IResult<Expr> {
    group(input)
        .or_else(|_| map(nil(input), Expr::Nil))
        .or_else(|_| map(bool_lit(input), Expr::Bool))
        .or_else(|_| map(str_lit(input), Expr::String))
        .or_else(|_| map(ident(input), Expr::Ident))
        .or_else(|_| map(num_lit(input), Expr::Number))
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

        _ => Err(ParseError::UnexpectedToken {
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

        _ => Err(ParseError::UnexpectedToken {
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

        _ => Err(ParseError::UnexpectedToken {
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
        Err(ParseError::UnexpectedToken {
            line: token.line,
            expected: Expected::Kind(kind.clone()),
            found: token.kind.clone(),
        })
    }
}
