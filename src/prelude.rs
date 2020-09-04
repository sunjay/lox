use std::time::{SystemTime, UNIX_EPOCH};

use crate::interpreter::{Interpreter, Value, EvalResult};

pub fn populate_prelude(ctx: &mut Interpreter) {
    ctx.env.insert("clock".into(), Value::NativeFunc(clock.into()))
}

fn clock(_ctx: &mut Interpreter) -> EvalResult {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis();

    Ok(Value::Number(since_the_epoch as f64 / 1000.0))
}
