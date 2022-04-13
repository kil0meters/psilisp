use crate::Expression;
use crate::LispError;

pub fn add(operands: Expression) -> Result<Expression, LispError> {
    let mut sum = 0;

    if let Expression::List(args) = operands {
        for num_expr in args {
            if let Expression::Number(num) = num_expr {
                sum += num;
            } else {
                return Err(LispError::Reason(
                    "Add expects a list of numbers".to_string(),
                ));
            }
        }
    } else {
        return Err(LispError::Reason(
            "Add expects a list of numbers".to_string(),
        ));
    }

    Ok(Expression::Number(sum))
}

pub fn sub(operands: Expression) -> Result<Expression, LispError> {
    if let Expression::List(mut args) = operands {
        let mut diff = match args.pop_front().unwrap() {
            Expression::Number(num) => num,
            _ => panic!(),
        };

        for num_expr in args {
            if let Expression::Number(num) = num_expr {
                diff -= num;
            } else {
                return Err(LispError::Reason(
                    "Sub expects a list of numbers".to_string(),
                ));
            }
        }

        Ok(Expression::Number(diff))
    } else {
        Err(LispError::Reason(
            "Sub expects a list of numbers".to_string(),
        ))
    }
}

pub fn eq(operands: Expression) -> Result<Expression, LispError> {
    if let Expression::List(mut args) = operands {
        let first = args
            .pop_front()
            .ok_or_else(|| LispError::Reason("= expects at least two arguments".to_string()))?;
        let second = args
            .pop_front()
            .ok_or_else(|| LispError::Reason("= expects at least two arguments".to_string()))?;

        match first {
            Expression::Number(lhs) => {
                if let Expression::Number(rhs) = second {
                    Ok(Expression::Bool(lhs == rhs))
                } else {
                    Err(LispError::Reason("Invalid comparison".to_string()))
                }
            }
            Expression::String(lhs) => {
                if let Expression::String(rhs) = second {
                    Ok(Expression::Bool(lhs == rhs))
                } else {
                    Err(LispError::Reason("Invalid comparison".to_string()))
                }
            }
            // what great code: just panic if equal doesn't work, what could go wrong!
            _ => panic!(),
        }
    } else {
        Err(LispError::Reason("= expects a list of numbers".to_string()))
    }
}

// pub fn less(operands: Expression) -> Result<Expression, LispError> {}

pub fn display(operands: Expression) -> Result<Expression, LispError> {
    println!("{operands}");
    Ok(Expression::Number(0))
}
