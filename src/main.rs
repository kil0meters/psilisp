#![warn(
    clippy::all,
    // clippy::restriction,
    clippy::pedantic,
    // clippy::nursery,
    // clippy::cargo,
)]

use core::fmt;
use std::{
    collections::{HashMap, LinkedList},
    fmt::Display,
    io::{self, Write},
    rc::Rc,
};

mod builtins;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String),
    String(String),
    Bool(bool),
    Number(i64),
    List(LinkedList<Expression>),
    Function(fn(Expression) -> Result<Expression, LispError>),
    Lambda(Rc<(Expression, Expression)>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(s) => write!(f, "{s}"),
            Expression::String(s) => write!(f, "{s}"),
            Expression::Number(n) => write!(f, "{n}"),
            Expression::Bool(b) => write!(f, "{b}"),
            Expression::Function(_) => write!(f, "Function"),
            Expression::Lambda(l) => {
                let params = &l.0;
                let body = &l.1;
                write!(f, "(lambda {params} {body})")
            }
            Expression::List(list) => {
                let s = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "({s})")
            }
        }
    }
}

#[derive(Debug)]
pub enum LispError {
    Reason(String),
}

impl Display for LispError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LispError::Reason(r) => write!(f, "{r}"),
        }
    }
}

struct Environment {
    data: LinkedList<HashMap<String, Expression>>,
}

impl Environment {
    fn new() -> Self {
        let mut env = Self {
            data: LinkedList::new(),
        };
        env.push_scope(HashMap::new());
        env
    }

    fn insert_global(&mut self, k: String, v: Expression) {
        self.data.front_mut().unwrap().insert(k, v);
    }

    // fn insert_scoped(&mut self, k: String, v: Expression) {
    //     self.data.back_mut().unwrap().insert(k, v);
    // }

    fn get(&self, k: &String) -> Option<&Expression> {
        for scope in self.data.iter().rev() {
            if let Some(s) = scope.get(k) {
                return Some(s);
            }
        }

        None
    }

    fn push_scope(&mut self, scope: HashMap<String, Expression>) {
        self.data.push_back(scope);
    }

    fn pop_scope(&mut self) {
        self.data.pop_back();
    }
}

impl Default for Environment {
    fn default() -> Self {
        let mut env = Environment::new();

        env.insert_global("+".to_string(), Expression::Function(builtins::add));
        env.insert_global("-".to_string(), Expression::Function(builtins::sub));
        env.insert_global("=".to_string(), Expression::Function(builtins::eq));
        env.insert_global(
            "display".to_string(),
            Expression::Function(builtins::display),
        );

        // include stdlib
        let mut tokens = tokenize(include_str!("stdlib.scm"));

        while !tokens.is_empty() {
            eval(parse(&mut tokens).unwrap(), &mut env).unwrap();
        }

        env
    }
}

fn tokenize(expression: &str) -> LinkedList<String> {
    expression
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn read_list(tokens: &mut LinkedList<String>) -> Result<Expression, LispError> {
    let mut token = tokens
        .front()
        .ok_or_else(|| LispError::Reason("Mising closing parenthesis.".to_string()))?;
    let mut list = LinkedList::new();

    loop {
        if token == ")" {
            tokens.pop_front();
            return Ok(Expression::List(list));
        }

        list.push_back(parse(tokens)?);
        token = tokens
            .front()
            .ok_or_else(|| LispError::Reason("Premature ending".to_string()))?;
    }
}

fn parse_atom(token: String) -> Expression {
    let float = token.parse::<i64>();

    match float {
        Ok(n) => Expression::Number(n),
        Err(_) => {
            if token.starts_with('\'') {
                Expression::String(token)
            } else {
                match token.as_ref() {
                    "true" => Expression::Bool(true),
                    "false" => Expression::Bool(false),
                    _ => Expression::Identifier(token),
                }
            }
        }
    }
}

fn parse(tokens: &mut LinkedList<String>) -> Result<Expression, LispError> {
    let token = tokens
        .pop_front()
        .ok_or_else(|| LispError::Reason("Premature ending".to_string()))?;

    match token.as_str() {
        "(" => read_list(tokens),
        ")" => Err(LispError::Reason(
            "Expressions must start with open parenthesis".to_string(),
        )),
        _ => Ok(parse_atom(token)),
    }
}

fn eval_if(
    args: &mut LinkedList<Expression>,
    env: &mut Environment,
) -> Result<Expression, LispError> {
    let test_form = args
        .pop_front()
        .ok_or_else(|| LispError::Reason("if: Expected test argument".to_string()))?;

    let test = eval(test_form, env)?;

    match test {
        Expression::Bool(b) => {
            if !b {
                args.pop_front().ok_or_else(|| {
                    LispError::Reason("Expected argument for if statement".to_string())
                })?;
            }

            let res_form = args.pop_front().ok_or_else(|| {
                LispError::Reason("Expected argument for if statement".to_string())
            })?;

            eval(res_form, env)
        }
        _ => Err(LispError::Reason(format!(
            "Unexpected test form \"{test}\""
        ))),
    }
}

fn eval_define(
    args: &mut LinkedList<Expression>,
    env: &mut Environment,
) -> Result<Expression, LispError> {
    let first = args
        .pop_front()
        .ok_or_else(|| LispError::Reason("set requires two arguments".to_string()))?;

    match first {
        Expression::Identifier(ref s) => {
            let second = args
                .pop_front()
                .ok_or_else(|| LispError::Reason("set requires two arguments".to_string()))?;

            let second = eval(second, env)?;
            env.insert_global(s.to_owned(), second);
            Ok(first)
        }
        _ => Err(LispError::Reason(
            "First argument must be an identifier".to_string(),
        )),
    }
}

fn eval_lambda(args: &mut LinkedList<Expression>) -> Result<Expression, LispError> {
    let first = args
        .pop_front()
        .ok_or_else(|| LispError::Reason("lambda requires 2 arguments".to_string()))?;

    let params = match first {
        Expression::List(l) => Expression::List(l),
        _ => {
            return Err(LispError::Reason(
                "Params must be in the form of a list".to_string(),
            ))
        }
    };

    let body = args
        .pop_front()
        .ok_or_else(|| LispError::Reason("lambda requires 2 arguments".to_string()))?;

    Ok(Expression::Lambda(Rc::new((params, body))))
}

fn builtin_operations(
    expr: &Expression,
    rest: &mut LinkedList<Expression>,
    env: &mut Environment,
) -> Option<Result<Expression, LispError>> {
    match expr {
        Expression::Identifier(s) => match s.as_ref() {
            "if" => Some(eval_if(rest, env)),
            "define" => Some(eval_define(rest, env)),
            "lambda" => Some(eval_lambda(rest)),
            "quote" => Some(Ok(Expression::Identifier(
                rest.front().unwrap().to_string(),
            ))),
            _ => None,
        },
        _ => None,
    }
}

fn eval(expr: Expression, env: &mut Environment) -> Result<Expression, LispError> {
    match expr {
        Expression::Number(_) | Expression::String(_) | Expression::Bool(_) => Ok(expr),
        Expression::Identifier(i) => Ok(env
            .get(&i)
            .ok_or_else(|| LispError::Reason(format!("Unknown identifier \"{i}\"")))?
            .clone()),
        Expression::List(mut list) => {
            let first = list.pop_front().unwrap();

            match builtin_operations(&first, &mut list, env) {
                Some(res) => res,
                None => {
                    let first_eval = eval(first, env)?;
                    match first_eval {
                        Expression::Function(f) => {
                            let args = list.iter().map(|x| eval(x.clone(), env)).collect::<Result<
                                LinkedList<Expression>,
                                LispError,
                            >>(
                            )?;

                            f(Expression::List(args))
                        }
                        Expression::Lambda(l) => {
                            let params_expr = &l.0;
                            let body = &l.1;

                            // Params must always be a list object.
                            let params = match params_expr {
                                Expression::List(list) => list,
                                _ => panic!(),
                            };

                            let mut scope = HashMap::new();
                            for param in params {
                                let param = match param {
                                    Expression::Identifier(s) => s,
                                    _ => panic!(),
                                };

                                let arg = eval(
                                    list.pop_front().ok_or_else(|| {
                                        LispError::Reason("Invalid number of arguments".to_string())
                                    })?,
                                    env,
                                )?;
                                scope.insert(param.to_owned(), arg);
                            }

                            env.push_scope(scope);
                            let res = eval(body.to_owned(), env);
                            env.pop_scope();

                            res
                        }
                        _ => Err(LispError::Reason(
                            "The first argument must be a function".to_string(),
                        )),
                    }
                }
            }
        }
        Expression::Lambda(_) => panic!("SHOuldn't happen"),
        Expression::Function(_) => panic!("Wowee you encountered an error!"),
    }
}

fn read() -> String {
    print!("klisp> ");
    io::stdout().flush().unwrap();

    let mut val = String::new();

    io::stdin().read_line(&mut val).unwrap();

    val
}

fn main() {
    let mut env = Environment::default();

    loop {
        let mut tokens = tokenize(&read());

        let res = match parse(&mut tokens) {
            Ok(expr) => match eval(expr, &mut env) {
                Ok(res) => res,
                Err(e) => {
                    eprintln!("error: {e}");
                    continue;
                }
            },

            Err(e) => {
                eprintln!("error: {e}");
                continue;
            }
        };

        println!("{res}");
    }
}
