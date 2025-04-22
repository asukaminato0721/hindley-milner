// src/parser.rs
use crate::ski::Expr;
use std::iter::Peekable;
use std::vec::IntoIter;

// 非常基础的 Token 类型
#[derive(Debug, Clone, PartialEq)]
enum Token {
    S,
    K,
    I,
    LParen,
    RParen,
}

// 基础词法分析器
fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(&c) = chars.peek() {
        match c {
            'S' => {
                tokens.push(Token::S);
                chars.next();
            }
            'K' => {
                tokens.push(Token::K);
                chars.next();
            }
            'I' => {
                tokens.push(Token::I);
                chars.next();
            }
            '(' => {
                tokens.push(Token::LParen);
                chars.next();
            }
            ')' => {
                tokens.push(Token::RParen);
                chars.next();
            }
            c if c.is_whitespace() => {
                chars.next();
            } // 跳过空白
            _ => return Err(format!("Invalid character: {}", c)),
        }
    }
    Ok(tokens)
}

// 递归下降解析器
fn parse_expr(tokens: &mut Peekable<IntoIter<Token>>) -> Result<Expr, String> {
    match tokens.next() {
        Some(Token::S) => Ok(Expr::S),
        Some(Token::K) => Ok(Expr::K),
        Some(Token::I) => Ok(Expr::I),
        Some(Token::LParen) => {
            // 解析形式为 (f x)
            let func = parse_expr(tokens)?;
            let arg = parse_expr(tokens)?;
            match tokens.next() {
                Some(Token::RParen) => Ok(Expr::Apply(Box::new(func), Box::new(arg))),
                _ => Err("Expected ')'".to_string()),
            }
        }
        Some(Token::RParen) => Err("Unexpected ')'".to_string()),
        None => Err("Unexpected end of input".to_string()),
    }
}

pub fn parse(input: &str) -> Result<Expr, String> {
    let tokens = tokenize(input)?;
    let mut token_iter = tokens.into_iter().peekable();
    let expr = parse_expr(&mut token_iter)?;
    if token_iter.peek().is_some() {
        Err("Unexpected tokens after expression".to_string())
    } else {
        Ok(expr)
    }
}
