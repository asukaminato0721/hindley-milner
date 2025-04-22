// src/main.rs

mod parser;

use ski::Expr;

fn main() {
    // --- 示例 1: 手动构建表达式 ---
    // I x = x, 所以 I I 应该归约为 I
    let i_i = Expr::Apply(Box::new(Expr::I), Box::new(Expr::I));
    println!("Expression: {}", i_i);
    let reduced_i_i = i_i.reduce();
    println!("Reduced:    {}\n", reduced_i_i); // 应该输出 I

    // K x y = x, 所以 K S I 应该归约为 S
    // (K S) I
    let k_s_i = Expr::Apply(
        Box::new(Expr::Apply(Box::new(Expr::K), Box::new(Expr::S))),
        Box::new(Expr::I),
    );
    println!("Expression: {}", k_s_i);
    let reduced_k_s_i = k_s_i.reduce();
    println!("Reduced:    {}\n", reduced_k_s_i); // 应该输出 S

    // S K K x = (K x) (K x) = x
    // ((S K) K) x
    // 为了演示，我们不直接给 x，只归约 S K K
    let s_k_k = Expr::Apply(
        Box::new(Expr::Apply(Box::new(Expr::S), Box::new(Expr::K))),
        Box::new(Expr::K),
    );
    println!("Expression: {}", s_k_k);
    // S K K -> (K x) (K x) 看起来像是 I，让我们应用一个虚拟的 x (用 I 代替)
    let s_k_k_i = Expr::Apply(Box::new(s_k_k), Box::new(Expr::I));
    println!("Expression with I: {}", s_k_k_i);
    let reduced_s_k_k_i = s_k_k_i.reduce();
    println!("Reduced:           {}\n", reduced_s_k_k_i); // 应该输出 I

    // --- 示例 2: 使用解析器 (如果实现了) ---
    if cfg!(feature = "parser") {
        // 假设你用 feature gate 控制 parser 模块
        println!("--- Using Parser ---");
        //let input = "(I I)";
        // let input = "((K S) I)";
        let input = "S (K (S I)) (S (K K) I)"; // S K K I

        match parser::parse(input) {
            Ok(expr) => {
                println!("Input String: {}", input);
                println!("Parsed Expr:  {}", expr);
                let reduced_expr = expr.reduce();
                println!("Reduced:      {}", reduced_expr);
            }
            Err(e) => {
                eprintln!("Parsing error: {}", e);
            }
        }
    } else {
        // 如果没有 parser，可以放一个提示信息
        println!("Parser feature not enabled. Build with --features parser to test parsing.");
    }
}
