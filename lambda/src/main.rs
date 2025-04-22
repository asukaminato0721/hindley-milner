use std::collections::HashSet;
use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};

// --- AST (Abstract Syntax Tree) 定义 ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Term {
    Var(String),               // 变量, e.g., "x"
    Abs(String, Box<Term>),    // 抽象 (Lambda), e.g., \x. M
    App(Box<Term>, Box<Term>), // 应用, e.g., M N
}

// --- 解析器 (Parser) ---

// 检查字符是否是合法的标识符字符
fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

// 解析器状态
struct Parser<'a> {
    input: &'a [u8], // 使用字节切片以方便处理 ASCII
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Parser {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    // 查看当前字符但不消费
    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).map(|&b| b as char)
    }

    // 消费当前字符并前进
    fn consume(&mut self) -> Option<char> {
        let char = self.peek();
        if char.is_some() {
            self.pos += 1;
        }
        char
    }

    // 跳过空白字符
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.consume();
            } else {
                break;
            }
        }
    }

    // 解析标识符
    fn parse_identifier(&mut self) -> Result<String, String> {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if is_identifier_char(c) {
                self.consume();
            } else {
                break;
            }
        }
        let ident_bytes = &self.input[start..self.pos];
        if ident_bytes.is_empty() {
            Err(format!("Expected identifier at position {}", start))
        } else {
            String::from_utf8(ident_bytes.to_vec())
                .map_err(|e| format!("Invalid UTF-8 in identifier: {}", e))
        }
    }

    // 解析原子项：变量 或 (项)
    fn parse_atom(&mut self) -> Result<Term, String> {
        self.skip_whitespace();
        match self.peek() {
            Some('(') => {
                self.consume(); // 消费 '('
                let term = self.parse_term()?; // 递归解析括号内的项
                self.skip_whitespace();
                match self.consume() {
                    Some(')') => Ok(term),
                    _ => Err(format!("Expected ')' at position {}", self.pos)),
                }
            }
            Some(c) if is_identifier_char(c) => {
                let ident = self.parse_identifier()?;
                Ok(Term::Var(ident))
            }
            Some('\\') => {
                // Lambda 抽象本身也可以看作一个“原子”单位，由 parse_term 处理
                Err("Unexpected '\\' when expecting an atom (variable or parens). Abstractions handled by parse_term.".to_string())
            }
            Some(c) => Err(format!(
                "Unexpected character '{}' when parsing atom at position {}",
                c, self.pos
            )),
            None => Err("Unexpected end of input when parsing atom".to_string()),
        }
    }

    // 解析 Lambda 抽象: \ident. term
    fn parse_abstraction(&mut self) -> Result<Term, String> {
        self.skip_whitespace();
        if self.consume() != Some('\\') {
            return Err("Expected '\\' to start abstraction".to_string());
        }

        let mut params = Vec::new();
        loop {
            self.skip_whitespace();
            // Expect an identifier
            let ident = self.parse_identifier()?;
            params.push(ident);

            // Check what follows the identifier
            self.skip_whitespace();
            match self.peek() {
                Some('.') => break,                           // Found the dot, end of parameters
                Some(c) if is_identifier_char(c) => continue, // Found another identifier, continue loop
                Some(c) => {
                    return Err(format!(
                        "Expected '.' or identifier after parameter, found '{}' at position {}",
                        c, self.pos
                    ));
                }
                None => return Err("Unexpected end of input after parameters".to_string()),
            }
        }

        // We broke the loop because we saw '.'
        self.skip_whitespace();
        if self.consume() != Some('.') {
            // This should technically not happen if the loop logic is correct, but good for robustness
            return Err(format!(
                "Internal parser error: Expected '.' after parameters at position {}",
                self.pos
            ));
        }
        // '.' 之后的部分是 body，递归调用 parse_term
        let body = self.parse_term()?;
        // Build nested abstractions from right to left
        let mut current_term = body;
        for param in params.into_iter().rev() {
            current_term = Term::Abs(param, Box::new(current_term));
        }
        Ok(current_term)
    }

    // 解析一个项：处理应用链（左结合）或单个抽象
    fn parse_term(&mut self) -> Result<Term, String> {
        self.skip_whitespace();

        // 首先检查是否是 Lambda 抽象
        let mut term = if self.peek() == Some('\\') {
            self.parse_abstraction()?
        } else {
            // 否则，它必须以原子开始（变量或括号）
            self.parse_atom()?
        };

        // 循环处理后续的应用（左结合）
        loop {
            self.skip_whitespace();
            // 尝试解析下一个原子项作为参数
            // 如果下一个字符可以开始一个原子（标识符或'('），则解析它
            match self.peek() {
                Some(c) if is_identifier_char(c) || c == '(' => {
                    let argument = self.parse_atom()?;
                    term = Term::App(Box::new(term), Box::new(argument));
                }
                // 如果下一个是 '\'，它开始一个新的抽象，这也可以是应用的参数
                Some('\\') => {
                    let argument = self.parse_abstraction()?;
                    term = Term::App(Box::new(term), Box::new(argument));
                }
                // 否则，没有更多的应用，结束循环
                _ => break,
            }
        }
        Ok(term)
    }

    // 主解析函数
    fn parse(&mut self) -> Result<Term, String> {
        let term = self.parse_term()?;
        self.skip_whitespace();
        // 确保所有输入都被消费了
        if self.pos < self.input.len() {
            Err(format!(
                "Unexpected trailing input starting at position {}: '{}'",
                self.pos,
                self.peek().unwrap_or('?')
            ))
        } else {
            Ok(term)
        }
    }
}

// 公开的解析接口
fn parse(input: &str) -> Result<Term, String> {
    let mut parser = Parser::new(input);
    parser.parse()
}

// --- 解释器 (Interpreter) ---

// 计算一个项中的自由变量集合
fn free_variables_recursive(term: &Term, bound: &mut HashSet<String>, free: &mut HashSet<String>) {
    match term {
        Term::Var(name) => {
            if !bound.contains(name) {
                free.insert(name.clone());
            }
        }
        Term::Abs(var, body) => {
            // 记录进入此抽象前该变量是否已被绑定
            let was_bound = bound.contains(var);
            bound.insert(var.clone()); // 将此抽象绑定的变量加入绑定集合
            free_variables_recursive(body, bound, free); // 递归计算 body 的自由变量
            // 离开此抽象作用域后，如果之前未绑定，则移除
            if !was_bound {
                bound.remove(var);
            }
        }
        Term::App(t1, t2) => {
            free_variables_recursive(t1, bound, free);
            free_variables_recursive(t2, bound, free);
        }
    }
}

// 获取项的自由变量集合的公共接口
fn get_free_variables(term: &Term) -> HashSet<String> {
    let mut bound = HashSet::new();
    let mut free = HashSet::new();
    free_variables_recursive(term, &mut bound, &mut free);
    free
}

// 用于生成新变量名的全局计数器 (用于 Alpha 转换)
// 实际应用中应使用 AtomicU64 或其他线程安全机制。
static VAR_COUNTER: AtomicU64 = AtomicU64::new(0);

// 生成一个基于 base 的新变量名，确保它不在 forbidden 集合中
fn fresh_var(base: &str, forbidden: &HashSet<String>) -> String {
    let mut name;
    loop {
        // 使用 unsafe 块来修改 static mut 计数器
        name = {
            let count = VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
            // 尝试移除数字和下划线，以获得更干净的基础名称
            let clean_base = base.trim_end_matches(|c: char| c.is_digit(10) || c == '_');
            format!(
                "{}{}",
                if clean_base.is_empty() {
                    "v"
                } else {
                    clean_base
                },
                count
            )
        };
        if !forbidden.contains(&name) {
            break;
        }
    }
    name
}

// 替换操作: term[var_name := replacement]
// 实现 Alpha 转换以避免变量捕获
fn substitute(term: &Term, var_name: &str, replacement: &Term) -> Term {
    match term {
        Term::Var(name) => {
            if name == var_name {
                replacement.clone() // 变量匹配，替换
            } else {
                term.clone() // 变量不匹配，保持不变
            }
        }
        Term::Abs(bound_var, body) => {
            if bound_var == var_name {
                // 变量被此抽象绑定，替换停止在此作用域
                term.clone()
            } else {
                let fv_replacement = get_free_variables(replacement);
                // 检查：如果此抽象绑定的变量 (bound_var) 是替换项 (replacement) 中的自由变量
                if fv_replacement.contains(bound_var) {
                    // --- Alpha 转换 ---
                    // 需要重命名 bound_var 以避免捕获
                    let mut forbidden = fv_replacement; // 禁止使用 replacement 中的自由变量
                    let fv_body = get_free_variables(body);
                    forbidden.extend(fv_body); // 也禁止使用 body 中的自由变量
                    forbidden.insert(bound_var.clone()); // 确保新名称与旧名称不同

                    let new_bound_var = fresh_var(bound_var, &forbidden);

                    // 1. 在 body 内部，将所有旧的 bound_var 替换为 new_bound_var
                    let new_body = substitute(body, bound_var, &Term::Var(new_bound_var.clone()));

                    // 2. 在重命名后的 body 中进行原始的替换 (var_name := replacement)
                    Term::Abs(
                        new_bound_var, // 使用新的绑定变量名
                        Box::new(substitute(&new_body, var_name, replacement)),
                    )
                } else {
                    // 不需要 Alpha 转换，直接在 body 中进行替换
                    Term::Abs(
                        bound_var.clone(),
                        Box::new(substitute(body, var_name, replacement)),
                    )
                }
            }
        }
        Term::App(t1, t2) => {
            // 递归地在左右子项中进行替换
            Term::App(
                Box::new(substitute(t1, var_name, replacement)),
                Box::new(substitute(t2, var_name, replacement)),
            )
        }
    }
}

// 执行一步 Beta 归约 (范式序 - 最左、最外)
// 返回 Some(reduced_term) 如果可以归约，否则返回 None
fn beta_reduction_step(term: &Term) -> Option<Term> {
    match term {
        // 情况 1: 应用 (M N)
        Term::App(t1, t2) => {
            // 优先尝试归约函数部分 t1 (最外)
            if let Some(reduced_t1) = beta_reduction_step(t1) {
                return Some(Term::App(Box::new(reduced_t1), t2.clone()));
            }

            // 如果 t1 无法归约且是一个抽象 (\x. Body)
            if let Term::Abs(var, body) = &**t1 {
                // 尝试归约参数部分 t2 (最左，在 t1 无法归约之后)
                if let Some(reduced_t2) = beta_reduction_step(t2) {
                    return Some(Term::App(t1.clone(), Box::new(reduced_t2)));
                }
                // 如果 t1 是抽象且 t2 也无法归约（或已是范式），则执行 Beta 归约
                return Some(substitute(body, var, t2)); // 应用：用 t2 替换 body 中的 var
            }

            // 如果 t1 无法归约且不是抽象，尝试归约参数部分 t2
            if let Some(reduced_t2) = beta_reduction_step(t2) {
                return Some(Term::App(t1.clone(), Box::new(reduced_t2)));
            }

            // 如果 t1 和 t2 都无法归约，且 t1 不是抽象，则此应用项无法归约
            None
        }
        // 情况 2: 抽象 \x. M
        Term::Abs(var, body) => {
            // 尝试在抽象体内进行归约
            beta_reduction_step(body)
                .map(|reduced_body| Term::Abs(var.clone(), Box::new(reduced_body)))
        }
        // 情况 3: 变量 x
        Term::Var(_) => None, // 变量本身无法归约
    }
}

// 解释（求值）一个项，通过重复应用 Beta 归约 (范式序)
// max_steps 用于防止无限循环
fn interpret(term: Term, max_steps: u32) -> (Term, u32) {
    let mut current_term = term;
    let mut steps = 0;
    while steps < max_steps {
        if let Some(next_term) = beta_reduction_step(&current_term) {
            current_term = next_term;
            steps += 1;
        } else {
            break; // 到达范式（Normal Form），无法再归约
        }
    }
    (current_term, steps)
}

// --- 美化打印 (Pretty Printer) ---

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 内部辅助函数，控制括号添加
        fn write_term(
            term: &Term,
            f: &mut fmt::Formatter<'_>,
            context: PrintContext,
        ) -> fmt::Result {
            match term {
                Term::Var(name) => write!(f, "{}", name),
                Term::Abs(var, body) => {
                    // 如果抽象是应用的一部分，需要加括号
                    if context == PrintContext::AppArg || context == PrintContext::AppFunc {
                        write!(f, "(")?;
                    }
                    write!(f, "\\{}", var)?;
                    // 检查 body 是否是另一个 Abs 以便链式打印: \x y. M
                    let mut current_body = body;
                    while let Term::Abs(inner_var, inner_body) = &**current_body {
                        write!(f, " {}", inner_var)?;
                        current_body = inner_body;
                    }
                    write!(f, ". {}", current_body)?; // 使用递归 Display 打印最终的 body

                    if context == PrintContext::AppArg || context == PrintContext::AppFunc {
                        write!(f, ")")?;
                    }
                    Ok(())
                }
                Term::App(t1, t2) => {
                    // 如果应用是另一个应用的参数，需要加括号 M (N P)
                    if context == PrintContext::AppArg {
                        write!(f, "(")?;
                    }
                    // 打印函数部分，标记其为函数上下文
                    write_term(t1, f, PrintContext::AppFunc)?;
                    write!(f, " ")?;
                    // 打印参数部分，标记其为参数上下文
                    write_term(t2, f, PrintContext::AppArg)?;

                    if context == PrintContext::AppArg {
                        write!(f, ")")?;
                    }
                    Ok(())
                }
            }
        }
        // 初始调用上下文为 TopLevel
        write_term(self, f, PrintContext::TopLevel)
    }
}

// 用于控制打印时是否需要加括号的上下文
#[derive(PartialEq, Eq, Clone, Copy)]
enum PrintContext {
    TopLevel, // 顶层
    AppFunc,  // 作为应用的函数部分 (左侧)
    AppArg,   // 作为应用的参数部分 (右侧)
}

// --- 主函数和示例 ---
fn main() {
    let examples = vec![
        r"(\x. x) y",                           // Identity applied to y
        r"(\x. x) (\y. y)",                     // Identity applied to identity
        r"(\f x. f x) (\y. y) z",               // Simple application
        r"(\x y. x) p q",                       // Kestrel combinator K = \x y. x
        r"(\x y z. x z (y z)) a b c",           // S combinator S = \x y z. x z (y z)
        r"(\n f x. f (n f x)) (\f x. f (f x))", // Successor applied to Church numeral 2
        r"(\x. \y. x) y",                       // Potential capture test (needs alpha-conversion)
        r"\x. x (\y. y x) z",                   // Nested structures
        r"(\x. x x) (\y. y y)",                 // Omega combinator (non-terminating)
    ];

    let max_steps = 100; // 设置最大归约步数

    for (i, input) in examples.iter().enumerate() {
        println!("[Example {}] Input: {}", i + 1, input);
        match parse(input) {
            Ok(term) => {
                println!("Parsed AST: {:?}", term); // 打印详细 AST 结构 (可选)
                println!("Pretty Parsed: {}", term); // 打印解析后的项

                println!("Interpreting (max_steps={})...", max_steps);
                let (result_term, steps_taken) = interpret(term.clone(), max_steps);

                println!("Result: {}", result_term);
                if steps_taken == max_steps && beta_reduction_step(&result_term).is_some() {
                    println!(
                        "Warning: Reached max steps limit ({}), result might not be in normal form.",
                        max_steps
                    );
                } else {
                    println!("Steps taken: {}", steps_taken);
                }
            }
            Err(e) => println!("Parse Error: {}", e),
        }
        println!("---");
    }

    // 手动测试一个更复杂的例子
    println!("[Manual Test] Input: {}", r"(\x y. (\z. x) z y) a b");
    match parse(r"(\x y. (\z. x) z y) a b") {
        Ok(term) => {
            println!("Pretty Parsed: {}", term);
            let (result, steps) = interpret(term, 100);
            println!("Result: {}", result);
            println!("Steps taken: {}", steps);
        }
        Err(e) => println!("Parse Error: {}", e),
    }
    println!("---");
    let result = interpret(
        parse(r"(\ x.\ y.\ z. z x y) (\ x.\ y.x) (\ x.\ y.y) (\ x.\ y.x)").unwrap(),
        max_steps,
    );
    println!("Result of complex example: {}", result.0);
}
