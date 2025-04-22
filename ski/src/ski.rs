// src/ski.rs

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    S,
    K,
    I,
    Apply(Box<Expr>, Box<Expr>), // 表示将第一个表达式应用于第二个表达式
}

// 为了方便显示，实现 Display trait
use std::fmt;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::S => write!(f, "S"),
            Expr::K => write!(f, "K"),
            Expr::I => write!(f, "I"),
            Expr::Apply(func, arg) => {
                // 为了可读性，添加括号
                // 对于 `Apply(Apply(S, K), I)` 会显示为 `(S K) I` (更常见)
                // 或者 `((S K) I)` （更明确，但可能繁琐）
                // 我们选择后者，因为更容易解析和理解结构
                write!(f, "({} {})", func, arg)
            }
        }
    }
}

impl Expr {
    // 执行一步归约
    // 返回 Some(reduced_expr) 如果发生了归约
    // 返回 None 如果不能再归约
    fn reduce_step(&self) -> Option<Expr> {
        match self {
            // 规则 I x -> x
            Expr::Apply(f, x) if **f == Expr::I => Some(*x.clone()),
            // 规则 K x y -> x
            // Matches Apply(Apply(K, x), y)
            Expr::Apply(maybe_kx, _y) if matches!(&**maybe_kx, Expr::Apply(k, _) if **k == Expr::K) =>
            {
                // We know maybe_kx is Apply(K, x), extract x
                if let Expr::Apply(_, x) = &**maybe_kx {
                    Some(*x.clone())
                } else {
                    unreachable!("Pattern already matched by guard");
                }
            }
            // 规则 S x y z -> (x z) (y z)
            // Matches Apply(Apply(Apply(S, x), y), z)
            Expr::Apply(maybe_sxy, z) if matches!(&**maybe_sxy, Expr::Apply(maybe_sx, _) if matches!(&**maybe_sx, Expr::Apply(s, _) if **s == Expr::S)) =>
            {
                // We know maybe_sxy is Apply(Apply(S, x), y), extract x, y, z
                if let Expr::Apply(maybe_sx, y) = &**maybe_sxy {
                    if let Expr::Apply(_, x) = &**maybe_sx {
                        let xz = Expr::Apply(x.clone(), z.clone());
                        let yz = Expr::Apply(y.clone(), z.clone());
                        Some(Expr::Apply(Box::new(xz), Box::new(yz)))
                    } else {
                        unreachable!("Pattern already matched by guard");
                    }
                } else {
                    unreachable!("Pattern already matched by guard");
                }
            }
            // General Apply: Try reducing f, then x (leftmost-outermost reduction)
            Expr::Apply(f, x) => {
                // Try reducing f first
                if let Some(reduced_f) = f.reduce_step() {
                    Some(Expr::Apply(Box::new(reduced_f), x.clone()))
                }
                // Then try reducing x
                else if let Some(reduced_x) = x.reduce_step() {
                    Some(Expr::Apply(f.clone(), Box::new(reduced_x)))
                } else {
                    None // Neither f nor x can be reduced
                }
            }
            // Base cases: S, K, I are already normal forms
            Expr::S | Expr::K | Expr::I => None,
        }
    }

    // 完全归约表达式，直到达到范式
    pub fn reduce(&self) -> Expr {
        let mut current = self.clone();
        // 循环应用 reduce_step 直到没有变化
        while let Some(next) = current.reduce_step() {
            current = next;
            // 可选：打印每一步的归约过程
            dbg!(&current);
        }
        current
    }
}
