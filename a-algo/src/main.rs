use std::fmt;

// --- Data Structures ---

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
enum Combinator {
    S,
    K,
    I,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Var(String),
    Comb(Combinator),
    // Box is needed for recursive enum variants to give them a known size
    App(Box<Expr>, Box<Expr>),
}

// --- Helper Function: Create an Application Expression ---
fn app(e1: Expr, e2: Expr) -> Expr {
    Expr::App(Box::new(e1), Box::new(e2))
}

// --- Display Implementation (for printing expressions) ---

impl fmt::Display for Combinator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Combinator::S => write!(f, "S"),
            Combinator::K => write!(f, "K"),
            Combinator::I => write!(f, "I"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Comb(c) => write!(f, "{}", c),
            // Add parentheses for clarity in applications
            Expr::App(e1, e2) => write!(f, "({} {})", e1, e2),
        }
    }
}

// --- Helper Function: Check if variable `var_name` is free in `expr` ---

fn contains_var(expr: &Expr, var_name: &str) -> bool {
    match expr {
        Expr::Var(name) => name == var_name,
        Expr::Comb(_) => false, // Combinators don't contain variables
        Expr::App(e1, e2) => contains_var(e1, var_name) || contains_var(e2, var_name),
    }
}

// --- Abstraction Function A(E, x) ---
// Implements the mapping from an expression E and variable x to a combinator expression.
// Note: This function itself is not a combinator, but it produces one.

fn abstract_expr(expr: &Expr, var_name: &str) -> Expr {
    // Rule 1: A(x, x) = I
    if let Expr::Var(name) = expr {
        if name == var_name {
            // If the expression is the variable we are abstracting, return I
            return Expr::Comb(Combinator::I);
        }
    }

    // Rule 2: A(E, x) = K E if x does not appear free in E
    // This check takes precedence over Rule 3 for non-variable expressions E.
    // It applies to Combinators and Variables other than x automatically.
    // It also applies to Applications where x is not free.
    if !contains_var(expr, var_name) {
        return app(
            Expr::Comb(Combinator::K), // K combinator
            expr.clone(),              // The original expression E
        );
    }

    // Rule 3: A(E1 E2, x) = S A(E1, x) A(E2, x)
    // This rule applies only if E is an application (E1 E2) AND x appears free in it
    // (otherwise Rule 2 would have applied).
    if let Expr::App(e1, e2) = expr {
        let abstracted_e1 = abstract_expr(e1, var_name);
        let abstracted_e2 = abstract_expr(e2, var_name);
        return app(
            app(
                Expr::Comb(Combinator::S), // S combinator
                abstracted_e1,             // Result of abstracting x from E1
            ),
            abstracted_e2, // Result of abstracting x from E2
        );
    }

    // This part should technically not be reached if the logic covers all cases:
    // 1. Var(x) -> Rule 1
    // 2. E not containing x -> Rule 2
    // 3. App(E1, E2) containing x -> Rule 3
    // Variables other than x are handled by Rule 2 via !contains_var.
    // Combinators are handled by Rule 2 via !contains_var.
    unreachable!(
        "Should have matched one of the abstraction rules. Expression: {}, Variable: {}",
        expr, var_name
    );
}

// --- Main Function: Derive the 'swap' combinator ---
#[test]
fn swap() {
    // Define the body of the swap function: (y x)
    // swap x y = y x
    // As a lambda expression: λx. λy. (y x)
    let body = app(Expr::Var("y".to_string()), Expr::Var("x".to_string()));
    println!("Original expression body for swap x y: {}", body); // (y x)

    // Step 1: Abstract the inner variable 'y'.
    // We are finding the function `f` such that `f y = y x`.
    // This corresponds to `swap x` in the original definition.
    // Calculate A(y x, y)
    println!("\nStep 1: Abstracting 'y' from {}", body);
    let swap_x = abstract_expr(&body, "y");
    // A(y x, y) = S A(y, y) A(x, y)
    //           = S I (K x)
    println!("Result (swap x) = A({}, y) = {}", body, swap_x); // (S I (K x))

    // Step 2: Abstract the outer variable 'x' from the result of Step 1.
    // We are finding the combinator `swap` such that `swap x = S I (K x)`.
    // Calculate A(S I (K x), x)
    println!("\nStep 2: Abstracting 'x' from {}", swap_x);
    let swap_combinator = abstract_expr(&swap_x, "x");
    // A(S I (K x), x) = S A(S I, x) A(K x, x)
    //                 = S (K (S I)) A(K x, x)  <-- Rule 2 applied to (S I)
    //                 = S (K (S I)) (S A(K, x) A(x, x))
    //                 = S (K (S I)) (S (K K) I)
    println!("Result (swap) = A({}, x) = {}", swap_x, swap_combinator);

    // Verify the expected result
    // Construct the expected expression manually for comparison
    let expected_swap = app(
        app(
            Expr::Comb(Combinator::S),
            app(
                Expr::Comb(Combinator::K),
                app(Expr::Comb(Combinator::S), Expr::Comb(Combinator::I)),
            ),
        ),
        app(
            app(
                Expr::Comb(Combinator::S),
                app(Expr::Comb(Combinator::K), Expr::Comb(Combinator::K)),
            ),
            Expr::Comb(Combinator::I),
        ),
    );

    println!("\nExpected swap combinator: {}", expected_swap);
    assert_eq!(swap_combinator, expected_swap);
    println!("Successfully derived the swap combinator!");
}
#[test]
fn c1() {
    // c1 x y z = x (y z)
    let body = app(
        Expr::Var("x".to_string()),
        app(Expr::Var("y".to_string()), Expr::Var("z".to_string())),
    );
    println!("Original expression body for c1 x y z: {}", body); // (x (y z))
    // Step 1: Abstract the inner variable 'y'.
    // We are finding the function `f` such that `f y = x (y z)`.
    // This corresponds to `c1 x z` in the original definition.
    // Calculate A(x (y z), y)
    println!("\nStep 1: Abstracting 'z' from {}", body);
    let c1_xy = abstract_expr(&body, "z");
    println!("Result (c1 x y) = A({}, z) = {}", body, c1_xy); // (S (K x) (S (K y) I))
    println!("\nStep 2: Abstracting 'y' from {}", c1_xy);
    let c1_x = abstract_expr(&c1_xy, "y");
    println!("Result (c1 x) = A({}, y) = {}", c1_xy, c1_x); // (S (K (S (K x))) (S (S (K S) (S (K K) I)) (K I)))
    println!("\nStep 3: Abstracting 'x' from {}", c1_x);
    let c1_combinator = abstract_expr(&c1_x, "x");
    println!("Result (c1) = A({}, x) = {}", c1_x, c1_combinator);
    println!("\nSuccessfully derived a combinator for c1!");
}
#[test]
fn c2() {
    let mut body = app(
        Expr::Var("x".to_string()),
        app(Expr::Var("z".to_string()), Expr::Var("y".to_string())),
    );
    for var_name in ["z", "y", "x"] {
        body = abstract_expr(&body, var_name);
        println!("Result for {} = {}", var_name, body);
    }

    println!("Result for c2 = {}", body);
}

fn main() {}
