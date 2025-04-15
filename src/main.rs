#![allow(unused)]
use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

// --- Type Representation ---

static NEXT_TYPE_VAR_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Int,
    String,
    Var(usize),                // Type variable, e.g., 'a, 'b represented by IDs
    Fun(Box<Type>, Box<Type>), // Function type: Arg -> Ret
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::String => write!(f, "string"),
            // Simple representation for type variables
            Type::Var(id) => write!(f, "'{}", (b'a' + (id % 26) as u8) as char),
            Type::Fun(arg, ret) => {
                // Add parentheses for nested functions for clarity
                match **arg {
                    Type::Fun(_, _) => write!(f, "({}) -> {}", arg, ret),
                    _ => write!(f, "{} -> {}", arg, ret),
                }
            }
        }
    }
}

// --- Abstract Syntax Tree (AST) ---

#[derive(Debug, Clone)]
enum Expr {
    Int(i32),
    Str(String),
    Var(String),                       // Variable identifier
    Lambda(String, Box<Expr>),         // Parameter name, Body expression
    App(Box<Expr>, Box<Expr>),         // Function application: func(arg)
    Let(String, Box<Expr>, Box<Expr>), // Let binding: let name = val_expr in body_expr
}

// --- Type Inference Infrastructure ---

type TypeEnvironment = HashMap<String, Type>;
type Substitution = HashMap<usize, Type>; // Map from Type Variable ID to Type

#[derive(Debug)]
enum InferenceError {
    UnboundVariable(String),
    TypeMismatch(Type, Type),
    OccursCheck(usize, Type), // Variable occurs within the type it's being unified with
    NotAFunction(Type),
}

impl fmt::Display for InferenceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InferenceError::UnboundVariable(name) => write!(f, "Unbound variable: {}", name),
            InferenceError::TypeMismatch(t1, t2) => {
                write!(f, "Type mismatch: Expected {}, found {}", t1, t2)
            }
            InferenceError::OccursCheck(id, t) => write!(
                f,
                "Occurs check failed: Cannot construct infinite type 'a = {} where 'a is '{}",
                t,
                (b'a' + (id % 26) as u8) as char
            ),
            InferenceError::NotAFunction(t) => {
                write!(f, "Not a function: Cannot apply arguments to type {}", t)
            }
        }
    }
}

// Helper to create new type variables
fn new_type_var() -> Type {
    Type::Var(NEXT_TYPE_VAR_ID.fetch_add(1, Ordering::SeqCst))
}

// Apply substitution to a type
fn apply_subst(subst: &Substitution, t: &Type) -> Type {
    match t {
        Type::Int | Type::String => t.clone(),
        Type::Var(id) => {
            if let Some(replacement) = subst.get(id) {
                // Recursively apply substitution to the replacement type
                // This handles cases like 'a -> 'b, 'b -> int
                apply_subst(subst, replacement)
            } else {
                t.clone() // No substitution for this variable
            }
        }
        Type::Fun(arg_t, ret_t) => Type::Fun(
            Box::new(apply_subst(subst, arg_t)),
            Box::new(apply_subst(subst, ret_t)),
        ),
    }
}

// Apply substitution to a type environment
fn apply_subst_to_env(subst: &Substitution, env: &TypeEnvironment) -> TypeEnvironment {
    env.iter()
        .map(|(name, t)| (name.clone(), apply_subst(subst, t)))
        .collect()
}

// Compose two substitutions (s2 after s1)
// apply(s1 then s2, T) == apply(s2, apply(s1, T))
fn compose_subst(s2: &Substitution, s1: &Substitution) -> Substitution {
    let mut composed: Substitution = s1.iter().map(|(id, t)| (*id, apply_subst(s2, t))).collect();
    // Add bindings from s2 that are not already present for the same variable id
    for (id, t) in s2 {
        composed.entry(*id).or_insert_with(|| t.clone());
    }
    composed
}

// Check if type variable `id` occurs in type `t`
fn occurs_check(id: usize, t: &Type) -> bool {
    match t {
        Type::Int | Type::String => false,
        Type::Var(other_id) => id == *other_id,
        Type::Fun(arg_t, ret_t) => occurs_check(id, arg_t) || occurs_check(id, ret_t),
    }
}

// Unification algorithm
fn unify(t1: &Type, t2: &Type) -> Result<Substitution, InferenceError> {
    match (t1, t2) {
        // Basic types match
        (Type::Int, Type::Int) | (Type::String, Type::String) => Ok(HashMap::new()),

        // Same type variable
        (Type::Var(id1), Type::Var(id2)) if id1 == id2 => Ok(HashMap::new()),

        // Variable unification (handle occurs check)
        (Type::Var(id), other_t) | (other_t, Type::Var(id)) => {
            if occurs_check(*id, other_t) {
                Err(InferenceError::OccursCheck(*id, other_t.clone()))
            } else {
                // Bind the variable 'id' to the type 'other_t'
                let mut subst = HashMap::new();
                subst.insert(*id, other_t.clone());
                Ok(subst)
            }
        }

        // Function unification
        (Type::Fun(arg1, ret1), Type::Fun(arg2, ret2)) => {
            // Unify arguments first
            let s1 = unify(arg1, arg2)?;
            // Apply resulting substitution to return types before unifying them
            let ret1_subst = apply_subst(&s1, ret1);
            let ret2_subst = apply_subst(&s1, ret2);
            let s2 = unify(&ret1_subst, &ret2_subst)?;
            // Compose the substitutions
            Ok(compose_subst(&s2, &s1))
        }

        // Mismatch
        _ => Err(InferenceError::TypeMismatch(t1.clone(), t2.clone())),
    }
}

// --- The Inference Function ---

fn infer(env: &TypeEnvironment, expr: &Expr) -> Result<(Type, Substitution), InferenceError> {
    match expr {
        Expr::Int(_) => Ok((Type::Int, HashMap::new())),
        Expr::Str(_) => Ok((Type::String, HashMap::new())),

        Expr::Var(name) => {
            if let Some(t) = env.get(name) {
                // In a full system, you'd instantiate polymorphic types here.
                // For simplicity, we just return the type found.
                Ok((t.clone(), HashMap::new()))
            } else {
                Err(InferenceError::UnboundVariable(name.clone()))
            }
        }

        Expr::Lambda(param_name, body) => {
            // 1. Create a fresh type variable for the parameter
            let param_type = new_type_var();

            // 2. Create a new environment with the parameter bound to its type variable
            let mut body_env = env.clone();
            body_env.insert(param_name.clone(), param_type.clone());

            // 3. Infer the type of the body in the new environment
            let (body_type, body_subst) = infer(&body_env, body)?;

            // 4. The parameter type might have been refined by unification within the body.
            //    Apply the substitution from the body inference to the parameter type.
            let refined_param_type = apply_subst(&body_subst, &param_type);

            // 5. The lambda's type is Fun(refined_param_type -> body_type)
            let lambda_type = Type::Fun(Box::new(refined_param_type), Box::new(body_type));

            // 6. Return the lambda type and the substitution derived from the body
            Ok((lambda_type, body_subst))
        }

        Expr::App(func_expr, arg_expr) => {
            // 1. Infer the type of the function expression
            let (func_type, s1) = infer(env, func_expr)?;
            // Apply substitution s1 to the environment before inferring the argument
            let env1 = apply_subst_to_env(&s1, env);

            // 2. Infer the type of the argument expression in the updated environment
            let (arg_type, s2) = infer(&env1, arg_expr)?;
            // Apply substitution s2 to the function type derived so far
            let func_type_s2 = apply_subst(&s2, &func_type);

            // 3. Create a fresh type variable for the result of the application
            let return_type_var = new_type_var();

            // 4. Unify the inferred function type (after substitutions) with a
            //    hypothetical function type: arg_type -> return_type_var
            let expected_func_type = Type::Fun(
                Box::new(arg_type.clone()),
                Box::new(return_type_var.clone()),
            );
            let s3 = unify(&func_type_s2, &expected_func_type)?;

            // 5. The final type of the application is the return type variable,
            //    after applying the unification substitution s3.
            let final_return_type = apply_subst(&s3, &return_type_var);

            // 6. Compose all substitutions: s3 after s2 after s1
            let final_subst = compose_subst(&s3, &compose_subst(&s2, &s1));

            Ok((final_return_type, final_subst))
        }

        Expr::Let(name, value_expr, body_expr) => {
            // 1. Infer the type of the value expression
            let (value_type, s1) = infer(env, value_expr)?;

            // 2. Create a new environment with the name bound to the inferred type
            //    Apply the substitution s1 to the environment first
            let mut body_env = apply_subst_to_env(&s1, env);
            // Apply s1 to the value_type as well before adding it
            let bound_type = apply_subst(&s1, &value_type);
            body_env.insert(name.clone(), bound_type);

            // 3. Infer the type of the body expression in the new environment
            let (body_type, s2) = infer(&body_env, body_expr)?;

            // 4. Compose the substitutions
            let final_subst = compose_subst(&s2, &s1);

            // 5. The result is the type of the body and the combined substitution
            Ok((body_type, final_subst))
        }
    }
}

// Top-level inference function
fn infer_top_level(expr: &Expr) -> Result<Type, InferenceError> {
    let initial_env = TypeEnvironment::new();
    // Reset type variable counter for consistent examples
    NEXT_TYPE_VAR_ID.store(0, Ordering::SeqCst);
    let (inferred_type, final_subst) = infer(&initial_env, expr)?;
    // Apply the final substitution to the inferred type to get the most specific type
    Ok(apply_subst(&final_subst, &inferred_type))
}

// --- Main Function with Examples ---

fn main() {
    println!("--- Basic Examples ---");

    // Example 1: Simple integer
    let expr1 = Expr::Int(42);
    match infer_top_level(&expr1) {
        Ok(t) => println!("Expr: Int(42), Inferred Type: {}", t), // int
        Err(e) => println!("Error: {}", e),
    }

    // Example 2: Simple string
    let expr2 = Expr::Str("hello".to_string());
    match infer_top_level(&expr2) {
        Ok(t) => println!("Expr: Str(\"hello\"), Inferred Type: {}", t), // string
        Err(e) => println!("Error: {}", e),
    }

    println!("\n--- Lambda Examples ---");

    // Example 3: Identity function: \x -> x
    // Expected: 'a -> 'a
    let expr3 = Expr::Lambda("x".to_string(), Box::new(Expr::Var("x".to_string())));
    match infer_top_level(&expr3) {
        Ok(t) => println!(r#"Expr: \x -> x, Inferred Type: {t}"#),
        Err(e) => println!("Error: {}", e),
    }

    // Example 4: Constant function: \y -> 5
    // Expected: 'a -> int
    let expr4 = Expr::Lambda("y".to_string(), Box::new(Expr::Int(5)));
    match infer_top_level(&expr4) {
        Ok(t) => println!("Expr: \\y -> 5, Inferred Type: {}", t),
        Err(e) => println!("Error: {}", e),
    }

    // Example 5: Function application: (\x -> x) (5)
    // Expected: int
    let expr5 = Expr::App(
        Box::new(Expr::Lambda(
            "x".to_string(),
            Box::new(Expr::Var("x".to_string())),
        )),
        Box::new(Expr::Int(5)),
    );
    match infer_top_level(&expr5) {
        Ok(t) => println!("Expr: (\\x -> x)(5), Inferred Type: {}", t),
        Err(e) => println!("Error: {}", e),
    }

    // Example 6: Function application: (\y -> "hello") (10)
    // Expected: string (parameter type 'a unifies with int, but result is string)
    let expr6 = Expr::App(
        Box::new(Expr::Lambda(
            "y".to_string(),
            Box::new(Expr::Str("hello".to_string())),
        )),
        Box::new(Expr::Int(10)),
    );
    match infer_top_level(&expr6) {
        Ok(t) => println!(r#"Expr: (\y -> "hello")(10), Inferred Type: {t}"#),
        Err(e) => println!("Error: {}", e),
    }

    println!("\n--- Let Binding Examples ---");

    // Example 7: let id = \x -> x in id(5)
    // Expected: int
    let expr7 = Expr::Let(
        "id".to_string(),
        Box::new(Expr::Lambda(
            "x".to_string(),
            Box::new(Expr::Var("x".to_string())),
        )), // \x -> x
        Box::new(Expr::App(
            // id(5)
            Box::new(Expr::Var("id".to_string())),
            Box::new(Expr::Int(5)),
        )),
    );
    match infer_top_level(&expr7) {
        Ok(t) => println!("Expr: let id = \\x -> x in id(5), Inferred Type: {}", t),
        Err(e) => println!("Error: {}", e),
    }

    // Example 8: let five = 5 in (\f -> f(five)) (\y -> "done")
    // Expected: string
    let expr8 = Expr::Let(
        "five".to_string(),
        Box::new(Expr::Int(5)), // five = 5
        Box::new(Expr::App(
            // (\f -> f(five)) (\y -> "done")
            Box::new(Expr::Lambda(
                // \f -> f(five)
                "f".to_string(),
                Box::new(Expr::App(
                    Box::new(Expr::Var("f".to_string())),
                    Box::new(Expr::Var("five".to_string())),
                )),
            )),
            Box::new(Expr::Lambda(
                // \y -> "done"
                "y".to_string(),
                Box::new(Expr::Str("done".to_string())),
            )),
        )),
    );
    match infer_top_level(&expr8) {
        Ok(t) => println!(
            r#"Expr: let five = 5 in (\f -> f(five)) (\y -> "done"), Inferred Type: {}"#,
            t
        ),
        Err(e) => println!("Error: {}", e),
    }

    println!("\n--- Error Example ---");
    // Example 9: Type mismatch: applying an integer
    // Expected: Error
    let expr9 = Expr::App(Box::new(Expr::Int(10)), Box::new(Expr::Int(5)));
    match infer_top_level(&expr9) {
        Ok(t) => println!("Expr: 10(5), Inferred Type: {}", t),
        Err(e) => println!("Expr: 10(5), Error: {}", e), // Expected: NotAFunction or TypeMismatch
    }
}
