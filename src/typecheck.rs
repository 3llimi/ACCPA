use crate::ast::*;
use crate::error::TypeError;
use std::collections::HashMap;

type TypeEnv = HashMap<String, Type>;

pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    let mut fn_env: HashMap<String, Type> = HashMap::new();
    
    for decl in &program.decls {
        match decl {
            Decl::DeclFun { name, param_decls, return_type, .. } => {
                let fn_type = build_function_type(param_decls, return_type)?;
                fn_env.insert(name.clone(), fn_type);
            }
            _ => {}
        }
    }
    
    if !fn_env.contains_key("main") {
        return Err(TypeError::ErrorMissingMain);
    }
    
    if let Some(main_type) = fn_env.get("main") {
        if !matches!(main_type, Type::Fun(_, _)) {
            return Err(TypeError::ErrorIncorrectTypeOfMain);
        }
    }
    
    for decl in &program.decls {
        typecheck_decl(decl, &fn_env)?;
    }
    
    Ok(())
}

fn build_function_type(params: &[ParamDecl], return_type: &Option<Type>) -> Result<Type, TypeError> {
    let param_types: Vec<Type> = params.iter().map(|p| p.type_.clone()).collect();
    let ret_type = return_type.clone().unwrap_or(Type::Unit);
    
    Ok(Type::Fun(param_types, Box::new(ret_type)))
}

fn typecheck_decl(decl: &Decl, fn_env: &HashMap<String, Type>) -> Result<(), TypeError> {
    match decl {
        Decl::DeclFun {
            param_decls,
            return_type,
            local_decls,
            return_expr,
            ..
        } => {
            let mut env: TypeEnv = HashMap::new();
            
            for param in param_decls {
                env.insert(param.name.clone(), param.type_.clone());
            }
            
            for (name, ty) in fn_env {
                env.insert(name.clone(), ty.clone());
            }
            
            for local_decl in local_decls {
                typecheck_decl(local_decl, fn_env)?;
            }
            
            let expected_return_type = return_type.clone().unwrap_or(Type::Unit);
            typecheck_expr(return_expr, &expected_return_type, &env)?;
            
            Ok(())
        }
        Decl::DeclGenericFun { .. } => Ok(()),
        Decl::DeclTypeAlias { .. } => Ok(()),
        Decl::DeclExceptionType(_) => Ok(()),
        Decl::DeclExceptionVariant { .. } => Ok(()),
    }
}

fn typecheck_expr(expr: &Expr, expected: &Type, env: &TypeEnv) -> Result<(), TypeError> {
    match expr {
        Expr::ConstTrue | Expr::ConstFalse => {
            if *expected != Type::Bool {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Bool,
                    expr: Some(format!("{:?}", expr)),
                });
            }
            Ok(())
        }
        
        Expr::ConstUnit => {
            if *expected != Type::Unit {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Unit,
                    expr: Some(format!("{:?}", expr)),
                });
            }
            Ok(())
        }
        
        Expr::ConstInt(_) => {
            if *expected != Type::Nat {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Nat,
                    expr: Some(format!("{:?}", expr)),
                });
            }
            Ok(())
        }
        
        Expr::Succ(n) => {
            if *expected != Type::Nat {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Nat,
                    expr: None,
                });
            }
            typecheck_expr(n, &Type::Nat, env)
        }
        
        Expr::Var(name) => {
            let var_type = env.get(name)
                .ok_or_else(|| TypeError::ErrorUndefinedVariable(name.clone()))?;
            
            if var_type != expected {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: var_type.clone(),
                    expr: Some(name.clone()),
                });
            }
            Ok(())
        }
        
        Expr::If(cond, then_branch, else_branch) => {
            typecheck_expr(cond, &Type::Bool, env)?;
            typecheck_expr(then_branch, expected, env)?;
            typecheck_expr(else_branch, expected, env)?;
            Ok(())
        }
        
        Expr::Abstraction(params, body) => {
            match expected {
                Type::Fun(param_types, return_type) => {
                    if params.len() != param_types.len() {
                        return Err(TypeError::ErrorUnexpectedLambda(expected.clone()));
                    }
                    
                    for (param, expected_param_type) in params.iter().zip(param_types.iter()) {
                        if &param.type_ != expected_param_type {
                            return Err(TypeError::ErrorUnexpectedTypeForParameter {
                                expected: expected_param_type.clone(),
                                found: param.type_.clone(),
                            });
                        }
                    }
                    
                    let mut new_env = env.clone();
                    for param in params {
                        new_env.insert(param.name.clone(), param.type_.clone());
                    }
                    
                    typecheck_expr(body, return_type, &new_env)
                }
                _ => Err(TypeError::ErrorUnexpectedLambda(expected.clone())),
            }
        }
        
        Expr::Application(func, args) => {
            let func_type = infer_expr(func, env)?;
            
            match func_type {
                Type::Fun(param_types, return_type) => {
                    if return_type.as_ref() != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: return_type.as_ref().clone(),
                            expr: None,
                        });
                    }
                    
                    if args.len() != param_types.len() {
                        return Err(TypeError::ErrorNotAFunction(Type::Fun(param_types, return_type)));
                    }
                    
                    for (arg, param_type) in args.iter().zip(param_types.iter()) {
                        typecheck_expr(arg, param_type, env)?;
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotAFunction(func_type)),
            }
        }
        
        Expr::NatPred(n) => {
            if *expected != Type::Nat {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Nat,
                    expr: None,
                });
            }
            typecheck_expr(n, &Type::Nat, env)
        }
        
        Expr::NatIsZero(n) => {
            if *expected != Type::Bool {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Bool,
                    expr: None,
                });
            }
            typecheck_expr(n, &Type::Nat, env)
        }
        
        Expr::NatRec(n, z, s) => {
            typecheck_expr(n, &Type::Nat, env)?;
            typecheck_expr(z, expected, env)?;
            
            // Step function: Nat → (T → T)
            let inner_fn = Type::Fun(vec![expected.clone()], Box::new(expected.clone()));
            let s_type = Type::Fun(vec![Type::Nat], Box::new(inner_fn));
            typecheck_expr(s, &s_type, env)
        }
        
        Expr::Tuple(elements) => {
            match expected {
                Type::Tuple(expected_types) => {
                    if elements.len() != expected_types.len() {
                        return Err(TypeError::ErrorUnexpectedTupleLength {
                            expected: expected_types.len(),
                            found: elements.len(),
                        });
                    }
                    
                    for (elem, expected_ty) in elements.iter().zip(expected_types.iter()) {
                        typecheck_expr(elem, expected_ty, env)?;
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorUnexpectedTuple(expected.clone())),
            }
        }
        
        Expr::DotTuple(tuple_expr, index) => {
            let tuple_type = infer_expr(tuple_expr, env)?;
            
            match tuple_type {
                Type::Tuple(types) => {
                    if *index == 0 || *index > types.len() {
                        return Err(TypeError::ErrorTupleIndexOutOfBounds {
                            index: *index,
                            length: types.len(),
                        });
                    }
                    
                    let field_type = &types[*index - 1];
                    if field_type != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: field_type.clone(),
                            expr: None,
                        });
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotATuple(tuple_type)),
            }
        }
        
        Expr::Record(bindings) => {
            match expected {
                Type::Record(expected_fields) => {
                    check_duplicate_record_fields(bindings)?;
                    
                    for expected_field in expected_fields {
                        let binding = bindings.iter()
                            .find(|b| b.name == expected_field.label)
                            .ok_or_else(|| TypeError::ErrorMissingRecordFields(vec![expected_field.label.clone()]))?;
                        
                        typecheck_expr(&binding.expr, &expected_field.type_, env)?;
                    }
                    
                    for binding in bindings {
                        if !expected_fields.iter().any(|f| f.label == binding.name) {
                            return Err(TypeError::ErrorUnexpectedRecordFields(vec![binding.name.clone()]));
                        }
                    }
                    
                    Ok(())
                }
                _ => Err(TypeError::ErrorUnexpectedRecord(expected.clone())),
            }
        }
        
        Expr::DotRecord(record_expr, field_name) => {
            let record_type = infer_expr(record_expr, env)?;
            
            match record_type {
                Type::Record(fields) => {
                    let field = fields.iter()
                        .find(|f| f.label == *field_name)
                        .ok_or_else(|| TypeError::ErrorUnexpectedFieldAccess(field_name.clone()))?;
                    
                    if &field.type_ != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: field.type_.clone(),
                            expr: None,
                        });
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotARecord(record_type)),
            }
        }
        
        Expr::TypeAscription(e, ty) => {
            if ty != expected {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: ty.clone(),
                    expr: None,
                });
            }
            typecheck_expr(e, ty, env)
        }
        
        Expr::Inl(e) => {
            match expected {
                Type::Sum(left_type, _right_type) => {
                    typecheck_expr(e, left_type, env)
                }
                _ => Err(TypeError::ErrorUnexpectedInjection(expected.clone())),
            }
        }
        
        Expr::Inr(e) => {
            match expected {
                Type::Sum(_left_type, right_type) => {
                    typecheck_expr(e, right_type, env)
                }
                _ => Err(TypeError::ErrorUnexpectedInjection(expected.clone())),
            }
        }
        
        Expr::Match(scrutinee, cases) => {
            if cases.is_empty() {
                return Err(TypeError::ErrorIllegalEmptyMatching);
            }
            
            let scrutinee_type = infer_expr(scrutinee, env)?;
            
            match &scrutinee_type {
                Type::Sum(_, _) => {
                    let has_inl = cases.iter().any(|c| matches!(c.pattern, Pattern::Inl(_)));
                    let has_inr = cases.iter().any(|c| matches!(c.pattern, Pattern::Inr(_)));
                    
                    if !has_inl || !has_inr {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                _ => {}
            }
            
            for case in cases {
                let pattern_env = typecheck_pattern(&case.pattern, &scrutinee_type, env)?;
                let mut case_env = env.clone();
                case_env.extend(pattern_env);
                typecheck_expr(&case.expr, expected, &case_env)?;
            }
            
            Ok(())
        }
        
        Expr::List(elements) => {
            match expected {
                Type::List(elem_type) => {
                    for elem in elements {
                        typecheck_expr(elem, elem_type, env)?;
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorUnexpectedList(expected.clone())),
            }
        }
        
        Expr::Cons(head, tail) => {
            match expected {
                Type::List(elem_type) => {
                    typecheck_expr(head, elem_type, env)?;
                    typecheck_expr(tail, expected, env)
                }
                _ => Err(TypeError::ErrorUnexpectedList(expected.clone())),
            }
        }
        
        Expr::ListHead(list) => {
            let list_type = infer_expr(list, env)?;
            match list_type {
                Type::List(elem_type) => {
                    if elem_type.as_ref() != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: elem_type.as_ref().clone(),
                            expr: None,
                        });
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotAList(list_type)),
            }
        }
        
        Expr::ListTail(list) => {
            let list_type = infer_expr(list, env)?;
            match list_type {
                Type::List(_) => {
                    if &list_type != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: list_type,
                            expr: None,
                        });
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotAList(list_type)),
            }
        }
        
        Expr::ListIsEmpty(list) => {
            if *expected != Type::Bool {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.clone(),
                    found: Type::Bool,
                    expr: None,
                });
            }
            
            let inferred = infer_expr(list, env)?;
            match inferred {
                Type::List(_) => Ok(()),
                _ => Err(TypeError::ErrorNotAList(inferred)),
            }
        }
        
        Expr::Variant(label, opt_expr) => {
            match expected {
                Type::Variant(fields) => {
                    let field = fields.iter()
                        .find(|f| f.label == *label)
                        .ok_or_else(|| TypeError::ErrorUnexpectedVariantLabel(label.clone()))?;
                    
                    match (&field.type_, opt_expr) {
                        (Some(expected_type), Some(expr)) => {
                            typecheck_expr(expr, expected_type, env)
                        }
                        (None, None) => Ok(()),
                        _ => Err(TypeError::ErrorUnexpectedVariantLabel(label.clone())),
                    }
                }
                _ => Err(TypeError::ErrorUnexpectedVariant(expected.clone())),
            }
        }
        
        Expr::Let(bindings, body) => {
            let mut new_env = env.clone();
            
            for binding in bindings {
                let pat_type = infer_expr(&binding.rhs, env)?;
                let pat_env = typecheck_pattern(&binding.pattern, &pat_type, env)?;
                new_env.extend(pat_env);
            }
            
            typecheck_expr(body, expected, &new_env)
        }
        
        Expr::Fix(f) => {
            let f_type = infer_expr(f, env)?;
            
            match &f_type { 
                Type::Fun(param_types, return_type) => {
                    if param_types.len() != 1 || param_types[0] != **return_type { 
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: Type::Fun(vec![expected.clone()], Box::new(expected.clone())),
                            found: f_type.clone(),
                            expr: None,
                        });
                    }
                    if return_type.as_ref() != expected {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: expected.clone(),
                            found: return_type.as_ref().clone(),
                            expr: None,
                        });
                    }
                    Ok(())
                }
                _ => Err(TypeError::ErrorNotAFunction(f_type)),
            }
        }
        
        _ => Ok(()),
    }
}

fn infer_expr(expr: &Expr, env: &TypeEnv) -> Result<Type, TypeError> {
    match expr {
        Expr::ConstTrue | Expr::ConstFalse => Ok(Type::Bool),
        Expr::ConstUnit => Ok(Type::Unit),
        Expr::ConstInt(_) => Ok(Type::Nat),
        Expr::Succ(_) => Ok(Type::Nat),
        
        Expr::Var(name) => {
            env.get(name)
                .cloned()
                .ok_or_else(|| TypeError::ErrorUndefinedVariable(name.clone()))
        }
        
        Expr::Abstraction(params, body) => {
            let mut new_env = env.clone();
            for param in params {
                new_env.insert(param.name.clone(), param.type_.clone());
            }
            
            let return_type = infer_expr(body, &new_env)?;
            let param_types: Vec<Type> = params.iter().map(|p| p.type_.clone()).collect();
            Ok(Type::Fun(param_types, Box::new(return_type)))
        }
        
        Expr::Application(func, args) => {
            let func_type = infer_expr(func, env)?;

            match func_type {
                Type::Fun(param_types, return_type) => {

                    if args.len() != param_types.len() {
                        return Err(TypeError::ErrorNotAFunction(
                            Type::Fun(param_types, return_type),
                        ));
                    }
                    for (arg, param_ty) in args.iter().zip(param_types.iter()) {
                        let arg_ty = infer_expr(arg, env)?;

                        if arg_ty != *param_ty {
                            return Err(TypeError::ErrorUnexpectedTypeForParameter {
                                expected: param_ty.clone(),
                                found: arg_ty,
                            });
                        }
                    }

                    Ok(*return_type)
                }

                _ => Err(TypeError::ErrorNotAFunction(func_type)),
            }
        }
        
        Expr::Tuple(elements) => {
            let types: Result<Vec<_>, _> = elements.iter()
                .map(|e| infer_expr(e, env))
                .collect();
            Ok(Type::Tuple(types?))
        }
        
        Expr::Record(bindings) => {
            let mut fields = Vec::new();
            for binding in bindings {
                let field_ty = infer_expr(&binding.expr, env)?;
                fields.push(RecordFieldType {
                    label: binding.name.clone(),
                    type_: field_ty,
                });
            }
            Ok(Type::Record(fields))
        }
        
        Expr::DotTuple(tuple_expr, index) => {
            let tuple_type = infer_expr(tuple_expr, env)?;
            match tuple_type {
                Type::Tuple(types) => {
                    if *index == 0 || *index > types.len() {
                        return Err(TypeError::ErrorTupleIndexOutOfBounds {
                            index: *index,
                            length: types.len(),
                        });
                    }
                    Ok(types[*index - 1].clone())
                }
                _ => Err(TypeError::ErrorNotATuple(tuple_type)),
            }
        }
        
        Expr::TypeAscription(_e, ty) => Ok(ty.clone()),
        Expr::If(_, then_branch, _) => infer_expr(then_branch, env),
        
        Expr::List(elements) => {
            if elements.is_empty() {
                Err(TypeError::ErrorAmbiguousListType)
            } else {
                let elem_type = infer_expr(&elements[0], env)?;
                Ok(Type::List(Box::new(elem_type)))
            }
        }
        
        Expr::Inl(_) | Expr::Inr(_) => Err(TypeError::ErrorAmbiguousSumType),
        Expr::Variant(_, _) => Err(TypeError::ErrorAmbiguousVariantType),
        
        Expr::Fix(f) => {
            let f_ty = infer_expr(f, env)?;
            match f_ty {
                Type::Fun(params, ret) if params.len() == 1 => Ok(*ret),
                _ => Err(TypeError::ErrorNotAFunction(f_ty)),
            }
        }
        
        Expr::NatRec(n, z, _s) => {
            infer_expr(z, env)
        }

        Expr::DotRecord(record_expr, field_name) => {
            let record_type = infer_expr(record_expr, env)?;
            match record_type {
                Type::Record(fields) => {
                    fields.iter()
                        .find(|f| f.label == *field_name)
                        .map(|f| f.type_.clone())
                        .ok_or_else(|| TypeError::ErrorUnexpectedFieldAccess(field_name.clone()))
                }
                _ => Err(TypeError::ErrorNotARecord(record_type)),
            }
        },
        
        _ => Err(TypeError::ErrorUnexpectedTypeForExpression {
            expected: Type::Bottom,
            found: Type::Bottom,
            expr: Some(format!("No inference: {:?}", expr)),
        }),
    }
}


fn typecheck_pattern(
    pattern: &Pattern,
    expected_type: &Type,
    _env: &TypeEnv,
) -> Result<TypeEnv, TypeError> {
    let mut new_env = TypeEnv::new();
    
    match pattern {
        Pattern::Var(name) => {
            new_env.insert(name.clone(), expected_type.clone());
            Ok(new_env)
        }
        
        Pattern::Inl(p) => {
            match expected_type {
                Type::Sum(left_type, _) => {
                    typecheck_pattern(p, left_type, _env)
                }
                _ => Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: "inl".to_string(),
                }),
            }
        }
        
        Pattern::Inr(p) => {
            match expected_type {
                Type::Sum(_, right_type) => {
                    typecheck_pattern(p, right_type, _env)
                }
                _ => Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: "inr".to_string(),
                }),
            }
        }
        
        _ => Ok(new_env),
    }
}

fn check_duplicate_record_fields(bindings: &[Binding]) -> Result<(), TypeError> {
    let mut seen = std::collections::HashSet::new();
    let mut duplicates = Vec::new();
    
    for binding in bindings {
        if !seen.insert(&binding.name) {
            duplicates.push(binding.name.clone());
        }
    }
    
    if !duplicates.is_empty() {
        return Err(TypeError::ErrorDuplicateRecordFields(duplicates));
    }
    
    Ok(())
}