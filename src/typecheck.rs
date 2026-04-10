use crate::ast::*;
use crate::error::TypeError;
use std::collections::{HashMap, HashSet};

type TypeEnv = HashMap<String, Type>;

#[derive(Debug, Clone)]
struct TypeCheckContext {
    extensions: HashSet<String>,
    exception_type: Option<Type>,
}

impl TypeCheckContext {
    fn has_extension(&self, ext: &str) -> bool {
        self.extensions.contains(ext)
    }
}

// STEP 1: typecheck_program
//   - Collect top-level function signatures into fn_env
//   - Parse active extensions into checker context
//   - Collect optional exception type declaration
//   - Type-check every declaration (first error stops checking)
pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    let mut fn_env: HashMap<String, Type> = HashMap::new();

    for decl in &program.decls {
        if let Decl::DeclFun {
            name,
            param_decls,
            return_type,
            ..
        } = decl
        {
            let fn_type = build_function_type(param_decls, return_type)?;
            fn_env.insert(name.clone(), fn_type);
        }
    }

    if !fn_env.contains_key("main") {
        return Err(TypeError::ErrorMissingMain);
    }

    let mut extensions = HashSet::new();
    for ext in &program.extensions {
        for name in &ext.extension_names {
            extensions.insert(normalize_extension_name(name));
        }
    }

    let mut ctx = TypeCheckContext {
        extensions,
        exception_type: None,
    };

    let mut seen_exception_type = false;
    let mut exception_variants: Vec<VariantFieldType> = Vec::new();
    let mut seen_exception_variant_labels: HashSet<String> = HashSet::new();

    for decl in &program.decls {
        match decl {
            Decl::DeclExceptionType(ty) => {
                if seen_exception_type {
                    return Err(TypeError::ErrorDuplicateExceptionType);
                }
                check_type_validity(ty)?;
                ctx.exception_type = Some(ty.clone());
                seen_exception_type = true;
            }
            Decl::DeclExceptionVariant { name, type_ } => {
                check_type_validity(type_)?;
                if !seen_exception_variant_labels.insert(name.clone()) {
                    return Err(TypeError::ErrorDuplicateExceptionVariant(name.clone()));
                }
                exception_variants.push(VariantFieldType {
                    label: name.clone(),
                    type_: Some(type_.clone()),
                });
            }
            _ => {}
        }
    }

    if seen_exception_type && !exception_variants.is_empty() {
        return Err(TypeError::ErrorConflictingExceptionDeclarations);
    }

    if ctx.exception_type.is_none() && !exception_variants.is_empty() {
        ctx.exception_type = Some(Type::Variant(exception_variants));
    }

    for decl in &program.decls {
        typecheck_decl(decl, &fn_env, &ctx)?;
    }

    Ok(())
}

fn normalize_extension_name(name: &str) -> String {
    name.trim_start_matches('#').trim().to_string()
}

fn build_function_type(params: &[ParamDecl], return_type: &Option<Type>) -> Result<Type, TypeError> {
    let param_types: Vec<Type> = params.iter().map(|p| p.type_.clone()).collect();
    let ret_type = return_type.clone().unwrap_or(Type::Unit);
    Ok(Type::Fun(param_types, Box::new(ret_type)))
}

// STEP 2: typecheck_decl
//   - Validate declared parameter/return types
//   - Reject illegal local exception type/variant declarations
//   - Build local environment from params + top-level + local functions
//   - Check function return expression against declared return type
fn typecheck_decl(decl: &Decl, fn_env: &HashMap<String, Type>, ctx: &TypeCheckContext) -> Result<(), TypeError> {
    match decl {
        Decl::DeclFun {
            name,
            param_decls,
            return_type,
            local_decls,
            return_expr,
            ..
        } => {
            for param in param_decls {
                check_type_validity(&param.type_).map_err(|e| with_function_context(e, name))?;
            }
            if let Some(ret_ty) = return_type {
                check_type_validity(ret_ty).map_err(|e| with_function_context(e, name))?;
            }

            for local_decl in local_decls {
                match local_decl {
                    Decl::DeclExceptionType(_) => {
                        return Err(with_function_context(
                            TypeError::ErrorIllegalLocalExceptionType,
                            name,
                        ));
                    }
                    Decl::DeclExceptionVariant { .. } => {
                        return Err(with_function_context(
                            TypeError::ErrorIllegalLocalOpenVariantException,
                            name,
                        ));
                    }
                    _ => {}
                }
            }

            // Local function declarations are in scope for this function body.
            let mut local_fn_env = fn_env.clone();
            for local_decl in local_decls {
                if let Decl::DeclFun {
                    name: local_name,
                    param_decls: local_params,
                    return_type: local_return_type,
                    ..
                } = local_decl
                {
                    let local_fn_type = build_function_type(local_params, local_return_type)
                        .map_err(|e| with_function_context(e, name))?;
                    local_fn_env.insert(local_name.clone(), local_fn_type);
                }
            }

            let mut env: TypeEnv = HashMap::new();
            for param in param_decls {
                env.insert(param.name.clone(), param.type_.clone());
            }
            for (name, ty) in &local_fn_env {
                env.insert(name.clone(), ty.clone());
            }

            for local_decl in local_decls {
                typecheck_decl(local_decl, &local_fn_env, ctx)
                    .map_err(|e| with_function_context(e, name))?;
            }

            let expected_return_type = return_type.clone().unwrap_or(Type::Unit);
            let inferred = infer_expr(return_expr, Some(&expected_return_type), &env, ctx)
                .map_err(|e| with_function_context(e, name))?;
            ensure_expected(return_expr, &inferred, &expected_return_type, ctx)
                .map_err(|e| with_function_context(e, name))
        }
        Decl::DeclGenericFun { .. } => Ok(()),
        Decl::DeclTypeAlias { .. } => Ok(()),
        Decl::DeclExceptionType(_) => Ok(()),
        Decl::DeclExceptionVariant { .. } => Ok(()),
    }
}

fn with_function_context(err: TypeError, function_name: &str) -> TypeError {
    match err {
        TypeError::ErrorUndefinedVariable(var) => {
            TypeError::ErrorUndefinedVariable(format!("{} [in function {}]", var, function_name))
        }
        TypeError::ErrorUnexpectedTypeForExpression {
            expected,
            found,
            expr,
        } => TypeError::ErrorUnexpectedTypeForExpression {
            expected,
            found,
            expr: Some(attach_context(expr, function_name)),
        },
        TypeError::ErrorUnexpectedSubtype {
            expected,
            found,
            expr,
        } => TypeError::ErrorUnexpectedSubtype {
            expected,
            found,
            expr: Some(attach_context(expr, function_name)),
        },
        TypeError::ErrorUnexpectedPatternForType { expected, pattern } => {
            TypeError::ErrorUnexpectedPatternForType {
                expected,
                pattern: format!("{} [in function {}]", pattern, function_name),
            }
        }
        other => other,
    }
}

fn attach_context(expr: Option<String>, function_name: &str) -> String {
    match expr {
        Some(e) => format!("{} [in function {}]", e, function_name),
        None => format!("[in function {}]", function_name),
    }
}

// STEP 3: infer_expr
//   - Infer expression type bottom-up
//   - Use expected type as a constraint when provided
//   - Apply Stage 2 rules (references, panic/exceptions, cast, sequencing)
fn infer_expr(
    expr: &Expr,
    expected: Option<&Type>,
    env: &TypeEnv,
    ctx: &TypeCheckContext,
) -> Result<Type, TypeError> {
    let inferred = match expr {
        Expr::ConstTrue | Expr::ConstFalse => Type::Bool,
        Expr::ConstUnit => Type::Unit,
        Expr::ConstInt(_) => Type::Nat,

        Expr::ConstMemory(_) => match expected {
            Some(Type::Ref(inner)) => Type::Ref(inner.clone()),
            Some(_) => return Err(TypeError::ErrorUnexpectedMemoryAddress),
            None => return Err(TypeError::ErrorAmbiguousReferenceType),
        },

        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::ErrorUndefinedVariable(name.clone()))?,

        Expr::Succ(n) | Expr::NatPred(n) => {
            infer_expr(n, Some(&Type::Nat), env, ctx)?;
            Type::Nat
        }

        Expr::NatIsZero(n) => {
            infer_expr(n, Some(&Type::Nat), env, ctx)?;
            Type::Bool
        }

        Expr::If(cond, then_branch, else_branch) => {
            infer_expr(cond, Some(&Type::Bool), env, ctx)?;
            match expected {
                Some(expected_ty) => {
                    infer_expr(then_branch, Some(expected_ty), env, ctx)?;
                    infer_expr(else_branch, Some(expected_ty), env, ctx)?;
                    expected_ty.clone()
                }
                None => {
                    let then_ty = infer_expr(then_branch, None, env, ctx)?;
                    infer_expr(else_branch, Some(&then_ty), env, ctx)?;
                    then_ty
                }
            }
        }

        Expr::Abstraction(params, body) => match expected {
            Some(Type::Fun(param_types, return_type)) => {
                if params.len() != param_types.len() {
                    return Err(TypeError::ErrorUnexpectedLambda(Type::Fun(
                        param_types.clone(),
                        return_type.clone(),
                    )));
                }

                let mut new_env = env.clone();
                for (param, expected_param_type) in params.iter().zip(param_types.iter()) {
                    if &param.type_ != expected_param_type {
                        return Err(TypeError::ErrorUnexpectedTypeForParameter {
                            expected: expected_param_type.clone(),
                            found: param.type_.clone(),
                        });
                    }
                    new_env.insert(param.name.clone(), param.type_.clone());
                }

                let body_ty = infer_expr(body, Some(return_type), &new_env, ctx)?;
                ensure_expected(body, &body_ty, return_type, ctx)?;
                Type::Fun(param_types.clone(), return_type.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedLambda(other.clone())),
            None => {
                let mut new_env = env.clone();
                for param in params {
                    new_env.insert(param.name.clone(), param.type_.clone());
                }
                let return_type = infer_expr(body, None, &new_env, ctx)?;
                let param_types: Vec<Type> = params.iter().map(|p| p.type_.clone()).collect();
                Type::Fun(param_types, Box::new(return_type))
            }
        },

        Expr::Application(func, args) => {
            let func_type = infer_expr(func, None, env, ctx)?;
            match func_type {
                Type::Fun(param_types, return_type) => {
                    if args.len() != param_types.len() {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: Type::Fun(param_types.clone(), return_type.clone()),
                            found: Type::Fun(param_types.clone(), return_type.clone()),
                            expr: Some(format!(
                                "Function expects {} arguments but got {}",
                                param_types.len(),
                                args.len()
                            )),
                        });
                    }

                    for (arg, param_type) in args.iter().zip(param_types.iter()) {
                    let arg_ty = infer_expr(arg, Some(param_type), env, ctx)?;
                    ensure_expected(arg, &arg_ty, param_type, ctx)?;
                    }
                    *return_type
                }
                _ => return Err(TypeError::ErrorNotAFunction(func_type)),
            }
        }

        Expr::NatRec(n, z, s) => {
            infer_expr(n, Some(&Type::Nat), env, ctx)?;
            let z_ty = infer_expr(z, expected, env, ctx)?;
            let step_ty = Type::Fun(
                vec![Type::Nat],
                Box::new(Type::Fun(vec![z_ty.clone()], Box::new(z_ty.clone()))),
            );
            infer_expr(s, Some(&step_ty), env, ctx)?;
            z_ty
        }

        Expr::Tuple(elements) => match expected {
            Some(Type::Tuple(expected_types)) => {
                if elements.len() != expected_types.len() {
                    return Err(TypeError::ErrorUnexpectedTupleLength {
                        expected: expected_types.len(),
                        found: elements.len(),
                    });
                }
                for (elem, expected_ty) in elements.iter().zip(expected_types.iter()) {
                    infer_expr(elem, Some(expected_ty), env, ctx)?;
                }
                Type::Tuple(expected_types.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedTuple(other.clone())),
            None => {
                let mut inferred = Vec::with_capacity(elements.len());
                for elem in elements {
                    inferred.push(infer_expr(elem, None, env, ctx)?);
                }
                Type::Tuple(inferred)
            }
        },

        Expr::DotTuple(tuple_expr, index) => {
            let tuple_type = infer_expr(tuple_expr, None, env, ctx)?;
            match tuple_type {
                Type::Tuple(types) => {
                    if *index == 0 || *index > types.len() {
                        return Err(TypeError::ErrorTupleIndexOutOfBounds {
                            index: *index,
                            length: types.len(),
                        });
                    }
                    types[*index - 1].clone()
                }
                _ => return Err(TypeError::ErrorNotATuple(tuple_type)),
            }
        }

        Expr::Record(bindings) => match expected {
            Some(Type::Record(expected_fields)) => {
                check_duplicate_record_fields(bindings)?;

                for expected_field in expected_fields {
                    let binding = bindings
                        .iter()
                        .find(|b| b.name == expected_field.label)
                        .ok_or_else(|| {
                            TypeError::ErrorMissingRecordFields(vec![expected_field.label.clone()])
                        })?;
                    infer_expr(&binding.expr, Some(&expected_field.type_), env, ctx)?;
                }

                if !ctx.has_extension("structural-subtyping") {
                    for binding in bindings {
                        if !expected_fields.iter().any(|f| f.label == binding.name) {
                            return Err(TypeError::ErrorUnexpectedRecordFields(vec![binding.name.clone()]));
                        }
                    }
                }

                let mut inferred_fields = Vec::with_capacity(bindings.len());
                for b in bindings {
                    inferred_fields.push(RecordFieldType {
                        label: b.name.clone(),
                        type_: infer_expr(&b.expr, None, env, ctx)?,
                    });
                }
                Type::Record(inferred_fields)
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedRecord(other.clone())),
            None => {
                check_duplicate_record_fields(bindings)?;
                let mut fields = Vec::new();
                for binding in bindings {
                    let field_ty = infer_expr(&binding.expr, None, env, ctx)?;
                    fields.push(RecordFieldType {
                        label: binding.name.clone(),
                        type_: field_ty,
                    });
                }
                Type::Record(fields)
            }
        },

        Expr::DotRecord(record_expr, field_name) => {
            let record_type = infer_expr(record_expr, None, env, ctx)?;
            match record_type {
                Type::Record(fields) => fields
                    .iter()
                    .find(|f| f.label == *field_name)
                    .map(|f| f.type_.clone())
                    .ok_or_else(|| TypeError::ErrorUnexpectedFieldAccess(field_name.clone()))?,
                _ => return Err(TypeError::ErrorNotARecord(record_type)),
            }
        }

        Expr::TypeAscription(e, ty) => {
            let inner_ty = infer_expr(e, Some(ty), env, ctx)?;
            ensure_expected(e, &inner_ty, ty, ctx)?;
            ty.clone()
        }

        Expr::Inl(e) => match expected {
            Some(Type::Sum(left_type, right_type)) => {
                infer_expr(e, Some(left_type), env, ctx)?;
                Type::Sum(left_type.clone(), right_type.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedInjection(other.clone())),
            None => {
                if ctx.has_extension("ambiguous-type-as-bottom") {
                    let left = infer_expr(e, None, env, ctx)?;
                    Type::Sum(Box::new(left), Box::new(Type::Bottom))
                } else {
                    return Err(TypeError::ErrorAmbiguousSumType);
                }
            }
        },

        Expr::Inr(e) => match expected {
            Some(Type::Sum(left_type, right_type)) => {
                infer_expr(e, Some(right_type), env, ctx)?;
                Type::Sum(left_type.clone(), right_type.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedInjection(other.clone())),
            None => {
                if ctx.has_extension("ambiguous-type-as-bottom") {
                    let right = infer_expr(e, None, env, ctx)?;
                    Type::Sum(Box::new(Type::Bottom), Box::new(right))
                } else {
                    return Err(TypeError::ErrorAmbiguousSumType);
                }
            }
        },

        Expr::Match(scrutinee, cases) => {
            if cases.is_empty() {
                return Err(TypeError::ErrorIllegalEmptyMatching);
            }

            let scrutinee_type = infer_expr(scrutinee, None, env, ctx)?;

            match &scrutinee_type {
                Type::Sum(_, _) => {
                    let has_inl = cases.iter().any(|c| matches!(c.pattern, Pattern::Inl(_)));
                    let has_inr = cases.iter().any(|c| matches!(c.pattern, Pattern::Inr(_)));
                    if !has_inl || !has_inr {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                Type::Variant(fields) => {
                    let covered_labels: HashSet<String> = cases
                        .iter()
                        .filter_map(|c| {
                            if let Pattern::Variant(label, _) = &c.pattern {
                                Some(label.clone())
                            } else {
                                None
                            }
                        })
                        .collect();
                    let required_labels: HashSet<String> =
                        fields.iter().map(|f| f.label.clone()).collect();
                    if covered_labels != required_labels {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                _ => {}
            }

            let mut result_type: Option<Type> = None;
            for case in cases {
                let pattern_env = typecheck_pattern(&case.pattern, &scrutinee_type, env)?;
                let mut case_env = env.clone();
                case_env.extend(pattern_env);

                if let Some(expected_ty) = expected {
                    infer_expr(&case.expr, Some(expected_ty), &case_env, ctx)?;
                } else {
                    let case_ty = infer_expr(&case.expr, result_type.as_ref(), &case_env, ctx)?;
                    if let Some(acc) = &result_type {
                        ensure_expected(&case.expr, &case_ty, acc, ctx)?;
                    } else {
                        result_type = Some(case_ty);
                    }
                }
            }
            match expected {
                Some(expected_ty) => expected_ty.clone(),
                None => result_type.unwrap_or(Type::Bottom),
            }
        }

        Expr::List(elements) => match expected {
            Some(Type::List(elem_type)) => {
                for elem in elements {
                    infer_expr(elem, Some(elem_type), env, ctx)?;
                }
                Type::List(elem_type.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedList(other.clone())),
            None => {
                if elements.is_empty() {
                    if ctx.has_extension("ambiguous-type-as-bottom") {
                        Type::List(Box::new(Type::Bottom))
                    } else {
                        return Err(TypeError::ErrorAmbiguousListType);
                    }
                } else {
                    let elem_ty = infer_expr(&elements[0], None, env, ctx)?;
                    for elem in &elements[1..] {
                        let actual = infer_expr(elem, Some(&elem_ty), env, ctx)?;
                        ensure_expected(elem, &actual, &elem_ty, ctx)?;
                    }
                    Type::List(Box::new(elem_ty))
                }
            }
        },

        Expr::Cons(head, tail) => match expected {
            Some(Type::List(elem_type)) => {
                infer_expr(head, Some(elem_type), env, ctx)?;
                infer_expr(tail, Some(&Type::List(elem_type.clone())), env, ctx)?;
                Type::List(elem_type.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedList(other.clone())),
            None => {
                let tail_ty = infer_expr(tail, None, env, ctx)?;
                match tail_ty {
                    Type::List(elem_type) => {
                        infer_expr(head, Some(&elem_type), env, ctx)?;
                        Type::List(elem_type)
                    }
                    _ => return Err(TypeError::ErrorNotAList(tail_ty)),
                }
            }
        },

        Expr::ListHead(list) => {
            let list_type = infer_expr(list, None, env, ctx)?;
            match list_type {
                Type::List(elem_type) => *elem_type,
                _ => return Err(TypeError::ErrorNotAList(list_type)),
            }
        }

        Expr::ListTail(list) => {
            let list_type = infer_expr(list, None, env, ctx)?;
            match list_type {
                Type::List(_) => list_type,
                _ => return Err(TypeError::ErrorNotAList(list_type)),
            }
        }

        Expr::ListIsEmpty(list) => {
            let inferred = infer_expr(list, None, env, ctx)?;
            match inferred {
                Type::List(_) => Type::Bool,
                _ => return Err(TypeError::ErrorNotAList(inferred)),
            }
        }

        Expr::Variant(label, opt_expr) => match expected {
            Some(Type::Variant(fields)) => {
                let field = fields
                    .iter()
                    .find(|f| f.label == *label)
                    .ok_or_else(|| TypeError::ErrorUnexpectedVariantLabel(label.clone()))?;
                match (&field.type_, opt_expr) {
                    (Some(expected_type), Some(expr)) => {
                        infer_expr(expr, Some(expected_type), env, ctx)?;
                    }
                    (None, None) => {}
                    _ => return Err(TypeError::ErrorUnexpectedVariantLabel(label.clone())),
                }
                Type::Variant(fields.clone())
            }
            Some(other) => return Err(TypeError::ErrorUnexpectedVariant(other.clone())),
            None => return Err(TypeError::ErrorAmbiguousVariantType),
        },

        Expr::Let(bindings, body) => {
            let mut new_env = env.clone();
            for binding in bindings {
                let rhs_ty = infer_expr(&binding.rhs, None, &new_env, ctx)?;
                let pat_env = typecheck_pattern(&binding.pattern, &rhs_ty, &new_env)?;
                new_env.extend(pat_env);
            }
            infer_expr(body, expected, &new_env, ctx)?
        }

        Expr::Fix(f) => {
            let f_type = infer_expr(f, None, env, ctx)?;
            let f_type_clone = f_type.clone();
            match f_type {
                Type::Fun(param_types, return_type) => {
                    if param_types.len() != 1 || param_types[0] != *return_type {
                        return Err(TypeError::ErrorNotAFunction(f_type_clone));
                    }
                    *return_type
                }
                _ => return Err(TypeError::ErrorNotAFunction(f_type_clone)),
            }
        }

        Expr::Sequence(e1, e2) => {
            infer_expr(e1, Some(&Type::Unit), env, ctx)?;
            infer_expr(e2, expected, env, ctx)?
        }

        Expr::Reference(e) => match expected {
            Some(Type::Ref(inner)) => {
                let found_inner = infer_expr(e, Some(inner), env, ctx)?;
                Type::Ref(Box::new(found_inner))
            }
            Some(_) => return Err(TypeError::ErrorUnexpectedReference),
            None => {
                let inner = infer_expr(e, None, env, ctx)?;
                Type::Ref(Box::new(inner))
            }
        },

        Expr::Dereference(e) => {
            let t = infer_expr(e, None, env, ctx)?;
            match t {
                Type::Ref(inner) => *inner,
                _ => return Err(TypeError::ErrorNotAReference(t)),
            }
        }

        Expr::Assignment(lhs, rhs) => {
            let t = infer_expr(lhs, None, env, ctx)?;
            match t {
                Type::Ref(inner) => {
                    infer_expr(rhs, Some(&inner), env, ctx)?;
                    Type::Unit
                }
                _ => return Err(TypeError::ErrorNotAReference(t)),
            }
        }

        Expr::Panic => match expected {
            Some(t) => t.clone(),
            None => {
                if ctx.has_extension("ambiguous-type-as-bottom") {
                    Type::Bottom
                } else {
                    return Err(TypeError::ErrorAmbiguousPanicType);
                }
            }
        },

        Expr::Throw(e) => {
            let exn_ty = ctx
                .exception_type
                .as_ref()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;
            infer_expr(e, Some(exn_ty), env, ctx)?;
            match expected {
                Some(t) => t.clone(),
                None => {
                    if ctx.has_extension("ambiguous-type-as-bottom") {
                        Type::Bottom
                    } else {
                        return Err(TypeError::ErrorAmbiguousThrowType);
                    }
                }
            }
        }

        Expr::TryWith(try_expr, with_expr) => {
            let exn_ty = ctx
                .exception_type
                .as_ref()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;
            let try_ty = infer_expr(try_expr, expected, env, ctx)?;
            let with_ty = Type::Fun(vec![exn_ty.clone()], Box::new(try_ty.clone()));
            infer_expr(with_expr, Some(&with_ty), env, ctx)?;
            try_ty
        }

        Expr::TryCatch(try_expr, pattern, catch_expr) => {
            let exn_ty = ctx
                .exception_type
                .as_ref()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;
            let try_ty = infer_expr(try_expr, expected, env, ctx)?;
            let mut env2 = env.clone();
            env2.extend(typecheck_pattern(pattern, exn_ty, env)?);
            let catch_ty = infer_expr(catch_expr, Some(&try_ty), &env2, ctx)?;
            ensure_expected(catch_expr, &catch_ty, &try_ty, ctx)?;
            try_ty
        }

        Expr::TypeCast(e, target_type) => {
            infer_expr(e, None, env, ctx)?;
            target_type.clone()
        }

        Expr::LogicalNot(e) => {
            infer_expr(e, Some(&Type::Bool), env, ctx)?;
            Type::Bool
        }

        Expr::Multiply(l, r)
        | Expr::Divide(l, r)
        | Expr::Add(l, r)
        | Expr::Subtract(l, r)
        | Expr::LessThan(l, r)
        | Expr::LessThanOrEqual(l, r)
        | Expr::GreaterThan(l, r)
        | Expr::GreaterThanOrEqual(l, r)
        | Expr::Equal(l, r)
        | Expr::NotEqual(l, r)
        | Expr::LogicalAnd(l, r)
        | Expr::LogicalOr(l, r) => {
            infer_expr(l, None, env, ctx)?;
            infer_expr(r, None, env, ctx)?;
            match expr {
                Expr::LogicalAnd(_, _) | Expr::LogicalOr(_, _) => Type::Bool,
                Expr::LessThan(_, _)
                | Expr::LessThanOrEqual(_, _)
                | Expr::GreaterThan(_, _)
                | Expr::GreaterThanOrEqual(_, _)
                | Expr::Equal(_, _)
                | Expr::NotEqual(_, _) => Type::Bool,
                _ => Type::Nat,
            }
        }

        Expr::LetRec(_, _)
        | Expr::TypeAbstraction(_, _)
        | Expr::TypeApplication(_, _)
        | Expr::Fold(_, _)
        | Expr::Unfold(_, _)
        | Expr::TryCastAs { .. } => {
            return Err(TypeError::ErrorUnexpectedTypeForExpression {
                expected: Type::Bottom,
                found: Type::Bottom,
                expr: Some(format!("No inference: {}", expr)),
            })
        }
    };

    if let Some(expected_ty) = expected {
        ensure_expected(expr, &inferred, expected_ty, ctx)?;
    }

    Ok(inferred)
}

// STEP 4: ensure_expected / types_match / is_subtype
//   - Compare inferred vs expected types
//   - Use equality by default, or subtyping when extension is enabled
//   - Raise subtype-specific error when structural-subtyping check fails
fn ensure_expected(
    expr: &Expr,
    found: &Type,
    expected: &Type,
    ctx: &TypeCheckContext,
) -> Result<(), TypeError> {
    if types_match(found, expected, ctx) {
        return Ok(());
    }

    if let (Type::Variant(found_labels), Type::Variant(expected_labels)) = (found, expected) {
        let missing: Vec<String> = expected_labels
            .iter()
            .filter(|expected_label| !found_labels.iter().any(|f| f.label == expected_label.label))
            .map(|label| label.label.clone())
            .collect();

        if !missing.is_empty() {
            return Err(TypeError::ErrorMissingVariantLabels(missing));
        }
    }

    if ctx.has_extension("structural-subtyping") {
        return Err(TypeError::ErrorUnexpectedSubtype {
            expected: expected.clone(),
            found: found.clone(),
            expr: Some(format!("{}", expr)),
        });
    }

    Err(TypeError::ErrorUnexpectedTypeForExpression {
        expected: expected.clone(),
        found: found.clone(),
        expr: Some(format!("{}", expr)),
    })
}

fn types_match(found: &Type, expected: &Type, ctx: &TypeCheckContext) -> bool {
    if ctx.has_extension("structural-subtyping") {
        is_subtype(found, expected)
    } else {
        found == expected
    }
}

fn is_subtype(source: &Type, target: &Type) -> bool {
    if source == target {
        return true;
    }

    if matches!(target, Type::Top) {
        return true;
    }

    if matches!(source, Type::Bottom) {
        return true;
    }

    match (source, target) {
        (Type::Fun(s_params, s_ret), Type::Fun(t_params, t_ret)) => {
            if s_params.len() != t_params.len() {
                return false;
            }

            for (s, t) in s_params.iter().zip(t_params.iter()) {
                if !is_subtype(t, s) {
                    return false;
                }
            }

            is_subtype(s_ret, t_ret)
        }
        (Type::Tuple(s_elems), Type::Tuple(t_elems)) => {
            if t_elems.len() > s_elems.len() {
                return false;
            }
            s_elems
                .iter()
                .zip(t_elems.iter())
                .all(|(s, t)| is_subtype(s, t))
        }
        (Type::Record(s_fields), Type::Record(t_fields)) => {
            for target_field in t_fields {
                let Some(source_field) = s_fields.iter().find(|f| f.label == target_field.label) else {
                    return false;
                };
                if !is_subtype(&source_field.type_, &target_field.type_) {
                    return false;
                }
            }
            true
        }
        (Type::Ref(s_inner), Type::Ref(t_inner)) => s_inner == t_inner,
        (Type::List(s_elem), Type::List(t_elem)) => is_subtype(s_elem, t_elem),
        (Type::Sum(s_l, s_r), Type::Sum(t_l, t_r)) => is_subtype(s_l, t_l) && is_subtype(s_r, t_r),
        (Type::Variant(s_labels), Type::Variant(t_labels)) => {
            for s_label in s_labels {
                let Some(t_label) = t_labels.iter().find(|f| f.label == s_label.label) else {
                    return false;
                };
                match (&s_label.type_, &t_label.type_) {
                    (Some(s_ty), Some(t_ty)) => {
                        if !is_subtype(s_ty, t_ty) {
                            return false;
                        }
                    }
                    (None, None) => {}
                    _ => return false,
                }
            }
            true
        }
        _ => false,
    }
}

// STEP 5: typecheck_pattern
//   - Check pattern compatibility against expected scrutinee type
//   - Return bindings introduced by pattern
fn typecheck_pattern(
    pattern: &Pattern,
    expected_type: &Type,
    env: &TypeEnv,
) -> Result<TypeEnv, TypeError> {
    let mut new_env = TypeEnv::new();

    match pattern {
        Pattern::Var(name) => {
            new_env.insert(name.clone(), expected_type.clone());
            Ok(new_env)
        }

        Pattern::Inl(p) => match expected_type {
            Type::Sum(left_type, _) => typecheck_pattern(p, left_type, env),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: "inl".to_string(),
            }),
        },

        Pattern::Inr(p) => match expected_type {
            Type::Sum(_, right_type) => typecheck_pattern(p, right_type, env),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: "inr".to_string(),
            }),
        },

        Pattern::Variant(label, opt_pattern) => match expected_type {
            Type::Variant(fields) => {
                let field = fields
                    .iter()
                    .find(|f| &f.label == label)
                    .ok_or_else(|| TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: label.clone(),
                    })?;

                match (&field.type_, opt_pattern) {
                    (Some(field_ty), Some(pat)) => typecheck_pattern(pat, field_ty, env),
                    (None, None) => Ok(new_env),
                    (Some(_), None) => Err(TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: format!("Variant {} should have data", label),
                    }),
                    (None, Some(_)) => Err(TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: format!("Variant {} should not have data", label),
                    }),
                }
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("variant {}", label),
            }),
        },

        Pattern::Ascription(pat, ty) | Pattern::CastAs(pat, ty) => {
            if ty != expected_type {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern(pat, ty, env)
        }

        Pattern::Tuple(patterns) => match expected_type {
            Type::Tuple(elem_types) => {
                if patterns.len() != elem_types.len() {
                    return Err(TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: format!("{}", pattern),
                    });
                }

                for (pat, elem_ty) in patterns.iter().zip(elem_types.iter()) {
                    new_env.extend(typecheck_pattern(pat, elem_ty, env)?);
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("{}", pattern),
            }),
        },

        Pattern::Record(fields) => match expected_type {
            Type::Record(expected_fields) => {
                for field_pattern in fields {
                    let expected_field = expected_fields
                        .iter()
                        .find(|f| f.label == field_pattern.label)
                        .ok_or_else(|| TypeError::ErrorUnexpectedPatternForType {
                            expected: expected_type.clone(),
                            pattern: format!("{}", pattern),
                        })?;

                    if let Some(nested_pattern) = &field_pattern.pattern {
                        new_env.extend(typecheck_pattern(nested_pattern, &expected_field.type_, env)?);
                    }
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("{}", pattern),
            }),
        },

        Pattern::List(patterns) => match expected_type {
            Type::List(elem_type) => {
                for pat in patterns {
                    new_env.extend(typecheck_pattern(pat, elem_type, env)?);
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("{}", pattern),
            }),
        },

        Pattern::Cons(head, tail) => match expected_type {
            Type::List(elem_type) => {
                new_env.extend(typecheck_pattern(head, elem_type, env)?);
                new_env.extend(typecheck_pattern(tail, &Type::List(elem_type.clone()), env)?);
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("{}", pattern),
            }),
        },

        Pattern::Int(_) => {
            if expected_type == &Type::Nat {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                })
            }
        }

        Pattern::Succ(inner) => {
            if expected_type != &Type::Nat {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern(inner, &Type::Nat, env)
        }

        Pattern::True | Pattern::False => {
            if expected_type == &Type::Bool {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                })
            }
        }

        Pattern::Unit => {
            if expected_type == &Type::Unit {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                })
            }
        }
    }
}

// STEP 6: helpers
//   - Validate type declarations and detect duplicate fields/labels
fn check_type_validity(ty: &Type) -> Result<(), TypeError> {
    match ty {
        Type::Record(fields) => {
            let mut seen = HashSet::new();
            let mut duplicates = Vec::new();

            for field in fields {
                if !seen.insert(&field.label) {
                    duplicates.push(field.label.clone());
                }
            }

            if !duplicates.is_empty() {
                return Err(TypeError::ErrorDuplicateRecordTypeFields(duplicates));
            }

            for field in fields {
                check_type_validity(&field.type_)?;
            }
            Ok(())
        }

        Type::Variant(fields) => {
            let mut seen = HashSet::new();
            let mut duplicates = Vec::new();

            for field in fields {
                if !seen.insert(&field.label) {
                    duplicates.push(field.label.clone());
                }
            }

            if !duplicates.is_empty() {
                return Err(TypeError::ErrorDuplicateVariantTypeFields(duplicates));
            }

            for field in fields {
                if let Some(field_ty) = &field.type_ {
                    check_type_validity(field_ty)?;
                }
            }
            Ok(())
        }

        Type::Fun(params, ret) => {
            for param in params {
                check_type_validity(param)?;
            }
            check_type_validity(ret)
        }

        Type::Tuple(types) => {
            for ty in types {
                check_type_validity(ty)?;
            }
            Ok(())
        }

        Type::List(ty) => check_type_validity(ty),

        Type::Sum(left, right) => {
            check_type_validity(left)?;
            check_type_validity(right)
        }

        Type::Ref(ty) => check_type_validity(ty),
        Type::Rec(_, ty) => check_type_validity(ty),
        Type::ForAll(_, ty) => check_type_validity(ty),

        Type::Bool | Type::Nat | Type::Unit | Type::Var(_) | Type::Top | Type::Bottom | Type::Auto => {
            Ok(())
        }
    }
}

fn check_duplicate_record_fields(bindings: &[Binding]) -> Result<(), TypeError> {
    let mut seen = HashSet::new();
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
