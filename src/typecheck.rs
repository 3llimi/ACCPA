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
//   - Validate main exists, has a function type, and has correct arity
//   - Collect optional exception type declaration
//   - Type-check every declaration (first error stops checking)
pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    let mut fn_env: HashMap<String, Type> = HashMap::new();

    // #exceptions + #open-variant-exceptions:
    // collect either a single declared exception type or open variant labels.
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

    // main must exist
    if !fn_env.contains_key("main") {
        return Err(TypeError::ErrorMissingMain);
    }

    //  main must have a function type 
    match fn_env.get("main") {
        Some(Type::Fun(_, _)) => {}
        _ => return Err(TypeError::ErrorIncorrectTypeOfMain),
    }

    //  main must have exactly 1 parameter (unless multiparameter enabled) 
    //   ERROR_INCORRECT_ARITY_OF_MAIN supersedes it when extensions are active.
    if let Some(Type::Fun(params, _)) = fn_env.get("main") {
        if params.len() != 1 {
            return Err(TypeError::ErrorIncorrectArityOfMain);
        }
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

    // #open-variant-exceptions: these two declaration styles cannot be mixed.
    if seen_exception_type && !exception_variants.is_empty() {
        return Err(TypeError::ErrorConflictingExceptionDeclarations);
    }

    // #open-variant-exceptions: open exception labels synthesize a Variant exn type.
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
            for (fname, ty) in &local_fn_env {
                env.insert(fname.clone(), ty.clone());
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
        TypeError::ErrorUnexpectedTypeForExpression { expected, found, expr } => {
            TypeError::ErrorUnexpectedTypeForExpression {
                expected,
                found,
                expr: Some(attach_context(expr, function_name)),
            }
        }
        TypeError::ErrorUnexpectedSubtype { expected, found, expr } => {
            TypeError::ErrorUnexpectedSubtype {
                expected,
                found,
                expr: Some(attach_context(expr, function_name)),
            }
        }
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
fn infer_expr(
    expr: &Expr,
    expected: Option<&Type>,
    env: &TypeEnv,
    ctx: &TypeCheckContext,
) -> Result<Type, TypeError> {
    let inferred = match expr {
        Expr::ConstTrue | Expr::ConstFalse => Type::Bool,
        Expr::ConstUnit => Type::Unit,

        //  #natural-literals: reject negative integers 
        Expr::ConstInt(n) => {
            if *n < 0 {
                return Err(TypeError::ErrorIllegalNegativeLiteral);
            }
            Type::Nat
        }

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

        //  Abstraction 
        Expr::Abstraction(params, body) => match expected {
            Some(Type::Fun(param_types, return_type)) => {
                // Wrong number of parameters → specific error, raised before body
                if params.len() != param_types.len() {
                    return Err(TypeError::ErrorUnexpectedNumberOfParametersInLambda {
                        expected: param_types.len(),
                        found: params.len(),
                    });
                }

                let mut new_env = env.clone();
                for (param, expected_param_type) in params.iter().zip(param_types.iter()) {
                    // #structural-subtyping: lambda parameter checking is contravariant.
                    if ctx.has_extension("structural-subtyping") {
                        // Function parameters are contravariant: expected parameter type
                        // must be a subtype of the lambda annotation.
                        if !is_subtype(expected_param_type, &param.type_) {
                            if let (Type::Record(expected_fields), Type::Record(found_fields)) =
                                (expected_param_type, &param.type_)
                            {
                                let missing: Vec<String> = found_fields
                                    .iter()
                                    .filter(|f| {
                                        !expected_fields.iter().any(|ef| ef.label == f.label)
                                    })
                                    .map(|f| f.label.clone())
                                    .collect();

                                if !missing.is_empty() {
                                    return Err(TypeError::ErrorMissingRecordFields(missing));
                                }
                            }

                            return Err(TypeError::ErrorUnexpectedSubtype {
                                expected: param.type_.clone(),
                                found: expected_param_type.clone(),
                                expr: Some(format!("{}", expr)),
                            });
                        }
                    } else if &param.type_ != expected_param_type {
                        return Err(TypeError::ErrorUnexpectedTypeForParameter {
                            expected: expected_param_type.clone(),
                            found: param.type_.clone(),
                        });
                    }
                    new_env.insert(param.name.clone(), param.type_.clone());
                }

                let body_ty = infer_expr(body, Some(return_type), &new_env, ctx)?;
                ensure_expected(body, &body_ty, return_type, ctx)?;
                let declared_param_types: Vec<Type> =
                    params.iter().map(|p| p.type_.clone()).collect();
                Type::Fun(declared_param_types, Box::new(body_ty))
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

        //  Application 
        Expr::Application(func, args) => {
            let func_type = infer_expr(func, None, env, ctx)?;
            match func_type {
                Type::Fun(param_types, return_type) => {
                    // Wrong argument count → specific error
                    if args.len() != param_types.len() {
                        return Err(TypeError::ErrorIncorrectNumberOfArguments {
                            expected: param_types.len(),
                            found: args.len(),
                        });
                    }
                    for (arg, param_type) in args.iter().zip(param_types.iter()) {
                        let arg_ty = infer_expr(arg, Some(param_type), env, ctx)?;
                        ensure_expected(arg, &arg_ty, param_type, ctx)?;
                    }
                    if matches!(&*return_type, Type::Top) {
                        expected.cloned().unwrap_or(*return_type)
                    } else {
                        *return_type
                    }
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
                            return Err(TypeError::ErrorUnexpectedRecordFields(vec![
                                binding.name.clone(),
                            ]));
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

        // #ambiguous-type-as-bottom: without an expected sum type, this extension
        // permits the missing side to be treated as Bottom.
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

        // #ambiguous-type-as-bottom: symmetric rule for right injection.
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

            let mut result_type: Option<Type> = None;
            let mut covered_variant_labels: HashSet<String> = HashSet::new();
            let mut has_inl = false;
            let mut has_inr = false;

            for case in cases {
                // Validate pattern against scrutinee type FIRST.
                let pattern_env = typecheck_pattern(&case.pattern, &scrutinee_type, env)?;

                match &case.pattern {
                    Pattern::Inl(_) => has_inl = true,
                    Pattern::Inr(_) => has_inr = true,
                    Pattern::Variant(label, _) => {
                        covered_variant_labels.insert(label.clone());
                    }
                    _ => {}
                }

                let mut case_env = env.clone();
                case_env.extend(pattern_env);

                if let Some(expected_ty) = expected {
                    infer_expr(&case.expr, Some(expected_ty), &case_env, ctx)?;
                } else {
                    let case_ty =
                        infer_expr(&case.expr, result_type.as_ref(), &case_env, ctx)?;
                    if let Some(acc) = &result_type {
                        ensure_expected(&case.expr, &case_ty, acc, ctx)?;
                    } else {
                        result_type = Some(case_ty);
                    }
                }
            }

            match &scrutinee_type {
                Type::Bool => {
                    let mut has_true = false;
                    let mut has_false = false;
                    for case in cases {
                        match &case.pattern {
                            Pattern::True => has_true = true,
                            Pattern::False => has_false = true,
                            Pattern::Var(_) => {
                                has_true = true;
                                has_false = true;
                            }
                            _ => {}
                        }
                    }
                    if !has_true || !has_false {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                Type::Nat => {
                    let mut has_zero = false;
                    let mut has_succ = false;
                    for case in cases {
                        match &case.pattern {
                            Pattern::Int(0) => has_zero = true,
                            Pattern::Succ(_) => has_succ = true,
                            Pattern::Var(_) => {
                                has_zero = true;
                                has_succ = true;
                            }
                            _ => {}
                        }
                    }
                    if !has_zero || !has_succ {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                Type::List(_) => {
                    let mut has_empty = false;
                    let mut has_cons_with_var_tail = false;
                    let mut has_wildcard = false;
                    
                    for case in cases {
                        match &case.pattern {
                            Pattern::List(elems) if elems.is_empty() => has_empty = true,
                            Pattern::Var(_) => {
                                has_wildcard = true;
                            }
                            Pattern::Cons(_, tail) => {
                                // Check if tail is a variable (covers all non-empty lists)
                                if matches!(tail.as_ref(), Pattern::Var(_)) {
                                    has_cons_with_var_tail = true;
                                }
                            }
                            _ => {}
                        }
                    }
                    
                    if !has_wildcard && !(has_empty && has_cons_with_var_tail) {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                Type::Sum(_, _) => {
                    if !has_inl || !has_inr {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                Type::Variant(fields) => {
                    let required_labels: HashSet<String> =
                        fields.iter().map(|f| f.label.clone()).collect();
                    if covered_variant_labels != required_labels {
                        return Err(TypeError::ErrorNonexhaustiveMatchPatterns);
                    }
                }
                _ => {}
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
                let head_ty = infer_expr(head, None, env, ctx)?;
                infer_expr(
                    tail,
                    Some(&Type::List(Box::new(head_ty.clone()))),
                    env,
                    ctx,
                )?;
                Type::List(Box::new(head_ty))
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

        // Variant expression 
        Expr::Variant(label, opt_expr) => match expected {
            Some(Type::Variant(fields)) => {
                let field = fields
                    .iter()
                    .find(|f| f.label == *label)
                    .ok_or_else(|| TypeError::ErrorUnexpectedVariantLabel(label.clone()))?;

                match (&field.type_, opt_expr) {
                    // Both present: check the expression
                    (Some(expected_type), Some(expr)) => {
                        infer_expr(expr, Some(expected_type), env, ctx)?;
                    }
                    // Both absent: nullary label used correctly
                    (None, None) => {}
                    // Label is nullary but expression was provided
                    (None, Some(_)) => {
                        return Err(TypeError::ErrorUnexpectedDataForNullaryLabel(label.clone()));
                    }
                    // Label expects data but none was provided
                    (Some(_), None) => {
                        return Err(TypeError::ErrorMissingDataForLabel(label.clone()));
                    }
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

        //  #letrec-bindings 
        //   Every binding MUST carry a PatternAsc (type annotation) so that
        //   the recursive type is known before the RHS is checked.
        Expr::LetRec(bindings, body) => {
            let mut new_env = env.clone();
            let mut binding_expected: Vec<Option<Type>> = Vec::new();

            // Pass 1: seed recursive environment.
            for binding in bindings.iter() {
                match &binding.pattern {
                    Pattern::Ascription(inner_pat, annotated_ty) => {
                        if let Pattern::Var(name) = inner_pat.as_ref() {
                            new_env.insert(name.clone(), annotated_ty.clone());
                            binding_expected.push(Some(annotated_ty.clone()));
                        } else {
                            return Err(TypeError::ErrorAmbiguousPatternType);
                        }
                    }
                    Pattern::Var(name) => {
                        // Unannotated letrec is only inferable for single-lambda RHS.
                        if let Expr::Abstraction(params, body_expr) = &binding.rhs {
                            if matches!(body_expr.as_ref(), Expr::Abstraction(_, _)) {
                                return Err(TypeError::ErrorAmbiguousPatternType);
                            }
                            let param_types: Vec<Type> =
                                params.iter().map(|p| p.type_.clone()).collect();
                            new_env.insert(name.clone(), Type::Fun(param_types, Box::new(Type::Top)));
                            binding_expected.push(None);
                        } else {
                            return Err(TypeError::ErrorAmbiguousPatternType);
                        }
                    }
                    _ => return Err(TypeError::ErrorAmbiguousPatternType),
                }
            }

            // Pass 2: infer each binding under the recursive environment.
            for (binding, exp_ty) in bindings.iter().zip(binding_expected.iter()) {
                let rhs_ty = infer_expr(&binding.rhs, exp_ty.as_ref(), &new_env, ctx)?;
                match &binding.pattern {
                    Pattern::Ascription(inner_pat, expected_ty) => {
                        ensure_expected(&binding.rhs, &rhs_ty, expected_ty, ctx)?;
                        if let Pattern::Var(name) = inner_pat.as_ref() {
                            new_env.insert(name.clone(), expected_ty.clone());
                        }
                    }
                    Pattern::Var(name) => {
                        new_env.insert(name.clone(), rhs_ty);
                    }
                    _ => return Err(TypeError::ErrorAmbiguousPatternType),
                }
            }

            infer_expr(body, expected, &new_env, ctx)?
        }

        Expr::Fix(f) => {
            let f_type = infer_expr(f, None, env, ctx)?;
            let f_type_clone = f_type.clone();
            match f_type {
                Type::Fun(param_types, return_type) => {
                    if param_types.len() != 1 {
                        return Err(TypeError::ErrorNotAFunction(f_type_clone));
                    }
                    if param_types[0] != *return_type {
                        return Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: Type::Fun(
                                vec![*return_type.clone()],
                                return_type.clone(),
                            ),
                            found: f_type_clone,
                            expr: Some(format!("{}", f)),
                        });
                    }
                    *return_type
                }
                _ => return Err(TypeError::ErrorNotAFunction(f_type_clone)),
            }
        }

        // #sequencing: first expression must be Unit, result is the second expression.
        Expr::Sequence(e1, e2) => {
            infer_expr(e1, Some(&Type::Unit), env, ctx)?;
            infer_expr(e2, expected, env, ctx)?
        }

        // #references: allocate and track Ref(inner) types.
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

        // #references: dereference requires Ref(t) and returns t.
        Expr::Dereference(e) => {
            let expected_ref_type = expected.map(|ty| Type::Ref(Box::new(ty.clone())));
            let t = infer_expr(e, expected_ref_type.as_ref(), env, ctx)?;
            match t {
                Type::Ref(inner) => *inner,
                _ => return Err(TypeError::ErrorNotAReference(t)),
            }
        }

        // #references: assignment requires a Ref(lhs_type), checks rhs against lhs_type.
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

        // #panic: polymorphic when expected type is known; otherwise ambiguous unless
        // #ambiguous-type-as-bottom is active.
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

        // #exceptions: thrown value must match the declared exception type.
        // Same ambiguity behavior as panic when no expected type is available.
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

        // #exceptions: both try and handler expressions must agree on result type.
        Expr::TryWith(try_expr, with_expr) => {
            let _exn_ty = ctx
                .exception_type
                .as_ref()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;

            match expected {
                Some(expected_ty) => {
                    infer_expr(try_expr, Some(expected_ty), env, ctx)?;
                    infer_expr(with_expr, Some(expected_ty), env, ctx)?;
                    expected_ty.clone()
                }
                None => {
                    let try_ty = infer_expr(try_expr, None, env, ctx)?;
                    let with_ty = infer_expr(with_expr, Some(&try_ty), env, ctx)?;
                    ensure_expected(with_expr, &with_ty, &try_ty, ctx)?;
                    try_ty
                }
            }
        }

        // #exceptions: catch pattern is checked against the declared exception type.
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

        // #try-cast-as (dynamic cast): check cast target compatibility, then type both
        // casted and fallback branches to a common result.
        Expr::TryCastAs {
            try_,
            to,
            casted_pattern,
            casted_arm,
            fallback_arm,
        } => {
            let try_ty = infer_expr(try_, None, env, ctx)?;

            if ctx.has_extension("structural-subtyping") {
                if !is_subtype(to, &try_ty) {
                    return Err(TypeError::ErrorUnexpectedSubtype {
                        expected: try_ty,
                        found: to.clone(),
                        expr: Some(format!("{}", expr)),
                    });
                }
            } else if to != &try_ty {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: try_ty,
                    found: to.clone(),
                    expr: Some(format!("{}", expr)),
                });
            }

            let mut casted_env = env.clone();
            casted_env.extend(typecheck_pattern(casted_pattern, to, env)?);

            match expected {
                Some(expected_ty) => {
                    infer_expr(casted_arm, Some(expected_ty), &casted_env, ctx)?;
                    infer_expr(fallback_arm, Some(expected_ty), env, ctx)?;
                    expected_ty.clone()
                }
                None => {
                    let casted_ty = infer_expr(casted_arm, None, &casted_env, ctx)?;
                    let fallback_ty = infer_expr(fallback_arm, Some(&casted_ty), env, ctx)?;
                    ensure_expected(fallback_arm, &fallback_ty, &casted_ty, ctx)?;
                    casted_ty
                }
            }
        }

        // #type-cast: explicit cast expression; result type is the target annotation.
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
                Expr::LogicalAnd(_, _)
                | Expr::LogicalOr(_, _)
                | Expr::LessThan(_, _)
                | Expr::LessThanOrEqual(_, _)
                | Expr::GreaterThan(_, _)
                | Expr::GreaterThanOrEqual(_, _)
                | Expr::Equal(_, _)
                | Expr::NotEqual(_, _) => Type::Bool,
                _ => Type::Nat,
            }
        }

        Expr::TypeAbstraction(_, _)
        | Expr::TypeApplication(_, _)
        | Expr::Fold(_, _)
        | Expr::Unfold(_, _) => {
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
            .filter(|el| !found_labels.iter().any(|f| f.label == el.label))
            .map(|el| el.label.clone())
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
    // #structural-subtyping: toggles matching from equality to subtype checking.
    if ctx.has_extension("structural-subtyping") {
        is_subtype(found, expected)
    } else {
        found == expected
    }
}

fn is_subtype(source: &Type, target: &Type) -> bool {
    // Top and Bottom types: Top is the universal supertype and Bottom the universal subtype.
    if source == target {
        return true;
    }
    // Top accepts every type.
    if matches!(target, Type::Top) {
        return true;
    }
    // Bottom is accepted by every target type.
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
            s_elems.iter().zip(t_elems.iter()).all(|(s, t)| is_subtype(s, t))
        }
        (Type::Record(s_fields), Type::Record(t_fields)) => {
            for target_field in t_fields {
                let Some(source_field) = s_fields.iter().find(|f| f.label == target_field.label)
                else {
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
        (Type::Sum(s_l, s_r), Type::Sum(t_l, t_r)) => {
            is_subtype(s_l, t_l) && is_subtype(s_r, t_r)
        }
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
//   Checks pattern compatibility and returns new variable bindings.
//   Duplicate variable detection runs first via check_duplicate_pattern_variables.
fn typecheck_pattern(
    pattern: &Pattern,
    expected_type: &Type,
    env: &TypeEnv,
) -> Result<TypeEnv, TypeError> {
    //  #structural-patterns: reject duplicate variable names 
    check_duplicate_pattern_variables(pattern)?;

    typecheck_pattern_inner(pattern, expected_type, env)
}

/// Recursive descent that does the actual type-directed pattern checking.
/// Separated so that duplicate-variable detection only runs at the top level.
fn typecheck_pattern_inner(
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
            Type::Sum(left_type, _) => typecheck_pattern_inner(p, left_type, env),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: "inl".to_string(),
            }),
        },

        Pattern::Inr(p) => match expected_type {
            Type::Sum(_, right_type) => typecheck_pattern_inner(p, right_type, env),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: "inr".to_string(),
            }),
        },

        //  Variant pattern with nullary-label error codes 
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
                    (Some(field_ty), Some(pat)) => typecheck_pattern_inner(pat, field_ty, env),
                    (None, None) => Ok(new_env),
                    // Label is nullary but pattern carries data
                    (None, Some(_)) => {
                        Err(TypeError::ErrorUnexpectedNonNullaryVariantPattern(label.clone()))
                    }
                    // Label carries data but pattern is nullary
                    (Some(_), None) => {
                        Err(TypeError::ErrorUnexpectedNullaryVariantPattern(label.clone()))
                    }
                }
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type.clone(),
                pattern: format!("variant {}", label),
            }),
        },

        Pattern::Ascription(pat, ty) => {
            if ty != expected_type {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern_inner(pat, ty, env)
        }

        // #type-cast-patterns: pattern cast is valid only when cast type is a
        // subtype of the expected scrutinee type.
        Pattern::CastAs(pat, ty) => {
            if !is_subtype(ty, expected_type) {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type.clone(),
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern_inner(pat, ty, env)
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
                    new_env.extend(typecheck_pattern_inner(pat, elem_ty, env)?);
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
                // Check that the pattern includes all required fields
                let pattern_labels: HashSet<String> =
                    fields.iter().map(|f| f.label.clone()).collect();
                let expected_labels: HashSet<String> =
                    expected_fields.iter().map(|f| f.label.clone()).collect();
                
                let missing: Vec<String> = expected_labels
                    .difference(&pattern_labels)
                    .cloned()
                    .collect();
                
                if !missing.is_empty() {
                    return Err(TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: format!("{}", pattern),
                    });
                }
                
                for field_pattern in fields {
                    let expected_field = expected_fields
                        .iter()
                        .find(|f| f.label == field_pattern.label)
                        .ok_or_else(|| TypeError::ErrorUnexpectedPatternForType {
                            expected: expected_type.clone(),
                            pattern: format!("{}", pattern),
                        })?;
                    if let Some(nested_pattern) = &field_pattern.pattern {
                        new_env.extend(typecheck_pattern_inner(
                            nested_pattern,
                            &expected_field.type_,
                            env,
                        )?);
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
                    new_env.extend(typecheck_pattern_inner(pat, elem_type, env)?);
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
                new_env.extend(typecheck_pattern_inner(head, elem_type, env)?);
                new_env.extend(typecheck_pattern_inner(
                    tail,
                    &Type::List(elem_type.clone()),
                    env,
                )?);
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
            typecheck_pattern_inner(inner, &Type::Nat, env)
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

/// Walk a pattern tree and report the first variable name that appears twice.
/// Called once at the top of typecheck_pattern so nested patterns are covered.
fn check_duplicate_pattern_variables(pattern: &Pattern) -> Result<(), TypeError> {
    let mut seen = HashSet::new();
    collect_pattern_variables(pattern, &mut seen)
}

fn collect_pattern_variables(
    pattern: &Pattern,
    seen: &mut HashSet<String>,
) -> Result<(), TypeError> {
    match pattern {
        Pattern::Var(name) => {
            if !seen.insert(name.clone()) {
                return Err(TypeError::ErrorDuplicatePatternVariable(name.clone()));
            }
            Ok(())
        }
        Pattern::Inl(p) | Pattern::Inr(p) | Pattern::Succ(p) => {
            collect_pattern_variables(p, seen)
        }
        Pattern::Variant(_, Some(p)) => collect_pattern_variables(p, seen),
        Pattern::Variant(_, None) => Ok(()),
        Pattern::Ascription(p, _) | Pattern::CastAs(p, _) => {
            collect_pattern_variables(p, seen)
        }
        Pattern::Tuple(pats) | Pattern::List(pats) => {
            for p in pats {
                collect_pattern_variables(p, seen)?;
            }
            Ok(())
        }
        Pattern::Record(fields) => {
            for field in fields {
                if let Some(p) = &field.pattern {
                    collect_pattern_variables(p, seen)?;
                }
            }
            Ok(())
        }
        Pattern::Cons(head, tail) => {
            collect_pattern_variables(head, seen)?;
            collect_pattern_variables(tail, seen)
        }
        // Leaf patterns with no variable bindings
        Pattern::True
        | Pattern::False
        | Pattern::Unit
        | Pattern::Int(_) => Ok(()),
    }
}

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
        Type::Bool
        | Type::Nat
        | Type::Unit
        | Type::Var(_)
        | Type::Top
        | Type::Bottom
        | Type::Auto => Ok(()),
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