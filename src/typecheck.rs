use crate::ast::*;
use crate::error::TypeError;
use std::collections::{HashMap, HashSet};

type TypeEnv = HashMap<String, Type>;

#[derive(Debug, Clone)]
struct TypeCheckContext {
    extensions: HashSet<String>,
    exception_type: Option<Type>,
    type_reconstruction_enabled: bool,
    strict_ambiguous_type_errors_enabled: bool,
    universal_types_enabled: bool,
    next_type_var_id: usize,
    type_var_subst: HashMap<String, Type>,
    active_type_scope: HashSet<String>,
    checked_expr_types: Vec<Type>,
}

impl TypeCheckContext {
    fn has_extension(&self, ext: &str) -> bool {
        self.extensions.contains(ext)
    }

    fn fresh_meta_type(&mut self) -> Type {
        self.next_type_var_id += 1;
        Type::Var(format!("?T{}", self.next_type_var_id))
    }

    fn is_meta_var_name(name: &str) -> bool {
        name.starts_with("?T")
    }

    fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(name) if Self::is_meta_var_name(name) => {
                if let Some(next) = self.type_var_subst.get(name) {
                    self.resolve_type(next)
                } else {
                    Type::Var(name.clone())
                }
            }
            Type::Fun(params, ret) => Type::Fun(
                params.iter().map(|p| self.resolve_type(p)).collect(),
                Box::new(self.resolve_type(ret)),
            ),
            Type::Tuple(types) => {
                Type::Tuple(types.iter().map(|t| self.resolve_type(t)).collect())
            }
            Type::Record(fields) => Type::Record(
                fields
                    .iter()
                    .map(|f| RecordFieldType {
                        label: f.label.clone(),
                        type_: self.resolve_type(&f.type_),
                    })
                    .collect(),
            ),
            Type::Sum(left, right) => Type::Sum(
                Box::new(self.resolve_type(left)),
                Box::new(self.resolve_type(right)),
            ),
            Type::List(inner) => Type::List(Box::new(self.resolve_type(inner))),
            Type::Variant(fields) => Type::Variant(
                fields
                    .iter()
                    .map(|f| VariantFieldType {
                        label: f.label.clone(),
                        type_: f.type_.as_ref().map(|t| self.resolve_type(t)),
                    })
                    .collect(),
            ),
            Type::Rec(var, inner) => Type::Rec(var.clone(), Box::new(self.resolve_type(inner))),
            Type::Ref(inner) => Type::Ref(Box::new(self.resolve_type(inner))),
            Type::ForAll(vars, inner) => {
                Type::ForAll(vars.clone(), Box::new(self.resolve_type(inner)))
            }
            _ => ty.clone(),
        }
    }
}

// STEP 1: typecheck_program
//   - Collect top-level function signatures into fn_env
//   - Parse active extensions into checker context
//   - Validate main exists, has a function type, and has correct arity
//   - Collect optional exception type declaration
//   - Type-check every declaration (first error stops checking)
pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    let mut extensions = HashSet::new();
    for ext in &program.extensions {
        for name in &ext.extension_names {
            extensions.insert(normalize_extension_name(name));
        }
    }

    let mut ctx = TypeCheckContext {
        extensions,
        exception_type: None,
        type_reconstruction_enabled: false,
        strict_ambiguous_type_errors_enabled: false,
        universal_types_enabled: false,
        next_type_var_id: 0,
        type_var_subst: HashMap::new(),
        active_type_scope: HashSet::new(),
        checked_expr_types: Vec::new(),
    };
    ctx.type_reconstruction_enabled = ctx.has_extension("type-reconstruction");
    // Ambiguous-type reporting is relevant during reconstruction.
    // #type-reconstruction mode unless an explicit strict toggle is requested.
    ctx.strict_ambiguous_type_errors_enabled = ctx.has_extension("strict-ambiguous-type-errors");
    ctx.universal_types_enabled = ctx.has_extension("universal-types");

    let rewritten_program = if ctx.type_reconstruction_enabled {
        rewrite_program_auto_types(program, &mut ctx)
    } else {
        program.clone()
    };
    let program = &rewritten_program;

    let mut fn_env: HashMap<String, Type> = HashMap::new();

    for decl in &program.decls {
        match decl {
            Decl::DeclFun {
                name,
                param_decls,
                return_type,
                ..
            } => {
                let fn_type = build_function_type(param_decls, return_type);
                fn_env.insert(name.clone(), fn_type);
            }
            Decl::DeclGenericFun {
                name,
                generics,
                param_decls,
                return_type,
                ..
            } => {
                if !ctx.universal_types_enabled {
                    return Err(TypeError::ErrorUnexpectedTypeForExpression {
                        expected: Type::Top,
                        found: Type::ForAll(
                            generics.clone(),
                            Box::new(build_function_type(param_decls, return_type)),
                        ),
                        expr: None,
                    });
                }

                if let Some(dup) = find_duplicate_name(generics) {
                    return Err(TypeError::ErrorDuplicateTypeParameter(dup));
                }

                // Validate generic signatures before publishing them in fn_env
                // so later declarations cannot observe ill-scoped type variables.
                let mut generic_scope = HashSet::new();
                for generic in generics {
                    generic_scope.insert(generic.clone());
                }
                for param in param_decls {
                    check_type_validity(&param.type_, &generic_scope, &ctx)?;
                }
                if let Some(ret_ty) = return_type {
                    check_type_validity(ret_ty, &generic_scope, &ctx)?;
                }

                let inner = build_function_type(param_decls, return_type);
                fn_env.insert(name.clone(), Type::ForAll(generics.clone(), Box::new(inner)));
            }
            _ => {}
        }
    }

    if !fn_env.contains_key("main") {
        return Err(TypeError::ErrorMissingMain);
    }

    match fn_env.get("main") {
        Some(Type::Fun(_, _)) => {}
        _ => return Err(TypeError::ErrorIncorrectTypeOfMain),
    }

    if let Some(Type::Fun(params, _)) = fn_env.get("main") {
        if params.len() != 1 {
            return Err(TypeError::ErrorIncorrectArityOfMain);
        }
    }

    let mut seen_exception_type = false;
    let mut exception_variants: Vec<VariantFieldType> = Vec::new();
    let mut seen_exception_variant_labels: HashSet<String> = HashSet::new();
    let empty_type_scope = HashSet::new();

    for decl in &program.decls {
        match decl {
            Decl::DeclExceptionType(ty) => {
                if seen_exception_type {
                    return Err(TypeError::ErrorDuplicateExceptionType);
                }
                check_type_validity(ty, &empty_type_scope, &ctx)?;
                ctx.exception_type = Some(ty.clone());
                seen_exception_type = true;
            }
            Decl::DeclExceptionVariant { name, type_ } => {
                check_type_validity(type_, &empty_type_scope, &ctx)?;
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

    let base_ctx = ctx.clone();
    match run_decl_typecheck_pass(program, &fn_env, &mut ctx, &empty_type_scope, false) {
        Ok(()) => Ok(()),
        Err(primary_err) => {
            if should_try_main_first_occurs_fallback(&primary_err, &ctx) {
                let mut fallback_ctx = base_ctx;
                if let Err(fallback_err) = run_decl_typecheck_pass(
                    program,
                    &fn_env,
                    &mut fallback_ctx,
                    &empty_type_scope,
                    true,
                ) {
                    if matches!(fallback_err, TypeError::ErrorOccursCheckInfiniteType) {
                        return Err(fallback_err);
                    }
                }
            }
            Err(primary_err)
        }
    }
}

fn normalize_extension_name(name: &str) -> String {
    name.trim_start_matches('#').trim().to_string()
}

fn build_function_type(params: &[ParamDecl], return_type: &Option<Type>) -> Type {
    let param_types: Vec<Type> = params.iter().map(|p| p.type_.clone()).collect();
    let ret_type = return_type.clone().unwrap_or(Type::Unit);
    Type::Fun(param_types, Box::new(ret_type))
}

fn is_main_function_decl(decl: &Decl) -> bool {
    matches!(decl, Decl::DeclFun { name, .. } if name == "main")
        || matches!(decl, Decl::DeclGenericFun { name, .. } if name == "main")
}

fn should_try_main_first_occurs_fallback(err: &TypeError, ctx: &TypeCheckContext) -> bool {
    if !ctx.type_reconstruction_enabled {
        return false;
    }

    match err {
        TypeError::ErrorUnexpectedTypeForExpression {
            expr: Some(expr),
            ..
        } => expr.contains("[in function main]"),
        _ => false,
    }
}

fn run_decl_typecheck_pass(
    program: &Program,
    fn_env: &HashMap<String, Type>,
    ctx: &mut TypeCheckContext,
    empty_type_scope: &HashSet<String>,
    main_first: bool,
) -> Result<(), TypeError> {
    let mut ordered_decls: Vec<&Decl> = if main_first {
        let mut main_first_decls: Vec<&Decl> = program
            .decls
            .iter()
            .filter(|decl| is_main_function_decl(decl))
            .collect();
        main_first_decls.extend(
            program
                .decls
                .iter()
                .filter(|decl| !is_main_function_decl(decl)),
        );
        main_first_decls
    } else {
        program.decls.iter().collect()
    };

    for decl in ordered_decls.drain(..) {
        typecheck_decl(decl, fn_env, ctx, empty_type_scope)?;
    }

    if ctx.strict_ambiguous_type_errors_enabled {
        check_ambiguous_types(fn_env, ctx)?;
    }

    Ok(())
}

fn find_duplicate_name(names: &[String]) -> Option<String> {
    let mut seen = HashSet::new();
    for name in names {
        if !seen.insert(name.clone()) {
            return Some(name.clone());
        }
    }
    None
}

fn rewrite_program_auto_types(program: &Program, ctx: &mut TypeCheckContext) -> Program {
    Program {
        language_decl: program.language_decl,
        extensions: program.extensions.clone(),
        decls: program
            .decls
            .iter()
            .map(|decl| rewrite_decl_auto_types(decl, ctx))
            .collect(),
    }
}

fn rewrite_decl_auto_types(decl: &Decl, ctx: &mut TypeCheckContext) -> Decl {
    match decl {
        Decl::DeclFun {
            annotations,
            name,
            param_decls,
            return_type,
            throws_types,
            local_decls,
            return_expr,
        } => Decl::DeclFun {
            annotations: annotations.clone(),
            name: name.clone(),
            param_decls: param_decls
                .iter()
                .map(|p| ParamDecl {
                    name: p.name.clone(),
                    type_: rewrite_type_auto_types(&p.type_, ctx),
                })
                .collect(),
            return_type: return_type
                .as_ref()
                .map(|ty| rewrite_type_auto_types(ty, ctx)),
            throws_types: throws_types
                .iter()
                .map(|ty| rewrite_type_auto_types(ty, ctx))
                .collect(),
            local_decls: local_decls
                .iter()
                .map(|d| rewrite_decl_auto_types(d, ctx))
                .collect(),
            return_expr: rewrite_expr_auto_types(return_expr, ctx),
        },
        Decl::DeclGenericFun {
            annotations,
            name,
            generics,
            param_decls,
            return_type,
            throws_types,
            local_decls,
            return_expr,
        } => Decl::DeclGenericFun {
            annotations: annotations.clone(),
            name: name.clone(),
            generics: generics.clone(),
            param_decls: param_decls
                .iter()
                .map(|p| ParamDecl {
                    name: p.name.clone(),
                    type_: rewrite_type_auto_types(&p.type_, ctx),
                })
                .collect(),
            return_type: return_type
                .as_ref()
                .map(|ty| rewrite_type_auto_types(ty, ctx)),
            throws_types: throws_types
                .iter()
                .map(|ty| rewrite_type_auto_types(ty, ctx))
                .collect(),
            local_decls: local_decls
                .iter()
                .map(|d| rewrite_decl_auto_types(d, ctx))
                .collect(),
            return_expr: rewrite_expr_auto_types(return_expr, ctx),
        },
        Decl::DeclTypeAlias { name, type_ } => Decl::DeclTypeAlias {
            name: name.clone(),
            type_: rewrite_type_auto_types(type_, ctx),
        },
        Decl::DeclExceptionType(ty) => {
            Decl::DeclExceptionType(rewrite_type_auto_types(ty, ctx))
        }
        Decl::DeclExceptionVariant { name, type_ } => Decl::DeclExceptionVariant {
            name: name.clone(),
            type_: rewrite_type_auto_types(type_, ctx),
        },
    }
}

fn rewrite_expr_auto_types(expr: &Expr, ctx: &mut TypeCheckContext) -> Expr {
    match expr {
        Expr::DotRecord(e, label) => {
            Expr::DotRecord(Box::new(rewrite_expr_auto_types(e, ctx)), label.clone())
        }
        Expr::DotTuple(e, idx) => Expr::DotTuple(Box::new(rewrite_expr_auto_types(e, ctx)), *idx),
        Expr::ConstTrue => Expr::ConstTrue,
        Expr::ConstFalse => Expr::ConstFalse,
        Expr::ConstUnit => Expr::ConstUnit,
        Expr::ConstInt(n) => Expr::ConstInt(*n),
        Expr::ConstMemory(addr) => Expr::ConstMemory(*addr),
        Expr::Var(name) => Expr::Var(name.clone()),
        Expr::Inl(e) => Expr::Inl(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Inr(e) => Expr::Inr(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Cons(h, t) => Expr::Cons(
            Box::new(rewrite_expr_auto_types(h, ctx)),
            Box::new(rewrite_expr_auto_types(t, ctx)),
        ),
        Expr::ListHead(e) => Expr::ListHead(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::ListIsEmpty(e) => Expr::ListIsEmpty(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::ListTail(e) => Expr::ListTail(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Succ(e) => Expr::Succ(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::LogicalNot(e) => Expr::LogicalNot(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::NatPred(e) => Expr::NatPred(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::NatIsZero(e) => Expr::NatIsZero(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Fix(e) => Expr::Fix(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::NatRec(n, z, s) => Expr::NatRec(
            Box::new(rewrite_expr_auto_types(n, ctx)),
            Box::new(rewrite_expr_auto_types(z, ctx)),
            Box::new(rewrite_expr_auto_types(s, ctx)),
        ),
        Expr::Fold(ty, e) => Expr::Fold(
            rewrite_type_auto_types(ty, ctx),
            Box::new(rewrite_expr_auto_types(e, ctx)),
        ),
        Expr::Unfold(ty, e) => Expr::Unfold(
            rewrite_type_auto_types(ty, ctx),
            Box::new(rewrite_expr_auto_types(e, ctx)),
        ),
        Expr::Application(fun, args) => Expr::Application(
            Box::new(rewrite_expr_auto_types(fun, ctx)),
            args.iter().map(|a| rewrite_expr_auto_types(a, ctx)).collect(),
        ),
        Expr::TypeApplication(fun, types) => Expr::TypeApplication(
            Box::new(rewrite_expr_auto_types(fun, ctx)),
            types
                .iter()
                .map(|ty| rewrite_type_auto_types(ty, ctx))
                .collect(),
        ),
        Expr::Multiply(l, r) => Expr::Multiply(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Divide(l, r) => Expr::Divide(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::LogicalAnd(l, r) => Expr::LogicalAnd(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Add(l, r) => Expr::Add(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Subtract(l, r) => Expr::Subtract(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::LogicalOr(l, r) => Expr::LogicalOr(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::TypeAscription(e, ty) => Expr::TypeAscription(
            Box::new(rewrite_expr_auto_types(e, ctx)),
            rewrite_type_auto_types(ty, ctx),
        ),
        Expr::Abstraction(params, body) => Expr::Abstraction(
            params
                .iter()
                .map(|p| ParamDecl {
                    name: p.name.clone(),
                    type_: rewrite_type_auto_types(&p.type_, ctx),
                })
                .collect(),
            Box::new(rewrite_expr_auto_types(body, ctx)),
        ),
        Expr::TypeAbstraction(generics, body) => {
            Expr::TypeAbstraction(generics.clone(), Box::new(rewrite_expr_auto_types(body, ctx)))
        }
        Expr::Tuple(elements) => {
            Expr::Tuple(elements.iter().map(|e| rewrite_expr_auto_types(e, ctx)).collect())
        }
        Expr::Record(bindings) => Expr::Record(
            bindings
                .iter()
                .map(|b| Binding {
                    name: b.name.clone(),
                    expr: rewrite_expr_auto_types(&b.expr, ctx),
                })
                .collect(),
        ),
        Expr::Variant(label, rhs) => Expr::Variant(
            label.clone(),
            rhs.as_ref()
                .map(|inner| Box::new(rewrite_expr_auto_types(inner, ctx))),
        ),
        Expr::Match(scrutinee, cases) => Expr::Match(
            Box::new(rewrite_expr_auto_types(scrutinee, ctx)),
            cases
                .iter()
                .map(|case| MatchCase {
                    pattern: rewrite_pattern_auto_types(&case.pattern, ctx),
                    expr: rewrite_expr_auto_types(&case.expr, ctx),
                })
                .collect(),
        ),
        Expr::List(elements) => {
            Expr::List(elements.iter().map(|e| rewrite_expr_auto_types(e, ctx)).collect())
        }
        Expr::If(c, t, e) => Expr::If(
            Box::new(rewrite_expr_auto_types(c, ctx)),
            Box::new(rewrite_expr_auto_types(t, ctx)),
            Box::new(rewrite_expr_auto_types(e, ctx)),
        ),
        Expr::Let(bindings, body) => Expr::Let(
            bindings
                .iter()
                .map(|b| PatternBinding {
                    pattern: rewrite_pattern_auto_types(&b.pattern, ctx),
                    rhs: rewrite_expr_auto_types(&b.rhs, ctx),
                })
                .collect(),
            Box::new(rewrite_expr_auto_types(body, ctx)),
        ),
        Expr::LetRec(bindings, body) => Expr::LetRec(
            bindings
                .iter()
                .map(|b| PatternBinding {
                    pattern: rewrite_pattern_auto_types(&b.pattern, ctx),
                    rhs: rewrite_expr_auto_types(&b.rhs, ctx),
                })
                .collect(),
            Box::new(rewrite_expr_auto_types(body, ctx)),
        ),
        Expr::LessThan(l, r) => Expr::LessThan(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::LessThanOrEqual(l, r) => Expr::LessThanOrEqual(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::GreaterThan(l, r) => Expr::GreaterThan(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::GreaterThanOrEqual(l, r) => Expr::GreaterThanOrEqual(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Equal(l, r) => Expr::Equal(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::NotEqual(l, r) => Expr::NotEqual(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Sequence(l, r) => Expr::Sequence(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::Assignment(l, r) => Expr::Assignment(
            Box::new(rewrite_expr_auto_types(l, ctx)),
            Box::new(rewrite_expr_auto_types(r, ctx)),
        ),
        Expr::TypeCast(e, ty) => Expr::TypeCast(
            Box::new(rewrite_expr_auto_types(e, ctx)),
            rewrite_type_auto_types(ty, ctx),
        ),
        Expr::Reference(e) => Expr::Reference(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Dereference(e) => Expr::Dereference(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::Panic => Expr::Panic,
        Expr::Throw(e) => Expr::Throw(Box::new(rewrite_expr_auto_types(e, ctx))),
        Expr::TryCatch(try_, pat, fallback) => Expr::TryCatch(
            Box::new(rewrite_expr_auto_types(try_, ctx)),
            rewrite_pattern_auto_types(pat, ctx),
            Box::new(rewrite_expr_auto_types(fallback, ctx)),
        ),
        Expr::TryCastAs {
            try_,
            to,
            casted_pattern,
            casted_arm,
            fallback_arm,
        } => Expr::TryCastAs {
            try_: Box::new(rewrite_expr_auto_types(try_, ctx)),
            to: rewrite_type_auto_types(to, ctx),
            casted_pattern: rewrite_pattern_auto_types(casted_pattern, ctx),
            casted_arm: Box::new(rewrite_expr_auto_types(casted_arm, ctx)),
            fallback_arm: Box::new(rewrite_expr_auto_types(fallback_arm, ctx)),
        },
        Expr::TryWith(try_, fallback) => Expr::TryWith(
            Box::new(rewrite_expr_auto_types(try_, ctx)),
            Box::new(rewrite_expr_auto_types(fallback, ctx)),
        ),
    }
}

fn rewrite_pattern_auto_types(pattern: &Pattern, ctx: &mut TypeCheckContext) -> Pattern {
    match pattern {
        Pattern::Var(name) => Pattern::Var(name.clone()),
        Pattern::Inl(p) => Pattern::Inl(Box::new(rewrite_pattern_auto_types(p, ctx))),
        Pattern::Inr(p) => Pattern::Inr(Box::new(rewrite_pattern_auto_types(p, ctx))),
        Pattern::Tuple(patterns) => Pattern::Tuple(
            patterns
                .iter()
                .map(|p| rewrite_pattern_auto_types(p, ctx))
                .collect(),
        ),
        Pattern::Record(fields) => Pattern::Record(
            fields
                .iter()
                .map(|f| LabelledPattern {
                    label: f.label.clone(),
                    pattern: f
                        .pattern
                        .as_ref()
                        .map(|p| rewrite_pattern_auto_types(p, ctx)),
                })
                .collect(),
        ),
        Pattern::Variant(label, rhs) => Pattern::Variant(
            label.clone(),
            rhs.as_ref()
                .map(|p| Box::new(rewrite_pattern_auto_types(p, ctx))),
        ),
        Pattern::List(patterns) => Pattern::List(
            patterns
                .iter()
                .map(|p| rewrite_pattern_auto_types(p, ctx))
                .collect(),
        ),
        Pattern::Cons(h, t) => Pattern::Cons(
            Box::new(rewrite_pattern_auto_types(h, ctx)),
            Box::new(rewrite_pattern_auto_types(t, ctx)),
        ),
        Pattern::Int(n) => Pattern::Int(*n),
        Pattern::Succ(p) => Pattern::Succ(Box::new(rewrite_pattern_auto_types(p, ctx))),
        Pattern::True => Pattern::True,
        Pattern::False => Pattern::False,
        Pattern::Unit => Pattern::Unit,
        Pattern::Ascription(p, ty) => Pattern::Ascription(
            Box::new(rewrite_pattern_auto_types(p, ctx)),
            rewrite_type_auto_types(ty, ctx),
        ),
        Pattern::CastAs(p, ty) => Pattern::CastAs(
            Box::new(rewrite_pattern_auto_types(p, ctx)),
            rewrite_type_auto_types(ty, ctx),
        ),
    }
}

fn rewrite_type_auto_types(ty: &Type, ctx: &mut TypeCheckContext) -> Type {
    match ty {
        Type::Auto => ctx.fresh_meta_type(),
        Type::Fun(params, ret) => Type::Fun(
            params
                .iter()
                .map(|p| rewrite_type_auto_types(p, ctx))
                .collect(),
            Box::new(rewrite_type_auto_types(ret, ctx)),
        ),
        Type::Tuple(types) => {
            Type::Tuple(types.iter().map(|t| rewrite_type_auto_types(t, ctx)).collect())
        }
        Type::Record(fields) => Type::Record(
            fields
                .iter()
                .map(|f| RecordFieldType {
                    label: f.label.clone(),
                    type_: rewrite_type_auto_types(&f.type_, ctx),
                })
                .collect(),
        ),
        Type::Sum(left, right) => Type::Sum(
            Box::new(rewrite_type_auto_types(left, ctx)),
            Box::new(rewrite_type_auto_types(right, ctx)),
        ),
        Type::List(inner) => Type::List(Box::new(rewrite_type_auto_types(inner, ctx))),
        Type::Variant(fields) => Type::Variant(
            fields
                .iter()
                .map(|f| VariantFieldType {
                    label: f.label.clone(),
                    type_: f.type_.as_ref().map(|t| rewrite_type_auto_types(t, ctx)),
                })
                .collect(),
        ),
        Type::Rec(name, inner) => {
            Type::Rec(name.clone(), Box::new(rewrite_type_auto_types(inner, ctx)))
        }
        Type::Ref(inner) => Type::Ref(Box::new(rewrite_type_auto_types(inner, ctx))),
        Type::ForAll(vars, inner) => {
            Type::ForAll(vars.clone(), Box::new(rewrite_type_auto_types(inner, ctx)))
        }
        _ => ty.clone(),
    }
}

// STEP 2: typecheck_decl
fn typecheck_decl(
    decl: &Decl,
    fn_env: &HashMap<String, Type>,
    ctx: &mut TypeCheckContext,
    type_scope: &HashSet<String>,
) -> Result<(), TypeError> {
    let saved_scope = ctx.active_type_scope.clone();
    ctx.active_type_scope = type_scope.clone();

    let result = match decl {
        Decl::DeclFun {
            name,
            param_decls,
            return_type,
            local_decls,
            return_expr,
            ..
        } => typecheck_fun_decl(
            name,
            param_decls,
            return_type,
            local_decls,
            return_expr,
            fn_env,
            ctx,
            type_scope,
        ),
        Decl::DeclGenericFun {
            name,
            generics,
            param_decls,
            return_type,
            local_decls,
            return_expr,
            ..
        } => {
            if !ctx.universal_types_enabled {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: Type::Top,
                    found: Type::ForAll(
                        generics.clone(),
                        Box::new(build_function_type(param_decls, return_type)),
                    ),
                    expr: None,
                });
            }

            if let Some(dup) = find_duplicate_name(generics) {
                return Err(with_function_context(
                    TypeError::ErrorDuplicateTypeParameter(dup),
                    name,
                ));
            }

            let mut generic_scope = type_scope.clone();
            for generic in generics {
                generic_scope.insert(generic.clone());
            }

            let saved_generic_scope = ctx.active_type_scope.clone();
            ctx.active_type_scope = generic_scope.clone();
            let generic_result = typecheck_fun_decl(
                name,
                param_decls,
                return_type,
                local_decls,
                return_expr,
                fn_env,
                ctx,
                &generic_scope,
            );
            ctx.active_type_scope = saved_generic_scope;
            generic_result
        }
        Decl::DeclTypeAlias { type_, .. } => check_type_validity(type_, type_scope, ctx),
        Decl::DeclExceptionType(_) => Ok(()),
        Decl::DeclExceptionVariant { .. } => Ok(()),
    };

    ctx.active_type_scope = saved_scope;
    result
}

fn typecheck_fun_decl(
    name: &str,
    param_decls: &[ParamDecl],
    return_type: &Option<Type>,
    local_decls: &[Decl],
    return_expr: &Expr,
    fn_env: &HashMap<String, Type>,
    ctx: &mut TypeCheckContext,
    type_scope: &HashSet<String>,
) -> Result<(), TypeError> {
    for param in param_decls {
        check_type_validity(&param.type_, type_scope, ctx)
            .map_err(|e| with_function_context(e, name))?;
    }
    if let Some(ret_ty) = return_type {
        check_type_validity(ret_ty, type_scope, ctx)
            .map_err(|e| with_function_context(e, name))?;
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
        match local_decl {
            Decl::DeclFun {
                name: local_name,
                param_decls: local_params,
                return_type: local_return_type,
                ..
            } => {
                let local_fn_type = build_function_type(local_params, local_return_type);
                local_fn_env.insert(local_name.clone(), local_fn_type);
            }
            Decl::DeclGenericFun {
                name: local_name,
                generics,
                param_decls: local_params,
                return_type: local_return_type,
                ..
            } => {
                if !ctx.universal_types_enabled {
                    return Err(with_function_context(
                        TypeError::ErrorUnexpectedTypeForExpression {
                            expected: Type::Top,
                            found: Type::ForAll(
                                generics.clone(),
                                Box::new(build_function_type(local_params, local_return_type)),
                            ),
                            expr: None,
                        },
                        name,
                    ));
                }

                if let Some(dup) = find_duplicate_name(generics) {
                    return Err(with_function_context(
                        TypeError::ErrorDuplicateTypeParameter(dup),
                        name,
                    ));
                }
                let inner = build_function_type(local_params, local_return_type);
                local_fn_env.insert(local_name.clone(), Type::ForAll(generics.clone(), Box::new(inner)));
            }
            _ => {}
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
        typecheck_decl(local_decl, &local_fn_env, ctx, type_scope)
            .map_err(|e| with_function_context(e, name))?;
    }

    let expected_return_type = return_type.clone().unwrap_or(Type::Unit);
    let inferred = infer_expr(return_expr, Some(&expected_return_type), &env, ctx)
        .map_err(|e| with_function_context(e, name))?;
    ensure_expected(return_expr, &inferred, &expected_return_type, ctx)
        .map_err(|e| with_function_context(e, name))?;

    ctx.checked_expr_types.push(inferred);
    Ok(())
}

fn with_function_context(err: TypeError, function_name: &str) -> TypeError {
    match err {
        TypeError::ErrorUndefinedVariable(var) => {
            TypeError::ErrorUndefinedVariable(format!("{} [in function {}]", var, function_name))
        }
        TypeError::ErrorUndefinedTypeVariable(var) => TypeError::ErrorUndefinedTypeVariable(
            format!("{} [in function {}]", var, function_name),
        ),
        TypeError::ErrorDuplicateTypeParameter(name) => TypeError::ErrorDuplicateTypeParameter(
            format!("{} [in function {}]", name, function_name),
        ),
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

fn fresh_named_type_var(ctx: &mut TypeCheckContext, base: &str) -> String {
    ctx.next_type_var_id += 1;
    format!("{}$S{}", base, ctx.next_type_var_id)
}

fn collect_free_type_vars(
    ty: &Type,
    bound: &mut HashSet<String>,
    out: &mut HashSet<String>,
) {
    match ty {
        Type::Var(name) => {
            if !bound.contains(name) {
                out.insert(name.clone());
            }
        }
        Type::Fun(params, ret) => {
            for p in params {
                collect_free_type_vars(p, bound, out);
            }
            collect_free_type_vars(ret, bound, out);
        }
        Type::Tuple(types) => {
            for t in types {
                collect_free_type_vars(t, bound, out);
            }
        }
        Type::Record(fields) => {
            for field in fields {
                collect_free_type_vars(&field.type_, bound, out);
            }
        }
        Type::Sum(left, right) => {
            collect_free_type_vars(left, bound, out);
            collect_free_type_vars(right, bound, out);
        }
        Type::List(inner) => collect_free_type_vars(inner, bound, out),
        Type::Variant(fields) => {
            for field in fields {
                if let Some(field_ty) = &field.type_ {
                    collect_free_type_vars(field_ty, bound, out);
                }
            }
        }
        Type::Ref(inner) => collect_free_type_vars(inner, bound, out),
        Type::Rec(name, inner) => {
            let inserted = bound.insert(name.clone());
            collect_free_type_vars(inner, bound, out);
            if inserted {
                bound.remove(name);
            }
        }
        Type::ForAll(vars, inner) => {
            let mut inserted_vars = Vec::new();
            for var in vars {
                if bound.insert(var.clone()) {
                    inserted_vars.push(var.clone());
                }
            }
            collect_free_type_vars(inner, bound, out);
            for var in inserted_vars {
                bound.remove(&var);
            }
        }
        _ => {}
    }
}

fn free_type_vars_in_subst(subst: &HashMap<String, Type>) -> HashSet<String> {
    let mut out = HashSet::new();
    for ty in subst.values() {
        collect_free_type_vars(ty, &mut HashSet::new(), &mut out);
    }
    out
}

fn fresh_bound_type_var(base: &str, used: &HashSet<String>) -> String {
    let mut idx = 0usize;
    loop {
        let candidate = format!("{}#{}", base, idx);
        if !used.contains(&candidate) {
            return candidate;
        }
        idx += 1;
    }
}

fn substitute_named_type_vars(ty: &Type, subst: &HashMap<String, Type>) -> Type {
    match ty {
        Type::Var(name) => subst.get(name).cloned().unwrap_or_else(|| Type::Var(name.clone())),
        Type::Fun(params, ret) => Type::Fun(
            params
                .iter()
                .map(|p| substitute_named_type_vars(p, subst))
                .collect(),
            Box::new(substitute_named_type_vars(ret, subst)),
        ),
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|t| substitute_named_type_vars(t, subst))
                .collect(),
        ),
        Type::Record(fields) => Type::Record(
            fields
                .iter()
                .map(|f| RecordFieldType {
                    label: f.label.clone(),
                    type_: substitute_named_type_vars(&f.type_, subst),
                })
                .collect(),
        ),
        Type::Sum(left, right) => Type::Sum(
            Box::new(substitute_named_type_vars(left, subst)),
            Box::new(substitute_named_type_vars(right, subst)),
        ),
        Type::List(inner) => Type::List(Box::new(substitute_named_type_vars(inner, subst))),
        Type::Variant(fields) => Type::Variant(
            fields
                .iter()
                .map(|f| VariantFieldType {
                    label: f.label.clone(),
                    type_: f.type_.as_ref().map(|t| substitute_named_type_vars(t, subst)),
                })
                .collect(),
        ),
        Type::Ref(inner) => Type::Ref(Box::new(substitute_named_type_vars(inner, subst))),
        Type::Rec(name, inner) => {
            let forbidden = free_type_vars_in_subst(subst);
            let mut used = forbidden.clone();
            used.insert(name.clone());
            for key in subst.keys() {
                used.insert(key.clone());
            }

            let mut binder_name = name.clone();
            let mut renamed_inner = inner.as_ref().clone();
            if forbidden.contains(name) {
                let fresh = fresh_bound_type_var(name, &used);
                let mut alpha_subst = HashMap::new();
                alpha_subst.insert(name.clone(), Type::Var(fresh.clone()));
                renamed_inner = substitute_named_type_vars(&renamed_inner, &alpha_subst);
                binder_name = fresh;
            }

            let mut next_subst = subst.clone();
            next_subst.remove(name);
            next_subst.remove(&binder_name);

            Type::Rec(
                binder_name,
                Box::new(substitute_named_type_vars(&renamed_inner, &next_subst)),
            )
        }
        Type::ForAll(vars, inner) => {
            let forbidden = free_type_vars_in_subst(subst);
            let mut used = forbidden.clone();
            for var in vars {
                used.insert(var.clone());
            }
            for key in subst.keys() {
                used.insert(key.clone());
            }

            let mut alpha_subst = HashMap::new();
            let mut renamed_vars = Vec::with_capacity(vars.len());
            for var in vars {
                if forbidden.contains(var) {
                    let fresh = fresh_bound_type_var(var, &used);
                    used.insert(fresh.clone());
                    alpha_subst.insert(var.clone(), Type::Var(fresh.clone()));
                    renamed_vars.push(fresh);
                } else {
                    renamed_vars.push(var.clone());
                }
            }

            let renamed_inner = if alpha_subst.is_empty() {
                inner.as_ref().clone()
            } else {
                substitute_named_type_vars(inner, &alpha_subst)
            };

            let mut next_subst = subst.clone();
            for var in vars {
                next_subst.remove(var);
            }
            for var in &renamed_vars {
                next_subst.remove(var);
            }

            Type::ForAll(
                renamed_vars,
                Box::new(substitute_named_type_vars(&renamed_inner, &next_subst)),
            )
        }
        _ => ty.clone(),
    }
}

fn types_alpha_equal(left: &Type, right: &Type) -> bool {
    types_alpha_equal_inner(left, right, &mut HashMap::new(), &mut HashMap::new())
}

fn types_alpha_equal_inner(
    left: &Type,
    right: &Type,
    lr_map: &mut HashMap<String, String>,
    rl_map: &mut HashMap<String, String>,
) -> bool {
    match (left, right) {
        (Type::Bool, Type::Bool)
        | (Type::Nat, Type::Nat)
        | (Type::Unit, Type::Unit)
        | (Type::Top, Type::Top)
        | (Type::Bottom, Type::Bottom)
        | (Type::Auto, Type::Auto) => true,
        (Type::Var(a), Type::Var(b)) => {
            if let Some(mapped) = lr_map.get(a) {
                return mapped == b;
            }
            if let Some(mapped) = rl_map.get(b) {
                return mapped == a;
            }
            a == b
        }
        (Type::Fun(lp, lr), Type::Fun(rp, rr)) => {
            if lp.len() != rp.len() {
                return false;
            }
            lp.iter()
                .zip(rp.iter())
                .all(|(l, r)| types_alpha_equal_inner(l, r, lr_map, rl_map))
                && types_alpha_equal_inner(lr, rr, lr_map, rl_map)
        }
        (Type::Tuple(lt), Type::Tuple(rt)) => {
            lt.len() == rt.len()
                && lt
                    .iter()
                    .zip(rt.iter())
                    .all(|(l, r)| types_alpha_equal_inner(l, r, lr_map, rl_map))
        }
        (Type::Record(lf), Type::Record(rf)) => {
            if lf.len() != rf.len() {
                return false;
            }
            lf.iter().all(|l_field| {
                rf.iter().find(|r_field| r_field.label == l_field.label).is_some_and(
                    |r_field| {
                        types_alpha_equal_inner(&l_field.type_, &r_field.type_, lr_map, rl_map)
                    },
                )
            })
        }
        (Type::Sum(ll, lr), Type::Sum(rl, rr)) => {
            types_alpha_equal_inner(ll, rl, lr_map, rl_map)
                && types_alpha_equal_inner(lr, rr, lr_map, rl_map)
        }
        (Type::List(l), Type::List(r)) => types_alpha_equal_inner(l, r, lr_map, rl_map),
        (Type::Ref(l), Type::Ref(r)) => types_alpha_equal_inner(l, r, lr_map, rl_map),
        (Type::Variant(lf), Type::Variant(rf)) => {
            if lf.len() != rf.len() {
                return false;
            }
            lf.iter().all(|l_field| {
                rf.iter().find(|r_field| r_field.label == l_field.label).is_some_and(
                    |r_field| match (&l_field.type_, &r_field.type_) {
                        (Some(lt), Some(rt)) => types_alpha_equal_inner(lt, rt, lr_map, rl_map),
                        (None, None) => true,
                        _ => false,
                    },
                )
            })
        }
        (Type::Rec(l_name, l_inner), Type::Rec(r_name, r_inner)) => {
            let old_lr = lr_map.insert(l_name.clone(), r_name.clone());
            let old_rl = rl_map.insert(r_name.clone(), l_name.clone());
            let result = types_alpha_equal_inner(l_inner, r_inner, lr_map, rl_map);
            if let Some(old) = old_lr {
                lr_map.insert(l_name.clone(), old);
            } else {
                lr_map.remove(l_name);
            }
            if let Some(old) = old_rl {
                rl_map.insert(r_name.clone(), old);
            } else {
                rl_map.remove(r_name);
            }
            result
        }
        (Type::ForAll(l_vars, l_inner), Type::ForAll(r_vars, r_inner)) => {
            if l_vars.len() != r_vars.len() {
                return false;
            }

            let mut old_lr: Vec<(String, Option<String>)> = Vec::new();
            let mut old_rl: Vec<(String, Option<String>)> = Vec::new();

            for (l, r) in l_vars.iter().zip(r_vars.iter()) {
                old_lr.push((l.clone(), lr_map.insert(l.clone(), r.clone())));
                old_rl.push((r.clone(), rl_map.insert(r.clone(), l.clone())));
            }

            let result = types_alpha_equal_inner(l_inner, r_inner, lr_map, rl_map);

            for (k, old) in old_lr {
                if let Some(v) = old {
                    lr_map.insert(k, v);
                } else {
                    lr_map.remove(&k);
                }
            }
            for (k, old) in old_rl {
                if let Some(v) = old {
                    rl_map.insert(k, v);
                } else {
                    rl_map.remove(&k);
                }
            }

            result
        }
        _ => false,
    }
}

fn occurs_in_meta(name: &str, ty: &Type, ctx: &TypeCheckContext) -> bool {
    let resolved = ctx.resolve_type(ty);
    match resolved {
        Type::Var(var_name) => var_name == name,
        Type::Fun(params, ret) => {
            params.iter().any(|p| occurs_in_meta(name, p, ctx)) || occurs_in_meta(name, &ret, ctx)
        }
        Type::Tuple(types) => types.iter().any(|t| occurs_in_meta(name, t, ctx)),
        Type::Record(fields) => fields
            .iter()
            .any(|f| occurs_in_meta(name, &f.type_, ctx)),
        Type::Sum(left, right) => {
            occurs_in_meta(name, &left, ctx) || occurs_in_meta(name, &right, ctx)
        }
        Type::List(inner) => occurs_in_meta(name, &inner, ctx),
        Type::Variant(fields) => fields
            .iter()
            .any(|f| f.type_.as_ref().is_some_and(|t| occurs_in_meta(name, t, ctx))),
        Type::Ref(inner) => occurs_in_meta(name, &inner, ctx),
        Type::Rec(_, inner) => occurs_in_meta(name, &inner, ctx),
        Type::ForAll(_, inner) => occurs_in_meta(name, &inner, ctx),
        _ => false,
    }
}

fn bind_meta_var(name: &str, ty: &Type, ctx: &mut TypeCheckContext) -> Result<(), TypeError> {
    let resolved_ty = ctx.resolve_type(ty);
    if let Type::Var(other) = &resolved_ty {
        if other == name {
            return Ok(());
        }
    }
    if occurs_in_meta(name, &resolved_ty, ctx) {
        return Err(TypeError::ErrorOccursCheckInfiniteType);
    }
    ctx.type_var_subst.insert(name.to_string(), resolved_ty);
    Ok(())
}

fn try_unify_types(found: &Type, expected: &Type, ctx: &mut TypeCheckContext) -> Result<bool, TypeError> {
    let left = ctx.resolve_type(found);
    let right = ctx.resolve_type(expected);

    if types_alpha_equal(&left, &right) {
        return Ok(true);
    }

    if let Type::Var(name) = &left {
        if TypeCheckContext::is_meta_var_name(name) {
            bind_meta_var(name, &right, ctx)?;
            return Ok(true);
        }
    }
    if let Type::Var(name) = &right {
        if TypeCheckContext::is_meta_var_name(name) {
            bind_meta_var(name, &left, ctx)?;
            return Ok(true);
        }
    }

    match (left, right) {
        (Type::Fun(left_params, left_ret), Type::Fun(right_params, right_ret)) => {
            if left_params.len() != right_params.len() {
                return Ok(false);
            }
            for (l, r) in left_params.iter().zip(right_params.iter()) {
                if !try_unify_types(l, r, ctx)? {
                    return Ok(false);
                }
            }
            try_unify_types(&left_ret, &right_ret, ctx)
        }
        (Type::Tuple(left_types), Type::Tuple(right_types)) => {
            if left_types.len() != right_types.len() {
                return Ok(false);
            }
            for (l, r) in left_types.iter().zip(right_types.iter()) {
                if !try_unify_types(l, r, ctx)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        (Type::Record(left_fields), Type::Record(right_fields)) => {
            if left_fields.len() != right_fields.len() {
                return Ok(false);
            }
            for left_field in &left_fields {
                let Some(right_field) = right_fields
                    .iter()
                    .find(|f| f.label == left_field.label)
                else {
                    return Ok(false);
                };
                if !try_unify_types(&left_field.type_, &right_field.type_, ctx)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        (Type::Sum(ll, lr), Type::Sum(rl, rr)) => {
            Ok(try_unify_types(&ll, &rl, ctx)? && try_unify_types(&lr, &rr, ctx)?)
        }
        (Type::List(l), Type::List(r)) => try_unify_types(&l, &r, ctx),
        (Type::Ref(l), Type::Ref(r)) => try_unify_types(&l, &r, ctx),
        (Type::Variant(left_labels), Type::Variant(right_labels)) => {
            if left_labels.len() != right_labels.len() {
                return Ok(false);
            }
            for left_label in &left_labels {
                let Some(right_label) = right_labels
                    .iter()
                    .find(|f| f.label == left_label.label)
                else {
                    return Ok(false);
                };
                match (&left_label.type_, &right_label.type_) {
                    (Some(lt), Some(rt)) => {
                        if !try_unify_types(lt, rt, ctx)? {
                            return Ok(false);
                        }
                    }
                    (None, None) => {}
                    _ => return Ok(false),
                }
            }
            Ok(true)
        }
        (Type::ForAll(left_vars, left_inner), Type::ForAll(right_vars, right_inner)) => {
            if left_vars.len() != right_vars.len() {
                return Ok(false);
            }
            let mut subst = HashMap::new();
            for (r, l) in right_vars.iter().zip(left_vars.iter()) {
                subst.insert(r.clone(), Type::Var(l.clone()));
            }
            let renamed_right = substitute_named_type_vars(&right_inner, &subst);
            try_unify_types(&left_inner, &renamed_right, ctx)
        }
        _ => Ok(false),
    }
}

fn require_unification(found: &Type, expected: &Type, ctx: &mut TypeCheckContext) -> Result<(), TypeError> {
    if try_unify_types(found, expected, ctx)? {
        return Ok(());
    }

    Err(TypeError::ErrorUnexpectedTypeForExpression {
        expected: ctx.resolve_type(expected),
        found: ctx.resolve_type(found),
        expr: None,
    })
}

fn is_unresolved_meta_var(ty: &Type, ctx: &TypeCheckContext) -> bool {
    match ctx.resolve_type(ty) {
        Type::Var(name) => TypeCheckContext::is_meta_var_name(&name),
        _ => false,
    }
}

fn ensure_function_shape(ty: &Type, arity: usize, ctx: &mut TypeCheckContext) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let params: Vec<Type> = (0..arity).map(|_| ctx.fresh_meta_type()).collect();
        let ret = ctx.fresh_meta_type();
        let shape = Type::Fun(params, Box::new(ret));
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn ensure_ref_shape(ty: &Type, ctx: &mut TypeCheckContext) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let inner = ctx.fresh_meta_type();
        let shape = Type::Ref(Box::new(inner));
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn ensure_list_shape(ty: &Type, ctx: &mut TypeCheckContext) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let inner = ctx.fresh_meta_type();
        let shape = Type::List(Box::new(inner));
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn ensure_tuple_shape(ty: &Type, tuple_len: usize, ctx: &mut TypeCheckContext) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let shape = Type::Tuple((0..tuple_len).map(|_| ctx.fresh_meta_type()).collect());
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn ensure_record_shape_from_fields(
    ty: &Type,
    field_names: &[String],
    ctx: &mut TypeCheckContext,
) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let shape = Type::Record(
            field_names
                .iter()
                .map(|name| RecordFieldType {
                    label: name.clone(),
                    type_: ctx.fresh_meta_type(),
                })
                .collect(),
        );
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn ensure_sum_shape(ty: &Type, ctx: &mut TypeCheckContext) -> Result<Type, TypeError> {
    let resolved = ctx.resolve_type(ty);
    if is_unresolved_meta_var(&resolved, ctx) {
        let shape = Type::Sum(Box::new(ctx.fresh_meta_type()), Box::new(ctx.fresh_meta_type()));
        require_unification(&resolved, &shape, ctx)?;
        return Ok(ctx.resolve_type(&shape));
    }
    Ok(resolved)
}

fn contains_unresolved_meta(ty: &Type, ctx: &TypeCheckContext) -> bool {
    match ctx.resolve_type(ty) {
        Type::Var(name) => TypeCheckContext::is_meta_var_name(&name),
        Type::Fun(params, ret) => {
            params.iter().any(|p| contains_unresolved_meta(p, ctx))
                || contains_unresolved_meta(&ret, ctx)
        }
        Type::Tuple(types) => types.iter().any(|t| contains_unresolved_meta(t, ctx)),
        Type::Record(fields) => fields
            .iter()
            .any(|f| contains_unresolved_meta(&f.type_, ctx)),
        Type::Sum(left, right) => {
            contains_unresolved_meta(&left, ctx) || contains_unresolved_meta(&right, ctx)
        }
        Type::List(inner) => contains_unresolved_meta(&inner, ctx),
        Type::Variant(fields) => fields
            .iter()
            .any(|f| f.type_.as_ref().is_some_and(|t| contains_unresolved_meta(t, ctx))),
        Type::Ref(inner) => contains_unresolved_meta(&inner, ctx),
        Type::Rec(_, inner) => contains_unresolved_meta(&inner, ctx),
        Type::ForAll(_, inner) => contains_unresolved_meta(&inner, ctx),
        _ => false,
    }
}

fn check_ambiguous_types(fn_env: &HashMap<String, Type>, ctx: &TypeCheckContext) -> Result<(), TypeError> {
    if fn_env
        .values()
        .map(|ty| ctx.resolve_type(ty))
        .any(|ty| contains_unresolved_meta(&ty, ctx))
    {
        return Err(TypeError::ErrorAmbiguousType);
    }

    if let Some(exn_ty) = &ctx.exception_type {
        if contains_unresolved_meta(&ctx.resolve_type(exn_ty), ctx) {
            return Err(TypeError::ErrorAmbiguousType);
        }
    }

    if ctx
        .checked_expr_types
        .iter()
        .map(|ty| ctx.resolve_type(ty))
        .any(|ty| contains_unresolved_meta(&ty, ctx))
    {
        return Err(TypeError::ErrorAmbiguousType);
    }

    Ok(())
}

fn effective_type_scope(env: &TypeEnv, ctx: &TypeCheckContext) -> HashSet<String> {
    let mut scope = ctx.active_type_scope.clone();
    for ty in env.values() {
        let resolved = ctx.resolve_type(ty);
        collect_free_type_vars(&resolved, &mut HashSet::new(), &mut scope);
    }
    scope
}

fn unfold_recursive_type_annotation(rec_ty: &Type) -> Option<Type> {
    match rec_ty {
        Type::Rec(var, body) => {
            let mut subst = HashMap::new();
            subst.insert(var.clone(), rec_ty.clone());
            Some(substitute_named_type_vars(body, &subst))
        }
        _ => None,
    }
}

// STEP 3: infer_expr
fn infer_expr(
    expr: &Expr,
    expected: Option<&Type>,
    env: &TypeEnv,
    ctx: &mut TypeCheckContext,
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
            Some(expected_ty) => {
                let shaped = ensure_ref_shape(expected_ty, ctx)?;
                match shaped {
                    Type::Ref(inner) => Type::Ref(inner),
                    _ => return Err(TypeError::ErrorUnexpectedMemoryAddress),
                }
            }
            None => {
                if ctx.type_reconstruction_enabled {
                    Type::Ref(Box::new(ctx.fresh_meta_type()))
                } else {
                    return Err(TypeError::ErrorAmbiguousReferenceType);
                }
            }
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
        Expr::Abstraction(params, body) => {
            let type_scope = effective_type_scope(env, ctx);
            for param in params {
                check_type_validity(&param.type_, &type_scope, ctx)?;
            }

            let expected_shape = match expected {
                Some(expected_ty) => Some(ensure_function_shape(expected_ty, params.len(), ctx)?),
                None => None,
            };

            match expected_shape {
                Some(Type::Fun(param_types, return_type)) => {
                    if params.len() != param_types.len() {
                        return Err(TypeError::ErrorUnexpectedNumberOfParametersInLambda {
                            expected: param_types.len(),
                            found: params.len(),
                        });
                    }

                    let mut new_env = env.clone();
                    for (param, expected_param_type) in params.iter().zip(param_types.iter()) {
                        if ctx.has_extension("structural-subtyping") {
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
                        } else if ctx.type_reconstruction_enabled {
                            if !try_unify_types(&param.type_, expected_param_type, ctx)? {
                                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                                    expected: ctx.resolve_type(expected_param_type),
                                    found: ctx.resolve_type(&param.type_),
                                    expr: Some(format!("{}", expr)),
                                });
                            }
                        } else if !types_alpha_equal(&param.type_, expected_param_type) {
                            return Err(TypeError::ErrorUnexpectedTypeForParameter {
                                expected: expected_param_type.clone(),
                                found: param.type_.clone(),
                            });
                        }
                        new_env.insert(param.name.clone(), ctx.resolve_type(&param.type_));
                    }

                    let body_ty = infer_expr(body, Some(&return_type), &new_env, ctx)?;
                    ensure_expected(body, &body_ty, &return_type, ctx)?;
                    let declared_param_types: Vec<Type> =
                        params.iter().map(|p| ctx.resolve_type(&p.type_)).collect();
                    let inferred_fun = Type::Fun(declared_param_types, Box::new(body_ty));
                    return Ok(ctx.resolve_type(&inferred_fun));
                }
                Some(other) => return Err(TypeError::ErrorUnexpectedLambda(other)),
                None => {
                    let mut new_env = env.clone();
                    for param in params {
                        new_env.insert(param.name.clone(), param.type_.clone());
                    }
                    let return_type = infer_expr(body, None, &new_env, ctx)?;
                    let param_types: Vec<Type> =
                        params.iter().map(|p| ctx.resolve_type(&p.type_)).collect();
                    Type::Fun(param_types, Box::new(return_type))
                }
            }
        }

        //  Application 
        Expr::Application(func, args) => {
            let raw_func_type = infer_expr(func, None, env, ctx)?;
            let func_type = ensure_function_shape(&raw_func_type, args.len(), ctx)?;
            match func_type {
                Type::Fun(param_types, return_type) => {
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

        Expr::Tuple(elements) => {
            let expected_tuple = match expected {
                Some(expected_ty) => Some(ensure_tuple_shape(expected_ty, elements.len(), ctx)?),
                None => None,
            };

            match expected_tuple {
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
                    Type::Tuple(expected_types)
                }
                Some(other) => return Err(TypeError::ErrorUnexpectedTuple(other)),
                None => {
                    let mut inferred = Vec::with_capacity(elements.len());
                    for elem in elements {
                        inferred.push(infer_expr(elem, None, env, ctx)?);
                    }
                    Type::Tuple(inferred)
                }
            }
        }

        Expr::DotTuple(tuple_expr, index) => {
            let tuple_type = infer_expr(tuple_expr, None, env, ctx)?;
            let tuple_type = ensure_tuple_shape(&tuple_type, *index, ctx)?;
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

        Expr::Record(bindings) => {
            let field_names: Vec<String> = bindings.iter().map(|b| b.name.clone()).collect();
            let expected_record = match expected {
                Some(expected_ty) => Some(ensure_record_shape_from_fields(expected_ty, &field_names, ctx)?),
                None => None,
            };

            match expected_record {
                Some(Type::Record(expected_fields)) => {
                    check_duplicate_record_fields(bindings)?;

                    if !ctx.has_extension("structural-subtyping") {
                        let unexpected: Vec<String> = bindings
                            .iter()
                            .filter(|b| !expected_fields.iter().any(|f| f.label == b.name))
                            .map(|b| b.name.clone())
                            .collect();
                        if !unexpected.is_empty() {
                            return Err(TypeError::ErrorUnexpectedRecordFields(unexpected));
                        }
                    }

                    for expected_field in &expected_fields {
                        let binding = bindings
                            .iter()
                            .find(|b| b.name == expected_field.label)
                            .ok_or_else(|| {
                                TypeError::ErrorMissingRecordFields(vec![expected_field.label.clone()])
                            })?;
                        infer_expr(&binding.expr, Some(&expected_field.type_), env, ctx)?;
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
                Some(other) => return Err(TypeError::ErrorUnexpectedRecord(other)),
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
            }
        }

        Expr::DotRecord(record_expr, field_name) => {
            let record_type = infer_expr(record_expr, None, env, ctx)?;
            let record_type = ensure_record_shape_from_fields(
                &record_type,
                &[field_name.clone()],
                ctx,
            )?;
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
            let type_scope = effective_type_scope(env, ctx);
            check_type_validity(ty, &type_scope, ctx)?;

            let inner_ty = infer_expr(e, Some(ty), env, ctx)?;
            ensure_expected(e, &inner_ty, ty, ctx)?;
            if let Some(expected_ty) = expected {
                ensure_expected(expr, ty, expected_ty, ctx)?;
            }
            return Ok(ctx.resolve_type(ty));
        }

        // #ambiguous-type-as-bottom: without an expected sum type, this extension
        // permits the missing side to be treated as Bottom.
        Expr::Inl(e) => {
            let expected_sum = match expected {
                Some(expected_ty) => Some(ensure_sum_shape(expected_ty, ctx)?),
                None => None,
            };
            match expected_sum {
                Some(Type::Sum(left_type, right_type)) => {
                    infer_expr(e, Some(&left_type), env, ctx)?;
                    Type::Sum(left_type, right_type)
                }
                Some(other) => {
                    if ctx.type_reconstruction_enabled {
                        let left = infer_expr(e, None, env, ctx)?;
                        let _ = other;
                        Type::Sum(Box::new(left), Box::new(ctx.fresh_meta_type()))
                    } else {
                        return Err(TypeError::ErrorUnexpectedInjection(other));
                    }
                }
                None => {
                    if ctx.type_reconstruction_enabled {
                        let left = infer_expr(e, None, env, ctx)?;
                        Type::Sum(Box::new(left), Box::new(ctx.fresh_meta_type()))
                    } else if ctx.has_extension("ambiguous-type-as-bottom") {
                        let left = infer_expr(e, None, env, ctx)?;
                        Type::Sum(Box::new(left), Box::new(Type::Bottom))
                    } else {
                        return Err(TypeError::ErrorAmbiguousSumType);
                    }
                }
            }
        }

        // #ambiguous-type-as-bottom: symmetric rule for right injection.
        Expr::Inr(e) => {
            let expected_sum = match expected {
                Some(expected_ty) => Some(ensure_sum_shape(expected_ty, ctx)?),
                None => None,
            };
            match expected_sum {
                Some(Type::Sum(left_type, right_type)) => {
                    infer_expr(e, Some(&right_type), env, ctx)?;
                    Type::Sum(left_type, right_type)
                }
                Some(other) => {
                    if ctx.type_reconstruction_enabled {
                        let right = infer_expr(e, None, env, ctx)?;
                        let _ = other;
                        Type::Sum(Box::new(ctx.fresh_meta_type()), Box::new(right))
                    } else {
                        return Err(TypeError::ErrorUnexpectedInjection(other));
                    }
                }
                None => {
                    if ctx.type_reconstruction_enabled {
                        let right = infer_expr(e, None, env, ctx)?;
                        Type::Sum(Box::new(ctx.fresh_meta_type()), Box::new(right))
                    } else if ctx.has_extension("ambiguous-type-as-bottom") {
                        let right = infer_expr(e, None, env, ctx)?;
                        Type::Sum(Box::new(Type::Bottom), Box::new(right))
                    } else {
                        return Err(TypeError::ErrorAmbiguousSumType);
                    }
                }
            }
        }

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
                let pattern_env = typecheck_pattern(&case.pattern, &scrutinee_type, env, ctx)?;

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

            let scrutinee_type = ctx.resolve_type(&scrutinee_type);

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

        Expr::List(elements) => {
            let expected_list = match expected {
                Some(expected_ty) => Some(ensure_list_shape(expected_ty, ctx)?),
                None => None,
            };

            match expected_list {
                Some(Type::List(elem_type)) => {
                    for elem in elements {
                        infer_expr(elem, Some(&elem_type), env, ctx)?;
                    }
                    Type::List(elem_type)
                }
                Some(other) => {
                    if ctx.type_reconstruction_enabled {
                        if elements.is_empty() {
                            Type::List(Box::new(ctx.fresh_meta_type()))
                        } else {
                            let elem_ty = infer_expr(&elements[0], None, env, ctx)?;
                            for elem in &elements[1..] {
                                let actual = infer_expr(elem, Some(&elem_ty), env, ctx)?;
                                ensure_expected(elem, &actual, &elem_ty, ctx)?;
                            }
                            let _ = other;
                            Type::List(Box::new(elem_ty))
                        }
                    } else {
                        return Err(TypeError::ErrorUnexpectedList(other));
                    }
                }
                None => {
                    if elements.is_empty() {
                        if ctx.type_reconstruction_enabled {
                            Type::List(Box::new(ctx.fresh_meta_type()))
                        } else if ctx.has_extension("ambiguous-type-as-bottom") {
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
            }
        }

        Expr::Cons(head, tail) => {
            let expected_list = match expected {
                Some(expected_ty) => Some(ensure_list_shape(expected_ty, ctx)?),
                None => None,
            };
            match expected_list {
                Some(Type::List(elem_type)) => {
                    infer_expr(head, Some(&elem_type), env, ctx)?;
                    infer_expr(tail, Some(&Type::List(elem_type.clone())), env, ctx)?;
                    Type::List(elem_type)
                }
                Some(other) => {
                    if ctx.type_reconstruction_enabled {
                        let head_ty = infer_expr(head, None, env, ctx)?;
                        infer_expr(
                            tail,
                            Some(&Type::List(Box::new(head_ty.clone()))),
                            env,
                            ctx,
                        )?;
                        let _ = other;
                        Type::List(Box::new(head_ty))
                    } else {
                        return Err(TypeError::ErrorUnexpectedList(other));
                    }
                }
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
            }
        }

        Expr::ListHead(list) => {
            let list_type = infer_expr(list, None, env, ctx)?;
            let list_type = ensure_list_shape(&list_type, ctx)?;
            match list_type {
                Type::List(elem_type) => *elem_type,
                _ => return Err(TypeError::ErrorNotAList(list_type)),
            }
        }

        Expr::ListTail(list) => {
            let list_type = infer_expr(list, None, env, ctx)?;
            let list_type = ensure_list_shape(&list_type, ctx)?;
            match list_type {
                Type::List(_) => list_type,
                _ => return Err(TypeError::ErrorNotAList(list_type)),
            }
        }

        Expr::ListIsEmpty(list) => {
            let inferred = infer_expr(list, None, env, ctx)?;
            let inferred = ensure_list_shape(&inferred, ctx)?;
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
                let pat_env = typecheck_pattern(&binding.pattern, &rhs_ty, &new_env, ctx)?;
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
            let raw_f_type = infer_expr(f, None, env, ctx)?;
            let f_type = ensure_function_shape(&raw_f_type, 1, ctx)?;
            let f_type_clone = f_type.clone();
            match f_type {
                Type::Fun(param_types, return_type) => {
                    if param_types.len() != 1 {
                        return Err(TypeError::ErrorNotAFunction(f_type_clone));
                    }
                    let same = if ctx.type_reconstruction_enabled {
                        try_unify_types(&param_types[0], &return_type, ctx)?
                    } else {
                        types_alpha_equal(&param_types[0], &return_type)
                    };
                    if !same {
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
        Expr::Reference(e) => {
            let expected_ref = match expected {
                Some(expected_ty) => Some(ensure_ref_shape(expected_ty, ctx)?),
                None => None,
            };

            match expected_ref {
                Some(Type::Ref(inner)) => {
                    let found_inner = infer_expr(e, Some(&inner), env, ctx)?;
                    Type::Ref(Box::new(found_inner))
                }
                Some(_) => return Err(TypeError::ErrorUnexpectedReference),
                None => {
                    let inner = infer_expr(e, None, env, ctx)?;
                    Type::Ref(Box::new(inner))
                }
            }
        }

        // #references: dereference requires Ref(t) and returns t.
        Expr::Dereference(e) => {
            let expected_ref_type = expected.map(|ty| Type::Ref(Box::new(ty.clone())));
            let t = infer_expr(e, expected_ref_type.as_ref(), env, ctx)?;
            let t = ensure_ref_shape(&t, ctx)?;
            match t {
                Type::Ref(inner) => *inner,
                _ => return Err(TypeError::ErrorNotAReference(t)),
            }
        }

        // #references: assignment requires a Ref(lhs_type), checks rhs against lhs_type.
        Expr::Assignment(lhs, rhs) => {
            let t = infer_expr(lhs, None, env, ctx)?;
            let t = ensure_ref_shape(&t, ctx)?;
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
                if ctx.type_reconstruction_enabled {
                    ctx.fresh_meta_type()
                } else if ctx.has_extension("ambiguous-type-as-bottom") {
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
                .clone()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;
            infer_expr(e, Some(&exn_ty), env, ctx)?;
            match expected {
                Some(t) => t.clone(),
                None => {
                    if ctx.type_reconstruction_enabled {
                        ctx.fresh_meta_type()
                    } else if ctx.has_extension("ambiguous-type-as-bottom") {
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
                .clone()
                .ok_or(TypeError::ErrorExceptionTypeNotDeclared)?;
            let try_ty = infer_expr(try_expr, expected, env, ctx)?;
            let mut env2 = env.clone();
            env2.extend(typecheck_pattern(pattern, &exn_ty, env, ctx)?);
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
            casted_env.extend(typecheck_pattern(casted_pattern, to, env, ctx)?);

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

        Expr::TypeAbstraction(generics, inner) => {
            if !ctx.universal_types_enabled {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: expected.cloned().unwrap_or(Type::Top),
                    found: Type::ForAll(generics.clone(), Box::new(Type::Top)),
                    expr: Some(format!("{}", expr)),
                });
            }

            if let Some(dup) = find_duplicate_name(generics) {
                return Err(TypeError::ErrorDuplicateTypeParameter(dup));
            }

            let used_names = effective_type_scope(env, ctx);
            let mut shadow_subst = HashMap::new();
            let mut unshadow_subst = HashMap::new();
            for generic in generics {
                if used_names.contains(generic) {
                    let fresh = fresh_named_type_var(ctx, generic);
                    shadow_subst.insert(generic.clone(), Type::Var(fresh.clone()));
                    unshadow_subst.insert(fresh, Type::Var(generic.clone()));
                }
            }

            let mut adjusted_env = env.clone();
            for ty in adjusted_env.values_mut() {
                *ty = substitute_named_type_vars(ty, &shadow_subst);
            }

            let saved_scope = ctx.active_type_scope.clone();
            for generic in generics {
                ctx.active_type_scope.insert(generic.clone());
            }

            let inferred_inner_res = (|| -> Result<Type, TypeError> {
                match expected {
                    Some(Type::ForAll(expected_generics, expected_inner)) => {
                        if generics.len() != expected_generics.len() {
                            return Err(TypeError::ErrorIncorrectNumberOfTypeArguments {
                                expected: expected_generics.len(),
                                found: generics.len(),
                            });
                        }

                        let mut rename_subst = HashMap::new();
                        for (expected_name, local_name) in
                            expected_generics.iter().zip(generics.iter())
                        {
                            rename_subst.insert(
                                expected_name.clone(),
                                Type::Var(local_name.clone()),
                            );
                        }
                        let renamed_expected_inner =
                            substitute_named_type_vars(expected_inner, &rename_subst);

                        infer_expr(inner, Some(&renamed_expected_inner), &adjusted_env, ctx)
                    }
                    Some(other) => {
                        let found_inner = infer_expr(inner, None, &adjusted_env, ctx)?;
                        let found_inner = if unshadow_subst.is_empty() {
                            found_inner
                        } else {
                            substitute_named_type_vars(&found_inner, &unshadow_subst)
                        };
                        Err(TypeError::ErrorUnexpectedTypeForExpression {
                            expected: other.clone(),
                            found: Type::ForAll(generics.clone(), Box::new(found_inner)),
                            expr: Some(format!("{}", expr)),
                        })
                    }
                    None => infer_expr(inner, None, &adjusted_env, ctx),
                }
            })();

            ctx.active_type_scope = saved_scope;
            let inferred_inner = inferred_inner_res?;
            let restored_inner = if unshadow_subst.is_empty() {
                inferred_inner
            } else {
                substitute_named_type_vars(&inferred_inner, &unshadow_subst)
            };

            let inferred_forall = Type::ForAll(generics.clone(), Box::new(restored_inner));
            return Ok(ctx.resolve_type(&inferred_forall));
        }

        Expr::TypeApplication(fun, type_args) => {
            if !ctx.universal_types_enabled {
                return Err(TypeError::ErrorNotAGenericFunction);
            }

            let fun_type = infer_expr(fun, None, env, ctx)?;
            let fun_type = ctx.resolve_type(&fun_type);

            let (generic_params, inner_type) = match fun_type {
                Type::ForAll(generic_params, inner_type) => (generic_params, inner_type),
                _ => return Err(TypeError::ErrorNotAGenericFunction),
            };

            if generic_params.len() != type_args.len() {
                return Err(TypeError::ErrorIncorrectNumberOfTypeArguments {
                    expected: generic_params.len(),
                    found: type_args.len(),
                });
            }

            let type_arg_scope = effective_type_scope(env, ctx);
            for type_arg in type_args {
                check_type_validity(type_arg, &type_arg_scope, ctx)?;
            }

            let mut subst = HashMap::new();
            for (name, arg) in generic_params.iter().zip(type_args.iter()) {
                subst.insert(name.clone(), arg.clone());
            }

            substitute_named_type_vars(&inner_type, &subst)
        }

        Expr::Fold(rec_ty, inner_expr) => {
            let type_scope = effective_type_scope(env, ctx);
            check_type_validity(rec_ty, &type_scope, ctx)?;
            let rec_ty = ctx.resolve_type(rec_ty);
            let Some(unfolded_ty) = unfold_recursive_type_annotation(&rec_ty) else {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: Type::Rec("X".to_string(), Box::new(ctx.fresh_meta_type())),
                    found: rec_ty,
                    expr: Some(format!("{}", expr)),
                });
            };

            infer_expr(inner_expr, Some(&unfolded_ty), env, ctx)?;
            rec_ty
        }

        Expr::Unfold(rec_ty, inner_expr) => {
            let type_scope = effective_type_scope(env, ctx);
            check_type_validity(rec_ty, &type_scope, ctx)?;
            let rec_ty = ctx.resolve_type(rec_ty);
            let Some(unfolded_ty) = unfold_recursive_type_annotation(&rec_ty) else {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: Type::Rec("X".to_string(), Box::new(ctx.fresh_meta_type())),
                    found: rec_ty,
                    expr: Some(format!("{}", expr)),
                });
            };

            infer_expr(inner_expr, Some(&rec_ty), env, ctx)?;
            unfolded_ty
        }
    };

    if let Some(expected_ty) = expected {
        if !types_match(&inferred, expected_ty, ctx) {
            ensure_expected(expr, &inferred, expected_ty, ctx)?;
        }
    }

    Ok(ctx.resolve_type(&inferred))
}

// STEP 4: ensure_expected / types_match / is_subtype
fn ensure_expected(
    expr: &Expr,
    found: &Type,
    expected: &Type,
    ctx: &mut TypeCheckContext,
) -> Result<(), TypeError> {
    if ctx.type_reconstruction_enabled && try_unify_types(found, expected, ctx)? {
        return Ok(());
    }

    if types_match(found, expected, ctx) {
        return Ok(());
    }

    let found = ctx.resolve_type(found);
    let expected = ctx.resolve_type(expected);

    if let (Type::Variant(found_labels), Type::Variant(expected_labels)) = (&found, &expected) {
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
            expected,
            found,
            expr: Some(format!("{}", expr)),
        });
    }

    Err(TypeError::ErrorUnexpectedTypeForExpression {
        expected,
        found,
        expr: Some(format!("{}", expr)),
    })
}

fn types_match(found: &Type, expected: &Type, ctx: &TypeCheckContext) -> bool {
    let found = ctx.resolve_type(found);
    let expected = ctx.resolve_type(expected);

    // #structural-subtyping: toggles matching from equality to subtype checking.
    if ctx.has_extension("structural-subtyping") {
        is_subtype(&found, &expected)
    } else {
        types_alpha_equal(&found, &expected)
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
    ctx: &mut TypeCheckContext,
) -> Result<TypeEnv, TypeError> {
    //  #structural-patterns: reject duplicate variable names 
    check_duplicate_pattern_variables(pattern)?;

    typecheck_pattern_inner(pattern, expected_type, env, ctx)
}

/// Recursive descent that does the actual type-directed pattern checking.
/// Separated so that duplicate-variable detection only runs at the top level.
fn typecheck_pattern_inner(
    pattern: &Pattern,
    expected_type: &Type,
    env: &TypeEnv,
    ctx: &mut TypeCheckContext,
) -> Result<TypeEnv, TypeError> {
    let mut new_env = TypeEnv::new();
    let expected_type = ctx.resolve_type(expected_type);

    match pattern {
        Pattern::Var(name) => {
            new_env.insert(name.clone(), expected_type);
            Ok(new_env)
        }

        Pattern::Inl(p) => {
            let shaped = ensure_sum_shape(&expected_type, ctx)?;
            match shaped {
            Type::Sum(left_type, _) => typecheck_pattern_inner(p, &left_type, env, ctx),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: "inl".to_string(),
            }),
            }
        }

        Pattern::Inr(p) => {
            let shaped = ensure_sum_shape(&expected_type, ctx)?;
            match shaped {
            Type::Sum(_, right_type) => typecheck_pattern_inner(p, &right_type, env, ctx),
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: "inr".to_string(),
            }),
            }
        }

        //  Variant pattern with nullary-label error codes 
        Pattern::Variant(label, opt_pattern) => match &expected_type {
            Type::Variant(fields) => {
                let field = fields
                    .iter()
                    .find(|f| &f.label == label)
                    .ok_or_else(|| TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type.clone(),
                        pattern: label.clone(),
                    })?;

                match (&field.type_, opt_pattern) {
                    (Some(field_ty), Some(pat)) => {
                        typecheck_pattern_inner(pat, field_ty, env, ctx)
                    }
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
            let ok = if ctx.type_reconstruction_enabled {
                try_unify_types(ty, &expected_type, ctx)?
            } else {
                types_alpha_equal(ty, &expected_type)
            };
            if !ok {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern_inner(pat, ty, env, ctx)
        }

        // #type-cast-patterns: pattern cast is valid only when cast type is a
        // subtype of the expected scrutinee type.
        Pattern::CastAs(pat, ty) => {
            if !is_subtype(ty, &expected_type) {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern_inner(pat, ty, env, ctx)
        }

        Pattern::Tuple(patterns) => {
            let shaped = ensure_tuple_shape(&expected_type, patterns.len(), ctx)?;
            match shaped {
            Type::Tuple(elem_types) => {
                if patterns.len() != elem_types.len() {
                    return Err(TypeError::ErrorUnexpectedPatternForType {
                        expected: expected_type,
                        pattern: format!("{}", pattern),
                    });
                }
                for (pat, elem_ty) in patterns.iter().zip(elem_types.iter()) {
                    new_env.extend(typecheck_pattern_inner(pat, elem_ty, env, ctx)?);
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: format!("{}", pattern),
            }),
            }
        }

        Pattern::Record(fields) => {
            let labels: Vec<String> = fields.iter().map(|f| f.label.clone()).collect();
            let shaped = ensure_record_shape_from_fields(&expected_type, &labels, ctx)?;
            match shaped {
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
                        expected: expected_type,
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
                            ctx,
                        )?);
                    }
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: format!("{}", pattern),
            }),
            }
        }

        Pattern::List(patterns) => {
            let shaped = ensure_list_shape(&expected_type, ctx)?;
            match shaped {
            Type::List(elem_type) => {
                for pat in patterns {
                    new_env.extend(typecheck_pattern_inner(pat, &elem_type, env, ctx)?);
                }
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: format!("{}", pattern),
            }),
            }
        }

        Pattern::Cons(head, tail) => {
            let shaped = ensure_list_shape(&expected_type, ctx)?;
            match shaped {
            Type::List(elem_type) => {
                new_env.extend(typecheck_pattern_inner(head, &elem_type, env, ctx)?);
                new_env.extend(typecheck_pattern_inner(
                    tail,
                    &Type::List(elem_type.clone()),
                    env,
                    ctx,
                )?);
                Ok(new_env)
            }
            _ => Err(TypeError::ErrorUnexpectedPatternForType {
                expected: expected_type,
                pattern: format!("{}", pattern),
            }),
            }
        }

        Pattern::Int(_) => {
            let ok = if ctx.type_reconstruction_enabled {
                try_unify_types(&expected_type, &Type::Nat, ctx)?
            } else {
                expected_type == Type::Nat
            };
            if ok {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
                    pattern: format!("{}", pattern),
                })
            }
        }

        Pattern::Succ(inner) => {
            let ok = if ctx.type_reconstruction_enabled {
                try_unify_types(&expected_type, &Type::Nat, ctx)?
            } else {
                expected_type == Type::Nat
            };
            if !ok {
                return Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
                    pattern: format!("{}", pattern),
                });
            }
            typecheck_pattern_inner(inner, &Type::Nat, env, ctx)
        }

        Pattern::True | Pattern::False => {
            let ok = if ctx.type_reconstruction_enabled {
                try_unify_types(&expected_type, &Type::Bool, ctx)?
            } else {
                expected_type == Type::Bool
            };
            if ok {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
                    pattern: format!("{}", pattern),
                })
            }
        }

        Pattern::Unit => {
            let ok = if ctx.type_reconstruction_enabled {
                try_unify_types(&expected_type, &Type::Unit, ctx)?
            } else {
                expected_type == Type::Unit
            };
            if ok {
                Ok(new_env)
            } else {
                Err(TypeError::ErrorUnexpectedPatternForType {
                    expected: expected_type,
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

fn check_type_validity(
    ty: &Type,
    type_scope: &HashSet<String>,
    ctx: &TypeCheckContext,
) -> Result<(), TypeError> {
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
                check_type_validity(&field.type_, type_scope, ctx)?;
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
                    check_type_validity(field_ty, type_scope, ctx)?;
                }
            }
            Ok(())
        }
        Type::Fun(params, ret) => {
            for param in params {
                check_type_validity(param, type_scope, ctx)?;
            }
            check_type_validity(ret, type_scope, ctx)
        }
        Type::Tuple(types) => {
            for ty in types {
                check_type_validity(ty, type_scope, ctx)?;
            }
            Ok(())
        }
        Type::List(ty) => check_type_validity(ty, type_scope, ctx),
        Type::Sum(left, right) => {
            check_type_validity(left, type_scope, ctx)?;
            check_type_validity(right, type_scope, ctx)
        }
        Type::Ref(ty) => check_type_validity(ty, type_scope, ctx),
        Type::Rec(name, ty) => {
            let mut next_scope = type_scope.clone();
            next_scope.insert(name.clone());
            check_type_validity(ty, &next_scope, ctx)
        }
        Type::ForAll(vars, ty) => {
            if !ctx.universal_types_enabled {
                return Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: Type::Top,
                    found: Type::ForAll(vars.clone(), ty.clone()),
                    expr: None,
                });
            }

            let mut next_scope = type_scope.clone();
            if let Some(dup) = find_duplicate_name(vars) {
                return Err(TypeError::ErrorDuplicateTypeParameter(dup));
            }
            for var in vars {
                next_scope.insert(var.clone());
            }
            check_type_validity(ty, &next_scope, ctx)
        }
        Type::Var(name) => {
            if TypeCheckContext::is_meta_var_name(name) || type_scope.contains(name) {
                Ok(())
            } else {
                Err(TypeError::ErrorUndefinedTypeVariable(name.clone()))
            }
        }
        Type::Bool
        | Type::Nat
        | Type::Unit
        | Type::Top
        | Type::Bottom => Ok(()),
        Type::Auto => {
            if ctx.type_reconstruction_enabled {
                Ok(())
            } else {
                Err(TypeError::ErrorUnexpectedTypeForExpression {
                    expected: Type::Top,
                    found: Type::Auto,
                    expr: None,
                })
            }
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