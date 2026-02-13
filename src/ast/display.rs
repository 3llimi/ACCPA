use super::*;
use std::{
    convert::identity,
    fmt::{self, Display, Formatter},
};

#[derive(Clone, Copy)]
struct Depth(usize);

impl Depth {
    fn next(self) -> Self {
        Self(self.0 + 1)
    }

    fn with<T: DisplayWithDepth>(self, inner: &T) -> WithDepth<'_, T> {
        WithDepth(inner, self)
    }
}

impl Display for Depth {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        (0..self.0).try_for_each(|_| formatter.write_str("  "))
    }
}

trait DisplayWithDepth: Display {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result;
}

impl<T: DisplayWithDepth> DisplayWithDepth for Box<T> {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        T::fmt_with_depth(self, formatter, depth)
    }
}

struct DisplayList<'a, T, F>(pub &'a [T], F, &'static str);

fn display_list<'a, T: Display>(list: &'a [T], separator: &'static str) -> impl Display + 'a {
    DisplayList(list, identity, separator)
}

impl<'a, T, U, F> Display for DisplayList<'a, T, F>
where
    U: Display,
    F: Fn(&'a T) -> U,
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let Self(items, map, separator) = self;
        let mut items = items.iter();

        if let Some(item) = items.next() {
            write!(formatter, "{}", map(item))?;
        }
        for item in items {
            write!(formatter, "{separator}{}", map(item))?;
        }

        Ok(())
    }
}

struct WithDepth<'a, T>(&'a T, Depth);

impl<T: DisplayWithDepth> Display for WithDepth<'_, T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt_with_depth(formatter, self.1)
    }
}

struct Parentheses<T>(T, bool);

impl<T: Display> Display for Parentheses<T> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        let Self(expr, needs_parentheses) = self;
        if *needs_parentheses {
            write!(formatter, "({expr})")
        } else {
            write!(formatter, "{expr}")
        }
    }
}

fn fmt_args<'a>(
    args: impl IntoIterator<Item = &'a Expr>,
    formatter: &mut Formatter<'_>,
    depth: Depth,
) -> fmt::Result {
    formatter.write_str("(")?;

    let mut args = args.into_iter();
    if let Some(arg) = args.next() {
        write!(formatter, "{}", depth.with(arg))?;
    }
    for arg in args {
        write!(formatter, ", {}", depth.with(arg))?;
    }

    formatter.write_str(")")
}

fn fmt_function_like<'a>(
    name: &'static str,
    args: impl IntoIterator<Item = &'a Expr>,
    formatter: &mut Formatter<'_>,
    depth: Depth,
) -> fmt::Result {
    formatter.write_str(name)?;
    fmt_args(args, formatter, depth)
}

fn fmt_operator(
    operator: &'static str,
    left: &Expr,
    right: &Expr,
    formatter: &mut Formatter,
    depth: Depth,
    needs_parentheses: impl Fn(&Expr) -> bool,
) -> fmt::Result {
    write!(
        formatter,
        "{} {operator} {}",
        Parentheses(depth.with(left), needs_parentheses(left)),
        Parentheses(depth.with(right), needs_parentheses(right)),
    )
}

fn whitespace_inside(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Multiply(..)
            | Expr::Divide(..)
            | Expr::Add(..)
            | Expr::Subtract(..)
            | Expr::LogicalAnd(..)
            | Expr::LogicalOr(..)
            | Expr::GreaterThan(..)
            | Expr::GreaterThanOrEqual(..)
            | Expr::LessThan(..)
            | Expr::LessThanOrEqual(..)
            | Expr::Equal(..)
            | Expr::NotEqual(..)
            | Expr::Assignment(..)
            | Expr::TypeAscription(..)
            | Expr::TypeCast(..)
            | Expr::Sequence(..)
            | Expr::Reference(..)
    )
}

fn is_complex(expr: &Expr) -> bool {
    whitespace_inside(expr)
        || matches!(
            expr,
            Expr::Let(..)
                | Expr::LetRec(..)
                | Expr::If(..)
                | Expr::Fold(..)
                | Expr::Unfold(..)
                | Expr::TypeAbstraction(..)
        )
}

fn fmt_let(
    name: &'static str,
    bindings: &[PatternBinding],
    inner: &Expr,
    formatter: &mut Formatter<'_>,
    depth: Depth,
) -> fmt::Result {
    let deeper = depth.next();
    write!(
        formatter,
        "{name}\n{}\n{depth}in\n{deeper}{}",
        DisplayList(bindings, |x| deeper.with(x), ",\n"),
        deeper.with(inner)
    )
}

impl Display for Annotation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::InlineAnnotation => "inline",
        })
    }
}

impl DisplayWithDepth for Decl {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        match self {
            Self::DeclTypeAlias { name, type_ } => write!(formatter, "type {name} = {type_}"),
            Self::DeclFun {
                annotations,
                name,
                param_decls,
                return_type,
                throws_types,
                local_decls,
                return_expr,
            }
            | Self::DeclGenericFun {
                annotations,
                name,
                generics: _,
                param_decls,
                return_type,
                throws_types,
                local_decls,
                return_expr,
            } => {
                write!(formatter, "{}", depth)?;
                if !annotations.is_empty() {
                    write!(formatter, "{} ", display_list(annotations, " "))?;
                }
                if let Self::DeclGenericFun { .. } = self {
                    write!(formatter, "generic ")?;
                }
                write!(formatter, "fn {name}")?;
                if let Self::DeclGenericFun { generics, .. } = self {
                    write!(formatter, "[{}]", display_list(generics, ", "))?;
                }
                write!(formatter, "({}) ", display_list(param_decls, ", "))?;
                if let Some(return_type) = return_type {
                    write!(formatter, "-> {return_type} ")?;
                }
                if !throws_types.is_empty() {
                    write!(formatter, "throws {} ", display_list(throws_types, ", "))?;
                }

                let deeper = depth.next();
                write!(formatter, "{{\n")?;
                if !local_decls.is_empty() {
                    write!(
                        formatter,
                        "{}\n\n",
                        DisplayList(local_decls, |x| deeper.with(x), "\n")
                    )?;
                }
                write!(
                    formatter,
                    "{deeper}return {}\n{depth}}}",
                    deeper.with(return_expr)
                )
            }
            Decl::DeclExceptionType(type_) => write!(formatter, "exception type = {type_}"),
            Decl::DeclExceptionVariant { name, type_ } => {
                write!(formatter, "exception variant {name} : {type_}")
            }
        }
    }
}

impl Display for Decl {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_depth(formatter, Depth(0))
    }
}

impl Display for ParamDecl {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.name, self.type_)
    }
}

impl DisplayWithDepth for Binding {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        write!(
            formatter,
            "{depth}{} = {}",
            self.name,
            depth.with(&self.expr),
        )
    }
}

impl Display for Binding {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_depth(formatter, Depth(0))
    }
}

impl DisplayWithDepth for MatchCase {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        write!(
            formatter,
            "{depth}{} => {}",
            self.pattern,
            depth.with(&self.expr)
        )
    }
}

impl Display for MatchCase {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_depth(formatter, Depth(0))
    }
}

impl DisplayWithDepth for PatternBinding {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        write!(
            formatter,
            "{depth}{} = {}",
            self.pattern,
            depth.with(&self.rhs)
        )
    }
}

impl Display for PatternBinding {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_depth(formatter, Depth(0))
    }
}

impl DisplayWithDepth for Expr {
    fn fmt_with_depth(&self, formatter: &mut Formatter<'_>, depth: Depth) -> fmt::Result {
        match self {
            Self::DotRecord(inner, field) => write!(
                formatter,
                "{}.{field}",
                Parentheses(depth.with(inner), is_complex(inner))
            ),
            Self::DotTuple(inner, index) => write!(
                formatter,
                "{}.{index}",
                Parentheses(depth.with(inner), is_complex(inner))
            ),
            Self::ConstTrue => write!(formatter, "true"),
            Self::ConstFalse => write!(formatter, "false"),
            Self::ConstUnit => write!(formatter, "unit"),
            Self::ConstInt(value) => write!(formatter, "{value}"),
            Self::ConstMemory(address) => write!(formatter, "<0x{address:x}>"),
            Self::Var(var) => write!(formatter, "{var}"),
            Self::Inl(inner) => write!(formatter, "inl({})", depth.with(inner)),
            Self::Inr(inner) => write!(formatter, "inr({})", depth.with(inner)),
            Self::Cons(head, tail) => write!(
                formatter,
                "cons({}, {})",
                depth.with(head),
                depth.with(tail)
            ),
            Self::ListHead(inner) => write!(formatter, "List::head({})", depth.with(inner)),
            Self::ListIsEmpty(inner) => {
                write!(formatter, "List::isempty({})", depth.with(inner))
            }
            Self::ListTail(inner) => write!(formatter, "List::tail({})", depth.with(inner)),
            Self::Succ(inner) => write!(formatter, "succ({})", depth.with(inner)),
            Self::LogicalNot(inner) => write!(formatter, "not({})", depth.with(inner)),
            Self::NatPred(inner) => write!(formatter, "Nat::pred({})", depth.with(inner)),
            Self::NatIsZero(inner) => {
                write!(formatter, "Nat::iszero({})", depth.with(inner))
            }
            Self::Fix(inner) => write!(formatter, "fix({})", depth.with(inner)),
            Self::NatRec(n, z, s) => write!(
                formatter,
                "Nat::rec({}, {}, {})",
                depth.with(n),
                depth.with(z),
                depth.with(s)
            ),
            Self::Fold(type_, inner) => write!(
                formatter,
                "fold [{type_}] {}",
                Parentheses(depth.with(inner), whitespace_inside(inner))
            ),
            Self::Unfold(type_, inner) => write!(
                formatter,
                "unfold [{type_}] {}",
                Parentheses(depth.with(inner), whitespace_inside(inner))
            ),
            Self::Application(function, args) => {
                write!(
                    formatter,
                    "{}",
                    Parentheses(depth.with(function), is_complex(function))
                )?;
                fmt_args(args, formatter, depth)
            }
            Self::TypeApplication(inner, generics) => write!(
                formatter,
                "{}[{}]",
                Parentheses(depth.with(inner), whitespace_inside(inner)),
                display_list(generics, ", "),
            ),
            Self::Multiply(left, right) => fmt_operator("*", left, right, formatter, depth, |x| {
                matches!(x, Self::Add(..) | Self::Subtract(..))
            }),
            Self::Divide(left, right) => fmt_operator("/", left, right, formatter, depth, |x| {
                matches!(x, Self::Add(..) | Self::Subtract(..))
            }),
            Self::LogicalAnd(left, right) => {
                fmt_operator("and", left, right, formatter, depth, |x| {
                    matches!(x, Self::LogicalOr(..))
                })
            }
            Self::Add(left, right) => fmt_operator("+", left, right, formatter, depth, |_| false),
            Self::Subtract(left, right) => {
                fmt_operator("-", left, right, formatter, depth, |_| false)
            }
            Self::LogicalOr(left, right) => {
                fmt_operator("or", left, right, formatter, depth, |x| {
                    matches!(x, Self::LogicalAnd(..))
                })
            }
            Self::TypeAscription(inner, type_) => write!(
                formatter,
                "{} as {}",
                Parentheses(depth.with(inner), whitespace_inside(inner)),
                Parentheses(type_, matches!(type_, Type::Sum(..)))
            ),
            Self::Abstraction(params, inner) => write!(
                formatter,
                "fn({}) {{\n{deeper}return {}\n{depth}}}",
                display_list(params, ", "),
                depth.next().with(inner),
                deeper = depth.next(),
            ),
            Self::TypeAbstraction(generics, inner) => write!(
                formatter,
                "generic [{}] {}",
                display_list(generics, ", "),
                depth.with(inner)
            ),
            Self::Tuple(elements) => write!(
                formatter,
                "{{{}}}",
                DisplayList(elements, |x| depth.with(x), ", "),
            ),
            Self::Record(fields) => write!(
                formatter,
                "{{\n{}\n{depth}}}",
                DisplayList(fields, |x| depth.next().with(x), ",\n")
            ),
            Self::Variant(label, None) => write!(formatter, "<| {label} |>"),
            Self::Variant(label, Some(inner)) => {
                write!(formatter, "<| {label} = {} |>", depth.next().with(inner))
            }
            Self::Match(inner, cases) => write!(
                formatter,
                "match {} {{\n{}\n{depth}}}",
                depth.with(inner),
                DisplayList(cases, |x| depth.next().with(x), " |\n"),
            ),
            Self::List(elements) => write!(
                formatter,
                "[{}]",
                DisplayList(elements, |x| depth.next().with(x), ", "),
            ),
            Self::LessThan(left, right) => {
                fmt_operator("<", left, right, formatter, depth, |_| false)
            }
            Self::LessThanOrEqual(left, right) => {
                fmt_operator("<=", left, right, formatter, depth, |_| false)
            }
            Self::GreaterThan(left, right) => {
                fmt_operator(">=", left, right, formatter, depth, |_| false)
            }
            Self::GreaterThanOrEqual(left, right) => {
                fmt_operator(">=", left, right, formatter, depth, |_| false)
            }
            Self::Equal(left, right) => {
                fmt_operator("==", left, right, formatter, depth, |_| false)
            }
            Self::NotEqual(left, right) => {
                fmt_operator("!=", left, right, formatter, depth, |_| false)
            }
            Self::Assignment(left, right) => {
                fmt_operator(":=", left, right, formatter, depth, |_| false)
            }
            Self::TypeCast(inner, type_) => write!(
                formatter,
                "{} cast as {}",
                Parentheses(depth.with(inner), whitespace_inside(inner)),
                Parentheses(type_, matches!(type_, Type::Sum(..)))
            ),
            Self::Reference(inner) => write!(
                formatter,
                "new {}",
                Parentheses(depth.with(inner), whitespace_inside(inner)),
            ),
            Self::Dereference(inner) => write!(
                formatter,
                "*{}",
                Parentheses(depth.with(inner), whitespace_inside(inner)),
            ),
            Self::Panic => write!(formatter, "panic!"),
            Self::Throw(inner) => write!(formatter, "throw({})", depth.with(inner)),
            Self::TryCatch(try_, pattern, fallback) => write!(
                formatter,
                "try {{\n{deeper}{}\n{depth}}} catch {{\n{deeper}{pattern} => {}\n{depth}}}",
                depth.next().with(try_),
                depth.next().with(fallback),
                deeper = depth.next(),
            ),
            Self::TryWith(try_, fallback) => write!(
                formatter,
                "try {{\n{deeper}{}\n{depth}}} with {{\n{deeper}{}\n{depth}}}",
                depth.next().with(try_),
                depth.next().with(fallback),
                deeper = depth.next(),
            ),
            Self::TryCastAs {
                try_,
                to,
                casted_pattern,
                casted_arm,
                fallback_arm,
            } => write!(
                formatter,
                "try {{\n\
                 {deeper}{}\n\
                 {depth}}} cast as {to} {{\n\
                 {deeper}{casted_pattern} => {}\n\
                 {depth}}} with {{\n\
                 {deeper}{}\n\
                 {depth}}}",
                depth.next().with(try_),
                depth.next().with(casted_arm),
                depth.next().with(fallback_arm),
                deeper = depth.next(),
            ),
            Self::If(condition, then, otherwise) => write!(
                formatter,
                "if {}\n{depth}then {}\n{depth}else {}",
                depth.with(condition),
                depth.next().with(then),
                depth.next().with(otherwise),
                depth = depth.next()
            ),
            Self::Let(bindings, inner) => fmt_let("let", bindings, inner, formatter, depth),
            Self::LetRec(bindings, inner) => fmt_let("letrec", bindings, inner, formatter, depth),
            Self::Sequence(left, right) => write!(
                formatter,
                "{};\n{depth}{}",
                depth.with(left),
                depth.with(right),
            ),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_with_depth(formatter, Depth(0))
    }
}

impl Display for RecordFieldType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} : {}", self.label, self.type_)
    }
}

impl Display for VariantFieldType {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match &self.type_ {
            Some(type_) => write!(formatter, "{} : {}", self.label, type_),
            None => write!(formatter, "{}", &self.label),
        }
    }
}

impl Display for Type {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(formatter, "Bool"),
            Self::Nat => write!(formatter, "Nat"),
            Self::Ref(inner) => write!(
                formatter,
                "&{}",
                Parentheses(inner, matches!(&**inner, Self::Sum(..))),
            ),
            Self::Sum(left, right) => write!(
                formatter,
                "{} + {}",
                Parentheses(
                    left,
                    matches!(
                        &**left,
                        Self::Sum(..) | Self::Fun(..) | Self::ForAll(..) | Self::Rec(..)
                    )
                ),
                Parentheses(right, matches!(&**right, Self::Sum(..))),
            ),
            Self::Fun(params, return_type) => write!(
                formatter,
                "fn({}) -> {return_type}",
                display_list(params, ", ")
            ),
            Self::ForAll(vars, inner) => {
                write!(formatter, "forall {}. {inner}", display_list(vars, ", "))
            }
            Self::Rec(name, inner) => write!(formatter, "μ {name}. {inner}"),
            Self::Tuple(types) => {
                write!(formatter, "{{{}}}", display_list(types, ", "))
            }
            Self::Record(fields) => {
                write!(formatter, "{{ {} }}", display_list(fields, ", "))
            }
            Self::Variant(variants) if variants.is_empty() => write!(formatter, "<| |>"),
            Self::Variant(variants) => {
                write!(formatter, "<| {} |>", display_list(variants, ", "))
            }
            Self::List(inner) => write!(formatter, "[{inner}]"),
            Self::Unit => write!(formatter, "Unit"),
            Self::Top => write!(formatter, "Top"),
            Self::Bottom => write!(formatter, "Bot"),
            Self::Auto => write!(formatter, "auto"),
            Self::Var(name) => write!(formatter, "{name}"),
        }
    }
}

impl Display for LabelledPattern {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "{} = {}", self.label, self.pattern)
    }
}

impl Display for Pattern {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variant(label, None) => write!(formatter, "<| {label} |>"),
            Self::Variant(label, Some(inner)) => write!(formatter, "<| {label} = {inner} |>"),
            Self::Inl(inner) => write!(formatter, "inl({inner})"),
            Self::Inr(inner) => write!(formatter, "inr({inner})"),
            Self::Tuple(patterns) => write!(formatter, "{{{}}}", display_list(patterns, ", ")),
            Self::Record(fields) => write!(formatter, "{{ {} }}", display_list(fields, ", ")),
            Self::List(patterns) => write!(formatter, "[{}]", display_list(patterns, ", ")),
            Self::Cons(head, tail) => write!(formatter, "cons({head}, {tail})"),
            Self::False => write!(formatter, "false"),
            Self::True => write!(formatter, "true"),
            Self::Unit => write!(formatter, "unit"),
            Self::Int(value) => write!(formatter, "{value}"),
            Self::Succ(inner) => write!(formatter, "succ({inner})"),
            Self::Var(var) => write!(formatter, "{var}"),
            Self::Ascription(inner, type_) => write!(formatter, "{inner} as {type_}"),
            Self::CastAs(inner, type_) => write!(formatter, "{inner} cast as {type_}"),
        }
    }
}
