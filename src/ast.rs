#![allow(dead_code)]

mod display;

pub type ExtensionName = String;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Program {
    pub language_decl: LanguageDecl,
    pub extensions: Vec<Extension>,
    pub decls: Vec<Decl>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum LanguageDecl {
    LanguageCore,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Extension {
    pub extension_names: Vec<ExtensionName>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Decl {
    DeclFun {
        annotations: Vec<Annotation>,
        name: String,
        param_decls: Vec<ParamDecl>,
        return_type: Option<Type>,
        throws_types: Vec<Type>,
        local_decls: Vec<Decl>,
        return_expr: Expr,
    },
    DeclGenericFun {
        annotations: Vec<Annotation>,
        name: String,
        generics: Vec<String>,
        param_decls: Vec<ParamDecl>,
        return_type: Option<Type>,
        throws_types: Vec<Type>,
        local_decls: Vec<Decl>,
        return_expr: Expr,
    },
    DeclTypeAlias {
        name: String,
        type_: Type,
    },
    DeclExceptionType(Type),
    DeclExceptionVariant {
        name: String,
        type_: Type,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Annotation {
    InlineAnnotation,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ParamDecl {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Binding {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RecordFieldType {
    pub label: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct VariantFieldType {
    pub label: String,
    pub type_: Option<Type>,
}

// Type enum
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Bool,
    Nat,
    Unit,
    Fun(Vec<Type>, Box<Type>),  // Note: Vec for params
    Tuple(Vec<Type>),
    Record(Vec<RecordFieldType>),
    Sum(Box<Type>, Box<Type>),
    List(Box<Type>),
    Variant(Vec<VariantFieldType>),
    Var(String),
    Rec(String, Box<Type>),
    Top,
    Bottom,
    Auto,
    Ref(Box<Type>),
    ForAll(Vec<String>, Box<Type>),
}

// Expr enum
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expr {
    DotRecord(Box<Expr>, String),
    DotTuple(Box<Expr>, usize),
    ConstTrue,
    ConstFalse,
    ConstUnit,
    ConstInt(usize),
    ConstMemory(usize),
    Var(String),
    Inl(Box<Expr>),
    Inr(Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    ListHead(Box<Expr>),
    ListIsEmpty(Box<Expr>),
    ListTail(Box<Expr>),
    Succ(Box<Expr>),
    LogicalNot(Box<Expr>),
    NatPred(Box<Expr>),
    NatIsZero(Box<Expr>),
    Fix(Box<Expr>),
    NatRec(Box<Expr>, Box<Expr>, Box<Expr>),
    Fold(Type, Box<Expr>),
    Unfold(Type, Box<Expr>),
    Application(Box<Expr>, Vec<Expr>),
    TypeApplication(Box<Expr>, Vec<Type>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    TypeAscription(Box<Expr>, Type),
    Abstraction(Vec<ParamDecl>, Box<Expr>),
    TypeAbstraction(Vec<String>, Box<Expr>),
    Tuple(Vec<Expr>),
    Record(Vec<Binding>),
    Variant(String, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<MatchCase>),
    List(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(Vec<PatternBinding>, Box<Expr>),
    LetRec(Vec<PatternBinding>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqual(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    Sequence(Box<Expr>, Box<Expr>),
    Assignment(Box<Expr>, Box<Expr>),
    TypeCast(Box<Expr>, Type),
    Reference(Box<Expr>),
    Dereference(Box<Expr>),
    Panic,
    Throw(Box<Expr>),
    TryCatch(Box<Expr>, Pattern, Box<Expr>),
    TryCastAs {
        try_: Box<Expr>,
        to: Type,
        casted_pattern: Pattern,
        casted_arm: Box<Expr>,
        fallback_arm: Box<Expr>,
    },
    TryWith(Box<Expr>, Box<Expr>),
}

// Pattern enum
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct PatternBinding {
    pub pattern: Pattern,
    pub rhs: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LabelledPattern { 
    pub label: String,
    pub pattern: Option<Pattern>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Pattern {
    Var(String),
    Inl(Box<Pattern>),
    Inr(Box<Pattern>),
    Tuple(Vec<Pattern>),
    Record(Vec<LabelledPattern>),
    Variant(String, Option<Box<Pattern>>),
    List(Vec<Pattern>),
    Cons(Box<Pattern>, Box<Pattern>),
    Int(usize),
    Succ(Box<Pattern>),
    True,
    False,
    Unit,
    Ascription(Box<Pattern>, Type),
    CastAs(Box<Pattern>, Type),
}