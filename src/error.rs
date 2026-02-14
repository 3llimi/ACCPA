use crate::ast::Type;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    ErrorMissingMain,
    ErrorIncorrectTypeOfMain,
    ErrorUndefinedVariable(String),
    ErrorIllegalNegativeLiteral,
    ErrorUnexpectedTypeForExpression {
        expected: Type,
        found: Type,
        expr: Option<String>,
    },
    ErrorNotAFunction(Type),
    ErrorNotATuple(Type),
    ErrorNotARecord(Type),
    ErrorNotAList(Type),
    ErrorUnexpectedLambda(Type),
    ErrorUnexpectedTypeForParameter {
        expected: Type,
        found: Type,
    },
    ErrorUnexpectedTuple(Type),
    ErrorUnexpectedRecord(Type),
    ErrorUnexpectedVariant(Type),
    ErrorUnexpectedList(Type),
    ErrorUnexpectedInjection(Type),
    ErrorMissingRecordFields(Vec<String>),
    ErrorUnexpectedRecordFields(Vec<String>),
    ErrorUnexpectedFieldAccess(String),
    ErrorUnexpectedVariantLabel(String),
    ErrorMissingVariantLabels(Vec<String>),
    ErrorTupleIndexOutOfBounds { index: usize, length: usize },
    ErrorUnexpectedTupleLength { expected: usize, found: usize },
    ErrorAmbiguousSumType,
    ErrorAmbiguousVariantType,
    ErrorAmbiguousListType,
    ErrorIllegalEmptyMatching,
    ErrorNonexhaustiveMatchPatterns,
    ErrorUnexpectedPatternForType { expected: Type, pattern: String },
    ErrorDuplicateRecordFields(Vec<String>),
    ErrorDuplicateRecordTypeFields(Vec<String>),
    ErrorDuplicateVariantTypeFields(Vec<String>),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::ErrorMissingMain => {
                write!(f, "ERROR_MISSING_MAIN:\n  No main function defined")
            }
            TypeError::ErrorIncorrectTypeOfMain => {
                write!(f, "ERROR_INCORRECT_TYPE_OF_MAIN:\n  main function must have a function type")
            }
            TypeError::ErrorUndefinedVariable(var) => {
                write!(f, "ERROR_UNDEFINED_VARIABLE:\n  Variable '{}' is not defined", var)
            }
            TypeError::ErrorIllegalNegativeLiteral => {
                write!(f, "ERROR_ILLEGAL_NEGATIVE_LITERAL:\n  Negative integers cannot be used where Nat is expected")
            }
            TypeError::ErrorUnexpectedTypeForExpression { expected, found, expr } => {
                write!(f, "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION:\n  expected type\n    {:?}\n  but found type\n    {:?}", expected, found)?;
                if let Some(e) = expr {
                    write!(f, "\n  for expression\n    {}", e)?;
                }
                Ok(())
            }
            TypeError::ErrorNotAFunction(ty) => {
                write!(f, "ERROR_NOT_A_FUNCTION:\n  Expected function type, but found {:?}", ty)
            }
            TypeError::ErrorNotATuple(ty) => {
                write!(f, "ERROR_NOT_A_TUPLE:\n  Expected tuple type, but found {:?}", ty)
            }
            TypeError::ErrorNotARecord(ty) => {
                write!(f, "ERROR_NOT_A_RECORD:\n  Expected record type, but found {:?}", ty)
            }
            TypeError::ErrorNotAList(ty) => {
                write!(f, "ERROR_NOT_A_LIST:\n  Expected list type, but found {:?}", ty)
            }
            TypeError::ErrorUnexpectedLambda(ty) => {
                write!(f, "ERROR_UNEXPECTED_LAMBDA:\n  Lambda checked against non-function type {:?}", ty)
            }
            TypeError::ErrorUnexpectedTypeForParameter { expected, found } => {
                write!(f, "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER:\n  expected {:?}\n  but found {:?}", expected, found)
            }
            TypeError::ErrorUnexpectedTuple(ty) => {
                write!(f, "ERROR_UNEXPECTED_TUPLE:\n  Tuple checked against non-tuple type {:?}", ty)
            }
            TypeError::ErrorUnexpectedRecord(ty) => {
                write!(f, "ERROR_UNEXPECTED_RECORD:\n  Record checked against non-record type {:?}", ty)
            }
            TypeError::ErrorUnexpectedVariant(ty) => {
                write!(f, "ERROR_UNEXPECTED_VARIANT:\n  Variant checked against non-variant type {:?}", ty)
            }
            TypeError::ErrorUnexpectedList(ty) => {
                write!(f, "ERROR_UNEXPECTED_LIST:\n  List checked against non-list type {:?}", ty)
            }
            TypeError::ErrorUnexpectedInjection(ty) => {
                write!(f, "ERROR_UNEXPECTED_INJECTION:\n  Injection checked against non-sum type {:?}", ty)
            }
            TypeError::ErrorMissingRecordFields(fields) => {
                write!(f, "ERROR_MISSING_RECORD_FIELDS:\n  Missing fields: {:?}", fields)
            }
            TypeError::ErrorUnexpectedRecordFields(fields) => {
                write!(f, "ERROR_UNEXPECTED_RECORD_FIELDS:\n  Unexpected fields: {:?}", fields)
            }
            TypeError::ErrorUnexpectedFieldAccess(field) => {
                write!(f, "ERROR_UNEXPECTED_FIELD_ACCESS:\n  Field '{}' does not exist", field)
            }
            TypeError::ErrorUnexpectedVariantLabel(label) => {
                write!(f, "ERROR_UNEXPECTED_VARIANT_LABEL:\n  Label '{}' does not exist in variant", label)
            }
            TypeError::ErrorMissingVariantLabels(labels) => {
                write!(f, "ERROR_MISSING_VARIANT_LABELS:\n  Missing labels: {:?}", labels)
            }
            TypeError::ErrorTupleIndexOutOfBounds { index, length } => {
                write!(f, "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS:\n  Index {} out of bounds for tuple of length {}", index, length)
            }
            TypeError::ErrorUnexpectedTupleLength { expected, found } => {
                write!(f, "ERROR_UNEXPECTED_TUPLE_LENGTH:\n  Expected tuple of length {}, but found length {}", expected, found)
            }
            TypeError::ErrorAmbiguousSumType => {
                write!(f, "ERROR_AMBIGUOUS_SUM_TYPE:\n  Cannot infer sum type for injection")
            }
            TypeError::ErrorAmbiguousVariantType => {
                write!(f, "ERROR_AMBIGUOUS_VARIANT_TYPE:\n  Cannot infer variant type")
            }
            TypeError::ErrorAmbiguousListType => {
                write!(f, "ERROR_AMBIGUOUS_LIST_TYPE:\n  Cannot infer list type (e.g., empty list)")
            }
            TypeError::ErrorIllegalEmptyMatching => {
                write!(f, "ERROR_ILLEGAL_EMPTY_MATCHING:\n  Match expression must have at least one case")
            }
            TypeError::ErrorNonexhaustiveMatchPatterns => {
                write!(f, "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS:\n  Pattern match is not exhaustive")
            }
            TypeError::ErrorUnexpectedPatternForType { expected, pattern } => {
                write!(f, "ERROR_UNEXPECTED_PATTERN_FOR_TYPE:\n  Pattern '{}' does not match type {:?}", pattern, expected)
            }
            TypeError::ErrorDuplicateRecordFields(fields) => {
                write!(f, "ERROR_DUPLICATE_RECORD_FIELDS:\n  Duplicate fields in record: {:?}", fields)
            }
            TypeError::ErrorDuplicateRecordTypeFields(fields) => {
                write!(f, "ERROR_DUPLICATE_RECORD_TYPE_FIELDS:\n  Duplicate fields in record type: {:?}", fields)
            }
            TypeError::ErrorDuplicateVariantTypeFields(labels) => {
                write!(f, "ERROR_DUPLICATE_VARIANT_TYPE_FIELDS:\n  Duplicate labels in variant type: {:?}", labels)
            }
        }
    }
}

impl std::error::Error for TypeError {}