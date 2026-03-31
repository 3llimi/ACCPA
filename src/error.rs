use crate::ast::Type;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    // ── Core ──────────────────────────────────────────────────────────────────
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

    // ── #multiparameter-functions / #nullary-functions ─────────────────────
    /// main is declared with n ≠ 1 parameters
    ErrorIncorrectArityOfMain,
    /// function call made with wrong number of arguments
    ErrorIncorrectNumberOfArguments { expected: usize, found: usize },
    /// anonymous function has wrong number of parameters for the expected type
    ErrorUnexpectedNumberOfParametersInLambda { expected: usize, found: usize },

    // ── #structural-patterns ──────────────────────────────────────────────
    /// a pattern binds the same variable name more than once
    ErrorDuplicatePatternVariable(String),
    /// cannot infer binding type from pattern without annotation
    ErrorAmbiguousPatternType,

    // ── #nullary-variant-labels ───────────────────────────────────────────
    /// variant expression provides data for a nullary label (NoTyping)
    ErrorUnexpectedDataForNullaryLabel(String),
    /// variant expression omits data for a label that expects it (SomeTyping)
    ErrorMissingDataForLabel(String),
    /// variant pattern carries data for a nullary label
    ErrorUnexpectedNonNullaryVariantPattern(String),
    /// variant pattern omits data for a label that carries data
    ErrorUnexpectedNullaryVariantPattern(String),

    // ── Exceptions / references / subtyping (Stage 2+) ───────────────────
    ErrorDuplicateExceptionType,
    ErrorDuplicateExceptionVariant(String),
    ErrorConflictingExceptionDeclarations,
    ErrorIllegalLocalExceptionType,
    ErrorIllegalLocalOpenVariantException,
    ErrorExceptionTypeNotDeclared,
    ErrorAmbiguousThrowType,
    ErrorAmbiguousReferenceType,
    ErrorAmbiguousPanicType,
    ErrorNotAReference(Type),
    ErrorUnexpectedMemoryAddress,
    ErrorUnexpectedReference,
    ErrorUnexpectedSubtype {
        expected: Type,
        found: Type,
        expr: Option<String>,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // ── Core ──────────────────────────────────────────────────────
            TypeError::ErrorMissingMain => {
                write!(f, "ERROR_MISSING_MAIN:\n  No main function defined")
            }
            TypeError::ErrorIncorrectTypeOfMain => {
                write!(
                    f,
                    "ERROR_INCORRECT_TYPE_OF_MAIN:\n  main function must have a function type"
                )
            }
            TypeError::ErrorUndefinedVariable(var) => {
                write!(
                    f,
                    "ERROR_UNDEFINED_VARIABLE:\n  Variable '{}' is not defined",
                    var
                )
            }
            TypeError::ErrorIllegalNegativeLiteral => {
                write!(
                    f,
                    "ERROR_ILLEGAL_NEGATIVE_LITERAL:\n  Negative integers cannot be used where Nat is expected"
                )
            }
            TypeError::ErrorUnexpectedTypeForExpression {
                expected,
                found,
                expr,
            } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION:\n  expected type\n    {}\n  but found type\n    {}",
                    expected, found
                )?;
                if let Some(e) = expr {
                    write!(f, "\n  for expression\n    {}", e)?;
                }
                Ok(())
            }
            TypeError::ErrorNotAFunction(ty) => {
                write!(
                    f,
                    "ERROR_NOT_A_FUNCTION:\n  Expected function type, but found {}",
                    ty
                )
            }
            TypeError::ErrorNotATuple(ty) => {
                write!(
                    f,
                    "ERROR_NOT_A_TUPLE:\n  Expected tuple type, but found {}",
                    ty
                )
            }
            TypeError::ErrorNotARecord(ty) => {
                write!(
                    f,
                    "ERROR_NOT_A_RECORD:\n  Expected record type, but found {}",
                    ty
                )
            }
            TypeError::ErrorNotAList(ty) => {
                write!(
                    f,
                    "ERROR_NOT_A_LIST:\n  Expected list type, but found {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedLambda(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_LAMBDA:\n  Lambda checked against non-function type {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedTypeForParameter { expected, found } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_TYPE_FOR_PARAMETER:\n  expected {}\n  but found {}",
                    expected, found
                )
            }
            TypeError::ErrorUnexpectedTuple(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_TUPLE:\n  Tuple checked against non-tuple type {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedRecord(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_RECORD:\n  Record checked against non-record type {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedVariant(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_VARIANT:\n  Variant checked against non-variant type {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedList(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_LIST:\n  List checked against non-list type {}",
                    ty
                )
            }
            TypeError::ErrorUnexpectedInjection(ty) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_INJECTION:\n  Injection checked against non-sum type {}",
                    ty
                )
            }
            TypeError::ErrorMissingRecordFields(fields) => {
                write!(
                    f,
                    "ERROR_MISSING_RECORD_FIELDS:\n  Missing fields: {}",
                    fields.join(", ")
                )
            }
            TypeError::ErrorUnexpectedRecordFields(fields) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_RECORD_FIELDS:\n  Unexpected fields: {}",
                    fields.join(", ")
                )
            }
            TypeError::ErrorUnexpectedFieldAccess(field) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_FIELD_ACCESS:\n  Field '{}' does not exist in record",
                    field
                )
            }
            TypeError::ErrorUnexpectedVariantLabel(label) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_VARIANT_LABEL:\n  Label '{}' does not exist in variant type",
                    label
                )
            }
            TypeError::ErrorMissingVariantLabels(labels) => {
                write!(
                    f,
                    "ERROR_MISSING_VARIANT_LABELS:\n  Missing labels: {}",
                    labels.join(", ")
                )
            }
            TypeError::ErrorTupleIndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "ERROR_TUPLE_INDEX_OUT_OF_BOUNDS:\n  Index {} is out of bounds for tuple of length {}",
                    index, length
                )
            }
            TypeError::ErrorUnexpectedTupleLength { expected, found } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_TUPLE_LENGTH:\n  Expected tuple of length {}, but found length {}",
                    expected, found
                )
            }
            TypeError::ErrorAmbiguousSumType => {
                write!(
                    f,
                    "ERROR_AMBIGUOUS_SUM_TYPE:\n  Cannot infer sum type for injection without context"
                )
            }
            TypeError::ErrorAmbiguousVariantType => {
                write!(
                    f,
                    "ERROR_AMBIGUOUS_VARIANT_TYPE:\n  Cannot infer variant type without context"
                )
            }
            TypeError::ErrorAmbiguousListType => {
                write!(
                    f,
                    "ERROR_AMBIGUOUS_LIST_TYPE:\n  Cannot infer list element type (e.g., empty list literal without context)"
                )
            }
            TypeError::ErrorIllegalEmptyMatching => {
                write!(
                    f,
                    "ERROR_ILLEGAL_EMPTY_MATCHING:\n  Match expression must have at least one case"
                )
            }
            TypeError::ErrorNonexhaustiveMatchPatterns => {
                write!(
                    f,
                    "ERROR_NONEXHAUSTIVE_MATCH_PATTERNS:\n  Pattern match is not exhaustive — some cases are not covered"
                )
            }
            TypeError::ErrorUnexpectedPatternForType { expected, pattern } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_PATTERN_FOR_TYPE:\n  Pattern '{}' does not match scrutinee type {}",
                    pattern, expected
                )
            }
            TypeError::ErrorDuplicateRecordFields(fields) => {
                write!(
                    f,
                    "ERROR_DUPLICATE_RECORD_FIELDS:\n  Duplicate field names in record expression: {}",
                    fields.join(", ")
                )
            }
            TypeError::ErrorDuplicateRecordTypeFields(fields) => {
                write!(
                    f,
                    "ERROR_DUPLICATE_RECORD_TYPE_FIELDS:\n  Duplicate field names in record type: {}",
                    fields.join(", ")
                )
            }
            TypeError::ErrorDuplicateVariantTypeFields(labels) => {
                write!(
                    f,
                    "ERROR_DUPLICATE_VARIANT_TYPE_FIELDS:\n  Duplicate labels in variant type: {}",
                    labels.join(", ")
                )
            }

            // ── #multiparameter-functions / #nullary-functions ─────────────
            TypeError::ErrorIncorrectArityOfMain => {
                write!(
                    f,
                    "ERROR_INCORRECT_ARITY_OF_MAIN:\n  main must be declared with exactly 1 parameter"
                )
            }
            TypeError::ErrorIncorrectNumberOfArguments { expected, found } => {
                write!(
                    f,
                    "ERROR_INCORRECT_NUMBER_OF_ARGUMENTS:\n  function expects {} argument(s) but was called with {}",
                    expected, found
                )
            }
            TypeError::ErrorUnexpectedNumberOfParametersInLambda { expected, found } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA:\n  expected lambda with {} parameter(s), but found {}",
                    expected, found
                )
            }

            // ── #structural-patterns ────────────────────────────────────
            TypeError::ErrorDuplicatePatternVariable(name) => {
                write!(
                    f,
                    "ERROR_DUPLICATE_PATTERN_VARIABLE:\n  Variable '{}' is bound more than once in the same pattern",
                    name
                )
            }
            TypeError::ErrorAmbiguousPatternType => {
                write!(
                    f,
                    "ERROR_AMBIGUOUS_PATTERN_TYPE:\n  Cannot infer type for this pattern without annotation"
                )
            }

            // ── #nullary-variant-labels ──────────────────────────────────
            TypeError::ErrorUnexpectedDataForNullaryLabel(label) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_DATA_FOR_NULLARY_LABEL:\n  Variant label '{}' is declared without data, but an expression was provided",
                    label
                )
            }
            TypeError::ErrorMissingDataForLabel(label) => {
                write!(
                    f,
                    "ERROR_MISSING_DATA_FOR_LABEL:\n  Variant label '{}' expects data, but none was provided",
                    label
                )
            }
            TypeError::ErrorUnexpectedNonNullaryVariantPattern(label) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_NON_NULLARY_VARIANT_PATTERN:\n  Pattern for label '{}' carries data, but that label is declared without data in the type",
                    label
                )
            }
            TypeError::ErrorUnexpectedNullaryVariantPattern(label) => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_NULLARY_VARIANT_PATTERN:\n  Pattern for label '{}' has no data, but that label carries data in the type",
                    label
                )
            }

            // ── Stage 2+ ────────────────────────────────────────────────
            TypeError::ErrorDuplicateExceptionType => write!(
                f,
                "ERROR_DUPLICATE_EXCEPTION_TYPE:\n  More than one exception type declaration in the same scope"
            ),
            TypeError::ErrorDuplicateExceptionVariant(label) => write!(
                f,
                "ERROR_DUPLICATE_EXCEPTION_VARIANT:\n  Duplicate exception variant label '{}' in open variant exception declarations",
                label
            ),
            TypeError::ErrorConflictingExceptionDeclarations => write!(
                f,
                "ERROR_CONFLICTING_EXCEPTION_DECLARATIONS:\n  Both exception type and exception variant declarations present in the same scope"
            ),
            TypeError::ErrorIllegalLocalExceptionType => write!(
                f,
                "ERROR_ILLEGAL_LOCAL_EXCEPTION_TYPE:\n  Exception type declaration appears in an illegal local scope"
            ),
            TypeError::ErrorIllegalLocalOpenVariantException => write!(
                f,
                "ERROR_ILLEGAL_LOCAL_OPEN_VARIANT_EXCEPTION:\n  Open variant exception declaration appears in an illegal local scope"
            ),
            TypeError::ErrorExceptionTypeNotDeclared => write!(
                f,
                "ERROR_EXCEPTION_TYPE_NOT_DECLARED:\n  Exceptions are used but no exception type has been declared"
            ),
            TypeError::ErrorAmbiguousThrowType => write!(
                f,
                "ERROR_AMBIGUOUS_THROW_TYPE:\n  Cannot determine type of throw expression without context"
            ),
            TypeError::ErrorAmbiguousReferenceType => write!(
                f,
                "ERROR_AMBIGUOUS_REFERENCE_TYPE:\n  Cannot determine reference type for memory address without context"
            ),
            TypeError::ErrorAmbiguousPanicType => write!(
                f,
                "ERROR_AMBIGUOUS_PANIC_TYPE:\n  Cannot determine type of panic! expression without context"
            ),
            TypeError::ErrorNotAReference(found) => write!(
                f,
                "ERROR_NOT_A_REFERENCE:\n  Expected a reference type\n  but found type\n    {}",
                found
            ),
            TypeError::ErrorUnexpectedMemoryAddress => write!(
                f,
                "ERROR_UNEXPECTED_MEMORY_ADDRESS:\n  Memory address literal used where non-reference type was expected"
            ),
            TypeError::ErrorUnexpectedReference => write!(
                f,
                "ERROR_UNEXPECTED_REFERENCE:\n  Reference expression used where non-reference type was expected"
            ),
            TypeError::ErrorUnexpectedSubtype {
                expected,
                found,
                expr,
            } => {
                write!(
                    f,
                    "ERROR_UNEXPECTED_SUBTYPE:\n  Expected a subtype of type\n    {}\n  but found type\n    {}",
                    expected, found
                )?;
                if let Some(e) = expr {
                    write!(f, "\n  for expression\n    {}", e)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for TypeError {}