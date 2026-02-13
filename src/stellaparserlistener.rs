#![allow(nonstandard_style)]
// Generated from src/stellaParser.g4 by ANTLR 4.8
use antlr_rust::tree::ParseTreeListener;
use super::stellaparser::*;

pub trait stellaParserListener<'input> : ParseTreeListener<'input,stellaParserContextType>{
/**
 * Enter a parse tree produced by {@link stellaParser#start_Program}.
 * @param ctx the parse tree
 */
fn enter_start_Program(&mut self, _ctx: &Start_ProgramContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#start_Program}.
 * @param ctx the parse tree
 */
fn exit_start_Program(&mut self, _ctx: &Start_ProgramContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#start_Expr}.
 * @param ctx the parse tree
 */
fn enter_start_Expr(&mut self, _ctx: &Start_ExprContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#start_Expr}.
 * @param ctx the parse tree
 */
fn exit_start_Expr(&mut self, _ctx: &Start_ExprContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#start_Type}.
 * @param ctx the parse tree
 */
fn enter_start_Type(&mut self, _ctx: &Start_TypeContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#start_Type}.
 * @param ctx the parse tree
 */
fn exit_start_Type(&mut self, _ctx: &Start_TypeContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#program}.
 * @param ctx the parse tree
 */
fn enter_program(&mut self, _ctx: &ProgramContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#program}.
 * @param ctx the parse tree
 */
fn exit_program(&mut self, _ctx: &ProgramContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LanguageCore}
 * labeled alternative in {@link stellaParser#languageDecl}.
 * @param ctx the parse tree
 */
fn enter_LanguageCore(&mut self, _ctx: &LanguageCoreContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LanguageCore}
 * labeled alternative in {@link stellaParser#languageDecl}.
 * @param ctx the parse tree
 */
fn exit_LanguageCore(&mut self, _ctx: &LanguageCoreContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code AnExtension}
 * labeled alternative in {@link stellaParser#extension}.
 * @param ctx the parse tree
 */
fn enter_AnExtension(&mut self, _ctx: &AnExtensionContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code AnExtension}
 * labeled alternative in {@link stellaParser#extension}.
 * @param ctx the parse tree
 */
fn exit_AnExtension(&mut self, _ctx: &AnExtensionContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclFun}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn enter_DeclFun(&mut self, _ctx: &DeclFunContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclFun}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn exit_DeclFun(&mut self, _ctx: &DeclFunContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclFunGeneric}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn enter_DeclFunGeneric(&mut self, _ctx: &DeclFunGenericContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclFunGeneric}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn exit_DeclFunGeneric(&mut self, _ctx: &DeclFunGenericContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclTypeAlias}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn enter_DeclTypeAlias(&mut self, _ctx: &DeclTypeAliasContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclTypeAlias}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn exit_DeclTypeAlias(&mut self, _ctx: &DeclTypeAliasContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclExceptionType}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn enter_DeclExceptionType(&mut self, _ctx: &DeclExceptionTypeContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclExceptionType}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn exit_DeclExceptionType(&mut self, _ctx: &DeclExceptionTypeContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DeclExceptionVariant}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn enter_DeclExceptionVariant(&mut self, _ctx: &DeclExceptionVariantContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DeclExceptionVariant}
 * labeled alternative in {@link stellaParser#decl}.
 * @param ctx the parse tree
 */
fn exit_DeclExceptionVariant(&mut self, _ctx: &DeclExceptionVariantContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code InlineAnnotation}
 * labeled alternative in {@link stellaParser#annotation}.
 * @param ctx the parse tree
 */
fn enter_InlineAnnotation(&mut self, _ctx: &InlineAnnotationContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code InlineAnnotation}
 * labeled alternative in {@link stellaParser#annotation}.
 * @param ctx the parse tree
 */
fn exit_InlineAnnotation(&mut self, _ctx: &InlineAnnotationContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#paramDecl}.
 * @param ctx the parse tree
 */
fn enter_paramDecl(&mut self, _ctx: &ParamDeclContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#paramDecl}.
 * @param ctx the parse tree
 */
fn exit_paramDecl(&mut self, _ctx: &ParamDeclContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Fold}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Fold(&mut self, _ctx: &FoldContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Fold}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Fold(&mut self, _ctx: &FoldContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Add}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Add(&mut self, _ctx: &AddContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Add}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Add(&mut self, _ctx: &AddContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code IsZero}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_IsZero(&mut self, _ctx: &IsZeroContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code IsZero}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_IsZero(&mut self, _ctx: &IsZeroContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Var}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Var(&mut self, _ctx: &VarContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Var}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Var(&mut self, _ctx: &VarContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeAbstraction}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TypeAbstraction(&mut self, _ctx: &TypeAbstractionContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeAbstraction}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TypeAbstraction(&mut self, _ctx: &TypeAbstractionContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Divide}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Divide(&mut self, _ctx: &DivideContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Divide}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Divide(&mut self, _ctx: &DivideContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LessThan}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LessThan(&mut self, _ctx: &LessThanContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LessThan}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LessThan(&mut self, _ctx: &LessThanContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DotRecord}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_DotRecord(&mut self, _ctx: &DotRecordContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DotRecord}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_DotRecord(&mut self, _ctx: &DotRecordContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code GreaterThan}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_GreaterThan(&mut self, _ctx: &GreaterThanContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code GreaterThan}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_GreaterThan(&mut self, _ctx: &GreaterThanContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Equal}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Equal(&mut self, _ctx: &EqualContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Equal}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Equal(&mut self, _ctx: &EqualContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Throw}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Throw(&mut self, _ctx: &ThrowContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Throw}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Throw(&mut self, _ctx: &ThrowContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Multiply}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Multiply(&mut self, _ctx: &MultiplyContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Multiply}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Multiply(&mut self, _ctx: &MultiplyContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConstMemory}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConstMemory(&mut self, _ctx: &ConstMemoryContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConstMemory}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConstMemory(&mut self, _ctx: &ConstMemoryContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code List}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_List(&mut self, _ctx: &ListContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code List}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_List(&mut self, _ctx: &ListContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TryCatch}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TryCatch(&mut self, _ctx: &TryCatchContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TryCatch}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TryCatch(&mut self, _ctx: &TryCatchContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TryCastAs}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TryCastAs(&mut self, _ctx: &TryCastAsContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TryCastAs}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TryCastAs(&mut self, _ctx: &TryCastAsContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Head}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Head(&mut self, _ctx: &HeadContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Head}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Head(&mut self, _ctx: &HeadContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TerminatingSemicolon}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TerminatingSemicolon(&mut self, _ctx: &TerminatingSemicolonContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TerminatingSemicolon}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TerminatingSemicolon(&mut self, _ctx: &TerminatingSemicolonContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NotEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_NotEqual(&mut self, _ctx: &NotEqualContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NotEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_NotEqual(&mut self, _ctx: &NotEqualContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConstUnit}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConstUnit(&mut self, _ctx: &ConstUnitContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConstUnit}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConstUnit(&mut self, _ctx: &ConstUnitContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Sequence}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Sequence(&mut self, _ctx: &SequenceContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Sequence}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Sequence(&mut self, _ctx: &SequenceContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConstFalse}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConstFalse(&mut self, _ctx: &ConstFalseContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConstFalse}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConstFalse(&mut self, _ctx: &ConstFalseContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Abstraction}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Abstraction(&mut self, _ctx: &AbstractionContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Abstraction}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Abstraction(&mut self, _ctx: &AbstractionContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConstInt}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConstInt(&mut self, _ctx: &ConstIntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConstInt}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConstInt(&mut self, _ctx: &ConstIntContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Variant}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Variant(&mut self, _ctx: &VariantContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Variant}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Variant(&mut self, _ctx: &VariantContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConstTrue}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConstTrue(&mut self, _ctx: &ConstTrueContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConstTrue}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConstTrue(&mut self, _ctx: &ConstTrueContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Subtract}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Subtract(&mut self, _ctx: &SubtractContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Subtract}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Subtract(&mut self, _ctx: &SubtractContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeCast}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TypeCast(&mut self, _ctx: &TypeCastContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeCast}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TypeCast(&mut self, _ctx: &TypeCastContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code If}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_If(&mut self, _ctx: &IfContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code If}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_If(&mut self, _ctx: &IfContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Application}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Application(&mut self, _ctx: &ApplicationContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Application}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Application(&mut self, _ctx: &ApplicationContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Deref}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Deref(&mut self, _ctx: &DerefContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Deref}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Deref(&mut self, _ctx: &DerefContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code IsEmpty}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_IsEmpty(&mut self, _ctx: &IsEmptyContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code IsEmpty}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_IsEmpty(&mut self, _ctx: &IsEmptyContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Panic}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Panic(&mut self, _ctx: &PanicContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Panic}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Panic(&mut self, _ctx: &PanicContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LessThanOrEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LessThanOrEqual(&mut self, _ctx: &LessThanOrEqualContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LessThanOrEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LessThanOrEqual(&mut self, _ctx: &LessThanOrEqualContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Succ}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Succ(&mut self, _ctx: &SuccContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Succ}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Succ(&mut self, _ctx: &SuccContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Inl}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Inl(&mut self, _ctx: &InlContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Inl}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Inl(&mut self, _ctx: &InlContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code GreaterThanOrEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_GreaterThanOrEqual(&mut self, _ctx: &GreaterThanOrEqualContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code GreaterThanOrEqual}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_GreaterThanOrEqual(&mut self, _ctx: &GreaterThanOrEqualContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Inr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Inr(&mut self, _ctx: &InrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Inr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Inr(&mut self, _ctx: &InrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Match}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Match(&mut self, _ctx: &MatchContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Match}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Match(&mut self, _ctx: &MatchContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LogicNot}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LogicNot(&mut self, _ctx: &LogicNotContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LogicNot}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LogicNot(&mut self, _ctx: &LogicNotContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ParenthesisedExpr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ParenthesisedExpr(&mut self, _ctx: &ParenthesisedExprContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ParenthesisedExpr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ParenthesisedExpr(&mut self, _ctx: &ParenthesisedExprContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Tail}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Tail(&mut self, _ctx: &TailContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Tail}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Tail(&mut self, _ctx: &TailContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Record}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Record(&mut self, _ctx: &RecordContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Record}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Record(&mut self, _ctx: &RecordContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LogicAnd}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LogicAnd(&mut self, _ctx: &LogicAndContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LogicAnd}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LogicAnd(&mut self, _ctx: &LogicAndContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeApplication}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TypeApplication(&mut self, _ctx: &TypeApplicationContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeApplication}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TypeApplication(&mut self, _ctx: &TypeApplicationContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LetRec}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LetRec(&mut self, _ctx: &LetRecContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LetRec}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LetRec(&mut self, _ctx: &LetRecContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code LogicOr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_LogicOr(&mut self, _ctx: &LogicOrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code LogicOr}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_LogicOr(&mut self, _ctx: &LogicOrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TryWith}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TryWith(&mut self, _ctx: &TryWithContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TryWith}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TryWith(&mut self, _ctx: &TryWithContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Pred}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Pred(&mut self, _ctx: &PredContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Pred}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Pred(&mut self, _ctx: &PredContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeAsc}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_TypeAsc(&mut self, _ctx: &TypeAscContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeAsc}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_TypeAsc(&mut self, _ctx: &TypeAscContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code NatRec}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_NatRec(&mut self, _ctx: &NatRecContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code NatRec}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_NatRec(&mut self, _ctx: &NatRecContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Unfold}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Unfold(&mut self, _ctx: &UnfoldContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Unfold}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Unfold(&mut self, _ctx: &UnfoldContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Ref}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Ref(&mut self, _ctx: &RefContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Ref}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Ref(&mut self, _ctx: &RefContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code DotTuple}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_DotTuple(&mut self, _ctx: &DotTupleContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code DotTuple}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_DotTuple(&mut self, _ctx: &DotTupleContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Fix}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Fix(&mut self, _ctx: &FixContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Fix}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Fix(&mut self, _ctx: &FixContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Let}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Let(&mut self, _ctx: &LetContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Let}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Let(&mut self, _ctx: &LetContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Assign}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Assign(&mut self, _ctx: &AssignContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Assign}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Assign(&mut self, _ctx: &AssignContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code Tuple}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_Tuple(&mut self, _ctx: &TupleContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code Tuple}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_Tuple(&mut self, _ctx: &TupleContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ConsList}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn enter_ConsList(&mut self, _ctx: &ConsListContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ConsList}
 * labeled alternative in {@link stellaParser#expr}.
 * @param ctx the parse tree
 */
fn exit_ConsList(&mut self, _ctx: &ConsListContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#patternBinding}.
 * @param ctx the parse tree
 */
fn enter_patternBinding(&mut self, _ctx: &PatternBindingContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#patternBinding}.
 * @param ctx the parse tree
 */
fn exit_patternBinding(&mut self, _ctx: &PatternBindingContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#binding}.
 * @param ctx the parse tree
 */
fn enter_binding(&mut self, _ctx: &BindingContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#binding}.
 * @param ctx the parse tree
 */
fn exit_binding(&mut self, _ctx: &BindingContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#matchCase}.
 * @param ctx the parse tree
 */
fn enter_matchCase(&mut self, _ctx: &MatchCaseContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#matchCase}.
 * @param ctx the parse tree
 */
fn exit_matchCase(&mut self, _ctx: &MatchCaseContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternCons}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternCons(&mut self, _ctx: &PatternConsContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternCons}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternCons(&mut self, _ctx: &PatternConsContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternTuple}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternTuple(&mut self, _ctx: &PatternTupleContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternTuple}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternTuple(&mut self, _ctx: &PatternTupleContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternList}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternList(&mut self, _ctx: &PatternListContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternList}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternList(&mut self, _ctx: &PatternListContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternRecord}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternRecord(&mut self, _ctx: &PatternRecordContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternRecord}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternRecord(&mut self, _ctx: &PatternRecordContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternVariant}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternVariant(&mut self, _ctx: &PatternVariantContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternVariant}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternVariant(&mut self, _ctx: &PatternVariantContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternAsc}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternAsc(&mut self, _ctx: &PatternAscContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternAsc}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternAsc(&mut self, _ctx: &PatternAscContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternInt}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternInt(&mut self, _ctx: &PatternIntContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternInt}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternInt(&mut self, _ctx: &PatternIntContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternInr}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternInr(&mut self, _ctx: &PatternInrContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternInr}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternInr(&mut self, _ctx: &PatternInrContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternTrue}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternTrue(&mut self, _ctx: &PatternTrueContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternTrue}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternTrue(&mut self, _ctx: &PatternTrueContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternInl}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternInl(&mut self, _ctx: &PatternInlContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternInl}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternInl(&mut self, _ctx: &PatternInlContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternVar}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternVar(&mut self, _ctx: &PatternVarContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternVar}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternVar(&mut self, _ctx: &PatternVarContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code ParenthesisedPattern}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_ParenthesisedPattern(&mut self, _ctx: &ParenthesisedPatternContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code ParenthesisedPattern}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_ParenthesisedPattern(&mut self, _ctx: &ParenthesisedPatternContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternSucc}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternSucc(&mut self, _ctx: &PatternSuccContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternSucc}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternSucc(&mut self, _ctx: &PatternSuccContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternFalse}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternFalse(&mut self, _ctx: &PatternFalseContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternFalse}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternFalse(&mut self, _ctx: &PatternFalseContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternUnit}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternUnit(&mut self, _ctx: &PatternUnitContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternUnit}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternUnit(&mut self, _ctx: &PatternUnitContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code PatternCastAs}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn enter_PatternCastAs(&mut self, _ctx: &PatternCastAsContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code PatternCastAs}
 * labeled alternative in {@link stellaParser#pattern}.
 * @param ctx the parse tree
 */
fn exit_PatternCastAs(&mut self, _ctx: &PatternCastAsContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#labelledPattern}.
 * @param ctx the parse tree
 */
fn enter_labelledPattern(&mut self, _ctx: &LabelledPatternContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#labelledPattern}.
 * @param ctx the parse tree
 */
fn exit_labelledPattern(&mut self, _ctx: &LabelledPatternContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeTuple}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeTuple(&mut self, _ctx: &TypeTupleContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeTuple}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeTuple(&mut self, _ctx: &TypeTupleContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeTop}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeTop(&mut self, _ctx: &TypeTopContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeTop}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeTop(&mut self, _ctx: &TypeTopContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeBool}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeBool(&mut self, _ctx: &TypeBoolContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeBool}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeBool(&mut self, _ctx: &TypeBoolContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeRef}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeRef(&mut self, _ctx: &TypeRefContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeRef}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeRef(&mut self, _ctx: &TypeRefContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeRec}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeRec(&mut self, _ctx: &TypeRecContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeRec}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeRec(&mut self, _ctx: &TypeRecContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeAuto}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeAuto(&mut self, _ctx: &TypeAutoContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeAuto}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeAuto(&mut self, _ctx: &TypeAutoContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeSum}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeSum(&mut self, _ctx: &TypeSumContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeSum}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeSum(&mut self, _ctx: &TypeSumContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeVar}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeVar(&mut self, _ctx: &TypeVarContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeVar}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeVar(&mut self, _ctx: &TypeVarContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeVariant}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeVariant(&mut self, _ctx: &TypeVariantContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeVariant}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeVariant(&mut self, _ctx: &TypeVariantContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeUnit}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeUnit(&mut self, _ctx: &TypeUnitContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeUnit}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeUnit(&mut self, _ctx: &TypeUnitContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeNat}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeNat(&mut self, _ctx: &TypeNatContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeNat}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeNat(&mut self, _ctx: &TypeNatContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeBottom}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeBottom(&mut self, _ctx: &TypeBottomContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeBottom}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeBottom(&mut self, _ctx: &TypeBottomContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeParens}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeParens(&mut self, _ctx: &TypeParensContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeParens}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeParens(&mut self, _ctx: &TypeParensContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeFun}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeFun(&mut self, _ctx: &TypeFunContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeFun}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeFun(&mut self, _ctx: &TypeFunContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeForAll}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeForAll(&mut self, _ctx: &TypeForAllContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeForAll}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeForAll(&mut self, _ctx: &TypeForAllContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeRecord}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeRecord(&mut self, _ctx: &TypeRecordContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeRecord}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeRecord(&mut self, _ctx: &TypeRecordContext<'input>) { }
/**
 * Enter a parse tree produced by the {@code TypeList}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn enter_TypeList(&mut self, _ctx: &TypeListContext<'input>) { }
/**
 * Exit a parse tree produced by the {@code TypeList}
 * labeled alternative in {@link stellaParser#stellatype}.
 * @param ctx the parse tree
 */
fn exit_TypeList(&mut self, _ctx: &TypeListContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#recordFieldType}.
 * @param ctx the parse tree
 */
fn enter_recordFieldType(&mut self, _ctx: &RecordFieldTypeContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#recordFieldType}.
 * @param ctx the parse tree
 */
fn exit_recordFieldType(&mut self, _ctx: &RecordFieldTypeContext<'input>) { }
/**
 * Enter a parse tree produced by {@link stellaParser#variantFieldType}.
 * @param ctx the parse tree
 */
fn enter_variantFieldType(&mut self, _ctx: &VariantFieldTypeContext<'input>) { }
/**
 * Exit a parse tree produced by {@link stellaParser#variantFieldType}.
 * @param ctx the parse tree
 */
fn exit_variantFieldType(&mut self, _ctx: &VariantFieldTypeContext<'input>) { }

}

antlr_rust::coerce_from!{ 'input : stellaParserListener<'input> }


