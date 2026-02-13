// Generated from src/stellaParser.g4 by ANTLR 4.8
#![allow(dead_code)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(nonstandard_style)]
#![allow(unused_imports)]
#![allow(unused_mut)]
#![allow(unused_braces)]
use antlr_rust::PredictionContextCache;
use antlr_rust::parser::{Parser, BaseParser, ParserRecog, ParserNodeType};
use antlr_rust::token_stream::TokenStream;
use antlr_rust::TokenSource;
use antlr_rust::parser_atn_simulator::ParserATNSimulator;
use antlr_rust::errors::*;
use antlr_rust::rule_context::{BaseRuleContext, CustomRuleContext, RuleContext};
use antlr_rust::recognizer::{Recognizer,Actions};
use antlr_rust::atn_deserializer::ATNDeserializer;
use antlr_rust::dfa::DFA;
use antlr_rust::atn::{ATN, INVALID_ALT};
use antlr_rust::error_strategy::{ErrorStrategy, DefaultErrorStrategy};
use antlr_rust::parser_rule_context::{BaseParserRuleContext, ParserRuleContext,cast,cast_mut};
use antlr_rust::tree::*;
use antlr_rust::token::{TOKEN_EOF,OwningToken,Token};
use antlr_rust::int_stream::EOF;
use antlr_rust::vocabulary::{Vocabulary,VocabularyImpl};
use antlr_rust::token_factory::{CommonTokenFactory,TokenFactory, TokenAware};
use super::stellaparserlistener::*;
use antlr_rust::lazy_static;
use antlr_rust::{TidAble,TidExt};

use std::marker::PhantomData;
use std::sync::Arc;
use std::rc::Rc;
use std::convert::TryFrom;
use std::cell::RefCell;
use std::ops::{DerefMut, Deref};
use std::borrow::{Borrow,BorrowMut};
use std::any::{Any,TypeId};

		pub const Surrogate_id_SYMB_0:isize=1; 
		pub const Surrogate_id_SYMB_1:isize=2; 
		pub const Surrogate_id_SYMB_2:isize=3; 
		pub const Surrogate_id_SYMB_3:isize=4; 
		pub const Surrogate_id_SYMB_4:isize=5; 
		pub const Surrogate_id_SYMB_5:isize=6; 
		pub const Surrogate_id_SYMB_6:isize=7; 
		pub const Surrogate_id_SYMB_7:isize=8; 
		pub const Surrogate_id_SYMB_8:isize=9; 
		pub const Surrogate_id_SYMB_9:isize=10; 
		pub const Surrogate_id_SYMB_10:isize=11; 
		pub const Surrogate_id_SYMB_11:isize=12; 
		pub const Surrogate_id_SYMB_12:isize=13; 
		pub const Surrogate_id_SYMB_13:isize=14; 
		pub const Surrogate_id_SYMB_14:isize=15; 
		pub const Surrogate_id_SYMB_15:isize=16; 
		pub const Surrogate_id_SYMB_16:isize=17; 
		pub const Surrogate_id_SYMB_17:isize=18; 
		pub const Surrogate_id_SYMB_18:isize=19; 
		pub const Surrogate_id_SYMB_19:isize=20; 
		pub const Surrogate_id_SYMB_20:isize=21; 
		pub const Surrogate_id_SYMB_21:isize=22; 
		pub const Surrogate_id_SYMB_22:isize=23; 
		pub const Surrogate_id_SYMB_23:isize=24; 
		pub const Surrogate_id_SYMB_24:isize=25; 
		pub const Surrogate_id_SYMB_25:isize=26; 
		pub const Surrogate_id_SYMB_26:isize=27; 
		pub const Surrogate_id_SYMB_27:isize=28; 
		pub const Surrogate_id_SYMB_28:isize=29; 
		pub const Surrogate_id_SYMB_29:isize=30; 
		pub const Surrogate_id_SYMB_30:isize=31; 
		pub const Surrogate_id_SYMB_31:isize=32; 
		pub const Surrogate_id_SYMB_32:isize=33; 
		pub const Surrogate_id_SYMB_33:isize=34; 
		pub const Surrogate_id_SYMB_34:isize=35; 
		pub const Surrogate_id_SYMB_35:isize=36; 
		pub const Surrogate_id_SYMB_36:isize=37; 
		pub const Surrogate_id_SYMB_37:isize=38; 
		pub const Surrogate_id_SYMB_38:isize=39; 
		pub const Surrogate_id_SYMB_39:isize=40; 
		pub const Surrogate_id_SYMB_40:isize=41; 
		pub const Surrogate_id_SYMB_41:isize=42; 
		pub const Surrogate_id_SYMB_42:isize=43; 
		pub const Surrogate_id_SYMB_43:isize=44; 
		pub const Surrogate_id_SYMB_44:isize=45; 
		pub const Surrogate_id_SYMB_45:isize=46; 
		pub const Surrogate_id_SYMB_46:isize=47; 
		pub const Surrogate_id_SYMB_47:isize=48; 
		pub const Surrogate_id_SYMB_48:isize=49; 
		pub const Surrogate_id_SYMB_49:isize=50; 
		pub const Surrogate_id_SYMB_50:isize=51; 
		pub const Surrogate_id_SYMB_51:isize=52; 
		pub const Surrogate_id_SYMB_52:isize=53; 
		pub const Surrogate_id_SYMB_53:isize=54; 
		pub const Surrogate_id_SYMB_54:isize=55; 
		pub const Surrogate_id_SYMB_55:isize=56; 
		pub const Surrogate_id_SYMB_56:isize=57; 
		pub const Surrogate_id_SYMB_57:isize=58; 
		pub const Surrogate_id_SYMB_58:isize=59; 
		pub const Surrogate_id_SYMB_59:isize=60; 
		pub const Surrogate_id_SYMB_60:isize=61; 
		pub const Surrogate_id_SYMB_61:isize=62; 
		pub const Surrogate_id_SYMB_62:isize=63; 
		pub const Surrogate_id_SYMB_63:isize=64; 
		pub const Surrogate_id_SYMB_64:isize=65; 
		pub const Surrogate_id_SYMB_65:isize=66; 
		pub const EXCEPTION:isize=67; 
		pub const VARIANT:isize=68; 
		pub const CAST:isize=69; 
		pub const ASSIGN:isize=70; 
		pub const REF_TYPE:isize=71; 
		pub const REFERENCE:isize=72; 
		pub const PANIC:isize=73; 
		pub const THROW:isize=74; 
		pub const TRY:isize=75; 
		pub const CATCH:isize=76; 
		pub const TOP_TYPE:isize=77; 
		pub const BOTTOM_TYPE:isize=78; 
		pub const AUTO_TYPE:isize=79; 
		pub const GENERIC:isize=80; 
		pub const FORALL:isize=81; 
		pub const COMMENT_antlr_builtin:isize=82; 
		pub const MULTICOMMENT_antlr_builtin:isize=83; 
		pub const StellaIdent:isize=84; 
		pub const ExtensionName:isize=85; 
		pub const MemoryAddress:isize=86; 
		pub const INTEGER:isize=87; 
		pub const WS:isize=88; 
		pub const ErrorToken:isize=89;
	pub const RULE_start_Program:usize = 0; 
	pub const RULE_start_Expr:usize = 1; 
	pub const RULE_start_Type:usize = 2; 
	pub const RULE_program:usize = 3; 
	pub const RULE_languageDecl:usize = 4; 
	pub const RULE_extension:usize = 5; 
	pub const RULE_decl:usize = 6; 
	pub const RULE_annotation:usize = 7; 
	pub const RULE_paramDecl:usize = 8; 
	pub const RULE_expr:usize = 9; 
	pub const RULE_patternBinding:usize = 10; 
	pub const RULE_binding:usize = 11; 
	pub const RULE_matchCase:usize = 12; 
	pub const RULE_pattern:usize = 13; 
	pub const RULE_labelledPattern:usize = 14; 
	pub const RULE_stellatype:usize = 15; 
	pub const RULE_recordFieldType:usize = 16; 
	pub const RULE_variantFieldType:usize = 17;
	pub const ruleNames: [&'static str; 18] =  [
		"start_Program", "start_Expr", "start_Type", "program", "languageDecl", 
		"extension", "decl", "annotation", "paramDecl", "expr", "patternBinding", 
		"binding", "matchCase", "pattern", "labelledPattern", "stellatype", "recordFieldType", 
		"variantFieldType"
	];


	pub const _LITERAL_NAMES: [Option<&'static str>;82] = [
		None, Some("','"), Some("';'"), Some("'('"), Some("')'"), Some("'{'"), 
		Some("'}'"), Some("'='"), Some("':'"), Some("'->'"), Some("'=>'"), Some("'|'"), 
		Some("'<|'"), Some("'|>'"), Some("'['"), Some("']'"), Some("'<'"), Some("'<='"), 
		Some("'>'"), Some("'>='"), Some("'=='"), Some("'!='"), Some("'+'"), Some("'-'"), 
		Some("'*'"), Some("'/'"), Some("'.'"), Some("'List::head'"), Some("'List::isempty'"), 
		Some("'List::tail'"), Some("'Nat::pred'"), Some("'Nat::iszero'"), Some("'Nat::rec'"), 
		Some("'Bool'"), Some("'Nat'"), Some("'Unit'"), Some("'and'"), Some("'as'"), 
		Some("'cons'"), Some("'core'"), Some("'else'"), Some("'extend'"), Some("'false'"), 
		Some("'fix'"), Some("'fn'"), Some("'fold'"), Some("'if'"), Some("'in'"), 
		Some("'inl'"), Some("'inline'"), Some("'inr'"), Some("'language'"), Some("'let'"), 
		Some("'letrec'"), Some("'match'"), Some("'not'"), Some("'or'"), Some("'return'"), 
		Some("'succ'"), Some("'then'"), Some("'throws'"), Some("'true'"), Some("'type'"), 
		Some("'unfold'"), Some("'unit'"), Some("'with'"), Some("'\u{00B5}'"), 
		Some("'exception'"), Some("'variant'"), Some("'cast'"), Some("':='"), 
		Some("'&'"), Some("'new'"), Some("'panic!'"), Some("'throw'"), Some("'try'"), 
		Some("'catch'"), Some("'Top'"), Some("'Bot'"), Some("'auto'"), Some("'generic'"), 
		Some("'forall'")
	];
	pub const _SYMBOLIC_NAMES: [Option<&'static str>;90]  = [
		None, Some("Surrogate_id_SYMB_0"), Some("Surrogate_id_SYMB_1"), Some("Surrogate_id_SYMB_2"), 
		Some("Surrogate_id_SYMB_3"), Some("Surrogate_id_SYMB_4"), Some("Surrogate_id_SYMB_5"), 
		Some("Surrogate_id_SYMB_6"), Some("Surrogate_id_SYMB_7"), Some("Surrogate_id_SYMB_8"), 
		Some("Surrogate_id_SYMB_9"), Some("Surrogate_id_SYMB_10"), Some("Surrogate_id_SYMB_11"), 
		Some("Surrogate_id_SYMB_12"), Some("Surrogate_id_SYMB_13"), Some("Surrogate_id_SYMB_14"), 
		Some("Surrogate_id_SYMB_15"), Some("Surrogate_id_SYMB_16"), Some("Surrogate_id_SYMB_17"), 
		Some("Surrogate_id_SYMB_18"), Some("Surrogate_id_SYMB_19"), Some("Surrogate_id_SYMB_20"), 
		Some("Surrogate_id_SYMB_21"), Some("Surrogate_id_SYMB_22"), Some("Surrogate_id_SYMB_23"), 
		Some("Surrogate_id_SYMB_24"), Some("Surrogate_id_SYMB_25"), Some("Surrogate_id_SYMB_26"), 
		Some("Surrogate_id_SYMB_27"), Some("Surrogate_id_SYMB_28"), Some("Surrogate_id_SYMB_29"), 
		Some("Surrogate_id_SYMB_30"), Some("Surrogate_id_SYMB_31"), Some("Surrogate_id_SYMB_32"), 
		Some("Surrogate_id_SYMB_33"), Some("Surrogate_id_SYMB_34"), Some("Surrogate_id_SYMB_35"), 
		Some("Surrogate_id_SYMB_36"), Some("Surrogate_id_SYMB_37"), Some("Surrogate_id_SYMB_38"), 
		Some("Surrogate_id_SYMB_39"), Some("Surrogate_id_SYMB_40"), Some("Surrogate_id_SYMB_41"), 
		Some("Surrogate_id_SYMB_42"), Some("Surrogate_id_SYMB_43"), Some("Surrogate_id_SYMB_44"), 
		Some("Surrogate_id_SYMB_45"), Some("Surrogate_id_SYMB_46"), Some("Surrogate_id_SYMB_47"), 
		Some("Surrogate_id_SYMB_48"), Some("Surrogate_id_SYMB_49"), Some("Surrogate_id_SYMB_50"), 
		Some("Surrogate_id_SYMB_51"), Some("Surrogate_id_SYMB_52"), Some("Surrogate_id_SYMB_53"), 
		Some("Surrogate_id_SYMB_54"), Some("Surrogate_id_SYMB_55"), Some("Surrogate_id_SYMB_56"), 
		Some("Surrogate_id_SYMB_57"), Some("Surrogate_id_SYMB_58"), Some("Surrogate_id_SYMB_59"), 
		Some("Surrogate_id_SYMB_60"), Some("Surrogate_id_SYMB_61"), Some("Surrogate_id_SYMB_62"), 
		Some("Surrogate_id_SYMB_63"), Some("Surrogate_id_SYMB_64"), Some("Surrogate_id_SYMB_65"), 
		Some("EXCEPTION"), Some("VARIANT"), Some("CAST"), Some("ASSIGN"), Some("REF_TYPE"), 
		Some("REFERENCE"), Some("PANIC"), Some("THROW"), Some("TRY"), Some("CATCH"), 
		Some("TOP_TYPE"), Some("BOTTOM_TYPE"), Some("AUTO_TYPE"), Some("GENERIC"), 
		Some("FORALL"), Some("COMMENT_antlr_builtin"), Some("MULTICOMMENT_antlr_builtin"), 
		Some("StellaIdent"), Some("ExtensionName"), Some("MemoryAddress"), Some("INTEGER"), 
		Some("WS"), Some("ErrorToken")
	];
	lazy_static!{
	    static ref _shared_context_cache: Arc<PredictionContextCache> = Arc::new(PredictionContextCache::new());
		static ref VOCABULARY: Box<dyn Vocabulary> = Box::new(VocabularyImpl::new(_LITERAL_NAMES.iter(), _SYMBOLIC_NAMES.iter(), None));
	}


type BaseParserType<'input, I> =
	BaseParser<'input,stellaParserExt<'input>, I, stellaParserContextType , dyn stellaParserListener<'input> + 'input >;

type TokenType<'input> = <LocalTokenFactory<'input> as TokenFactory<'input>>::Tok;
pub type LocalTokenFactory<'input> = CommonTokenFactory;

pub type stellaParserTreeWalker<'input,'a> =
	ParseTreeWalker<'input, 'a, stellaParserContextType , dyn stellaParserListener<'input> + 'a>;

/// Parser for stellaParser grammar
pub struct stellaParser<'input,I,H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	base:BaseParserType<'input,I>,
	interpreter:Arc<ParserATNSimulator>,
	_shared_context_cache: Box<PredictionContextCache>,
    pub err_handler: H,
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn get_serialized_atn() -> &'static str { _serializedATN }

    pub fn set_error_strategy(&mut self, strategy: H) {
        self.err_handler = strategy
    }

    pub fn with_strategy(input: I, strategy: H) -> Self {
		antlr_rust::recognizer::check_version("0","3");
		let interpreter = Arc::new(ParserATNSimulator::new(
			_ATN.clone(),
			_decision_to_DFA.clone(),
			_shared_context_cache.clone(),
		));
		Self {
			base: BaseParser::new_base_parser(
				input,
				Arc::clone(&interpreter),
				stellaParserExt{
					_pd: Default::default(),
				}
			),
			interpreter,
            _shared_context_cache: Box::new(PredictionContextCache::new()),
            err_handler: strategy,
        }
    }

}

type DynStrategy<'input,I> = Box<dyn ErrorStrategy<'input,BaseParserType<'input,I>> + 'input>;

impl<'input, I> stellaParser<'input, I, DynStrategy<'input,I>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
    pub fn with_dyn_strategy(input: I) -> Self{
    	Self::with_strategy(input,Box::new(DefaultErrorStrategy::new()))
    }
}

impl<'input, I> stellaParser<'input, I, DefaultErrorStrategy<'input,stellaParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
    pub fn new(input: I) -> Self{
    	Self::with_strategy(input,DefaultErrorStrategy::new())
    }
}

/// Trait for monomorphized trait object that corresponds to the nodes of parse tree generated for stellaParser
pub trait stellaParserContext<'input>:
	for<'x> Listenable<dyn stellaParserListener<'input> + 'x > + 
	ParserRuleContext<'input, TF=LocalTokenFactory<'input>, Ctx=stellaParserContextType>
{}

antlr_rust::coerce_from!{ 'input : stellaParserContext<'input> }

impl<'input> stellaParserContext<'input> for TerminalNode<'input,stellaParserContextType> {}
impl<'input> stellaParserContext<'input> for ErrorNode<'input,stellaParserContextType> {}

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn stellaParserContext<'input> + 'input }

antlr_rust::tid! { impl<'input> TidAble<'input> for dyn stellaParserListener<'input> + 'input }

pub struct stellaParserContextType;
antlr_rust::tid!{stellaParserContextType}

impl<'input> ParserNodeType<'input> for stellaParserContextType{
	type TF = LocalTokenFactory<'input>;
	type Type = dyn stellaParserContext<'input> + 'input;
}

impl<'input, I, H> Deref for stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
    type Target = BaseParserType<'input,I>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'input, I, H> DerefMut for stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

pub struct stellaParserExt<'input>{
	_pd: PhantomData<&'input str>,
}

impl<'input> stellaParserExt<'input>{
}
antlr_rust::tid! { stellaParserExt<'a> }

impl<'input> TokenAware<'input> for stellaParserExt<'input>{
	type TF = LocalTokenFactory<'input>;
}

impl<'input,I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>> ParserRecog<'input, BaseParserType<'input,I>> for stellaParserExt<'input>{}

impl<'input,I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>> Actions<'input, BaseParserType<'input,I>> for stellaParserExt<'input>{
	fn get_grammar_file_name(&self) -> & str{ "stellaParser.g4"}

   	fn get_rule_names(&self) -> &[& str] {&ruleNames}

   	fn get_vocabulary(&self) -> &dyn Vocabulary { &**VOCABULARY }
	fn sempred(_localctx: Option<&(dyn stellaParserContext<'input> + 'input)>, rule_index: isize, pred_index: isize,
			   recog:&mut BaseParserType<'input,I>
	)->bool{
		match rule_index {
					9 => stellaParser::<'input,I,_>::expr_sempred(_localctx.and_then(|x|x.downcast_ref()), pred_index, recog),
					13 => stellaParser::<'input,I,_>::pattern_sempred(_localctx.and_then(|x|x.downcast_ref()), pred_index, recog),
					15 => stellaParser::<'input,I,_>::stellatype_sempred(_localctx.and_then(|x|x.downcast_ref()), pred_index, recog),
			_ => true
		}
	}
}

impl<'input, I> stellaParser<'input, I, DefaultErrorStrategy<'input,stellaParserContextType>>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
{
	fn expr_sempred(_localctx: Option<&ExprContext<'input>>, pred_index:isize,
						recog:&mut <Self as Deref>::Target
		) -> bool {
		match pred_index {
				0=>{
					recog.precpred(None, 30)
				}
				1=>{
					recog.precpred(None, 29)
				}
				2=>{
					recog.precpred(None, 28)
				}
				3=>{
					recog.precpred(None, 25)
				}
				4=>{
					recog.precpred(None, 24)
				}
				5=>{
					recog.precpred(None, 23)
				}
				6=>{
					recog.precpred(None, 14)
				}
				7=>{
					recog.precpred(None, 13)
				}
				8=>{
					recog.precpred(None, 12)
				}
				9=>{
					recog.precpred(None, 11)
				}
				10=>{
					recog.precpred(None, 10)
				}
				11=>{
					recog.precpred(None, 9)
				}
				12=>{
					recog.precpred(None, 8)
				}
				13=>{
					recog.precpred(None, 6)
				}
				14=>{
					recog.precpred(None, 59)
				}
				15=>{
					recog.precpred(None, 58)
				}
				16=>{
					recog.precpred(None, 32)
				}
				17=>{
					recog.precpred(None, 31)
				}
				18=>{
					recog.precpred(None, 22)
				}
				19=>{
					recog.precpred(None, 21)
				}
				20=>{
					recog.precpred(None, 1)
				}
			_ => true
		}
	}
	fn pattern_sempred(_localctx: Option<&PatternContext<'input>>, pred_index:isize,
						recog:&mut <Self as Deref>::Target
		) -> bool {
		match pred_index {
				21=>{
					recog.precpred(None, 3)
				}
				22=>{
					recog.precpred(None, 2)
				}
			_ => true
		}
	}
	fn stellatype_sempred(_localctx: Option<&StellatypeContext<'input>>, pred_index:isize,
						recog:&mut <Self as Deref>::Target
		) -> bool {
		match pred_index {
				23=>{
					recog.precpred(None, 14)
				}
			_ => true
		}
	}
}
//------------------- start_Program ----------------
pub type Start_ProgramContextAll<'input> = Start_ProgramContext<'input>;


pub type Start_ProgramContext<'input> = BaseParserRuleContext<'input,Start_ProgramContextExt<'input>>;

#[derive(Clone)]
pub struct Start_ProgramContextExt<'input>{
	pub x: Option<Rc<ProgramContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for Start_ProgramContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for Start_ProgramContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_start_Program(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_start_Program(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for Start_ProgramContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_start_Program }
	//fn type_rule_index() -> usize where Self: Sized { RULE_start_Program }
}
antlr_rust::tid!{Start_ProgramContextExt<'a>}

impl<'input> Start_ProgramContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Start_ProgramContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Start_ProgramContextExt{
				x: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait Start_ProgramContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<Start_ProgramContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token EOF
/// Returns `None` if there is no child corresponding to token EOF
fn EOF(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(EOF, 0)
}
fn program(&self) -> Option<Rc<ProgramContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Start_ProgramContextAttrs<'input> for Start_ProgramContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn start_Program(&mut self,)
	-> Result<Rc<Start_ProgramContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Start_ProgramContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 0, RULE_start_Program);
        let mut _localctx: Rc<Start_ProgramContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule program*/
			recog.base.set_state(36);
			let tmp = recog.program()?;
			 cast_mut::<_,Start_ProgramContext >(&mut _localctx).x = Some(tmp.clone());
			  

			recog.base.set_state(37);
			recog.base.match_token(EOF,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- start_Expr ----------------
pub type Start_ExprContextAll<'input> = Start_ExprContext<'input>;


pub type Start_ExprContext<'input> = BaseParserRuleContext<'input,Start_ExprContextExt<'input>>;

#[derive(Clone)]
pub struct Start_ExprContextExt<'input>{
	pub x: Option<Rc<ExprContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for Start_ExprContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for Start_ExprContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_start_Expr(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_start_Expr(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for Start_ExprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_start_Expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_start_Expr }
}
antlr_rust::tid!{Start_ExprContextExt<'a>}

impl<'input> Start_ExprContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Start_ExprContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Start_ExprContextExt{
				x: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait Start_ExprContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<Start_ExprContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token EOF
/// Returns `None` if there is no child corresponding to token EOF
fn EOF(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(EOF, 0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Start_ExprContextAttrs<'input> for Start_ExprContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn start_Expr(&mut self,)
	-> Result<Rc<Start_ExprContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Start_ExprContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 2, RULE_start_Expr);
        let mut _localctx: Rc<Start_ExprContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule expr*/
			recog.base.set_state(39);
			let tmp = recog.expr_rec(0)?;
			 cast_mut::<_,Start_ExprContext >(&mut _localctx).x = Some(tmp.clone());
			  

			recog.base.set_state(40);
			recog.base.match_token(EOF,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- start_Type ----------------
pub type Start_TypeContextAll<'input> = Start_TypeContext<'input>;


pub type Start_TypeContext<'input> = BaseParserRuleContext<'input,Start_TypeContextExt<'input>>;

#[derive(Clone)]
pub struct Start_TypeContextExt<'input>{
	pub x: Option<Rc<StellatypeContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for Start_TypeContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for Start_TypeContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_start_Type(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_start_Type(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for Start_TypeContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_start_Type }
	//fn type_rule_index() -> usize where Self: Sized { RULE_start_Type }
}
antlr_rust::tid!{Start_TypeContextExt<'a>}

impl<'input> Start_TypeContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<Start_TypeContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,Start_TypeContextExt{
				x: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait Start_TypeContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<Start_TypeContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token EOF
/// Returns `None` if there is no child corresponding to token EOF
fn EOF(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(EOF, 0)
}
fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> Start_TypeContextAttrs<'input> for Start_TypeContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn start_Type(&mut self,)
	-> Result<Rc<Start_TypeContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = Start_TypeContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 4, RULE_start_Type);
        let mut _localctx: Rc<Start_TypeContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule stellatype*/
			recog.base.set_state(42);
			let tmp = recog.stellatype_rec(0)?;
			 cast_mut::<_,Start_TypeContext >(&mut _localctx).x = Some(tmp.clone());
			  

			recog.base.set_state(43);
			recog.base.match_token(EOF,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- program ----------------
pub type ProgramContextAll<'input> = ProgramContext<'input>;


pub type ProgramContext<'input> = BaseParserRuleContext<'input,ProgramContextExt<'input>>;

#[derive(Clone)]
pub struct ProgramContextExt<'input>{
	pub extension: Option<Rc<ExtensionContextAll<'input>>>,
	pub extensions:Vec<Rc<ExtensionContextAll<'input>>>,
	pub decl: Option<Rc<DeclContextAll<'input>>>,
	pub decls:Vec<Rc<DeclContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for ProgramContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ProgramContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_program(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_program(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for ProgramContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_program }
	//fn type_rule_index() -> usize where Self: Sized { RULE_program }
}
antlr_rust::tid!{ProgramContextExt<'a>}

impl<'input> ProgramContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ProgramContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ProgramContextExt{
				extension: None, decl: None, 
				extensions: Vec::new(), decls: Vec::new(), 
				ph:PhantomData
			}),
		)
	}
}

pub trait ProgramContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<ProgramContextExt<'input>>{

fn languageDecl(&self) -> Option<Rc<LanguageDeclContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn extension_all(&self) ->  Vec<Rc<ExtensionContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn extension(&self, i: usize) -> Option<Rc<ExtensionContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}
fn decl_all(&self) ->  Vec<Rc<DeclContextAll<'input>>> where Self:Sized{
	self.children_of_type()
}
fn decl(&self, i: usize) -> Option<Rc<DeclContextAll<'input>>> where Self:Sized{
	self.child_of_type(i)
}

}

impl<'input> ProgramContextAttrs<'input> for ProgramContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn program(&mut self,)
	-> Result<Rc<ProgramContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = ProgramContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 6, RULE_program);
        let mut _localctx: Rc<ProgramContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule languageDecl*/
			recog.base.set_state(45);
			recog.languageDecl()?;

			recog.base.set_state(49);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			while _la==Surrogate_id_SYMB_40 {
				{
				{
				/*InvokeRule extension*/
				recog.base.set_state(46);
				let tmp = recog.extension()?;
				 cast_mut::<_,ProgramContext >(&mut _localctx).extension = Some(tmp.clone());
				  

				let temp =  cast_mut::<_,ProgramContext >(&mut _localctx).extension.clone().unwrap()
				 ;
				 cast_mut::<_,ProgramContext >(&mut _localctx).extensions.push(temp);
				  
				}
				}
				recog.base.set_state(51);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
			}
			recog.base.set_state(55);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			while ((((_la - 44)) & !0x3f) == 0 && ((1usize << (_la - 44)) & ((1usize << (Surrogate_id_SYMB_43 - 44)) | (1usize << (Surrogate_id_SYMB_48 - 44)) | (1usize << (Surrogate_id_SYMB_61 - 44)) | (1usize << (EXCEPTION - 44)))) != 0) || _la==GENERIC {
				{
				{
				/*InvokeRule decl*/
				recog.base.set_state(52);
				let tmp = recog.decl()?;
				 cast_mut::<_,ProgramContext >(&mut _localctx).decl = Some(tmp.clone());
				  

				let temp =  cast_mut::<_,ProgramContext >(&mut _localctx).decl.clone().unwrap()
				 ;
				 cast_mut::<_,ProgramContext >(&mut _localctx).decls.push(temp);
				  
				}
				}
				recog.base.set_state(57);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
			}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- languageDecl ----------------
#[derive(Debug)]
pub enum LanguageDeclContextAll<'input>{
	LanguageCoreContext(LanguageCoreContext<'input>),
Error(LanguageDeclContext<'input>)
}
antlr_rust::tid!{LanguageDeclContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for LanguageDeclContextAll<'input>{}

impl<'input> stellaParserContext<'input> for LanguageDeclContextAll<'input>{}

impl<'input> Deref for LanguageDeclContextAll<'input>{
	type Target = dyn LanguageDeclContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use LanguageDeclContextAll::*;
		match self{
			LanguageCoreContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LanguageDeclContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type LanguageDeclContext<'input> = BaseParserRuleContext<'input,LanguageDeclContextExt<'input>>;

#[derive(Clone)]
pub struct LanguageDeclContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for LanguageDeclContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LanguageDeclContext<'input>{
}

impl<'input> CustomRuleContext<'input> for LanguageDeclContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_languageDecl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_languageDecl }
}
antlr_rust::tid!{LanguageDeclContextExt<'a>}

impl<'input> LanguageDeclContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<LanguageDeclContextAll<'input>> {
		Rc::new(
		LanguageDeclContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,LanguageDeclContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait LanguageDeclContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<LanguageDeclContextExt<'input>>{


}

impl<'input> LanguageDeclContextAttrs<'input> for LanguageDeclContext<'input>{}

pub type LanguageCoreContext<'input> = BaseParserRuleContext<'input,LanguageCoreContextExt<'input>>;

pub trait LanguageCoreContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_50
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_50
	fn Surrogate_id_SYMB_50(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_50, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_38
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_38
	fn Surrogate_id_SYMB_38(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_38, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_1
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_1
	fn Surrogate_id_SYMB_1(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_1, 0)
	}
}

impl<'input> LanguageCoreContextAttrs<'input> for LanguageCoreContext<'input>{}

pub struct LanguageCoreContextExt<'input>{
	base:LanguageDeclContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LanguageCoreContextExt<'a>}

impl<'input> stellaParserContext<'input> for LanguageCoreContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LanguageCoreContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LanguageCore(self);
	}
}

impl<'input> CustomRuleContext<'input> for LanguageCoreContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_languageDecl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_languageDecl }
}

impl<'input> Borrow<LanguageDeclContextExt<'input>> for LanguageCoreContext<'input>{
	fn borrow(&self) -> &LanguageDeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<LanguageDeclContextExt<'input>> for LanguageCoreContext<'input>{
	fn borrow_mut(&mut self) -> &mut LanguageDeclContextExt<'input> { &mut self.base }
}

impl<'input> LanguageDeclContextAttrs<'input> for LanguageCoreContext<'input> {}

impl<'input> LanguageCoreContextExt<'input>{
	fn new(ctx: &dyn LanguageDeclContextAttrs<'input>) -> Rc<LanguageDeclContextAll<'input>>  {
		Rc::new(
			LanguageDeclContextAll::LanguageCoreContext(
				BaseParserRuleContext::copy_from(ctx,LanguageCoreContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn languageDecl(&mut self,)
	-> Result<Rc<LanguageDeclContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = LanguageDeclContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 8, RULE_languageDecl);
        let mut _localctx: Rc<LanguageDeclContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			let tmp = LanguageCoreContextExt::new(&**_localctx);
			recog.base.enter_outer_alt(Some(tmp.clone()), 1);
			_localctx = tmp;
			{
			recog.base.set_state(58);
			recog.base.match_token(Surrogate_id_SYMB_50,&mut recog.err_handler)?;

			recog.base.set_state(59);
			recog.base.match_token(Surrogate_id_SYMB_38,&mut recog.err_handler)?;

			recog.base.set_state(60);
			recog.base.match_token(Surrogate_id_SYMB_1,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- extension ----------------
#[derive(Debug)]
pub enum ExtensionContextAll<'input>{
	AnExtensionContext(AnExtensionContext<'input>),
Error(ExtensionContext<'input>)
}
antlr_rust::tid!{ExtensionContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for ExtensionContextAll<'input>{}

impl<'input> stellaParserContext<'input> for ExtensionContextAll<'input>{}

impl<'input> Deref for ExtensionContextAll<'input>{
	type Target = dyn ExtensionContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use ExtensionContextAll::*;
		match self{
			AnExtensionContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ExtensionContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type ExtensionContext<'input> = BaseParserRuleContext<'input,ExtensionContextExt<'input>>;

#[derive(Clone)]
pub struct ExtensionContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for ExtensionContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ExtensionContext<'input>{
}

impl<'input> CustomRuleContext<'input> for ExtensionContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_extension }
	//fn type_rule_index() -> usize where Self: Sized { RULE_extension }
}
antlr_rust::tid!{ExtensionContextExt<'a>}

impl<'input> ExtensionContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ExtensionContextAll<'input>> {
		Rc::new(
		ExtensionContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ExtensionContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait ExtensionContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<ExtensionContextExt<'input>>{


}

impl<'input> ExtensionContextAttrs<'input> for ExtensionContext<'input>{}

pub type AnExtensionContext<'input> = BaseParserRuleContext<'input,AnExtensionContextExt<'input>>;

pub trait AnExtensionContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_40
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_40
	fn Surrogate_id_SYMB_40(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_40, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_64
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_64
	fn Surrogate_id_SYMB_64(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_64, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_1
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_1
	fn Surrogate_id_SYMB_1(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_1, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token ExtensionName in current rule
	fn ExtensionName_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token ExtensionName, starting from 0.
	/// Returns `None` if number of children corresponding to token ExtensionName is less or equal than `i`.
	fn ExtensionName(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(ExtensionName, i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> AnExtensionContextAttrs<'input> for AnExtensionContext<'input>{}

pub struct AnExtensionContextExt<'input>{
	base:ExtensionContextExt<'input>,
	pub ExtensionName: Option<TokenType<'input>>,
	pub extensionNames:Vec<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{AnExtensionContextExt<'a>}

impl<'input> stellaParserContext<'input> for AnExtensionContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AnExtensionContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_AnExtension(self);
	}
}

impl<'input> CustomRuleContext<'input> for AnExtensionContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_extension }
	//fn type_rule_index() -> usize where Self: Sized { RULE_extension }
}

impl<'input> Borrow<ExtensionContextExt<'input>> for AnExtensionContext<'input>{
	fn borrow(&self) -> &ExtensionContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExtensionContextExt<'input>> for AnExtensionContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExtensionContextExt<'input> { &mut self.base }
}

impl<'input> ExtensionContextAttrs<'input> for AnExtensionContext<'input> {}

impl<'input> AnExtensionContextExt<'input>{
	fn new(ctx: &dyn ExtensionContextAttrs<'input>) -> Rc<ExtensionContextAll<'input>>  {
		Rc::new(
			ExtensionContextAll::AnExtensionContext(
				BaseParserRuleContext::copy_from(ctx,AnExtensionContextExt{
					ExtensionName:None, 
        			extensionNames:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn extension(&mut self,)
	-> Result<Rc<ExtensionContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = ExtensionContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 10, RULE_extension);
        let mut _localctx: Rc<ExtensionContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			let tmp = AnExtensionContextExt::new(&**_localctx);
			recog.base.enter_outer_alt(Some(tmp.clone()), 1);
			_localctx = tmp;
			{
			recog.base.set_state(62);
			recog.base.match_token(Surrogate_id_SYMB_40,&mut recog.err_handler)?;

			recog.base.set_state(63);
			recog.base.match_token(Surrogate_id_SYMB_64,&mut recog.err_handler)?;

			recog.base.set_state(64);
			let tmp = recog.base.match_token(ExtensionName,&mut recog.err_handler)?;
			if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
			ctx.ExtensionName = Some(tmp.clone()); } else {unreachable!("cant cast");}  

			let temp = if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
			ctx.ExtensionName.clone().unwrap() } else {unreachable!("cant cast");} ;
			if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
			ctx.extensionNames.push(temp); } else {unreachable!("cant cast");}  
			recog.base.set_state(69);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			while _la==Surrogate_id_SYMB_0 {
				{
				{
				recog.base.set_state(65);
				recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

				recog.base.set_state(66);
				let tmp = recog.base.match_token(ExtensionName,&mut recog.err_handler)?;
				if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
				ctx.ExtensionName = Some(tmp.clone()); } else {unreachable!("cant cast");}  

				let temp = if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
				ctx.ExtensionName.clone().unwrap() } else {unreachable!("cant cast");} ;
				if let ExtensionContextAll::AnExtensionContext(ctx) = cast_mut::<_,ExtensionContextAll >(&mut _localctx){
				ctx.extensionNames.push(temp); } else {unreachable!("cant cast");}  
				}
				}
				recog.base.set_state(71);
				recog.err_handler.sync(&mut recog.base)?;
				_la = recog.base.input.la(1);
			}
			recog.base.set_state(72);
			recog.base.match_token(Surrogate_id_SYMB_1,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- decl ----------------
#[derive(Debug)]
pub enum DeclContextAll<'input>{
	DeclTypeAliasContext(DeclTypeAliasContext<'input>),
	DeclExceptionTypeContext(DeclExceptionTypeContext<'input>),
	DeclFunContext(DeclFunContext<'input>),
	DeclExceptionVariantContext(DeclExceptionVariantContext<'input>),
	DeclFunGenericContext(DeclFunGenericContext<'input>),
Error(DeclContext<'input>)
}
antlr_rust::tid!{DeclContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for DeclContextAll<'input>{}

impl<'input> stellaParserContext<'input> for DeclContextAll<'input>{}

impl<'input> Deref for DeclContextAll<'input>{
	type Target = dyn DeclContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use DeclContextAll::*;
		match self{
			DeclTypeAliasContext(inner) => inner,
			DeclExceptionTypeContext(inner) => inner,
			DeclFunContext(inner) => inner,
			DeclExceptionVariantContext(inner) => inner,
			DeclFunGenericContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type DeclContext<'input> = BaseParserRuleContext<'input,DeclContextExt<'input>>;

#[derive(Clone)]
pub struct DeclContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for DeclContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclContext<'input>{
}

impl<'input> CustomRuleContext<'input> for DeclContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}
antlr_rust::tid!{DeclContextExt<'a>}

impl<'input> DeclContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<DeclContextAll<'input>> {
		Rc::new(
		DeclContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,DeclContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait DeclContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<DeclContextExt<'input>>{


}

impl<'input> DeclContextAttrs<'input> for DeclContext<'input>{}

pub type DeclTypeAliasContext<'input> = BaseParserRuleContext<'input,DeclTypeAliasContextExt<'input>>;

pub trait DeclTypeAliasContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_61
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_61
	fn Surrogate_id_SYMB_61(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_61, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
	fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_6, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> DeclTypeAliasContextAttrs<'input> for DeclTypeAliasContext<'input>{}

pub struct DeclTypeAliasContextExt<'input>{
	base:DeclContextExt<'input>,
	pub name: Option<TokenType<'input>>,
	pub atype: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DeclTypeAliasContextExt<'a>}

impl<'input> stellaParserContext<'input> for DeclTypeAliasContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclTypeAliasContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DeclTypeAlias(self);
	}
}

impl<'input> CustomRuleContext<'input> for DeclTypeAliasContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}

impl<'input> Borrow<DeclContextExt<'input>> for DeclTypeAliasContext<'input>{
	fn borrow(&self) -> &DeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<DeclContextExt<'input>> for DeclTypeAliasContext<'input>{
	fn borrow_mut(&mut self) -> &mut DeclContextExt<'input> { &mut self.base }
}

impl<'input> DeclContextAttrs<'input> for DeclTypeAliasContext<'input> {}

impl<'input> DeclTypeAliasContextExt<'input>{
	fn new(ctx: &dyn DeclContextAttrs<'input>) -> Rc<DeclContextAll<'input>>  {
		Rc::new(
			DeclContextAll::DeclTypeAliasContext(
				BaseParserRuleContext::copy_from(ctx,DeclTypeAliasContextExt{
					name:None, 
        			atype:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DeclExceptionTypeContext<'input> = BaseParserRuleContext<'input,DeclExceptionTypeContextExt<'input>>;

pub trait DeclExceptionTypeContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token EXCEPTION
	/// Returns `None` if there is no child corresponding to token EXCEPTION
	fn EXCEPTION(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(EXCEPTION, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_61
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_61
	fn Surrogate_id_SYMB_61(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_61, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
	fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_6, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> DeclExceptionTypeContextAttrs<'input> for DeclExceptionTypeContext<'input>{}

pub struct DeclExceptionTypeContextExt<'input>{
	base:DeclContextExt<'input>,
	pub exceptionType: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DeclExceptionTypeContextExt<'a>}

impl<'input> stellaParserContext<'input> for DeclExceptionTypeContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclExceptionTypeContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DeclExceptionType(self);
	}
}

impl<'input> CustomRuleContext<'input> for DeclExceptionTypeContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}

impl<'input> Borrow<DeclContextExt<'input>> for DeclExceptionTypeContext<'input>{
	fn borrow(&self) -> &DeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<DeclContextExt<'input>> for DeclExceptionTypeContext<'input>{
	fn borrow_mut(&mut self) -> &mut DeclContextExt<'input> { &mut self.base }
}

impl<'input> DeclContextAttrs<'input> for DeclExceptionTypeContext<'input> {}

impl<'input> DeclExceptionTypeContextExt<'input>{
	fn new(ctx: &dyn DeclContextAttrs<'input>) -> Rc<DeclContextAll<'input>>  {
		Rc::new(
			DeclContextAll::DeclExceptionTypeContext(
				BaseParserRuleContext::copy_from(ctx,DeclExceptionTypeContextExt{
        			exceptionType:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DeclFunContext<'input> = BaseParserRuleContext<'input,DeclFunContextExt<'input>>;

pub trait DeclFunContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_43
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_43
	fn Surrogate_id_SYMB_43(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_43, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_56
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_56
	fn Surrogate_id_SYMB_56(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_56, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_8
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_8
	fn Surrogate_id_SYMB_8(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_8, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_59
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_59
	fn Surrogate_id_SYMB_59(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_59, 0)
	}
	fn annotation_all(&self) ->  Vec<Rc<AnnotationContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn annotation(&self, i: usize) -> Option<Rc<AnnotationContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn paramDecl_all(&self) ->  Vec<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn paramDecl(&self, i: usize) -> Option<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn decl_all(&self) ->  Vec<Rc<DeclContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn decl(&self, i: usize) -> Option<Rc<DeclContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> DeclFunContextAttrs<'input> for DeclFunContext<'input>{}

pub struct DeclFunContextExt<'input>{
	base:DeclContextExt<'input>,
	pub annotation: Option<Rc<AnnotationContextAll<'input>>>,
	pub annotations:Vec<Rc<AnnotationContextAll<'input>>>,
	pub name: Option<TokenType<'input>>,
	pub paramDecl: Option<Rc<ParamDeclContextAll<'input>>>,
	pub paramDecls:Vec<Rc<ParamDeclContextAll<'input>>>,
	pub returnType: Option<Rc<StellatypeContextAll<'input>>>,
	pub stellatype: Option<Rc<StellatypeContextAll<'input>>>,
	pub throwTypes:Vec<Rc<StellatypeContextAll<'input>>>,
	pub decl: Option<Rc<DeclContextAll<'input>>>,
	pub localDecls:Vec<Rc<DeclContextAll<'input>>>,
	pub returnExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DeclFunContextExt<'a>}

impl<'input> stellaParserContext<'input> for DeclFunContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclFunContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DeclFun(self);
	}
}

impl<'input> CustomRuleContext<'input> for DeclFunContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}

impl<'input> Borrow<DeclContextExt<'input>> for DeclFunContext<'input>{
	fn borrow(&self) -> &DeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<DeclContextExt<'input>> for DeclFunContext<'input>{
	fn borrow_mut(&mut self) -> &mut DeclContextExt<'input> { &mut self.base }
}

impl<'input> DeclContextAttrs<'input> for DeclFunContext<'input> {}

impl<'input> DeclFunContextExt<'input>{
	fn new(ctx: &dyn DeclContextAttrs<'input>) -> Rc<DeclContextAll<'input>>  {
		Rc::new(
			DeclContextAll::DeclFunContext(
				BaseParserRuleContext::copy_from(ctx,DeclFunContextExt{
					name:None, 
        			annotation:None, paramDecl:None, returnType:None, stellatype:None, decl:None, returnExpr:None, 
        			annotations:Vec::new(), paramDecls:Vec::new(), throwTypes:Vec::new(), localDecls:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DeclExceptionVariantContext<'input> = BaseParserRuleContext<'input,DeclExceptionVariantContextExt<'input>>;

pub trait DeclExceptionVariantContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token EXCEPTION
	/// Returns `None` if there is no child corresponding to token EXCEPTION
	fn EXCEPTION(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(EXCEPTION, 0)
	}
	/// Retrieves first TerminalNode corresponding to token VARIANT
	/// Returns `None` if there is no child corresponding to token VARIANT
	fn VARIANT(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(VARIANT, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_7
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_7
	fn Surrogate_id_SYMB_7(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_7, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> DeclExceptionVariantContextAttrs<'input> for DeclExceptionVariantContext<'input>{}

pub struct DeclExceptionVariantContextExt<'input>{
	base:DeclContextExt<'input>,
	pub name: Option<TokenType<'input>>,
	pub variantType: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DeclExceptionVariantContextExt<'a>}

impl<'input> stellaParserContext<'input> for DeclExceptionVariantContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclExceptionVariantContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DeclExceptionVariant(self);
	}
}

impl<'input> CustomRuleContext<'input> for DeclExceptionVariantContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}

impl<'input> Borrow<DeclContextExt<'input>> for DeclExceptionVariantContext<'input>{
	fn borrow(&self) -> &DeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<DeclContextExt<'input>> for DeclExceptionVariantContext<'input>{
	fn borrow_mut(&mut self) -> &mut DeclContextExt<'input> { &mut self.base }
}

impl<'input> DeclContextAttrs<'input> for DeclExceptionVariantContext<'input> {}

impl<'input> DeclExceptionVariantContextExt<'input>{
	fn new(ctx: &dyn DeclContextAttrs<'input>) -> Rc<DeclContextAll<'input>>  {
		Rc::new(
			DeclContextAll::DeclExceptionVariantContext(
				BaseParserRuleContext::copy_from(ctx,DeclExceptionVariantContextExt{
					name:None, 
        			variantType:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DeclFunGenericContext<'input> = BaseParserRuleContext<'input,DeclFunGenericContextExt<'input>>;

pub trait DeclFunGenericContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token GENERIC
	/// Returns `None` if there is no child corresponding to token GENERIC
	fn GENERIC(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(GENERIC, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_43
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_43
	fn Surrogate_id_SYMB_43(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_43, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_56
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_56
	fn Surrogate_id_SYMB_56(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_56, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token StellaIdent in current rule
	fn StellaIdent_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token StellaIdent, starting from 0.
	/// Returns `None` if number of children corresponding to token StellaIdent is less or equal than `i`.
	fn StellaIdent(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, i)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_8
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_8
	fn Surrogate_id_SYMB_8(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_8, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_59
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_59
	fn Surrogate_id_SYMB_59(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_59, 0)
	}
	fn annotation_all(&self) ->  Vec<Rc<AnnotationContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn annotation(&self, i: usize) -> Option<Rc<AnnotationContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn paramDecl_all(&self) ->  Vec<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn paramDecl(&self, i: usize) -> Option<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn decl_all(&self) ->  Vec<Rc<DeclContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn decl(&self, i: usize) -> Option<Rc<DeclContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> DeclFunGenericContextAttrs<'input> for DeclFunGenericContext<'input>{}

pub struct DeclFunGenericContextExt<'input>{
	base:DeclContextExt<'input>,
	pub annotation: Option<Rc<AnnotationContextAll<'input>>>,
	pub annotations:Vec<Rc<AnnotationContextAll<'input>>>,
	pub name: Option<TokenType<'input>>,
	pub StellaIdent: Option<TokenType<'input>>,
	pub generics:Vec<TokenType<'input>>,
	pub paramDecl: Option<Rc<ParamDeclContextAll<'input>>>,
	pub paramDecls:Vec<Rc<ParamDeclContextAll<'input>>>,
	pub returnType: Option<Rc<StellatypeContextAll<'input>>>,
	pub stellatype: Option<Rc<StellatypeContextAll<'input>>>,
	pub throwTypes:Vec<Rc<StellatypeContextAll<'input>>>,
	pub decl: Option<Rc<DeclContextAll<'input>>>,
	pub localDecls:Vec<Rc<DeclContextAll<'input>>>,
	pub returnExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DeclFunGenericContextExt<'a>}

impl<'input> stellaParserContext<'input> for DeclFunGenericContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DeclFunGenericContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DeclFunGeneric(self);
	}
}

impl<'input> CustomRuleContext<'input> for DeclFunGenericContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_decl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_decl }
}

impl<'input> Borrow<DeclContextExt<'input>> for DeclFunGenericContext<'input>{
	fn borrow(&self) -> &DeclContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<DeclContextExt<'input>> for DeclFunGenericContext<'input>{
	fn borrow_mut(&mut self) -> &mut DeclContextExt<'input> { &mut self.base }
}

impl<'input> DeclContextAttrs<'input> for DeclFunGenericContext<'input> {}

impl<'input> DeclFunGenericContextExt<'input>{
	fn new(ctx: &dyn DeclContextAttrs<'input>) -> Rc<DeclContextAll<'input>>  {
		Rc::new(
			DeclContextAll::DeclFunGenericContext(
				BaseParserRuleContext::copy_from(ctx,DeclFunGenericContextExt{
					name:None, StellaIdent:None, 
        			generics:Vec::new(), 
        			annotation:None, paramDecl:None, returnType:None, stellatype:None, decl:None, returnExpr:None, 
        			annotations:Vec::new(), paramDecls:Vec::new(), throwTypes:Vec::new(), localDecls:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn decl(&mut self,)
	-> Result<Rc<DeclContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = DeclContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 12, RULE_decl);
        let mut _localctx: Rc<DeclContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			recog.base.set_state(190);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(18,&mut recog.base)? {
				1 =>{
					let tmp = DeclFunContextExt::new(&**_localctx);
					recog.base.enter_outer_alt(Some(tmp.clone()), 1);
					_localctx = tmp;
					{
					recog.base.set_state(77);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_48 {
						{
						{
						/*InvokeRule annotation*/
						recog.base.set_state(74);
						let tmp = recog.annotation()?;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotation = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotation.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotations.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(79);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(80);
					recog.base.match_token(Surrogate_id_SYMB_43,&mut recog.err_handler)?;

					recog.base.set_state(81);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(82);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					recog.base.set_state(91);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==StellaIdent {
						{
						/*InvokeRule paramDecl*/
						recog.base.set_state(83);
						let tmp = recog.paramDecl()?;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(88);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(84);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule paramDecl*/
							recog.base.set_state(85);
							let tmp = recog.paramDecl()?;
							if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(90);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(93);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					recog.base.set_state(96);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_8 {
						{
						recog.base.set_state(94);
						recog.base.match_token(Surrogate_id_SYMB_8,&mut recog.err_handler)?;

						/*InvokeRule stellatype*/
						recog.base.set_state(95);
						let tmp = recog.stellatype_rec(0)?;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.returnType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						}
					}

					recog.base.set_state(107);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_59 {
						{
						recog.base.set_state(98);
						recog.base.match_token(Surrogate_id_SYMB_59,&mut recog.err_handler)?;

						/*InvokeRule stellatype*/
						recog.base.set_state(99);
						let tmp = recog.stellatype_rec(0)?;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.throwTypes.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(104);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(100);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(101);
							let tmp = recog.stellatype_rec(0)?;
							if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.throwTypes.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(106);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(109);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(113);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while ((((_la - 44)) & !0x3f) == 0 && ((1usize << (_la - 44)) & ((1usize << (Surrogate_id_SYMB_43 - 44)) | (1usize << (Surrogate_id_SYMB_48 - 44)) | (1usize << (Surrogate_id_SYMB_61 - 44)) | (1usize << (EXCEPTION - 44)))) != 0) || _la==GENERIC {
						{
						{
						/*InvokeRule decl*/
						recog.base.set_state(110);
						let tmp = recog.decl()?;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.decl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.decl.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.localDecls.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(115);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(116);
					recog.base.match_token(Surrogate_id_SYMB_56,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(117);
					let tmp = recog.expr_rec(0)?;
					if let DeclContextAll::DeclFunContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.returnExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(118);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					let tmp = DeclFunGenericContextExt::new(&**_localctx);
					recog.base.enter_outer_alt(Some(tmp.clone()), 2);
					_localctx = tmp;
					{
					recog.base.set_state(123);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_48 {
						{
						{
						/*InvokeRule annotation*/
						recog.base.set_state(120);
						let tmp = recog.annotation()?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotation = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotation.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.annotations.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(125);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(126);
					recog.base.match_token(GENERIC,&mut recog.err_handler)?;

					recog.base.set_state(127);
					recog.base.match_token(Surrogate_id_SYMB_43,&mut recog.err_handler)?;

					recog.base.set_state(128);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(129);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					recog.base.set_state(130);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.StellaIdent = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.StellaIdent.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.generics.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(135);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(131);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						recog.base.set_state(132);
						let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.StellaIdent = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.StellaIdent.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.generics.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(137);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(138);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					recog.base.set_state(139);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					recog.base.set_state(148);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==StellaIdent {
						{
						/*InvokeRule paramDecl*/
						recog.base.set_state(140);
						let tmp = recog.paramDecl()?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(145);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(141);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule paramDecl*/
							recog.base.set_state(142);
							let tmp = recog.paramDecl()?;
							if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(147);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(150);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					recog.base.set_state(153);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_8 {
						{
						recog.base.set_state(151);
						recog.base.match_token(Surrogate_id_SYMB_8,&mut recog.err_handler)?;

						/*InvokeRule stellatype*/
						recog.base.set_state(152);
						let tmp = recog.stellatype_rec(0)?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.returnType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						}
					}

					recog.base.set_state(164);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_59 {
						{
						recog.base.set_state(155);
						recog.base.match_token(Surrogate_id_SYMB_59,&mut recog.err_handler)?;

						/*InvokeRule stellatype*/
						recog.base.set_state(156);
						let tmp = recog.stellatype_rec(0)?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.throwTypes.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(161);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(157);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(158);
							let tmp = recog.stellatype_rec(0)?;
							if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
							ctx.throwTypes.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(163);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(166);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(170);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while ((((_la - 44)) & !0x3f) == 0 && ((1usize << (_la - 44)) & ((1usize << (Surrogate_id_SYMB_43 - 44)) | (1usize << (Surrogate_id_SYMB_48 - 44)) | (1usize << (Surrogate_id_SYMB_61 - 44)) | (1usize << (EXCEPTION - 44)))) != 0) || _la==GENERIC {
						{
						{
						/*InvokeRule decl*/
						recog.base.set_state(167);
						let tmp = recog.decl()?;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.decl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.decl.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
						ctx.localDecls.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(172);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(173);
					recog.base.match_token(Surrogate_id_SYMB_56,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(174);
					let tmp = recog.expr_rec(0)?;
					if let DeclContextAll::DeclFunGenericContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.returnExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(175);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				3 =>{
					let tmp = DeclTypeAliasContextExt::new(&**_localctx);
					recog.base.enter_outer_alt(Some(tmp.clone()), 3);
					_localctx = tmp;
					{
					recog.base.set_state(177);
					recog.base.match_token(Surrogate_id_SYMB_61,&mut recog.err_handler)?;

					recog.base.set_state(178);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let DeclContextAll::DeclTypeAliasContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(179);
					recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(180);
					let tmp = recog.stellatype_rec(0)?;
					if let DeclContextAll::DeclTypeAliasContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.atype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				4 =>{
					let tmp = DeclExceptionTypeContextExt::new(&**_localctx);
					recog.base.enter_outer_alt(Some(tmp.clone()), 4);
					_localctx = tmp;
					{
					recog.base.set_state(181);
					recog.base.match_token(EXCEPTION,&mut recog.err_handler)?;

					recog.base.set_state(182);
					recog.base.match_token(Surrogate_id_SYMB_61,&mut recog.err_handler)?;

					recog.base.set_state(183);
					recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(184);
					let tmp = recog.stellatype_rec(0)?;
					if let DeclContextAll::DeclExceptionTypeContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.exceptionType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				5 =>{
					let tmp = DeclExceptionVariantContextExt::new(&**_localctx);
					recog.base.enter_outer_alt(Some(tmp.clone()), 5);
					_localctx = tmp;
					{
					recog.base.set_state(185);
					recog.base.match_token(EXCEPTION,&mut recog.err_handler)?;

					recog.base.set_state(186);
					recog.base.match_token(VARIANT,&mut recog.err_handler)?;

					recog.base.set_state(187);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let DeclContextAll::DeclExceptionVariantContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(188);
					recog.base.match_token(Surrogate_id_SYMB_7,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(189);
					let tmp = recog.stellatype_rec(0)?;
					if let DeclContextAll::DeclExceptionVariantContext(ctx) = cast_mut::<_,DeclContextAll >(&mut _localctx){
					ctx.variantType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}

				_ => {}
			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- annotation ----------------
#[derive(Debug)]
pub enum AnnotationContextAll<'input>{
	InlineAnnotationContext(InlineAnnotationContext<'input>),
Error(AnnotationContext<'input>)
}
antlr_rust::tid!{AnnotationContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for AnnotationContextAll<'input>{}

impl<'input> stellaParserContext<'input> for AnnotationContextAll<'input>{}

impl<'input> Deref for AnnotationContextAll<'input>{
	type Target = dyn AnnotationContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use AnnotationContextAll::*;
		match self{
			InlineAnnotationContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AnnotationContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type AnnotationContext<'input> = BaseParserRuleContext<'input,AnnotationContextExt<'input>>;

#[derive(Clone)]
pub struct AnnotationContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for AnnotationContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AnnotationContext<'input>{
}

impl<'input> CustomRuleContext<'input> for AnnotationContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_annotation }
	//fn type_rule_index() -> usize where Self: Sized { RULE_annotation }
}
antlr_rust::tid!{AnnotationContextExt<'a>}

impl<'input> AnnotationContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<AnnotationContextAll<'input>> {
		Rc::new(
		AnnotationContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,AnnotationContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait AnnotationContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<AnnotationContextExt<'input>>{


}

impl<'input> AnnotationContextAttrs<'input> for AnnotationContext<'input>{}

pub type InlineAnnotationContext<'input> = BaseParserRuleContext<'input,InlineAnnotationContextExt<'input>>;

pub trait InlineAnnotationContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_48
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_48
	fn Surrogate_id_SYMB_48(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_48, 0)
	}
}

impl<'input> InlineAnnotationContextAttrs<'input> for InlineAnnotationContext<'input>{}

pub struct InlineAnnotationContextExt<'input>{
	base:AnnotationContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{InlineAnnotationContextExt<'a>}

impl<'input> stellaParserContext<'input> for InlineAnnotationContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for InlineAnnotationContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_InlineAnnotation(self);
	}
}

impl<'input> CustomRuleContext<'input> for InlineAnnotationContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_annotation }
	//fn type_rule_index() -> usize where Self: Sized { RULE_annotation }
}

impl<'input> Borrow<AnnotationContextExt<'input>> for InlineAnnotationContext<'input>{
	fn borrow(&self) -> &AnnotationContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<AnnotationContextExt<'input>> for InlineAnnotationContext<'input>{
	fn borrow_mut(&mut self) -> &mut AnnotationContextExt<'input> { &mut self.base }
}

impl<'input> AnnotationContextAttrs<'input> for InlineAnnotationContext<'input> {}

impl<'input> InlineAnnotationContextExt<'input>{
	fn new(ctx: &dyn AnnotationContextAttrs<'input>) -> Rc<AnnotationContextAll<'input>>  {
		Rc::new(
			AnnotationContextAll::InlineAnnotationContext(
				BaseParserRuleContext::copy_from(ctx,InlineAnnotationContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn annotation(&mut self,)
	-> Result<Rc<AnnotationContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = AnnotationContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 14, RULE_annotation);
        let mut _localctx: Rc<AnnotationContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			let tmp = InlineAnnotationContextExt::new(&**_localctx);
			recog.base.enter_outer_alt(Some(tmp.clone()), 1);
			_localctx = tmp;
			{
			recog.base.set_state(192);
			recog.base.match_token(Surrogate_id_SYMB_48,&mut recog.err_handler)?;

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- paramDecl ----------------
pub type ParamDeclContextAll<'input> = ParamDeclContext<'input>;


pub type ParamDeclContext<'input> = BaseParserRuleContext<'input,ParamDeclContextExt<'input>>;

#[derive(Clone)]
pub struct ParamDeclContextExt<'input>{
	pub name: Option<TokenType<'input>>,
	pub paramType: Option<Rc<StellatypeContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for ParamDeclContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ParamDeclContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_paramDecl(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_paramDecl(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for ParamDeclContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_paramDecl }
	//fn type_rule_index() -> usize where Self: Sized { RULE_paramDecl }
}
antlr_rust::tid!{ParamDeclContextExt<'a>}

impl<'input> ParamDeclContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ParamDeclContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ParamDeclContextExt{
				name: None, 
				paramType: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait ParamDeclContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<ParamDeclContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_7
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_7
fn Surrogate_id_SYMB_7(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_7, 0)
}
/// Retrieves first TerminalNode corresponding to token StellaIdent
/// Returns `None` if there is no child corresponding to token StellaIdent
fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(StellaIdent, 0)
}
fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> ParamDeclContextAttrs<'input> for ParamDeclContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn paramDecl(&mut self,)
	-> Result<Rc<ParamDeclContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = ParamDeclContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 16, RULE_paramDecl);
        let mut _localctx: Rc<ParamDeclContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(194);
			let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
			 cast_mut::<_,ParamDeclContext >(&mut _localctx).name = Some(tmp.clone());
			  

			recog.base.set_state(195);
			recog.base.match_token(Surrogate_id_SYMB_7,&mut recog.err_handler)?;

			/*InvokeRule stellatype*/
			recog.base.set_state(196);
			let tmp = recog.stellatype_rec(0)?;
			 cast_mut::<_,ParamDeclContext >(&mut _localctx).paramType = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- expr ----------------
#[derive(Debug)]
pub enum ExprContextAll<'input>{
	FoldContext(FoldContext<'input>),
	AddContext(AddContext<'input>),
	IsZeroContext(IsZeroContext<'input>),
	VarContext(VarContext<'input>),
	TypeAbstractionContext(TypeAbstractionContext<'input>),
	DivideContext(DivideContext<'input>),
	LessThanContext(LessThanContext<'input>),
	DotRecordContext(DotRecordContext<'input>),
	GreaterThanContext(GreaterThanContext<'input>),
	EqualContext(EqualContext<'input>),
	ThrowContext(ThrowContext<'input>),
	MultiplyContext(MultiplyContext<'input>),
	ConstMemoryContext(ConstMemoryContext<'input>),
	ListContext(ListContext<'input>),
	TryCatchContext(TryCatchContext<'input>),
	TryCastAsContext(TryCastAsContext<'input>),
	HeadContext(HeadContext<'input>),
	TerminatingSemicolonContext(TerminatingSemicolonContext<'input>),
	NotEqualContext(NotEqualContext<'input>),
	ConstUnitContext(ConstUnitContext<'input>),
	SequenceContext(SequenceContext<'input>),
	ConstFalseContext(ConstFalseContext<'input>),
	AbstractionContext(AbstractionContext<'input>),
	ConstIntContext(ConstIntContext<'input>),
	VariantContext(VariantContext<'input>),
	ConstTrueContext(ConstTrueContext<'input>),
	SubtractContext(SubtractContext<'input>),
	TypeCastContext(TypeCastContext<'input>),
	IfContext(IfContext<'input>),
	ApplicationContext(ApplicationContext<'input>),
	DerefContext(DerefContext<'input>),
	IsEmptyContext(IsEmptyContext<'input>),
	PanicContext(PanicContext<'input>),
	LessThanOrEqualContext(LessThanOrEqualContext<'input>),
	SuccContext(SuccContext<'input>),
	InlContext(InlContext<'input>),
	GreaterThanOrEqualContext(GreaterThanOrEqualContext<'input>),
	InrContext(InrContext<'input>),
	MatchContext(MatchContext<'input>),
	LogicNotContext(LogicNotContext<'input>),
	ParenthesisedExprContext(ParenthesisedExprContext<'input>),
	TailContext(TailContext<'input>),
	RecordContext(RecordContext<'input>),
	LogicAndContext(LogicAndContext<'input>),
	TypeApplicationContext(TypeApplicationContext<'input>),
	LetRecContext(LetRecContext<'input>),
	LogicOrContext(LogicOrContext<'input>),
	TryWithContext(TryWithContext<'input>),
	PredContext(PredContext<'input>),
	TypeAscContext(TypeAscContext<'input>),
	NatRecContext(NatRecContext<'input>),
	UnfoldContext(UnfoldContext<'input>),
	RefContext(RefContext<'input>),
	DotTupleContext(DotTupleContext<'input>),
	FixContext(FixContext<'input>),
	LetContext(LetContext<'input>),
	AssignContext(AssignContext<'input>),
	TupleContext(TupleContext<'input>),
	ConsListContext(ConsListContext<'input>),
Error(ExprContext<'input>)
}
antlr_rust::tid!{ExprContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for ExprContextAll<'input>{}

impl<'input> stellaParserContext<'input> for ExprContextAll<'input>{}

impl<'input> Deref for ExprContextAll<'input>{
	type Target = dyn ExprContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use ExprContextAll::*;
		match self{
			FoldContext(inner) => inner,
			AddContext(inner) => inner,
			IsZeroContext(inner) => inner,
			VarContext(inner) => inner,
			TypeAbstractionContext(inner) => inner,
			DivideContext(inner) => inner,
			LessThanContext(inner) => inner,
			DotRecordContext(inner) => inner,
			GreaterThanContext(inner) => inner,
			EqualContext(inner) => inner,
			ThrowContext(inner) => inner,
			MultiplyContext(inner) => inner,
			ConstMemoryContext(inner) => inner,
			ListContext(inner) => inner,
			TryCatchContext(inner) => inner,
			TryCastAsContext(inner) => inner,
			HeadContext(inner) => inner,
			TerminatingSemicolonContext(inner) => inner,
			NotEqualContext(inner) => inner,
			ConstUnitContext(inner) => inner,
			SequenceContext(inner) => inner,
			ConstFalseContext(inner) => inner,
			AbstractionContext(inner) => inner,
			ConstIntContext(inner) => inner,
			VariantContext(inner) => inner,
			ConstTrueContext(inner) => inner,
			SubtractContext(inner) => inner,
			TypeCastContext(inner) => inner,
			IfContext(inner) => inner,
			ApplicationContext(inner) => inner,
			DerefContext(inner) => inner,
			IsEmptyContext(inner) => inner,
			PanicContext(inner) => inner,
			LessThanOrEqualContext(inner) => inner,
			SuccContext(inner) => inner,
			InlContext(inner) => inner,
			GreaterThanOrEqualContext(inner) => inner,
			InrContext(inner) => inner,
			MatchContext(inner) => inner,
			LogicNotContext(inner) => inner,
			ParenthesisedExprContext(inner) => inner,
			TailContext(inner) => inner,
			RecordContext(inner) => inner,
			LogicAndContext(inner) => inner,
			TypeApplicationContext(inner) => inner,
			LetRecContext(inner) => inner,
			LogicOrContext(inner) => inner,
			TryWithContext(inner) => inner,
			PredContext(inner) => inner,
			TypeAscContext(inner) => inner,
			NatRecContext(inner) => inner,
			UnfoldContext(inner) => inner,
			RefContext(inner) => inner,
			DotTupleContext(inner) => inner,
			FixContext(inner) => inner,
			LetContext(inner) => inner,
			AssignContext(inner) => inner,
			TupleContext(inner) => inner,
			ConsListContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ExprContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type ExprContext<'input> = BaseParserRuleContext<'input,ExprContextExt<'input>>;

#[derive(Clone)]
pub struct ExprContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for ExprContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ExprContext<'input>{
}

impl<'input> CustomRuleContext<'input> for ExprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}
antlr_rust::tid!{ExprContextExt<'a>}

impl<'input> ExprContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<ExprContextAll<'input>> {
		Rc::new(
		ExprContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,ExprContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait ExprContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<ExprContextExt<'input>>{


}

impl<'input> ExprContextAttrs<'input> for ExprContext<'input>{}

pub type FoldContext<'input> = BaseParserRuleContext<'input,FoldContextExt<'input>>;

pub trait FoldContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_44
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_44
	fn Surrogate_id_SYMB_44(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_44, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> FoldContextAttrs<'input> for FoldContext<'input>{}

pub struct FoldContextExt<'input>{
	base:ExprContextExt<'input>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{FoldContextExt<'a>}

impl<'input> stellaParserContext<'input> for FoldContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for FoldContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Fold(self);
	}
}

impl<'input> CustomRuleContext<'input> for FoldContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for FoldContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for FoldContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for FoldContext<'input> {}

impl<'input> FoldContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::FoldContext(
				BaseParserRuleContext::copy_from(ctx,FoldContextExt{
        			type_:None, expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type AddContext<'input> = BaseParserRuleContext<'input,AddContextExt<'input>>;

pub trait AddContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_21
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_21
	fn Surrogate_id_SYMB_21(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_21, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> AddContextAttrs<'input> for AddContext<'input>{}

pub struct AddContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{AddContextExt<'a>}

impl<'input> stellaParserContext<'input> for AddContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AddContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Add(self);
	}
}

impl<'input> CustomRuleContext<'input> for AddContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for AddContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for AddContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for AddContext<'input> {}

impl<'input> AddContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::AddContext(
				BaseParserRuleContext::copy_from(ctx,AddContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type IsZeroContext<'input> = BaseParserRuleContext<'input,IsZeroContextExt<'input>>;

pub trait IsZeroContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_30
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_30
	fn Surrogate_id_SYMB_30(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_30, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> IsZeroContextAttrs<'input> for IsZeroContext<'input>{}

pub struct IsZeroContextExt<'input>{
	base:ExprContextExt<'input>,
	pub n: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{IsZeroContextExt<'a>}

impl<'input> stellaParserContext<'input> for IsZeroContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for IsZeroContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_IsZero(self);
	}
}

impl<'input> CustomRuleContext<'input> for IsZeroContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for IsZeroContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for IsZeroContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for IsZeroContext<'input> {}

impl<'input> IsZeroContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::IsZeroContext(
				BaseParserRuleContext::copy_from(ctx,IsZeroContextExt{
        			n:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type VarContext<'input> = BaseParserRuleContext<'input,VarContextExt<'input>>;

pub trait VarContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
}

impl<'input> VarContextAttrs<'input> for VarContext<'input>{}

pub struct VarContextExt<'input>{
	base:ExprContextExt<'input>,
	pub name: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{VarContextExt<'a>}

impl<'input> stellaParserContext<'input> for VarContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for VarContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Var(self);
	}
}

impl<'input> CustomRuleContext<'input> for VarContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for VarContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for VarContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for VarContext<'input> {}

impl<'input> VarContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::VarContext(
				BaseParserRuleContext::copy_from(ctx,VarContextExt{
					name:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeAbstractionContext<'input> = BaseParserRuleContext<'input,TypeAbstractionContextExt<'input>>;

pub trait TypeAbstractionContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token GENERIC
	/// Returns `None` if there is no child corresponding to token GENERIC
	fn GENERIC(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(GENERIC, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token StellaIdent in current rule
	fn StellaIdent_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token StellaIdent, starting from 0.
	/// Returns `None` if number of children corresponding to token StellaIdent is less or equal than `i`.
	fn StellaIdent(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, i)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeAbstractionContextAttrs<'input> for TypeAbstractionContext<'input>{}

pub struct TypeAbstractionContextExt<'input>{
	base:ExprContextExt<'input>,
	pub StellaIdent: Option<TokenType<'input>>,
	pub generics:Vec<TokenType<'input>>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeAbstractionContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeAbstractionContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeAbstractionContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeAbstraction(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeAbstractionContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TypeAbstractionContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TypeAbstractionContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TypeAbstractionContext<'input> {}

impl<'input> TypeAbstractionContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TypeAbstractionContext(
				BaseParserRuleContext::copy_from(ctx,TypeAbstractionContextExt{
					StellaIdent:None, 
        			generics:Vec::new(), 
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DivideContext<'input> = BaseParserRuleContext<'input,DivideContextExt<'input>>;

pub trait DivideContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_24
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_24
	fn Surrogate_id_SYMB_24(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_24, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> DivideContextAttrs<'input> for DivideContext<'input>{}

pub struct DivideContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DivideContextExt<'a>}

impl<'input> stellaParserContext<'input> for DivideContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DivideContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Divide(self);
	}
}

impl<'input> CustomRuleContext<'input> for DivideContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for DivideContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for DivideContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for DivideContext<'input> {}

impl<'input> DivideContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::DivideContext(
				BaseParserRuleContext::copy_from(ctx,DivideContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LessThanContext<'input> = BaseParserRuleContext<'input,LessThanContextExt<'input>>;

pub trait LessThanContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_15
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_15
	fn Surrogate_id_SYMB_15(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_15, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> LessThanContextAttrs<'input> for LessThanContext<'input>{}

pub struct LessThanContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LessThanContextExt<'a>}

impl<'input> stellaParserContext<'input> for LessThanContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LessThanContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LessThan(self);
	}
}

impl<'input> CustomRuleContext<'input> for LessThanContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LessThanContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LessThanContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LessThanContext<'input> {}

impl<'input> LessThanContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LessThanContext(
				BaseParserRuleContext::copy_from(ctx,LessThanContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DotRecordContext<'input> = BaseParserRuleContext<'input,DotRecordContextExt<'input>>;

pub trait DotRecordContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_25
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_25
	fn Surrogate_id_SYMB_25(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_25, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
}

impl<'input> DotRecordContextAttrs<'input> for DotRecordContext<'input>{}

pub struct DotRecordContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub label: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DotRecordContextExt<'a>}

impl<'input> stellaParserContext<'input> for DotRecordContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DotRecordContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DotRecord(self);
	}
}

impl<'input> CustomRuleContext<'input> for DotRecordContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for DotRecordContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for DotRecordContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for DotRecordContext<'input> {}

impl<'input> DotRecordContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::DotRecordContext(
				BaseParserRuleContext::copy_from(ctx,DotRecordContextExt{
					label:None, 
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type GreaterThanContext<'input> = BaseParserRuleContext<'input,GreaterThanContextExt<'input>>;

pub trait GreaterThanContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_17
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_17
	fn Surrogate_id_SYMB_17(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_17, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> GreaterThanContextAttrs<'input> for GreaterThanContext<'input>{}

pub struct GreaterThanContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{GreaterThanContextExt<'a>}

impl<'input> stellaParserContext<'input> for GreaterThanContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for GreaterThanContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_GreaterThan(self);
	}
}

impl<'input> CustomRuleContext<'input> for GreaterThanContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for GreaterThanContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for GreaterThanContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for GreaterThanContext<'input> {}

impl<'input> GreaterThanContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::GreaterThanContext(
				BaseParserRuleContext::copy_from(ctx,GreaterThanContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type EqualContext<'input> = BaseParserRuleContext<'input,EqualContextExt<'input>>;

pub trait EqualContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_19
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_19
	fn Surrogate_id_SYMB_19(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_19, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> EqualContextAttrs<'input> for EqualContext<'input>{}

pub struct EqualContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{EqualContextExt<'a>}

impl<'input> stellaParserContext<'input> for EqualContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for EqualContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Equal(self);
	}
}

impl<'input> CustomRuleContext<'input> for EqualContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for EqualContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for EqualContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for EqualContext<'input> {}

impl<'input> EqualContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::EqualContext(
				BaseParserRuleContext::copy_from(ctx,EqualContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ThrowContext<'input> = BaseParserRuleContext<'input,ThrowContextExt<'input>>;

pub trait ThrowContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token THROW
	/// Returns `None` if there is no child corresponding to token THROW
	fn THROW(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(THROW, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> ThrowContextAttrs<'input> for ThrowContext<'input>{}

pub struct ThrowContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ThrowContextExt<'a>}

impl<'input> stellaParserContext<'input> for ThrowContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ThrowContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Throw(self);
	}
}

impl<'input> CustomRuleContext<'input> for ThrowContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ThrowContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ThrowContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ThrowContext<'input> {}

impl<'input> ThrowContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ThrowContext(
				BaseParserRuleContext::copy_from(ctx,ThrowContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type MultiplyContext<'input> = BaseParserRuleContext<'input,MultiplyContextExt<'input>>;

pub trait MultiplyContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_23
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_23
	fn Surrogate_id_SYMB_23(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_23, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> MultiplyContextAttrs<'input> for MultiplyContext<'input>{}

pub struct MultiplyContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{MultiplyContextExt<'a>}

impl<'input> stellaParserContext<'input> for MultiplyContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for MultiplyContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Multiply(self);
	}
}

impl<'input> CustomRuleContext<'input> for MultiplyContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for MultiplyContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for MultiplyContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for MultiplyContext<'input> {}

impl<'input> MultiplyContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::MultiplyContext(
				BaseParserRuleContext::copy_from(ctx,MultiplyContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConstMemoryContext<'input> = BaseParserRuleContext<'input,ConstMemoryContextExt<'input>>;

pub trait ConstMemoryContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token MemoryAddress
	/// Returns `None` if there is no child corresponding to token MemoryAddress
	fn MemoryAddress(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(MemoryAddress, 0)
	}
}

impl<'input> ConstMemoryContextAttrs<'input> for ConstMemoryContext<'input>{}

pub struct ConstMemoryContextExt<'input>{
	base:ExprContextExt<'input>,
	pub mem: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConstMemoryContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConstMemoryContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConstMemoryContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConstMemory(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConstMemoryContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConstMemoryContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConstMemoryContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConstMemoryContext<'input> {}

impl<'input> ConstMemoryContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConstMemoryContext(
				BaseParserRuleContext::copy_from(ctx,ConstMemoryContextExt{
					mem:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ListContext<'input> = BaseParserRuleContext<'input,ListContextExt<'input>>;

pub trait ListContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> ListContextAttrs<'input> for ListContext<'input>{}

pub struct ListContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr: Option<Rc<ExprContextAll<'input>>>,
	pub exprs:Vec<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ListContextExt<'a>}

impl<'input> stellaParserContext<'input> for ListContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ListContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_List(self);
	}
}

impl<'input> CustomRuleContext<'input> for ListContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ListContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ListContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ListContext<'input> {}

impl<'input> ListContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ListContext(
				BaseParserRuleContext::copy_from(ctx,ListContextExt{
        			expr:None, 
        			exprs:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TryCatchContext<'input> = BaseParserRuleContext<'input,TryCatchContextExt<'input>>;

pub trait TryCatchContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token TRY
	/// Returns `None` if there is no child corresponding to token TRY
	fn TRY(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(TRY, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_4 in current rule
	fn Surrogate_id_SYMB_4_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_4, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_4 is less or equal than `i`.
	fn Surrogate_id_SYMB_4(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_5 in current rule
	fn Surrogate_id_SYMB_5_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_5, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_5 is less or equal than `i`.
	fn Surrogate_id_SYMB_5(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, i)
	}
	/// Retrieves first TerminalNode corresponding to token CATCH
	/// Returns `None` if there is no child corresponding to token CATCH
	fn CATCH(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(CATCH, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_9
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_9
	fn Surrogate_id_SYMB_9(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_9, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TryCatchContextAttrs<'input> for TryCatchContext<'input>{}

pub struct TryCatchContextExt<'input>{
	base:ExprContextExt<'input>,
	pub tryExpr: Option<Rc<ExprContextAll<'input>>>,
	pub pat: Option<Rc<PatternContextAll<'input>>>,
	pub fallbackExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TryCatchContextExt<'a>}

impl<'input> stellaParserContext<'input> for TryCatchContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TryCatchContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TryCatch(self);
	}
}

impl<'input> CustomRuleContext<'input> for TryCatchContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TryCatchContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TryCatchContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TryCatchContext<'input> {}

impl<'input> TryCatchContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TryCatchContext(
				BaseParserRuleContext::copy_from(ctx,TryCatchContextExt{
        			tryExpr:None, pat:None, fallbackExpr:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TryCastAsContext<'input> = BaseParserRuleContext<'input,TryCastAsContextExt<'input>>;

pub trait TryCastAsContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token TRY
	/// Returns `None` if there is no child corresponding to token TRY
	fn TRY(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(TRY, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_4 in current rule
	fn Surrogate_id_SYMB_4_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_4, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_4 is less or equal than `i`.
	fn Surrogate_id_SYMB_4(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_5 in current rule
	fn Surrogate_id_SYMB_5_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_5, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_5 is less or equal than `i`.
	fn Surrogate_id_SYMB_5(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, i)
	}
	/// Retrieves first TerminalNode corresponding to token CAST
	/// Returns `None` if there is no child corresponding to token CAST
	fn CAST(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(CAST, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_36
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_36
	fn Surrogate_id_SYMB_36(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_36, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_9
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_9
	fn Surrogate_id_SYMB_9(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_9, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_64
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_64
	fn Surrogate_id_SYMB_64(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_64, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TryCastAsContextAttrs<'input> for TryCastAsContext<'input>{}

pub struct TryCastAsContextExt<'input>{
	base:ExprContextExt<'input>,
	pub tryExpr: Option<Rc<ExprContextAll<'input>>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub fallbackExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TryCastAsContextExt<'a>}

impl<'input> stellaParserContext<'input> for TryCastAsContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TryCastAsContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TryCastAs(self);
	}
}

impl<'input> CustomRuleContext<'input> for TryCastAsContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TryCastAsContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TryCastAsContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TryCastAsContext<'input> {}

impl<'input> TryCastAsContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TryCastAsContext(
				BaseParserRuleContext::copy_from(ctx,TryCastAsContextExt{
        			tryExpr:None, type_:None, pattern_:None, expr_:None, fallbackExpr:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type HeadContext<'input> = BaseParserRuleContext<'input,HeadContextExt<'input>>;

pub trait HeadContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_26
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_26
	fn Surrogate_id_SYMB_26(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_26, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> HeadContextAttrs<'input> for HeadContext<'input>{}

pub struct HeadContextExt<'input>{
	base:ExprContextExt<'input>,
	pub list: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{HeadContextExt<'a>}

impl<'input> stellaParserContext<'input> for HeadContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for HeadContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Head(self);
	}
}

impl<'input> CustomRuleContext<'input> for HeadContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for HeadContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for HeadContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for HeadContext<'input> {}

impl<'input> HeadContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::HeadContext(
				BaseParserRuleContext::copy_from(ctx,HeadContextExt{
        			list:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TerminatingSemicolonContext<'input> = BaseParserRuleContext<'input,TerminatingSemicolonContextExt<'input>>;

pub trait TerminatingSemicolonContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_1
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_1
	fn Surrogate_id_SYMB_1(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_1, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TerminatingSemicolonContextAttrs<'input> for TerminatingSemicolonContext<'input>{}

pub struct TerminatingSemicolonContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TerminatingSemicolonContextExt<'a>}

impl<'input> stellaParserContext<'input> for TerminatingSemicolonContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TerminatingSemicolonContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TerminatingSemicolon(self);
	}
}

impl<'input> CustomRuleContext<'input> for TerminatingSemicolonContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TerminatingSemicolonContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TerminatingSemicolonContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TerminatingSemicolonContext<'input> {}

impl<'input> TerminatingSemicolonContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TerminatingSemicolonContext(
				BaseParserRuleContext::copy_from(ctx,TerminatingSemicolonContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type NotEqualContext<'input> = BaseParserRuleContext<'input,NotEqualContextExt<'input>>;

pub trait NotEqualContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_20
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_20
	fn Surrogate_id_SYMB_20(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_20, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> NotEqualContextAttrs<'input> for NotEqualContext<'input>{}

pub struct NotEqualContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{NotEqualContextExt<'a>}

impl<'input> stellaParserContext<'input> for NotEqualContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for NotEqualContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_NotEqual(self);
	}
}

impl<'input> CustomRuleContext<'input> for NotEqualContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for NotEqualContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for NotEqualContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for NotEqualContext<'input> {}

impl<'input> NotEqualContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::NotEqualContext(
				BaseParserRuleContext::copy_from(ctx,NotEqualContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConstUnitContext<'input> = BaseParserRuleContext<'input,ConstUnitContextExt<'input>>;

pub trait ConstUnitContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_63
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_63
	fn Surrogate_id_SYMB_63(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_63, 0)
	}
}

impl<'input> ConstUnitContextAttrs<'input> for ConstUnitContext<'input>{}

pub struct ConstUnitContextExt<'input>{
	base:ExprContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConstUnitContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConstUnitContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConstUnitContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConstUnit(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConstUnitContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConstUnitContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConstUnitContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConstUnitContext<'input> {}

impl<'input> ConstUnitContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConstUnitContext(
				BaseParserRuleContext::copy_from(ctx,ConstUnitContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type SequenceContext<'input> = BaseParserRuleContext<'input,SequenceContextExt<'input>>;

pub trait SequenceContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_1
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_1
	fn Surrogate_id_SYMB_1(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_1, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> SequenceContextAttrs<'input> for SequenceContext<'input>{}

pub struct SequenceContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr1: Option<Rc<ExprContextAll<'input>>>,
	pub expr2: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{SequenceContextExt<'a>}

impl<'input> stellaParserContext<'input> for SequenceContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for SequenceContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Sequence(self);
	}
}

impl<'input> CustomRuleContext<'input> for SequenceContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for SequenceContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for SequenceContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for SequenceContext<'input> {}

impl<'input> SequenceContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::SequenceContext(
				BaseParserRuleContext::copy_from(ctx,SequenceContextExt{
        			expr1:None, expr2:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConstFalseContext<'input> = BaseParserRuleContext<'input,ConstFalseContextExt<'input>>;

pub trait ConstFalseContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_41
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_41
	fn Surrogate_id_SYMB_41(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_41, 0)
	}
}

impl<'input> ConstFalseContextAttrs<'input> for ConstFalseContext<'input>{}

pub struct ConstFalseContextExt<'input>{
	base:ExprContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConstFalseContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConstFalseContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConstFalseContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConstFalse(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConstFalseContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConstFalseContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConstFalseContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConstFalseContext<'input> {}

impl<'input> ConstFalseContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConstFalseContext(
				BaseParserRuleContext::copy_from(ctx,ConstFalseContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type AbstractionContext<'input> = BaseParserRuleContext<'input,AbstractionContextExt<'input>>;

pub trait AbstractionContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_43
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_43
	fn Surrogate_id_SYMB_43(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_43, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_56
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_56
	fn Surrogate_id_SYMB_56(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_56, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn paramDecl_all(&self) ->  Vec<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn paramDecl(&self, i: usize) -> Option<Rc<ParamDeclContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> AbstractionContextAttrs<'input> for AbstractionContext<'input>{}

pub struct AbstractionContextExt<'input>{
	base:ExprContextExt<'input>,
	pub paramDecl: Option<Rc<ParamDeclContextAll<'input>>>,
	pub paramDecls:Vec<Rc<ParamDeclContextAll<'input>>>,
	pub returnExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{AbstractionContextExt<'a>}

impl<'input> stellaParserContext<'input> for AbstractionContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AbstractionContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Abstraction(self);
	}
}

impl<'input> CustomRuleContext<'input> for AbstractionContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for AbstractionContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for AbstractionContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for AbstractionContext<'input> {}

impl<'input> AbstractionContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::AbstractionContext(
				BaseParserRuleContext::copy_from(ctx,AbstractionContextExt{
        			paramDecl:None, returnExpr:None, 
        			paramDecls:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConstIntContext<'input> = BaseParserRuleContext<'input,ConstIntContextExt<'input>>;

pub trait ConstIntContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token INTEGER
	/// Returns `None` if there is no child corresponding to token INTEGER
	fn INTEGER(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(INTEGER, 0)
	}
}

impl<'input> ConstIntContextAttrs<'input> for ConstIntContext<'input>{}

pub struct ConstIntContextExt<'input>{
	base:ExprContextExt<'input>,
	pub n: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConstIntContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConstIntContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConstIntContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConstInt(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConstIntContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConstIntContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConstIntContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConstIntContext<'input> {}

impl<'input> ConstIntContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConstIntContext(
				BaseParserRuleContext::copy_from(ctx,ConstIntContextExt{
					n:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type VariantContext<'input> = BaseParserRuleContext<'input,VariantContextExt<'input>>;

pub trait VariantContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_11
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_11
	fn Surrogate_id_SYMB_11(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_11, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_12
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_12
	fn Surrogate_id_SYMB_12(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_12, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
	fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_6, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> VariantContextAttrs<'input> for VariantContext<'input>{}

pub struct VariantContextExt<'input>{
	base:ExprContextExt<'input>,
	pub label: Option<TokenType<'input>>,
	pub rhs: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{VariantContextExt<'a>}

impl<'input> stellaParserContext<'input> for VariantContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for VariantContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Variant(self);
	}
}

impl<'input> CustomRuleContext<'input> for VariantContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for VariantContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for VariantContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for VariantContext<'input> {}

impl<'input> VariantContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::VariantContext(
				BaseParserRuleContext::copy_from(ctx,VariantContextExt{
					label:None, 
        			rhs:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConstTrueContext<'input> = BaseParserRuleContext<'input,ConstTrueContextExt<'input>>;

pub trait ConstTrueContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_60
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_60
	fn Surrogate_id_SYMB_60(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_60, 0)
	}
}

impl<'input> ConstTrueContextAttrs<'input> for ConstTrueContext<'input>{}

pub struct ConstTrueContextExt<'input>{
	base:ExprContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConstTrueContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConstTrueContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConstTrueContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConstTrue(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConstTrueContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConstTrueContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConstTrueContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConstTrueContext<'input> {}

impl<'input> ConstTrueContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConstTrueContext(
				BaseParserRuleContext::copy_from(ctx,ConstTrueContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type SubtractContext<'input> = BaseParserRuleContext<'input,SubtractContextExt<'input>>;

pub trait SubtractContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_22
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_22
	fn Surrogate_id_SYMB_22(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_22, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> SubtractContextAttrs<'input> for SubtractContext<'input>{}

pub struct SubtractContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{SubtractContextExt<'a>}

impl<'input> stellaParserContext<'input> for SubtractContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for SubtractContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Subtract(self);
	}
}

impl<'input> CustomRuleContext<'input> for SubtractContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for SubtractContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for SubtractContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for SubtractContext<'input> {}

impl<'input> SubtractContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::SubtractContext(
				BaseParserRuleContext::copy_from(ctx,SubtractContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeCastContext<'input> = BaseParserRuleContext<'input,TypeCastContextExt<'input>>;

pub trait TypeCastContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token CAST
	/// Returns `None` if there is no child corresponding to token CAST
	fn CAST(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(CAST, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_36
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_36
	fn Surrogate_id_SYMB_36(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_36, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeCastContextAttrs<'input> for TypeCastContext<'input>{}

pub struct TypeCastContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeCastContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeCastContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeCastContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeCast(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeCastContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TypeCastContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TypeCastContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TypeCastContext<'input> {}

impl<'input> TypeCastContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TypeCastContext(
				BaseParserRuleContext::copy_from(ctx,TypeCastContextExt{
        			expr_:None, type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type IfContext<'input> = BaseParserRuleContext<'input,IfContextExt<'input>>;

pub trait IfContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_45
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_45
	fn Surrogate_id_SYMB_45(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_45, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_58
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_58
	fn Surrogate_id_SYMB_58(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_58, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_39
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_39
	fn Surrogate_id_SYMB_39(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_39, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> IfContextAttrs<'input> for IfContext<'input>{}

pub struct IfContextExt<'input>{
	base:ExprContextExt<'input>,
	pub condition: Option<Rc<ExprContextAll<'input>>>,
	pub thenExpr: Option<Rc<ExprContextAll<'input>>>,
	pub elseExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{IfContextExt<'a>}

impl<'input> stellaParserContext<'input> for IfContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for IfContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_If(self);
	}
}

impl<'input> CustomRuleContext<'input> for IfContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for IfContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for IfContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for IfContext<'input> {}

impl<'input> IfContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::IfContext(
				BaseParserRuleContext::copy_from(ctx,IfContextExt{
        			condition:None, thenExpr:None, elseExpr:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ApplicationContext<'input> = BaseParserRuleContext<'input,ApplicationContextExt<'input>>;

pub trait ApplicationContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> ApplicationContextAttrs<'input> for ApplicationContext<'input>{}

pub struct ApplicationContextExt<'input>{
	base:ExprContextExt<'input>,
	pub fun: Option<Rc<ExprContextAll<'input>>>,
	pub expr: Option<Rc<ExprContextAll<'input>>>,
	pub args:Vec<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ApplicationContextExt<'a>}

impl<'input> stellaParserContext<'input> for ApplicationContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ApplicationContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Application(self);
	}
}

impl<'input> CustomRuleContext<'input> for ApplicationContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ApplicationContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ApplicationContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ApplicationContext<'input> {}

impl<'input> ApplicationContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ApplicationContext(
				BaseParserRuleContext::copy_from(ctx,ApplicationContextExt{
        			fun:None, expr:None, 
        			args:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DerefContext<'input> = BaseParserRuleContext<'input,DerefContextExt<'input>>;

pub trait DerefContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_23
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_23
	fn Surrogate_id_SYMB_23(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_23, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> DerefContextAttrs<'input> for DerefContext<'input>{}

pub struct DerefContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DerefContextExt<'a>}

impl<'input> stellaParserContext<'input> for DerefContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DerefContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Deref(self);
	}
}

impl<'input> CustomRuleContext<'input> for DerefContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for DerefContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for DerefContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for DerefContext<'input> {}

impl<'input> DerefContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::DerefContext(
				BaseParserRuleContext::copy_from(ctx,DerefContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type IsEmptyContext<'input> = BaseParserRuleContext<'input,IsEmptyContextExt<'input>>;

pub trait IsEmptyContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_27
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_27
	fn Surrogate_id_SYMB_27(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_27, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> IsEmptyContextAttrs<'input> for IsEmptyContext<'input>{}

pub struct IsEmptyContextExt<'input>{
	base:ExprContextExt<'input>,
	pub list: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{IsEmptyContextExt<'a>}

impl<'input> stellaParserContext<'input> for IsEmptyContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for IsEmptyContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_IsEmpty(self);
	}
}

impl<'input> CustomRuleContext<'input> for IsEmptyContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for IsEmptyContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for IsEmptyContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for IsEmptyContext<'input> {}

impl<'input> IsEmptyContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::IsEmptyContext(
				BaseParserRuleContext::copy_from(ctx,IsEmptyContextExt{
        			list:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PanicContext<'input> = BaseParserRuleContext<'input,PanicContextExt<'input>>;

pub trait PanicContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token PANIC
	/// Returns `None` if there is no child corresponding to token PANIC
	fn PANIC(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(PANIC, 0)
	}
}

impl<'input> PanicContextAttrs<'input> for PanicContext<'input>{}

pub struct PanicContextExt<'input>{
	base:ExprContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PanicContextExt<'a>}

impl<'input> stellaParserContext<'input> for PanicContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PanicContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Panic(self);
	}
}

impl<'input> CustomRuleContext<'input> for PanicContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for PanicContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for PanicContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for PanicContext<'input> {}

impl<'input> PanicContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::PanicContext(
				BaseParserRuleContext::copy_from(ctx,PanicContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LessThanOrEqualContext<'input> = BaseParserRuleContext<'input,LessThanOrEqualContextExt<'input>>;

pub trait LessThanOrEqualContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_16
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_16
	fn Surrogate_id_SYMB_16(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_16, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> LessThanOrEqualContextAttrs<'input> for LessThanOrEqualContext<'input>{}

pub struct LessThanOrEqualContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LessThanOrEqualContextExt<'a>}

impl<'input> stellaParserContext<'input> for LessThanOrEqualContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LessThanOrEqualContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LessThanOrEqual(self);
	}
}

impl<'input> CustomRuleContext<'input> for LessThanOrEqualContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LessThanOrEqualContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LessThanOrEqualContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LessThanOrEqualContext<'input> {}

impl<'input> LessThanOrEqualContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LessThanOrEqualContext(
				BaseParserRuleContext::copy_from(ctx,LessThanOrEqualContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type SuccContext<'input> = BaseParserRuleContext<'input,SuccContextExt<'input>>;

pub trait SuccContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_57
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_57
	fn Surrogate_id_SYMB_57(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_57, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> SuccContextAttrs<'input> for SuccContext<'input>{}

pub struct SuccContextExt<'input>{
	base:ExprContextExt<'input>,
	pub n: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{SuccContextExt<'a>}

impl<'input> stellaParserContext<'input> for SuccContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for SuccContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Succ(self);
	}
}

impl<'input> CustomRuleContext<'input> for SuccContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for SuccContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for SuccContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for SuccContext<'input> {}

impl<'input> SuccContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::SuccContext(
				BaseParserRuleContext::copy_from(ctx,SuccContextExt{
        			n:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type InlContext<'input> = BaseParserRuleContext<'input,InlContextExt<'input>>;

pub trait InlContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_47
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_47
	fn Surrogate_id_SYMB_47(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_47, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> InlContextAttrs<'input> for InlContext<'input>{}

pub struct InlContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{InlContextExt<'a>}

impl<'input> stellaParserContext<'input> for InlContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for InlContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Inl(self);
	}
}

impl<'input> CustomRuleContext<'input> for InlContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for InlContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for InlContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for InlContext<'input> {}

impl<'input> InlContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::InlContext(
				BaseParserRuleContext::copy_from(ctx,InlContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type GreaterThanOrEqualContext<'input> = BaseParserRuleContext<'input,GreaterThanOrEqualContextExt<'input>>;

pub trait GreaterThanOrEqualContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_18
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_18
	fn Surrogate_id_SYMB_18(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_18, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> GreaterThanOrEqualContextAttrs<'input> for GreaterThanOrEqualContext<'input>{}

pub struct GreaterThanOrEqualContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{GreaterThanOrEqualContextExt<'a>}

impl<'input> stellaParserContext<'input> for GreaterThanOrEqualContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for GreaterThanOrEqualContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_GreaterThanOrEqual(self);
	}
}

impl<'input> CustomRuleContext<'input> for GreaterThanOrEqualContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for GreaterThanOrEqualContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for GreaterThanOrEqualContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for GreaterThanOrEqualContext<'input> {}

impl<'input> GreaterThanOrEqualContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::GreaterThanOrEqualContext(
				BaseParserRuleContext::copy_from(ctx,GreaterThanOrEqualContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type InrContext<'input> = BaseParserRuleContext<'input,InrContextExt<'input>>;

pub trait InrContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_49
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_49
	fn Surrogate_id_SYMB_49(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_49, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> InrContextAttrs<'input> for InrContext<'input>{}

pub struct InrContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{InrContextExt<'a>}

impl<'input> stellaParserContext<'input> for InrContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for InrContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Inr(self);
	}
}

impl<'input> CustomRuleContext<'input> for InrContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for InrContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for InrContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for InrContext<'input> {}

impl<'input> InrContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::InrContext(
				BaseParserRuleContext::copy_from(ctx,InrContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type MatchContext<'input> = BaseParserRuleContext<'input,MatchContextExt<'input>>;

pub trait MatchContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_53
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_53
	fn Surrogate_id_SYMB_53(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_53, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn matchCase_all(&self) ->  Vec<Rc<MatchCaseContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn matchCase(&self, i: usize) -> Option<Rc<MatchCaseContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_10 in current rule
	fn Surrogate_id_SYMB_10_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_10, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_10 is less or equal than `i`.
	fn Surrogate_id_SYMB_10(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_10, i)
	}
}

impl<'input> MatchContextAttrs<'input> for MatchContext<'input>{}

pub struct MatchContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub matchCase: Option<Rc<MatchCaseContextAll<'input>>>,
	pub cases:Vec<Rc<MatchCaseContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{MatchContextExt<'a>}

impl<'input> stellaParserContext<'input> for MatchContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for MatchContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Match(self);
	}
}

impl<'input> CustomRuleContext<'input> for MatchContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for MatchContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for MatchContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for MatchContext<'input> {}

impl<'input> MatchContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::MatchContext(
				BaseParserRuleContext::copy_from(ctx,MatchContextExt{
        			expr_:None, matchCase:None, 
        			cases:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LogicNotContext<'input> = BaseParserRuleContext<'input,LogicNotContextExt<'input>>;

pub trait LogicNotContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_54
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_54
	fn Surrogate_id_SYMB_54(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_54, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> LogicNotContextAttrs<'input> for LogicNotContext<'input>{}

pub struct LogicNotContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LogicNotContextExt<'a>}

impl<'input> stellaParserContext<'input> for LogicNotContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LogicNotContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LogicNot(self);
	}
}

impl<'input> CustomRuleContext<'input> for LogicNotContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LogicNotContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LogicNotContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LogicNotContext<'input> {}

impl<'input> LogicNotContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LogicNotContext(
				BaseParserRuleContext::copy_from(ctx,LogicNotContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ParenthesisedExprContext<'input> = BaseParserRuleContext<'input,ParenthesisedExprContextExt<'input>>;

pub trait ParenthesisedExprContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> ParenthesisedExprContextAttrs<'input> for ParenthesisedExprContext<'input>{}

pub struct ParenthesisedExprContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ParenthesisedExprContextExt<'a>}

impl<'input> stellaParserContext<'input> for ParenthesisedExprContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ParenthesisedExprContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ParenthesisedExpr(self);
	}
}

impl<'input> CustomRuleContext<'input> for ParenthesisedExprContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ParenthesisedExprContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ParenthesisedExprContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ParenthesisedExprContext<'input> {}

impl<'input> ParenthesisedExprContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ParenthesisedExprContext(
				BaseParserRuleContext::copy_from(ctx,ParenthesisedExprContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TailContext<'input> = BaseParserRuleContext<'input,TailContextExt<'input>>;

pub trait TailContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_28
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_28
	fn Surrogate_id_SYMB_28(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_28, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TailContextAttrs<'input> for TailContext<'input>{}

pub struct TailContextExt<'input>{
	base:ExprContextExt<'input>,
	pub list: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TailContextExt<'a>}

impl<'input> stellaParserContext<'input> for TailContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TailContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Tail(self);
	}
}

impl<'input> CustomRuleContext<'input> for TailContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TailContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TailContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TailContext<'input> {}

impl<'input> TailContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TailContext(
				BaseParserRuleContext::copy_from(ctx,TailContextExt{
        			list:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type RecordContext<'input> = BaseParserRuleContext<'input,RecordContextExt<'input>>;

pub trait RecordContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn binding_all(&self) ->  Vec<Rc<BindingContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn binding(&self, i: usize) -> Option<Rc<BindingContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> RecordContextAttrs<'input> for RecordContext<'input>{}

pub struct RecordContextExt<'input>{
	base:ExprContextExt<'input>,
	pub binding: Option<Rc<BindingContextAll<'input>>>,
	pub bindings:Vec<Rc<BindingContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{RecordContextExt<'a>}

impl<'input> stellaParserContext<'input> for RecordContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for RecordContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Record(self);
	}
}

impl<'input> CustomRuleContext<'input> for RecordContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for RecordContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for RecordContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for RecordContext<'input> {}

impl<'input> RecordContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::RecordContext(
				BaseParserRuleContext::copy_from(ctx,RecordContextExt{
        			binding:None, 
        			bindings:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LogicAndContext<'input> = BaseParserRuleContext<'input,LogicAndContextExt<'input>>;

pub trait LogicAndContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_35
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_35
	fn Surrogate_id_SYMB_35(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_35, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> LogicAndContextAttrs<'input> for LogicAndContext<'input>{}

pub struct LogicAndContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LogicAndContextExt<'a>}

impl<'input> stellaParserContext<'input> for LogicAndContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LogicAndContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LogicAnd(self);
	}
}

impl<'input> CustomRuleContext<'input> for LogicAndContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LogicAndContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LogicAndContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LogicAndContext<'input> {}

impl<'input> LogicAndContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LogicAndContext(
				BaseParserRuleContext::copy_from(ctx,LogicAndContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeApplicationContext<'input> = BaseParserRuleContext<'input,TypeApplicationContextExt<'input>>;

pub trait TypeApplicationContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeApplicationContextAttrs<'input> for TypeApplicationContext<'input>{}

pub struct TypeApplicationContextExt<'input>{
	base:ExprContextExt<'input>,
	pub fun: Option<Rc<ExprContextAll<'input>>>,
	pub stellatype: Option<Rc<StellatypeContextAll<'input>>>,
	pub types:Vec<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeApplicationContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeApplicationContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeApplicationContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeApplication(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeApplicationContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TypeApplicationContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TypeApplicationContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TypeApplicationContext<'input> {}

impl<'input> TypeApplicationContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TypeApplicationContext(
				BaseParserRuleContext::copy_from(ctx,TypeApplicationContextExt{
        			fun:None, stellatype:None, 
        			types:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LetRecContext<'input> = BaseParserRuleContext<'input,LetRecContextExt<'input>>;

pub trait LetRecContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_52
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_52
	fn Surrogate_id_SYMB_52(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_52, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_46
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_46
	fn Surrogate_id_SYMB_46(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_46, 0)
	}
	fn patternBinding_all(&self) ->  Vec<Rc<PatternBindingContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn patternBinding(&self, i: usize) -> Option<Rc<PatternBindingContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> LetRecContextAttrs<'input> for LetRecContext<'input>{}

pub struct LetRecContextExt<'input>{
	base:ExprContextExt<'input>,
	pub patternBinding: Option<Rc<PatternBindingContextAll<'input>>>,
	pub patternBindings:Vec<Rc<PatternBindingContextAll<'input>>>,
	pub body: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LetRecContextExt<'a>}

impl<'input> stellaParserContext<'input> for LetRecContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LetRecContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LetRec(self);
	}
}

impl<'input> CustomRuleContext<'input> for LetRecContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LetRecContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LetRecContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LetRecContext<'input> {}

impl<'input> LetRecContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LetRecContext(
				BaseParserRuleContext::copy_from(ctx,LetRecContextExt{
        			patternBinding:None, body:None, 
        			patternBindings:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LogicOrContext<'input> = BaseParserRuleContext<'input,LogicOrContextExt<'input>>;

pub trait LogicOrContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_55
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_55
	fn Surrogate_id_SYMB_55(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_55, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> LogicOrContextAttrs<'input> for LogicOrContext<'input>{}

pub struct LogicOrContextExt<'input>{
	base:ExprContextExt<'input>,
	pub left: Option<Rc<ExprContextAll<'input>>>,
	pub right: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LogicOrContextExt<'a>}

impl<'input> stellaParserContext<'input> for LogicOrContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LogicOrContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_LogicOr(self);
	}
}

impl<'input> CustomRuleContext<'input> for LogicOrContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LogicOrContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LogicOrContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LogicOrContext<'input> {}

impl<'input> LogicOrContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LogicOrContext(
				BaseParserRuleContext::copy_from(ctx,LogicOrContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TryWithContext<'input> = BaseParserRuleContext<'input,TryWithContextExt<'input>>;

pub trait TryWithContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token TRY
	/// Returns `None` if there is no child corresponding to token TRY
	fn TRY(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(TRY, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_4 in current rule
	fn Surrogate_id_SYMB_4_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_4, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_4 is less or equal than `i`.
	fn Surrogate_id_SYMB_4(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_5 in current rule
	fn Surrogate_id_SYMB_5_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_5, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_5 is less or equal than `i`.
	fn Surrogate_id_SYMB_5(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, i)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_64
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_64
	fn Surrogate_id_SYMB_64(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_64, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> TryWithContextAttrs<'input> for TryWithContext<'input>{}

pub struct TryWithContextExt<'input>{
	base:ExprContextExt<'input>,
	pub tryExpr: Option<Rc<ExprContextAll<'input>>>,
	pub fallbackExpr: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TryWithContextExt<'a>}

impl<'input> stellaParserContext<'input> for TryWithContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TryWithContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TryWith(self);
	}
}

impl<'input> CustomRuleContext<'input> for TryWithContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TryWithContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TryWithContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TryWithContext<'input> {}

impl<'input> TryWithContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TryWithContext(
				BaseParserRuleContext::copy_from(ctx,TryWithContextExt{
        			tryExpr:None, fallbackExpr:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PredContext<'input> = BaseParserRuleContext<'input,PredContextExt<'input>>;

pub trait PredContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_29
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_29
	fn Surrogate_id_SYMB_29(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_29, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PredContextAttrs<'input> for PredContext<'input>{}

pub struct PredContextExt<'input>{
	base:ExprContextExt<'input>,
	pub n: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PredContextExt<'a>}

impl<'input> stellaParserContext<'input> for PredContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PredContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Pred(self);
	}
}

impl<'input> CustomRuleContext<'input> for PredContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for PredContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for PredContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for PredContext<'input> {}

impl<'input> PredContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::PredContext(
				BaseParserRuleContext::copy_from(ctx,PredContextExt{
        			n:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeAscContext<'input> = BaseParserRuleContext<'input,TypeAscContextExt<'input>>;

pub trait TypeAscContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_36
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_36
	fn Surrogate_id_SYMB_36(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_36, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeAscContextAttrs<'input> for TypeAscContext<'input>{}

pub struct TypeAscContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeAscContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeAscContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeAscContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeAsc(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeAscContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TypeAscContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TypeAscContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TypeAscContext<'input> {}

impl<'input> TypeAscContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TypeAscContext(
				BaseParserRuleContext::copy_from(ctx,TypeAscContextExt{
        			expr_:None, type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type NatRecContext<'input> = BaseParserRuleContext<'input,NatRecContextExt<'input>>;

pub trait NatRecContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_31
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_31
	fn Surrogate_id_SYMB_31(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_31, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> NatRecContextAttrs<'input> for NatRecContext<'input>{}

pub struct NatRecContextExt<'input>{
	base:ExprContextExt<'input>,
	pub n: Option<Rc<ExprContextAll<'input>>>,
	pub initial: Option<Rc<ExprContextAll<'input>>>,
	pub step: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{NatRecContextExt<'a>}

impl<'input> stellaParserContext<'input> for NatRecContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for NatRecContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_NatRec(self);
	}
}

impl<'input> CustomRuleContext<'input> for NatRecContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for NatRecContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for NatRecContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for NatRecContext<'input> {}

impl<'input> NatRecContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::NatRecContext(
				BaseParserRuleContext::copy_from(ctx,NatRecContextExt{
        			n:None, initial:None, step:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type UnfoldContext<'input> = BaseParserRuleContext<'input,UnfoldContextExt<'input>>;

pub trait UnfoldContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_62
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_62
	fn Surrogate_id_SYMB_62(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_62, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> UnfoldContextAttrs<'input> for UnfoldContext<'input>{}

pub struct UnfoldContextExt<'input>{
	base:ExprContextExt<'input>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{UnfoldContextExt<'a>}

impl<'input> stellaParserContext<'input> for UnfoldContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for UnfoldContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Unfold(self);
	}
}

impl<'input> CustomRuleContext<'input> for UnfoldContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for UnfoldContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for UnfoldContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for UnfoldContext<'input> {}

impl<'input> UnfoldContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::UnfoldContext(
				BaseParserRuleContext::copy_from(ctx,UnfoldContextExt{
        			type_:None, expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type RefContext<'input> = BaseParserRuleContext<'input,RefContextExt<'input>>;

pub trait RefContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token REFERENCE
	/// Returns `None` if there is no child corresponding to token REFERENCE
	fn REFERENCE(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(REFERENCE, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> RefContextAttrs<'input> for RefContext<'input>{}

pub struct RefContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{RefContextExt<'a>}

impl<'input> stellaParserContext<'input> for RefContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for RefContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Ref(self);
	}
}

impl<'input> CustomRuleContext<'input> for RefContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for RefContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for RefContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for RefContext<'input> {}

impl<'input> RefContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::RefContext(
				BaseParserRuleContext::copy_from(ctx,RefContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type DotTupleContext<'input> = BaseParserRuleContext<'input,DotTupleContextExt<'input>>;

pub trait DotTupleContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_25
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_25
	fn Surrogate_id_SYMB_25(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_25, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves first TerminalNode corresponding to token INTEGER
	/// Returns `None` if there is no child corresponding to token INTEGER
	fn INTEGER(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(INTEGER, 0)
	}
}

impl<'input> DotTupleContextAttrs<'input> for DotTupleContext<'input>{}

pub struct DotTupleContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	pub index: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{DotTupleContextExt<'a>}

impl<'input> stellaParserContext<'input> for DotTupleContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for DotTupleContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_DotTuple(self);
	}
}

impl<'input> CustomRuleContext<'input> for DotTupleContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for DotTupleContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for DotTupleContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for DotTupleContext<'input> {}

impl<'input> DotTupleContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::DotTupleContext(
				BaseParserRuleContext::copy_from(ctx,DotTupleContextExt{
					index:None, 
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type FixContext<'input> = BaseParserRuleContext<'input,FixContextExt<'input>>;

pub trait FixContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_42
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_42
	fn Surrogate_id_SYMB_42(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_42, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> FixContextAttrs<'input> for FixContext<'input>{}

pub struct FixContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{FixContextExt<'a>}

impl<'input> stellaParserContext<'input> for FixContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for FixContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Fix(self);
	}
}

impl<'input> CustomRuleContext<'input> for FixContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for FixContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for FixContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for FixContext<'input> {}

impl<'input> FixContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::FixContext(
				BaseParserRuleContext::copy_from(ctx,FixContextExt{
        			expr_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type LetContext<'input> = BaseParserRuleContext<'input,LetContextExt<'input>>;

pub trait LetContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_51
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_51
	fn Surrogate_id_SYMB_51(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_51, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_46
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_46
	fn Surrogate_id_SYMB_46(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_46, 0)
	}
	fn patternBinding_all(&self) ->  Vec<Rc<PatternBindingContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn patternBinding(&self, i: usize) -> Option<Rc<PatternBindingContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> LetContextAttrs<'input> for LetContext<'input>{}

pub struct LetContextExt<'input>{
	base:ExprContextExt<'input>,
	pub patternBinding: Option<Rc<PatternBindingContextAll<'input>>>,
	pub patternBindings:Vec<Rc<PatternBindingContextAll<'input>>>,
	pub body: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{LetContextExt<'a>}

impl<'input> stellaParserContext<'input> for LetContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LetContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Let(self);
	}
}

impl<'input> CustomRuleContext<'input> for LetContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for LetContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for LetContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for LetContext<'input> {}

impl<'input> LetContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::LetContext(
				BaseParserRuleContext::copy_from(ctx,LetContextExt{
        			patternBinding:None, body:None, 
        			patternBindings:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type AssignContext<'input> = BaseParserRuleContext<'input,AssignContextExt<'input>>;

pub trait AssignContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token ASSIGN
	/// Returns `None` if there is no child corresponding to token ASSIGN
	fn ASSIGN(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(ASSIGN, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> AssignContextAttrs<'input> for AssignContext<'input>{}

pub struct AssignContextExt<'input>{
	base:ExprContextExt<'input>,
	pub lhs: Option<Rc<ExprContextAll<'input>>>,
	pub rhs: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{AssignContextExt<'a>}

impl<'input> stellaParserContext<'input> for AssignContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for AssignContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Assign(self);
	}
}

impl<'input> CustomRuleContext<'input> for AssignContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for AssignContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for AssignContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for AssignContext<'input> {}

impl<'input> AssignContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::AssignContext(
				BaseParserRuleContext::copy_from(ctx,AssignContextExt{
        			lhs:None, rhs:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TupleContext<'input> = BaseParserRuleContext<'input,TupleContextExt<'input>>;

pub trait TupleContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TupleContextAttrs<'input> for TupleContext<'input>{}

pub struct TupleContextExt<'input>{
	base:ExprContextExt<'input>,
	pub expr: Option<Rc<ExprContextAll<'input>>>,
	pub exprs:Vec<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TupleContextExt<'a>}

impl<'input> stellaParserContext<'input> for TupleContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TupleContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_Tuple(self);
	}
}

impl<'input> CustomRuleContext<'input> for TupleContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for TupleContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for TupleContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for TupleContext<'input> {}

impl<'input> TupleContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::TupleContext(
				BaseParserRuleContext::copy_from(ctx,TupleContextExt{
        			expr:None, 
        			exprs:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ConsListContext<'input> = BaseParserRuleContext<'input,ConsListContextExt<'input>>;

pub trait ConsListContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_37
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_37
	fn Surrogate_id_SYMB_37(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_37, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_0
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_0
	fn Surrogate_id_SYMB_0(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn expr_all(&self) ->  Vec<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn expr(&self, i: usize) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> ConsListContextAttrs<'input> for ConsListContext<'input>{}

pub struct ConsListContextExt<'input>{
	base:ExprContextExt<'input>,
	pub head: Option<Rc<ExprContextAll<'input>>>,
	pub tail: Option<Rc<ExprContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ConsListContextExt<'a>}

impl<'input> stellaParserContext<'input> for ConsListContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ConsListContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ConsList(self);
	}
}

impl<'input> CustomRuleContext<'input> for ConsListContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_expr }
	//fn type_rule_index() -> usize where Self: Sized { RULE_expr }
}

impl<'input> Borrow<ExprContextExt<'input>> for ConsListContext<'input>{
	fn borrow(&self) -> &ExprContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<ExprContextExt<'input>> for ConsListContext<'input>{
	fn borrow_mut(&mut self) -> &mut ExprContextExt<'input> { &mut self.base }
}

impl<'input> ExprContextAttrs<'input> for ConsListContext<'input> {}

impl<'input> ConsListContextExt<'input>{
	fn new(ctx: &dyn ExprContextAttrs<'input>) -> Rc<ExprContextAll<'input>>  {
		Rc::new(
			ExprContextAll::ConsListContext(
				BaseParserRuleContext::copy_from(ctx,ConsListContextExt{
        			head:None, tail:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn  expr(&mut self,)
	-> Result<Rc<ExprContextAll<'input>>,ANTLRError> {
		self.expr_rec(0)
	}

	fn expr_rec(&mut self, _p: isize)
	-> Result<Rc<ExprContextAll<'input>>,ANTLRError> {
		let recog = self;
		let _parentctx = recog.ctx.take();
		let _parentState = recog.base.get_state();
		let mut _localctx = ExprContextExt::new(_parentctx.clone(), recog.base.get_state());
		recog.base.enter_recursion_rule(_localctx.clone(), 18, RULE_expr, _p);
	    let mut _localctx: Rc<ExprContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
		let _startState = 18;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {
			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(452);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(32,&mut recog.base)? {
				1 =>{
					{
					let mut tmp = ConstTrueContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();


					recog.base.set_state(199);
					recog.base.match_token(Surrogate_id_SYMB_60,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					{
					let mut tmp = ConstFalseContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(200);
					recog.base.match_token(Surrogate_id_SYMB_41,&mut recog.err_handler)?;

					}
				}
			,
				3 =>{
					{
					let mut tmp = ConstUnitContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(201);
					recog.base.match_token(Surrogate_id_SYMB_63,&mut recog.err_handler)?;

					}
				}
			,
				4 =>{
					{
					let mut tmp = ConstIntContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(202);
					let tmp = recog.base.match_token(INTEGER,&mut recog.err_handler)?;
					if let ExprContextAll::ConstIntContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				5 =>{
					{
					let mut tmp = ConstMemoryContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(203);
					let tmp = recog.base.match_token(MemoryAddress,&mut recog.err_handler)?;
					if let ExprContextAll::ConstMemoryContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.mem = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				6 =>{
					{
					let mut tmp = VarContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(204);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let ExprContextAll::VarContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				7 =>{
					{
					let mut tmp = PanicContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(205);
					recog.base.match_token(PANIC,&mut recog.err_handler)?;

					}
				}
			,
				8 =>{
					{
					let mut tmp = ThrowContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(206);
					recog.base.match_token(THROW,&mut recog.err_handler)?;

					recog.base.set_state(207);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(208);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::ThrowContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(209);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				9 =>{
					{
					let mut tmp = TryCatchContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(211);
					recog.base.match_token(TRY,&mut recog.err_handler)?;

					recog.base.set_state(212);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(213);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryCatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.tryExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(214);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					recog.base.set_state(215);
					recog.base.match_token(CATCH,&mut recog.err_handler)?;

					recog.base.set_state(216);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(217);
					let tmp = recog.pattern_rec(0)?;
					if let ExprContextAll::TryCatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.pat = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(218);
					recog.base.match_token(Surrogate_id_SYMB_9,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(219);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryCatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.fallbackExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(220);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				10 =>{
					{
					let mut tmp = TryCastAsContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(222);
					recog.base.match_token(TRY,&mut recog.err_handler)?;

					recog.base.set_state(223);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(224);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryCastAsContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.tryExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(225);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					recog.base.set_state(226);
					recog.base.match_token(CAST,&mut recog.err_handler)?;

					recog.base.set_state(227);
					recog.base.match_token(Surrogate_id_SYMB_36,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(228);
					let tmp = recog.stellatype_rec(0)?;
					if let ExprContextAll::TryCastAsContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(229);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(230);
					let tmp = recog.pattern_rec(0)?;
					if let ExprContextAll::TryCastAsContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(231);
					recog.base.match_token(Surrogate_id_SYMB_9,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(232);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryCastAsContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(233);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					recog.base.set_state(234);
					recog.base.match_token(Surrogate_id_SYMB_64,&mut recog.err_handler)?;

					recog.base.set_state(235);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(236);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryCastAsContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.fallbackExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(237);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				11 =>{
					{
					let mut tmp = TryWithContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(239);
					recog.base.match_token(TRY,&mut recog.err_handler)?;

					recog.base.set_state(240);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(241);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryWithContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.tryExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(242);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					recog.base.set_state(243);
					recog.base.match_token(Surrogate_id_SYMB_64,&mut recog.err_handler)?;

					recog.base.set_state(244);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(245);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TryWithContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.fallbackExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(246);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				12 =>{
					{
					let mut tmp = InlContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(248);
					recog.base.match_token(Surrogate_id_SYMB_47,&mut recog.err_handler)?;

					recog.base.set_state(249);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(250);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::InlContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(251);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				13 =>{
					{
					let mut tmp = InrContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(253);
					recog.base.match_token(Surrogate_id_SYMB_49,&mut recog.err_handler)?;

					recog.base.set_state(254);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(255);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::InrContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(256);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				14 =>{
					{
					let mut tmp = ConsListContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(258);
					recog.base.match_token(Surrogate_id_SYMB_37,&mut recog.err_handler)?;

					recog.base.set_state(259);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(260);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::ConsListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.head = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(261);
					recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(262);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::ConsListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.tail = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(263);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				15 =>{
					{
					let mut tmp = HeadContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(265);
					recog.base.match_token(Surrogate_id_SYMB_26,&mut recog.err_handler)?;

					recog.base.set_state(266);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(267);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::HeadContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.list = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(268);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				16 =>{
					{
					let mut tmp = IsEmptyContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(270);
					recog.base.match_token(Surrogate_id_SYMB_27,&mut recog.err_handler)?;

					recog.base.set_state(271);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(272);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::IsEmptyContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.list = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(273);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				17 =>{
					{
					let mut tmp = TailContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(275);
					recog.base.match_token(Surrogate_id_SYMB_28,&mut recog.err_handler)?;

					recog.base.set_state(276);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(277);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::TailContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.list = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(278);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				18 =>{
					{
					let mut tmp = SuccContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(280);
					recog.base.match_token(Surrogate_id_SYMB_57,&mut recog.err_handler)?;

					recog.base.set_state(281);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(282);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::SuccContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(283);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				19 =>{
					{
					let mut tmp = LogicNotContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(285);
					recog.base.match_token(Surrogate_id_SYMB_54,&mut recog.err_handler)?;

					recog.base.set_state(286);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(287);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::LogicNotContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(288);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				20 =>{
					{
					let mut tmp = PredContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(290);
					recog.base.match_token(Surrogate_id_SYMB_29,&mut recog.err_handler)?;

					recog.base.set_state(291);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(292);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::PredContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(293);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				21 =>{
					{
					let mut tmp = IsZeroContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(295);
					recog.base.match_token(Surrogate_id_SYMB_30,&mut recog.err_handler)?;

					recog.base.set_state(296);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(297);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::IsZeroContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(298);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				22 =>{
					{
					let mut tmp = FixContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(300);
					recog.base.match_token(Surrogate_id_SYMB_42,&mut recog.err_handler)?;

					recog.base.set_state(301);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(302);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::FixContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(303);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				23 =>{
					{
					let mut tmp = NatRecContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(305);
					recog.base.match_token(Surrogate_id_SYMB_31,&mut recog.err_handler)?;

					recog.base.set_state(306);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(307);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::NatRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(308);
					recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(309);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::NatRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.initial = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(310);
					recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(311);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::NatRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.step = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(312);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				24 =>{
					{
					let mut tmp = FoldContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(314);
					recog.base.match_token(Surrogate_id_SYMB_44,&mut recog.err_handler)?;

					recog.base.set_state(315);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(316);
					let tmp = recog.stellatype_rec(0)?;
					if let ExprContextAll::FoldContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(317);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(318);
					let tmp = recog.expr_rec(34)?;
					if let ExprContextAll::FoldContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				25 =>{
					{
					let mut tmp = UnfoldContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(320);
					recog.base.match_token(Surrogate_id_SYMB_62,&mut recog.err_handler)?;

					recog.base.set_state(321);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(322);
					let tmp = recog.stellatype_rec(0)?;
					if let ExprContextAll::UnfoldContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(323);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(324);
					let tmp = recog.expr_rec(33)?;
					if let ExprContextAll::UnfoldContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				26 =>{
					{
					let mut tmp = RefContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(326);
					recog.base.match_token(REFERENCE,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(327);
					let tmp = recog.expr_rec(27)?;
					if let ExprContextAll::RefContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				27 =>{
					{
					let mut tmp = DerefContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(328);
					recog.base.match_token(Surrogate_id_SYMB_23,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(329);
					let tmp = recog.expr_rec(26)?;
					if let ExprContextAll::DerefContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				28 =>{
					{
					let mut tmp = AbstractionContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(330);
					recog.base.match_token(Surrogate_id_SYMB_43,&mut recog.err_handler)?;

					recog.base.set_state(331);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					recog.base.set_state(340);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==StellaIdent {
						{
						/*InvokeRule paramDecl*/
						recog.base.set_state(332);
						let tmp = recog.paramDecl()?;
						if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(337);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(333);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule paramDecl*/
							recog.base.set_state(334);
							let tmp = recog.paramDecl()?;
							if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.paramDecl = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.paramDecl.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.paramDecls.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(339);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(342);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					recog.base.set_state(343);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(344);
					recog.base.match_token(Surrogate_id_SYMB_56,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(345);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::AbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.returnExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(346);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				29 =>{
					{
					let mut tmp = TupleContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(348);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(357);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13) | (1usize << Surrogate_id_SYMB_23) | (1usize << Surrogate_id_SYMB_26) | (1usize << Surrogate_id_SYMB_27) | (1usize << Surrogate_id_SYMB_28) | (1usize << Surrogate_id_SYMB_29) | (1usize << Surrogate_id_SYMB_30))) != 0) || ((((_la - 32)) & !0x3f) == 0 && ((1usize << (_la - 32)) & ((1usize << (Surrogate_id_SYMB_31 - 32)) | (1usize << (Surrogate_id_SYMB_37 - 32)) | (1usize << (Surrogate_id_SYMB_41 - 32)) | (1usize << (Surrogate_id_SYMB_42 - 32)) | (1usize << (Surrogate_id_SYMB_43 - 32)) | (1usize << (Surrogate_id_SYMB_44 - 32)) | (1usize << (Surrogate_id_SYMB_45 - 32)) | (1usize << (Surrogate_id_SYMB_47 - 32)) | (1usize << (Surrogate_id_SYMB_49 - 32)) | (1usize << (Surrogate_id_SYMB_51 - 32)) | (1usize << (Surrogate_id_SYMB_52 - 32)) | (1usize << (Surrogate_id_SYMB_53 - 32)) | (1usize << (Surrogate_id_SYMB_54 - 32)) | (1usize << (Surrogate_id_SYMB_57 - 32)) | (1usize << (Surrogate_id_SYMB_60 - 32)) | (1usize << (Surrogate_id_SYMB_62 - 32)))) != 0) || ((((_la - 64)) & !0x3f) == 0 && ((1usize << (_la - 64)) & ((1usize << (Surrogate_id_SYMB_63 - 64)) | (1usize << (REFERENCE - 64)) | (1usize << (PANIC - 64)) | (1usize << (THROW - 64)) | (1usize << (TRY - 64)) | (1usize << (GENERIC - 64)) | (1usize << (StellaIdent - 64)) | (1usize << (MemoryAddress - 64)) | (1usize << (INTEGER - 64)))) != 0) {
						{
						/*InvokeRule expr*/
						recog.base.set_state(349);
						let tmp = recog.expr_rec(0)?;
						if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.exprs.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(354);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(350);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(351);
							let tmp = recog.expr_rec(0)?;
							if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let ExprContextAll::TupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.exprs.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(356);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(359);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				30 =>{
					{
					let mut tmp = RecordContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(360);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule binding*/
					recog.base.set_state(361);
					let tmp = recog.binding()?;
					if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.binding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.binding.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.bindings.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(366);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(362);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						/*InvokeRule binding*/
						recog.base.set_state(363);
						let tmp = recog.binding()?;
						if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.binding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.binding.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::RecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.bindings.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(368);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(369);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				31 =>{
					{
					let mut tmp = VariantContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(371);
					recog.base.match_token(Surrogate_id_SYMB_11,&mut recog.err_handler)?;

					recog.base.set_state(372);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let ExprContextAll::VariantContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.label = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(375);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_6 {
						{
						recog.base.set_state(373);
						recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

						/*InvokeRule expr*/
						recog.base.set_state(374);
						let tmp = recog.expr_rec(0)?;
						if let ExprContextAll::VariantContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.rhs = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						}
					}

					recog.base.set_state(377);
					recog.base.match_token(Surrogate_id_SYMB_12,&mut recog.err_handler)?;

					}
				}
			,
				32 =>{
					{
					let mut tmp = MatchContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(378);
					recog.base.match_token(Surrogate_id_SYMB_53,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(379);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(380);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(389);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13))) != 0) || ((((_la - 38)) & !0x3f) == 0 && ((1usize << (_la - 38)) & ((1usize << (Surrogate_id_SYMB_37 - 38)) | (1usize << (Surrogate_id_SYMB_41 - 38)) | (1usize << (Surrogate_id_SYMB_47 - 38)) | (1usize << (Surrogate_id_SYMB_49 - 38)) | (1usize << (Surrogate_id_SYMB_57 - 38)) | (1usize << (Surrogate_id_SYMB_60 - 38)) | (1usize << (Surrogate_id_SYMB_63 - 38)))) != 0) || _la==StellaIdent || _la==INTEGER {
						{
						/*InvokeRule matchCase*/
						recog.base.set_state(381);
						let tmp = recog.matchCase()?;
						if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.matchCase = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.matchCase.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.cases.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(386);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_10 {
							{
							{
							recog.base.set_state(382);
							recog.base.match_token(Surrogate_id_SYMB_10,&mut recog.err_handler)?;

							/*InvokeRule matchCase*/
							recog.base.set_state(383);
							let tmp = recog.matchCase()?;
							if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.matchCase = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.matchCase.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let ExprContextAll::MatchContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.cases.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(388);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(391);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				33 =>{
					{
					let mut tmp = ListContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(393);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					recog.base.set_state(402);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13) | (1usize << Surrogate_id_SYMB_23) | (1usize << Surrogate_id_SYMB_26) | (1usize << Surrogate_id_SYMB_27) | (1usize << Surrogate_id_SYMB_28) | (1usize << Surrogate_id_SYMB_29) | (1usize << Surrogate_id_SYMB_30))) != 0) || ((((_la - 32)) & !0x3f) == 0 && ((1usize << (_la - 32)) & ((1usize << (Surrogate_id_SYMB_31 - 32)) | (1usize << (Surrogate_id_SYMB_37 - 32)) | (1usize << (Surrogate_id_SYMB_41 - 32)) | (1usize << (Surrogate_id_SYMB_42 - 32)) | (1usize << (Surrogate_id_SYMB_43 - 32)) | (1usize << (Surrogate_id_SYMB_44 - 32)) | (1usize << (Surrogate_id_SYMB_45 - 32)) | (1usize << (Surrogate_id_SYMB_47 - 32)) | (1usize << (Surrogate_id_SYMB_49 - 32)) | (1usize << (Surrogate_id_SYMB_51 - 32)) | (1usize << (Surrogate_id_SYMB_52 - 32)) | (1usize << (Surrogate_id_SYMB_53 - 32)) | (1usize << (Surrogate_id_SYMB_54 - 32)) | (1usize << (Surrogate_id_SYMB_57 - 32)) | (1usize << (Surrogate_id_SYMB_60 - 32)) | (1usize << (Surrogate_id_SYMB_62 - 32)))) != 0) || ((((_la - 64)) & !0x3f) == 0 && ((1usize << (_la - 64)) & ((1usize << (Surrogate_id_SYMB_63 - 64)) | (1usize << (REFERENCE - 64)) | (1usize << (PANIC - 64)) | (1usize << (THROW - 64)) | (1usize << (TRY - 64)) | (1usize << (GENERIC - 64)) | (1usize << (StellaIdent - 64)) | (1usize << (MemoryAddress - 64)) | (1usize << (INTEGER - 64)))) != 0) {
						{
						/*InvokeRule expr*/
						recog.base.set_state(394);
						let tmp = recog.expr_rec(0)?;
						if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.exprs.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(399);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(395);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(396);
							let tmp = recog.expr_rec(0)?;
							if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let ExprContextAll::ListContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.exprs.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(401);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(404);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					}
				}
			,
				34 =>{
					{
					let mut tmp = IfContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(405);
					recog.base.match_token(Surrogate_id_SYMB_45,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(406);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::IfContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.condition = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(407);
					recog.base.match_token(Surrogate_id_SYMB_58,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(408);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::IfContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.thenExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(409);
					recog.base.match_token(Surrogate_id_SYMB_39,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(410);
					let tmp = recog.expr_rec(7)?;
					if let ExprContextAll::IfContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.elseExpr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				35 =>{
					{
					let mut tmp = LetContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(412);
					recog.base.match_token(Surrogate_id_SYMB_51,&mut recog.err_handler)?;

					/*InvokeRule patternBinding*/
					recog.base.set_state(413);
					let tmp = recog.patternBinding()?;
					if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBinding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBinding.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBindings.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(418);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(414);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						/*InvokeRule patternBinding*/
						recog.base.set_state(415);
						let tmp = recog.patternBinding()?;
						if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBinding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBinding.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBindings.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(420);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(421);
					recog.base.match_token(Surrogate_id_SYMB_46,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(422);
					let tmp = recog.expr_rec(5)?;
					if let ExprContextAll::LetContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.body = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				36 =>{
					{
					let mut tmp = LetRecContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(424);
					recog.base.match_token(Surrogate_id_SYMB_52,&mut recog.err_handler)?;

					/*InvokeRule patternBinding*/
					recog.base.set_state(425);
					let tmp = recog.patternBinding()?;
					if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBinding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBinding.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.patternBindings.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(430);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(426);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						/*InvokeRule patternBinding*/
						recog.base.set_state(427);
						let tmp = recog.patternBinding()?;
						if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBinding = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBinding.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.patternBindings.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(432);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(433);
					recog.base.match_token(Surrogate_id_SYMB_46,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(434);
					let tmp = recog.expr_rec(4)?;
					if let ExprContextAll::LetRecContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.body = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				37 =>{
					{
					let mut tmp = TypeAbstractionContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(436);
					recog.base.match_token(GENERIC,&mut recog.err_handler)?;

					recog.base.set_state(437);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					recog.base.set_state(438);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.StellaIdent = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.StellaIdent.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.generics.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(443);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(439);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						recog.base.set_state(440);
						let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
						if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.StellaIdent = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.StellaIdent.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
						ctx.generics.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(445);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(446);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(447);
					let tmp = recog.expr_rec(3)?;
					if let ExprContextAll::TypeAbstractionContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				38 =>{
					{
					let mut tmp = ParenthesisedExprContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(448);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule expr*/
					recog.base.set_state(449);
					let tmp = recog.expr_rec(0)?;
					if let ExprContextAll::ParenthesisedExprContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
					ctx.expr_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(450);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}

				_ => {}
			}

			let tmp = recog.input.lt(-1).cloned();
			recog.ctx.as_ref().unwrap().set_stop(tmp);
			recog.base.set_state(538);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(37,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					recog.trigger_exit_rule_event();
					_prevctx = _localctx.clone();
					{
					recog.base.set_state(536);
					recog.err_handler.sync(&mut recog.base)?;
					match  recog.interpreter.adaptive_predict(36,&mut recog.base)? {
						1 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = MultiplyContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::MultiplyContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(454);
							if !({recog.precpred(None, 30)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 30)".to_owned()), None))?;
							}
							recog.base.set_state(455);
							recog.base.match_token(Surrogate_id_SYMB_23,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(456);
							let tmp = recog.expr_rec(31)?;
							if let ExprContextAll::MultiplyContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						2 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = DivideContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::DivideContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(457);
							if !({recog.precpred(None, 29)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 29)".to_owned()), None))?;
							}
							recog.base.set_state(458);
							recog.base.match_token(Surrogate_id_SYMB_24,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(459);
							let tmp = recog.expr_rec(30)?;
							if let ExprContextAll::DivideContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						3 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = LogicAndContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::LogicAndContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(460);
							if !({recog.precpred(None, 28)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 28)".to_owned()), None))?;
							}
							recog.base.set_state(461);
							recog.base.match_token(Surrogate_id_SYMB_35,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(462);
							let tmp = recog.expr_rec(29)?;
							if let ExprContextAll::LogicAndContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						4 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = AddContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::AddContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(463);
							if !({recog.precpred(None, 25)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 25)".to_owned()), None))?;
							}
							recog.base.set_state(464);
							recog.base.match_token(Surrogate_id_SYMB_21,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(465);
							let tmp = recog.expr_rec(26)?;
							if let ExprContextAll::AddContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						5 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = SubtractContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::SubtractContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(466);
							if !({recog.precpred(None, 24)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 24)".to_owned()), None))?;
							}
							recog.base.set_state(467);
							recog.base.match_token(Surrogate_id_SYMB_22,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(468);
							let tmp = recog.expr_rec(25)?;
							if let ExprContextAll::SubtractContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						6 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = LogicOrContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::LogicOrContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(469);
							if !({recog.precpred(None, 23)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 23)".to_owned()), None))?;
							}
							recog.base.set_state(470);
							recog.base.match_token(Surrogate_id_SYMB_55,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(471);
							let tmp = recog.expr_rec(24)?;
							if let ExprContextAll::LogicOrContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						7 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = LessThanContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::LessThanContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(472);
							if !({recog.precpred(None, 14)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 14)".to_owned()), None))?;
							}
							recog.base.set_state(473);
							recog.base.match_token(Surrogate_id_SYMB_15,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(474);
							let tmp = recog.expr_rec(15)?;
							if let ExprContextAll::LessThanContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						8 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = LessThanOrEqualContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::LessThanOrEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(475);
							if !({recog.precpred(None, 13)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 13)".to_owned()), None))?;
							}
							recog.base.set_state(476);
							recog.base.match_token(Surrogate_id_SYMB_16,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(477);
							let tmp = recog.expr_rec(14)?;
							if let ExprContextAll::LessThanOrEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						9 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = GreaterThanContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::GreaterThanContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(478);
							if !({recog.precpred(None, 12)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 12)".to_owned()), None))?;
							}
							recog.base.set_state(479);
							recog.base.match_token(Surrogate_id_SYMB_17,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(480);
							let tmp = recog.expr_rec(13)?;
							if let ExprContextAll::GreaterThanContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						10 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = GreaterThanOrEqualContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::GreaterThanOrEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(481);
							if !({recog.precpred(None, 11)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 11)".to_owned()), None))?;
							}
							recog.base.set_state(482);
							recog.base.match_token(Surrogate_id_SYMB_18,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(483);
							let tmp = recog.expr_rec(12)?;
							if let ExprContextAll::GreaterThanOrEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						11 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = EqualContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::EqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(484);
							if !({recog.precpred(None, 10)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 10)".to_owned()), None))?;
							}
							recog.base.set_state(485);
							recog.base.match_token(Surrogate_id_SYMB_19,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(486);
							let tmp = recog.expr_rec(11)?;
							if let ExprContextAll::EqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						12 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = NotEqualContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::NotEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.left = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(487);
							if !({recog.precpred(None, 9)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 9)".to_owned()), None))?;
							}
							recog.base.set_state(488);
							recog.base.match_token(Surrogate_id_SYMB_20,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(489);
							let tmp = recog.expr_rec(10)?;
							if let ExprContextAll::NotEqualContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						13 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = AssignContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::AssignContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.lhs = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(490);
							if !({recog.precpred(None, 8)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 8)".to_owned()), None))?;
							}
							recog.base.set_state(491);
							recog.base.match_token(ASSIGN,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(492);
							let tmp = recog.expr_rec(9)?;
							if let ExprContextAll::AssignContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.rhs = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						14 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = SequenceContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::SequenceContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr1 = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(493);
							if !({recog.precpred(None, 6)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 6)".to_owned()), None))?;
							}
							recog.base.set_state(494);
							recog.base.match_token(Surrogate_id_SYMB_1,&mut recog.err_handler)?;

							/*InvokeRule expr*/
							recog.base.set_state(495);
							let tmp = recog.expr_rec(7)?;
							if let ExprContextAll::SequenceContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.expr2 = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						15 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = DotRecordContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::DotRecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(496);
							if !({recog.precpred(None, 59)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 59)".to_owned()), None))?;
							}
							recog.base.set_state(497);
							recog.base.match_token(Surrogate_id_SYMB_25,&mut recog.err_handler)?;

							recog.base.set_state(498);
							let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
							if let ExprContextAll::DotRecordContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.label = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						16 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = DotTupleContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::DotTupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(499);
							if !({recog.precpred(None, 58)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 58)".to_owned()), None))?;
							}
							recog.base.set_state(500);
							recog.base.match_token(Surrogate_id_SYMB_25,&mut recog.err_handler)?;

							recog.base.set_state(501);
							let tmp = recog.base.match_token(INTEGER,&mut recog.err_handler)?;
							if let ExprContextAll::DotTupleContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.index = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						17 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = ApplicationContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.fun = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(502);
							if !({recog.precpred(None, 32)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 32)".to_owned()), None))?;
							}
							recog.base.set_state(503);
							recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

							recog.base.set_state(512);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
							if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13) | (1usize << Surrogate_id_SYMB_23) | (1usize << Surrogate_id_SYMB_26) | (1usize << Surrogate_id_SYMB_27) | (1usize << Surrogate_id_SYMB_28) | (1usize << Surrogate_id_SYMB_29) | (1usize << Surrogate_id_SYMB_30))) != 0) || ((((_la - 32)) & !0x3f) == 0 && ((1usize << (_la - 32)) & ((1usize << (Surrogate_id_SYMB_31 - 32)) | (1usize << (Surrogate_id_SYMB_37 - 32)) | (1usize << (Surrogate_id_SYMB_41 - 32)) | (1usize << (Surrogate_id_SYMB_42 - 32)) | (1usize << (Surrogate_id_SYMB_43 - 32)) | (1usize << (Surrogate_id_SYMB_44 - 32)) | (1usize << (Surrogate_id_SYMB_45 - 32)) | (1usize << (Surrogate_id_SYMB_47 - 32)) | (1usize << (Surrogate_id_SYMB_49 - 32)) | (1usize << (Surrogate_id_SYMB_51 - 32)) | (1usize << (Surrogate_id_SYMB_52 - 32)) | (1usize << (Surrogate_id_SYMB_53 - 32)) | (1usize << (Surrogate_id_SYMB_54 - 32)) | (1usize << (Surrogate_id_SYMB_57 - 32)) | (1usize << (Surrogate_id_SYMB_60 - 32)) | (1usize << (Surrogate_id_SYMB_62 - 32)))) != 0) || ((((_la - 64)) & !0x3f) == 0 && ((1usize << (_la - 64)) & ((1usize << (Surrogate_id_SYMB_63 - 64)) | (1usize << (REFERENCE - 64)) | (1usize << (PANIC - 64)) | (1usize << (THROW - 64)) | (1usize << (TRY - 64)) | (1usize << (GENERIC - 64)) | (1usize << (StellaIdent - 64)) | (1usize << (MemoryAddress - 64)) | (1usize << (INTEGER - 64)))) != 0) {
								{
								/*InvokeRule expr*/
								recog.base.set_state(504);
								let tmp = recog.expr_rec(0)?;
								if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

								let temp = if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
								if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.args.push(temp); } else {unreachable!("cant cast");}  
								recog.base.set_state(509);
								recog.err_handler.sync(&mut recog.base)?;
								_la = recog.base.input.la(1);
								while _la==Surrogate_id_SYMB_0 {
									{
									{
									recog.base.set_state(505);
									recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

									/*InvokeRule expr*/
									recog.base.set_state(506);
									let tmp = recog.expr_rec(0)?;
									if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
									ctx.expr = Some(tmp.clone()); } else {unreachable!("cant cast");}  

									let temp = if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
									ctx.expr.clone().unwrap() } else {unreachable!("cant cast");} ;
									if let ExprContextAll::ApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
									ctx.args.push(temp); } else {unreachable!("cant cast");}  
									}
									}
									recog.base.set_state(511);
									recog.err_handler.sync(&mut recog.base)?;
									_la = recog.base.input.la(1);
								}
								}
							}

							recog.base.set_state(514);
							recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

							}
						}
					,
						18 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = TypeApplicationContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.fun = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(515);
							if !({recog.precpred(None, 31)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 31)".to_owned()), None))?;
							}
							recog.base.set_state(516);
							recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

							{
							/*InvokeRule stellatype*/
							recog.base.set_state(517);
							let tmp = recog.stellatype_rec(0)?;
							if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.types.push(temp); } else {unreachable!("cant cast");}  
							recog.base.set_state(522);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
							while _la==Surrogate_id_SYMB_0 {
								{
								{
								recog.base.set_state(518);
								recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

								/*InvokeRule stellatype*/
								recog.base.set_state(519);
								let tmp = recog.stellatype_rec(0)?;
								if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

								let temp = if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
								if let ExprContextAll::TypeApplicationContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
								ctx.types.push(temp); } else {unreachable!("cant cast");}  
								}
								}
								recog.base.set_state(524);
								recog.err_handler.sync(&mut recog.base)?;
								_la = recog.base.input.la(1);
							}
							}
							recog.base.set_state(525);
							recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

							}
						}
					,
						19 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = TypeAscContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::TypeAscContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(527);
							if !({recog.precpred(None, 22)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 22)".to_owned()), None))?;
							}
							recog.base.set_state(528);
							recog.base.match_token(Surrogate_id_SYMB_36,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(529);
							let tmp = recog.stellatype_rec(0)?;
							if let ExprContextAll::TypeAscContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						20 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = TypeCastContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::TypeCastContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(530);
							if !({recog.precpred(None, 21)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 21)".to_owned()), None))?;
							}
							recog.base.set_state(531);
							recog.base.match_token(CAST,&mut recog.err_handler)?;

							recog.base.set_state(532);
							recog.base.match_token(Surrogate_id_SYMB_36,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(533);
							let tmp = recog.stellatype_rec(0)?;
							if let ExprContextAll::TypeCastContext(ctx) = cast_mut::<_,ExprContextAll >(&mut _localctx){
							ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						21 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = TerminatingSemicolonContextExt::new(&**ExprContextExt::new(_parentctx.clone(), _parentState));
							if let ExprContextAll::TerminatingSemicolonContext(ctx) = cast_mut::<_,ExprContextAll >(&mut tmp){
								ctx.expr_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_expr);
							_localctx = tmp;
							recog.base.set_state(534);
							if !({recog.precpred(None, 1)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 1)".to_owned()), None))?;
							}
							recog.base.set_state(535);
							recog.base.match_token(Surrogate_id_SYMB_1,&mut recog.err_handler)?;

							}
						}

						_ => {}
					}
					} 
				}
				recog.base.set_state(540);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(37,&mut recog.base)?;
			}
			}
			Ok(())
		})();
		match result {
		Ok(_) => {},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re)=>{
			//_localctx.exception = re;
			recog.err_handler.report_error(&mut recog.base, re);
	        recog.err_handler.recover(&mut recog.base, re)?;}
		}
		recog.base.unroll_recursion_context(_parentctx);

		Ok(_localctx)
	}
}
//------------------- patternBinding ----------------
pub type PatternBindingContextAll<'input> = PatternBindingContext<'input>;


pub type PatternBindingContext<'input> = BaseParserRuleContext<'input,PatternBindingContextExt<'input>>;

#[derive(Clone)]
pub struct PatternBindingContextExt<'input>{
	pub pat: Option<Rc<PatternContextAll<'input>>>,
	pub rhs: Option<Rc<ExprContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for PatternBindingContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternBindingContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_patternBinding(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_patternBinding(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for PatternBindingContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_patternBinding }
	//fn type_rule_index() -> usize where Self: Sized { RULE_patternBinding }
}
antlr_rust::tid!{PatternBindingContextExt<'a>}

impl<'input> PatternBindingContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<PatternBindingContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,PatternBindingContextExt{
				pat: None, rhs: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait PatternBindingContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<PatternBindingContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_6, 0)
}
fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> PatternBindingContextAttrs<'input> for PatternBindingContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn patternBinding(&mut self,)
	-> Result<Rc<PatternBindingContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = PatternBindingContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 20, RULE_patternBinding);
        let mut _localctx: Rc<PatternBindingContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule pattern*/
			recog.base.set_state(541);
			let tmp = recog.pattern_rec(0)?;
			 cast_mut::<_,PatternBindingContext >(&mut _localctx).pat = Some(tmp.clone());
			  

			recog.base.set_state(542);
			recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(543);
			let tmp = recog.expr_rec(0)?;
			 cast_mut::<_,PatternBindingContext >(&mut _localctx).rhs = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- binding ----------------
pub type BindingContextAll<'input> = BindingContext<'input>;


pub type BindingContext<'input> = BaseParserRuleContext<'input,BindingContextExt<'input>>;

#[derive(Clone)]
pub struct BindingContextExt<'input>{
	pub name: Option<TokenType<'input>>,
	pub rhs: Option<Rc<ExprContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for BindingContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for BindingContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_binding(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_binding(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for BindingContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_binding }
	//fn type_rule_index() -> usize where Self: Sized { RULE_binding }
}
antlr_rust::tid!{BindingContextExt<'a>}

impl<'input> BindingContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<BindingContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,BindingContextExt{
				name: None, 
				rhs: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait BindingContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<BindingContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_6, 0)
}
/// Retrieves first TerminalNode corresponding to token StellaIdent
/// Returns `None` if there is no child corresponding to token StellaIdent
fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(StellaIdent, 0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> BindingContextAttrs<'input> for BindingContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn binding(&mut self,)
	-> Result<Rc<BindingContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = BindingContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 22, RULE_binding);
        let mut _localctx: Rc<BindingContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(545);
			let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
			 cast_mut::<_,BindingContext >(&mut _localctx).name = Some(tmp.clone());
			  

			recog.base.set_state(546);
			recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(547);
			let tmp = recog.expr_rec(0)?;
			 cast_mut::<_,BindingContext >(&mut _localctx).rhs = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- matchCase ----------------
pub type MatchCaseContextAll<'input> = MatchCaseContext<'input>;


pub type MatchCaseContext<'input> = BaseParserRuleContext<'input,MatchCaseContextExt<'input>>;

#[derive(Clone)]
pub struct MatchCaseContextExt<'input>{
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	pub expr_: Option<Rc<ExprContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for MatchCaseContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for MatchCaseContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_matchCase(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_matchCase(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for MatchCaseContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_matchCase }
	//fn type_rule_index() -> usize where Self: Sized { RULE_matchCase }
}
antlr_rust::tid!{MatchCaseContextExt<'a>}

impl<'input> MatchCaseContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<MatchCaseContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,MatchCaseContextExt{
				pattern_: None, expr_: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait MatchCaseContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<MatchCaseContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_9
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_9
fn Surrogate_id_SYMB_9(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_9, 0)
}
fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}
fn expr(&self) -> Option<Rc<ExprContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> MatchCaseContextAttrs<'input> for MatchCaseContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn matchCase(&mut self,)
	-> Result<Rc<MatchCaseContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = MatchCaseContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 24, RULE_matchCase);
        let mut _localctx: Rc<MatchCaseContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			/*InvokeRule pattern*/
			recog.base.set_state(549);
			let tmp = recog.pattern_rec(0)?;
			 cast_mut::<_,MatchCaseContext >(&mut _localctx).pattern_ = Some(tmp.clone());
			  

			recog.base.set_state(550);
			recog.base.match_token(Surrogate_id_SYMB_9,&mut recog.err_handler)?;

			/*InvokeRule expr*/
			recog.base.set_state(551);
			let tmp = recog.expr_rec(0)?;
			 cast_mut::<_,MatchCaseContext >(&mut _localctx).expr_ = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- pattern ----------------
#[derive(Debug)]
pub enum PatternContextAll<'input>{
	PatternConsContext(PatternConsContext<'input>),
	PatternTupleContext(PatternTupleContext<'input>),
	PatternListContext(PatternListContext<'input>),
	PatternRecordContext(PatternRecordContext<'input>),
	PatternVariantContext(PatternVariantContext<'input>),
	PatternAscContext(PatternAscContext<'input>),
	PatternIntContext(PatternIntContext<'input>),
	PatternInrContext(PatternInrContext<'input>),
	PatternTrueContext(PatternTrueContext<'input>),
	PatternInlContext(PatternInlContext<'input>),
	PatternVarContext(PatternVarContext<'input>),
	ParenthesisedPatternContext(ParenthesisedPatternContext<'input>),
	PatternSuccContext(PatternSuccContext<'input>),
	PatternFalseContext(PatternFalseContext<'input>),
	PatternUnitContext(PatternUnitContext<'input>),
	PatternCastAsContext(PatternCastAsContext<'input>),
Error(PatternContext<'input>)
}
antlr_rust::tid!{PatternContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for PatternContextAll<'input>{}

impl<'input> stellaParserContext<'input> for PatternContextAll<'input>{}

impl<'input> Deref for PatternContextAll<'input>{
	type Target = dyn PatternContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use PatternContextAll::*;
		match self{
			PatternConsContext(inner) => inner,
			PatternTupleContext(inner) => inner,
			PatternListContext(inner) => inner,
			PatternRecordContext(inner) => inner,
			PatternVariantContext(inner) => inner,
			PatternAscContext(inner) => inner,
			PatternIntContext(inner) => inner,
			PatternInrContext(inner) => inner,
			PatternTrueContext(inner) => inner,
			PatternInlContext(inner) => inner,
			PatternVarContext(inner) => inner,
			ParenthesisedPatternContext(inner) => inner,
			PatternSuccContext(inner) => inner,
			PatternFalseContext(inner) => inner,
			PatternUnitContext(inner) => inner,
			PatternCastAsContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type PatternContext<'input> = BaseParserRuleContext<'input,PatternContextExt<'input>>;

#[derive(Clone)]
pub struct PatternContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for PatternContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternContext<'input>{
}

impl<'input> CustomRuleContext<'input> for PatternContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}
antlr_rust::tid!{PatternContextExt<'a>}

impl<'input> PatternContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<PatternContextAll<'input>> {
		Rc::new(
		PatternContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,PatternContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait PatternContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<PatternContextExt<'input>>{


}

impl<'input> PatternContextAttrs<'input> for PatternContext<'input>{}

pub type PatternConsContext<'input> = BaseParserRuleContext<'input,PatternConsContextExt<'input>>;

pub trait PatternConsContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_37
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_37
	fn Surrogate_id_SYMB_37(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_37, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_0
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_0
	fn Surrogate_id_SYMB_0(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn pattern_all(&self) ->  Vec<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn pattern(&self, i: usize) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> PatternConsContextAttrs<'input> for PatternConsContext<'input>{}

pub struct PatternConsContextExt<'input>{
	base:PatternContextExt<'input>,
	pub head: Option<Rc<PatternContextAll<'input>>>,
	pub tail: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternConsContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternConsContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternConsContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternCons(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternConsContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternConsContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternConsContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternConsContext<'input> {}

impl<'input> PatternConsContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternConsContext(
				BaseParserRuleContext::copy_from(ctx,PatternConsContextExt{
        			head:None, tail:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternTupleContext<'input> = BaseParserRuleContext<'input,PatternTupleContextExt<'input>>;

pub trait PatternTupleContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn pattern_all(&self) ->  Vec<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn pattern(&self, i: usize) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> PatternTupleContextAttrs<'input> for PatternTupleContext<'input>{}

pub struct PatternTupleContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern: Option<Rc<PatternContextAll<'input>>>,
	pub patterns:Vec<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternTupleContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternTupleContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternTupleContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternTuple(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternTupleContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternTupleContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternTupleContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternTupleContext<'input> {}

impl<'input> PatternTupleContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternTupleContext(
				BaseParserRuleContext::copy_from(ctx,PatternTupleContextExt{
        			pattern:None, 
        			patterns:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternListContext<'input> = BaseParserRuleContext<'input,PatternListContextExt<'input>>;

pub trait PatternListContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn pattern_all(&self) ->  Vec<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn pattern(&self, i: usize) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> PatternListContextAttrs<'input> for PatternListContext<'input>{}

pub struct PatternListContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern: Option<Rc<PatternContextAll<'input>>>,
	pub patterns:Vec<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternListContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternListContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternListContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternList(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternListContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternListContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternListContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternListContext<'input> {}

impl<'input> PatternListContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternListContext(
				BaseParserRuleContext::copy_from(ctx,PatternListContextExt{
        			pattern:None, 
        			patterns:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternRecordContext<'input> = BaseParserRuleContext<'input,PatternRecordContextExt<'input>>;

pub trait PatternRecordContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn labelledPattern_all(&self) ->  Vec<Rc<LabelledPatternContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn labelledPattern(&self, i: usize) -> Option<Rc<LabelledPatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> PatternRecordContextAttrs<'input> for PatternRecordContext<'input>{}

pub struct PatternRecordContextExt<'input>{
	base:PatternContextExt<'input>,
	pub labelledPattern: Option<Rc<LabelledPatternContextAll<'input>>>,
	pub patterns:Vec<Rc<LabelledPatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternRecordContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternRecordContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternRecordContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternRecord(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternRecordContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternRecordContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternRecordContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternRecordContext<'input> {}

impl<'input> PatternRecordContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternRecordContext(
				BaseParserRuleContext::copy_from(ctx,PatternRecordContextExt{
        			labelledPattern:None, 
        			patterns:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternVariantContext<'input> = BaseParserRuleContext<'input,PatternVariantContextExt<'input>>;

pub trait PatternVariantContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_11
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_11
	fn Surrogate_id_SYMB_11(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_11, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_12
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_12
	fn Surrogate_id_SYMB_12(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_12, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
	fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_6, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternVariantContextAttrs<'input> for PatternVariantContext<'input>{}

pub struct PatternVariantContextExt<'input>{
	base:PatternContextExt<'input>,
	pub label: Option<TokenType<'input>>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternVariantContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternVariantContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternVariantContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternVariant(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternVariantContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternVariantContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternVariantContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternVariantContext<'input> {}

impl<'input> PatternVariantContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternVariantContext(
				BaseParserRuleContext::copy_from(ctx,PatternVariantContextExt{
					label:None, 
        			pattern_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternAscContext<'input> = BaseParserRuleContext<'input,PatternAscContextExt<'input>>;

pub trait PatternAscContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_36
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_36
	fn Surrogate_id_SYMB_36(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_36, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternAscContextAttrs<'input> for PatternAscContext<'input>{}

pub struct PatternAscContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternAscContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternAscContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternAscContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternAsc(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternAscContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternAscContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternAscContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternAscContext<'input> {}

impl<'input> PatternAscContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternAscContext(
				BaseParserRuleContext::copy_from(ctx,PatternAscContextExt{
        			pattern_:None, type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternIntContext<'input> = BaseParserRuleContext<'input,PatternIntContextExt<'input>>;

pub trait PatternIntContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token INTEGER
	/// Returns `None` if there is no child corresponding to token INTEGER
	fn INTEGER(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(INTEGER, 0)
	}
}

impl<'input> PatternIntContextAttrs<'input> for PatternIntContext<'input>{}

pub struct PatternIntContextExt<'input>{
	base:PatternContextExt<'input>,
	pub n: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternIntContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternIntContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternIntContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternInt(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternIntContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternIntContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternIntContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternIntContext<'input> {}

impl<'input> PatternIntContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternIntContext(
				BaseParserRuleContext::copy_from(ctx,PatternIntContextExt{
					n:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternInrContext<'input> = BaseParserRuleContext<'input,PatternInrContextExt<'input>>;

pub trait PatternInrContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_49
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_49
	fn Surrogate_id_SYMB_49(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_49, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternInrContextAttrs<'input> for PatternInrContext<'input>{}

pub struct PatternInrContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternInrContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternInrContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternInrContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternInr(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternInrContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternInrContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternInrContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternInrContext<'input> {}

impl<'input> PatternInrContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternInrContext(
				BaseParserRuleContext::copy_from(ctx,PatternInrContextExt{
        			pattern_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternTrueContext<'input> = BaseParserRuleContext<'input,PatternTrueContextExt<'input>>;

pub trait PatternTrueContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_60
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_60
	fn Surrogate_id_SYMB_60(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_60, 0)
	}
}

impl<'input> PatternTrueContextAttrs<'input> for PatternTrueContext<'input>{}

pub struct PatternTrueContextExt<'input>{
	base:PatternContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternTrueContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternTrueContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternTrueContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternTrue(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternTrueContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternTrueContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternTrueContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternTrueContext<'input> {}

impl<'input> PatternTrueContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternTrueContext(
				BaseParserRuleContext::copy_from(ctx,PatternTrueContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternInlContext<'input> = BaseParserRuleContext<'input,PatternInlContextExt<'input>>;

pub trait PatternInlContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_47
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_47
	fn Surrogate_id_SYMB_47(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_47, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternInlContextAttrs<'input> for PatternInlContext<'input>{}

pub struct PatternInlContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternInlContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternInlContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternInlContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternInl(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternInlContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternInlContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternInlContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternInlContext<'input> {}

impl<'input> PatternInlContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternInlContext(
				BaseParserRuleContext::copy_from(ctx,PatternInlContextExt{
        			pattern_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternVarContext<'input> = BaseParserRuleContext<'input,PatternVarContextExt<'input>>;

pub trait PatternVarContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
}

impl<'input> PatternVarContextAttrs<'input> for PatternVarContext<'input>{}

pub struct PatternVarContextExt<'input>{
	base:PatternContextExt<'input>,
	pub name: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternVarContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternVarContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternVarContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternVar(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternVarContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternVarContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternVarContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternVarContext<'input> {}

impl<'input> PatternVarContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternVarContext(
				BaseParserRuleContext::copy_from(ctx,PatternVarContextExt{
					name:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type ParenthesisedPatternContext<'input> = BaseParserRuleContext<'input,ParenthesisedPatternContextExt<'input>>;

pub trait ParenthesisedPatternContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> ParenthesisedPatternContextAttrs<'input> for ParenthesisedPatternContext<'input>{}

pub struct ParenthesisedPatternContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{ParenthesisedPatternContextExt<'a>}

impl<'input> stellaParserContext<'input> for ParenthesisedPatternContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for ParenthesisedPatternContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_ParenthesisedPattern(self);
	}
}

impl<'input> CustomRuleContext<'input> for ParenthesisedPatternContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for ParenthesisedPatternContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for ParenthesisedPatternContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for ParenthesisedPatternContext<'input> {}

impl<'input> ParenthesisedPatternContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::ParenthesisedPatternContext(
				BaseParserRuleContext::copy_from(ctx,ParenthesisedPatternContextExt{
        			pattern_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternSuccContext<'input> = BaseParserRuleContext<'input,PatternSuccContextExt<'input>>;

pub trait PatternSuccContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_57
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_57
	fn Surrogate_id_SYMB_57(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_57, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternSuccContextAttrs<'input> for PatternSuccContext<'input>{}

pub struct PatternSuccContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternSuccContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternSuccContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternSuccContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternSucc(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternSuccContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternSuccContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternSuccContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternSuccContext<'input> {}

impl<'input> PatternSuccContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternSuccContext(
				BaseParserRuleContext::copy_from(ctx,PatternSuccContextExt{
        			pattern_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternFalseContext<'input> = BaseParserRuleContext<'input,PatternFalseContextExt<'input>>;

pub trait PatternFalseContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_41
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_41
	fn Surrogate_id_SYMB_41(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_41, 0)
	}
}

impl<'input> PatternFalseContextAttrs<'input> for PatternFalseContext<'input>{}

pub struct PatternFalseContextExt<'input>{
	base:PatternContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternFalseContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternFalseContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternFalseContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternFalse(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternFalseContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternFalseContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternFalseContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternFalseContext<'input> {}

impl<'input> PatternFalseContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternFalseContext(
				BaseParserRuleContext::copy_from(ctx,PatternFalseContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternUnitContext<'input> = BaseParserRuleContext<'input,PatternUnitContextExt<'input>>;

pub trait PatternUnitContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_63
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_63
	fn Surrogate_id_SYMB_63(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_63, 0)
	}
}

impl<'input> PatternUnitContextAttrs<'input> for PatternUnitContext<'input>{}

pub struct PatternUnitContextExt<'input>{
	base:PatternContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternUnitContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternUnitContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternUnitContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternUnit(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternUnitContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternUnitContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternUnitContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternUnitContext<'input> {}

impl<'input> PatternUnitContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternUnitContext(
				BaseParserRuleContext::copy_from(ctx,PatternUnitContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type PatternCastAsContext<'input> = BaseParserRuleContext<'input,PatternCastAsContextExt<'input>>;

pub trait PatternCastAsContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token CAST
	/// Returns `None` if there is no child corresponding to token CAST
	fn CAST(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(CAST, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_36
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_36
	fn Surrogate_id_SYMB_36(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_36, 0)
	}
	fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> PatternCastAsContextAttrs<'input> for PatternCastAsContext<'input>{}

pub struct PatternCastAsContextExt<'input>{
	base:PatternContextExt<'input>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{PatternCastAsContextExt<'a>}

impl<'input> stellaParserContext<'input> for PatternCastAsContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for PatternCastAsContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_PatternCastAs(self);
	}
}

impl<'input> CustomRuleContext<'input> for PatternCastAsContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_pattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_pattern }
}

impl<'input> Borrow<PatternContextExt<'input>> for PatternCastAsContext<'input>{
	fn borrow(&self) -> &PatternContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<PatternContextExt<'input>> for PatternCastAsContext<'input>{
	fn borrow_mut(&mut self) -> &mut PatternContextExt<'input> { &mut self.base }
}

impl<'input> PatternContextAttrs<'input> for PatternCastAsContext<'input> {}

impl<'input> PatternCastAsContextExt<'input>{
	fn new(ctx: &dyn PatternContextAttrs<'input>) -> Rc<PatternContextAll<'input>>  {
		Rc::new(
			PatternContextAll::PatternCastAsContext(
				BaseParserRuleContext::copy_from(ctx,PatternCastAsContextExt{
        			pattern_:None, type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn  pattern(&mut self,)
	-> Result<Rc<PatternContextAll<'input>>,ANTLRError> {
		self.pattern_rec(0)
	}

	fn pattern_rec(&mut self, _p: isize)
	-> Result<Rc<PatternContextAll<'input>>,ANTLRError> {
		let recog = self;
		let _parentctx = recog.ctx.take();
		let _parentState = recog.base.get_state();
		let mut _localctx = PatternContextExt::new(_parentctx.clone(), recog.base.get_state());
		recog.base.enter_recursion_rule(_localctx.clone(), 26, RULE_pattern, _p);
	    let mut _localctx: Rc<PatternContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
		let _startState = 26;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {
			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(628);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(45,&mut recog.base)? {
				1 =>{
					{
					let mut tmp = PatternVariantContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();


					recog.base.set_state(554);
					recog.base.match_token(Surrogate_id_SYMB_11,&mut recog.err_handler)?;

					recog.base.set_state(555);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let PatternContextAll::PatternVariantContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.label = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(558);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==Surrogate_id_SYMB_6 {
						{
						recog.base.set_state(556);
						recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

						/*InvokeRule pattern*/
						recog.base.set_state(557);
						let tmp = recog.pattern_rec(0)?;
						if let PatternContextAll::PatternVariantContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						}
					}

					recog.base.set_state(560);
					recog.base.match_token(Surrogate_id_SYMB_12,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					{
					let mut tmp = PatternInlContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(561);
					recog.base.match_token(Surrogate_id_SYMB_47,&mut recog.err_handler)?;

					recog.base.set_state(562);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(563);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::PatternInlContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(564);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				3 =>{
					{
					let mut tmp = PatternInrContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(566);
					recog.base.match_token(Surrogate_id_SYMB_49,&mut recog.err_handler)?;

					recog.base.set_state(567);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(568);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::PatternInrContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(569);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				4 =>{
					{
					let mut tmp = PatternTupleContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(571);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(580);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13))) != 0) || ((((_la - 38)) & !0x3f) == 0 && ((1usize << (_la - 38)) & ((1usize << (Surrogate_id_SYMB_37 - 38)) | (1usize << (Surrogate_id_SYMB_41 - 38)) | (1usize << (Surrogate_id_SYMB_47 - 38)) | (1usize << (Surrogate_id_SYMB_49 - 38)) | (1usize << (Surrogate_id_SYMB_57 - 38)) | (1usize << (Surrogate_id_SYMB_60 - 38)) | (1usize << (Surrogate_id_SYMB_63 - 38)))) != 0) || _la==StellaIdent || _la==INTEGER {
						{
						/*InvokeRule pattern*/
						recog.base.set_state(572);
						let tmp = recog.pattern_rec(0)?;
						if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.pattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.pattern.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(577);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(573);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule pattern*/
							recog.base.set_state(574);
							let tmp = recog.pattern_rec(0)?;
							if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.pattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.pattern.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let PatternContextAll::PatternTupleContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(579);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(582);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				5 =>{
					{
					let mut tmp = PatternRecordContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(583);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(592);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==StellaIdent {
						{
						/*InvokeRule labelledPattern*/
						recog.base.set_state(584);
						let tmp = recog.labelledPattern()?;
						if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.labelledPattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.labelledPattern.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(589);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(585);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule labelledPattern*/
							recog.base.set_state(586);
							let tmp = recog.labelledPattern()?;
							if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.labelledPattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.labelledPattern.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let PatternContextAll::PatternRecordContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(591);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(594);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				6 =>{
					{
					let mut tmp = PatternListContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(595);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					recog.base.set_state(604);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13))) != 0) || ((((_la - 38)) & !0x3f) == 0 && ((1usize << (_la - 38)) & ((1usize << (Surrogate_id_SYMB_37 - 38)) | (1usize << (Surrogate_id_SYMB_41 - 38)) | (1usize << (Surrogate_id_SYMB_47 - 38)) | (1usize << (Surrogate_id_SYMB_49 - 38)) | (1usize << (Surrogate_id_SYMB_57 - 38)) | (1usize << (Surrogate_id_SYMB_60 - 38)) | (1usize << (Surrogate_id_SYMB_63 - 38)))) != 0) || _la==StellaIdent || _la==INTEGER {
						{
						/*InvokeRule pattern*/
						recog.base.set_state(596);
						let tmp = recog.pattern_rec(0)?;
						if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.pattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.pattern.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
						ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(601);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(597);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule pattern*/
							recog.base.set_state(598);
							let tmp = recog.pattern_rec(0)?;
							if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.pattern = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.pattern.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let PatternContextAll::PatternListContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.patterns.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(603);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(606);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					}
				}
			,
				7 =>{
					{
					let mut tmp = PatternConsContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(607);
					recog.base.match_token(Surrogate_id_SYMB_37,&mut recog.err_handler)?;

					recog.base.set_state(608);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(609);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::PatternConsContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.head = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(610);
					recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(611);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::PatternConsContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.tail = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(612);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				8 =>{
					{
					let mut tmp = PatternFalseContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(614);
					recog.base.match_token(Surrogate_id_SYMB_41,&mut recog.err_handler)?;

					}
				}
			,
				9 =>{
					{
					let mut tmp = PatternTrueContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(615);
					recog.base.match_token(Surrogate_id_SYMB_60,&mut recog.err_handler)?;

					}
				}
			,
				10 =>{
					{
					let mut tmp = PatternUnitContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(616);
					recog.base.match_token(Surrogate_id_SYMB_63,&mut recog.err_handler)?;

					}
				}
			,
				11 =>{
					{
					let mut tmp = PatternIntContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(617);
					let tmp = recog.base.match_token(INTEGER,&mut recog.err_handler)?;
					if let PatternContextAll::PatternIntContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.n = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				12 =>{
					{
					let mut tmp = PatternSuccContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(618);
					recog.base.match_token(Surrogate_id_SYMB_57,&mut recog.err_handler)?;

					recog.base.set_state(619);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(620);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::PatternSuccContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(621);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}
			,
				13 =>{
					{
					let mut tmp = PatternVarContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(623);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let PatternContextAll::PatternVarContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				14 =>{
					{
					let mut tmp = ParenthesisedPatternContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(624);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule pattern*/
					recog.base.set_state(625);
					let tmp = recog.pattern_rec(0)?;
					if let PatternContextAll::ParenthesisedPatternContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
					ctx.pattern_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(626);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}

				_ => {}
			}

			let tmp = recog.input.lt(-1).cloned();
			recog.ctx.as_ref().unwrap().set_stop(tmp);
			recog.base.set_state(639);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(47,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					recog.trigger_exit_rule_event();
					_prevctx = _localctx.clone();
					{
					recog.base.set_state(637);
					recog.err_handler.sync(&mut recog.base)?;
					match  recog.interpreter.adaptive_predict(46,&mut recog.base)? {
						1 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = PatternAscContextExt::new(&**PatternContextExt::new(_parentctx.clone(), _parentState));
							if let PatternContextAll::PatternAscContext(ctx) = cast_mut::<_,PatternContextAll >(&mut tmp){
								ctx.pattern_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_pattern);
							_localctx = tmp;
							recog.base.set_state(630);
							if !({recog.precpred(None, 3)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 3)".to_owned()), None))?;
							}
							recog.base.set_state(631);
							recog.base.match_token(Surrogate_id_SYMB_36,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(632);
							let tmp = recog.stellatype_rec(0)?;
							if let PatternContextAll::PatternAscContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}
					,
						2 =>{
							{
							/*recRuleLabeledAltStartAction*/
							let mut tmp = PatternCastAsContextExt::new(&**PatternContextExt::new(_parentctx.clone(), _parentState));
							if let PatternContextAll::PatternCastAsContext(ctx) = cast_mut::<_,PatternContextAll >(&mut tmp){
								ctx.pattern_ = Some(_prevctx.clone());
							} else {unreachable!("cant cast");}
							recog.push_new_recursion_context(tmp.clone(), _startState, RULE_pattern);
							_localctx = tmp;
							recog.base.set_state(633);
							if !({recog.precpred(None, 2)}) {
								Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 2)".to_owned()), None))?;
							}
							recog.base.set_state(634);
							recog.base.match_token(CAST,&mut recog.err_handler)?;

							recog.base.set_state(635);
							recog.base.match_token(Surrogate_id_SYMB_36,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(636);
							let tmp = recog.stellatype_rec(0)?;
							if let PatternContextAll::PatternCastAsContext(ctx) = cast_mut::<_,PatternContextAll >(&mut _localctx){
							ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							}
						}

						_ => {}
					}
					} 
				}
				recog.base.set_state(641);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(47,&mut recog.base)?;
			}
			}
			Ok(())
		})();
		match result {
		Ok(_) => {},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re)=>{
			//_localctx.exception = re;
			recog.err_handler.report_error(&mut recog.base, re);
	        recog.err_handler.recover(&mut recog.base, re)?;}
		}
		recog.base.unroll_recursion_context(_parentctx);

		Ok(_localctx)
	}
}
//------------------- labelledPattern ----------------
pub type LabelledPatternContextAll<'input> = LabelledPatternContext<'input>;


pub type LabelledPatternContext<'input> = BaseParserRuleContext<'input,LabelledPatternContextExt<'input>>;

#[derive(Clone)]
pub struct LabelledPatternContextExt<'input>{
	pub label: Option<TokenType<'input>>,
	pub pattern_: Option<Rc<PatternContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for LabelledPatternContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for LabelledPatternContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_labelledPattern(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_labelledPattern(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for LabelledPatternContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_labelledPattern }
	//fn type_rule_index() -> usize where Self: Sized { RULE_labelledPattern }
}
antlr_rust::tid!{LabelledPatternContextExt<'a>}

impl<'input> LabelledPatternContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<LabelledPatternContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,LabelledPatternContextExt{
				label: None, 
				pattern_: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait LabelledPatternContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<LabelledPatternContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_6
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_6
fn Surrogate_id_SYMB_6(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_6, 0)
}
/// Retrieves first TerminalNode corresponding to token StellaIdent
/// Returns `None` if there is no child corresponding to token StellaIdent
fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(StellaIdent, 0)
}
fn pattern(&self) -> Option<Rc<PatternContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> LabelledPatternContextAttrs<'input> for LabelledPatternContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn labelledPattern(&mut self,)
	-> Result<Rc<LabelledPatternContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = LabelledPatternContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 28, RULE_labelledPattern);
        let mut _localctx: Rc<LabelledPatternContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(642);
			let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
			 cast_mut::<_,LabelledPatternContext >(&mut _localctx).label = Some(tmp.clone());
			  

			recog.base.set_state(643);
			recog.base.match_token(Surrogate_id_SYMB_6,&mut recog.err_handler)?;

			/*InvokeRule pattern*/
			recog.base.set_state(644);
			let tmp = recog.pattern_rec(0)?;
			 cast_mut::<_,LabelledPatternContext >(&mut _localctx).pattern_ = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- stellatype ----------------
#[derive(Debug)]
pub enum StellatypeContextAll<'input>{
	TypeTupleContext(TypeTupleContext<'input>),
	TypeTopContext(TypeTopContext<'input>),
	TypeBoolContext(TypeBoolContext<'input>),
	TypeRefContext(TypeRefContext<'input>),
	TypeRecContext(TypeRecContext<'input>),
	TypeAutoContext(TypeAutoContext<'input>),
	TypeSumContext(TypeSumContext<'input>),
	TypeVarContext(TypeVarContext<'input>),
	TypeVariantContext(TypeVariantContext<'input>),
	TypeUnitContext(TypeUnitContext<'input>),
	TypeNatContext(TypeNatContext<'input>),
	TypeBottomContext(TypeBottomContext<'input>),
	TypeParensContext(TypeParensContext<'input>),
	TypeFunContext(TypeFunContext<'input>),
	TypeForAllContext(TypeForAllContext<'input>),
	TypeRecordContext(TypeRecordContext<'input>),
	TypeListContext(TypeListContext<'input>),
Error(StellatypeContext<'input>)
}
antlr_rust::tid!{StellatypeContextAll<'a>}

impl<'input> antlr_rust::parser_rule_context::DerefSeal for StellatypeContextAll<'input>{}

impl<'input> stellaParserContext<'input> for StellatypeContextAll<'input>{}

impl<'input> Deref for StellatypeContextAll<'input>{
	type Target = dyn StellatypeContextAttrs<'input> + 'input;
	fn deref(&self) -> &Self::Target{
		use StellatypeContextAll::*;
		match self{
			TypeTupleContext(inner) => inner,
			TypeTopContext(inner) => inner,
			TypeBoolContext(inner) => inner,
			TypeRefContext(inner) => inner,
			TypeRecContext(inner) => inner,
			TypeAutoContext(inner) => inner,
			TypeSumContext(inner) => inner,
			TypeVarContext(inner) => inner,
			TypeVariantContext(inner) => inner,
			TypeUnitContext(inner) => inner,
			TypeNatContext(inner) => inner,
			TypeBottomContext(inner) => inner,
			TypeParensContext(inner) => inner,
			TypeFunContext(inner) => inner,
			TypeForAllContext(inner) => inner,
			TypeRecordContext(inner) => inner,
			TypeListContext(inner) => inner,
Error(inner) => inner
		}
	}
}
impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for StellatypeContextAll<'input>{
    fn enter(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().enter(listener) }
    fn exit(&self, listener: &mut (dyn stellaParserListener<'input> + 'a)) { self.deref().exit(listener) }
}



pub type StellatypeContext<'input> = BaseParserRuleContext<'input,StellatypeContextExt<'input>>;

#[derive(Clone)]
pub struct StellatypeContextExt<'input>{
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for StellatypeContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for StellatypeContext<'input>{
}

impl<'input> CustomRuleContext<'input> for StellatypeContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}
antlr_rust::tid!{StellatypeContextExt<'a>}

impl<'input> StellatypeContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<StellatypeContextAll<'input>> {
		Rc::new(
		StellatypeContextAll::Error(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,StellatypeContextExt{
				ph:PhantomData
			}),
		)
		)
	}
}

pub trait StellatypeContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<StellatypeContextExt<'input>>{


}

impl<'input> StellatypeContextAttrs<'input> for StellatypeContext<'input>{}

pub type TypeTupleContext<'input> = BaseParserRuleContext<'input,TypeTupleContextExt<'input>>;

pub trait TypeTupleContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeTupleContextAttrs<'input> for TypeTupleContext<'input>{}

pub struct TypeTupleContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub stellatype: Option<Rc<StellatypeContextAll<'input>>>,
	pub types:Vec<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeTupleContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeTupleContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeTupleContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeTuple(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeTupleContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeTupleContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeTupleContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeTupleContext<'input> {}

impl<'input> TypeTupleContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeTupleContext(
				BaseParserRuleContext::copy_from(ctx,TypeTupleContextExt{
        			stellatype:None, 
        			types:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeTopContext<'input> = BaseParserRuleContext<'input,TypeTopContextExt<'input>>;

pub trait TypeTopContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token TOP_TYPE
	/// Returns `None` if there is no child corresponding to token TOP_TYPE
	fn TOP_TYPE(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(TOP_TYPE, 0)
	}
}

impl<'input> TypeTopContextAttrs<'input> for TypeTopContext<'input>{}

pub struct TypeTopContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeTopContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeTopContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeTopContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeTop(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeTopContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeTopContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeTopContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeTopContext<'input> {}

impl<'input> TypeTopContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeTopContext(
				BaseParserRuleContext::copy_from(ctx,TypeTopContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeBoolContext<'input> = BaseParserRuleContext<'input,TypeBoolContextExt<'input>>;

pub trait TypeBoolContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_32
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_32
	fn Surrogate_id_SYMB_32(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_32, 0)
	}
}

impl<'input> TypeBoolContextAttrs<'input> for TypeBoolContext<'input>{}

pub struct TypeBoolContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeBoolContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeBoolContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeBoolContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeBool(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeBoolContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeBoolContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeBoolContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeBoolContext<'input> {}

impl<'input> TypeBoolContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeBoolContext(
				BaseParserRuleContext::copy_from(ctx,TypeBoolContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeRefContext<'input> = BaseParserRuleContext<'input,TypeRefContextExt<'input>>;

pub trait TypeRefContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token REF_TYPE
	/// Returns `None` if there is no child corresponding to token REF_TYPE
	fn REF_TYPE(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(REF_TYPE, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeRefContextAttrs<'input> for TypeRefContext<'input>{}

pub struct TypeRefContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeRefContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeRefContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeRefContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeRef(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeRefContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeRefContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeRefContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeRefContext<'input> {}

impl<'input> TypeRefContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeRefContext(
				BaseParserRuleContext::copy_from(ctx,TypeRefContextExt{
        			type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeRecContext<'input> = BaseParserRuleContext<'input,TypeRecContextExt<'input>>;

pub trait TypeRecContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_65
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_65
	fn Surrogate_id_SYMB_65(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_65, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_25
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_25
	fn Surrogate_id_SYMB_25(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_25, 0)
	}
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeRecContextAttrs<'input> for TypeRecContext<'input>{}

pub struct TypeRecContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub var: Option<TokenType<'input>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeRecContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeRecContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeRecContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeRec(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeRecContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeRecContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeRecContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeRecContext<'input> {}

impl<'input> TypeRecContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeRecContext(
				BaseParserRuleContext::copy_from(ctx,TypeRecContextExt{
					var:None, 
        			type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeAutoContext<'input> = BaseParserRuleContext<'input,TypeAutoContextExt<'input>>;

pub trait TypeAutoContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token AUTO_TYPE
	/// Returns `None` if there is no child corresponding to token AUTO_TYPE
	fn AUTO_TYPE(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(AUTO_TYPE, 0)
	}
}

impl<'input> TypeAutoContextAttrs<'input> for TypeAutoContext<'input>{}

pub struct TypeAutoContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeAutoContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeAutoContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeAutoContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeAuto(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeAutoContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeAutoContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeAutoContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeAutoContext<'input> {}

impl<'input> TypeAutoContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeAutoContext(
				BaseParserRuleContext::copy_from(ctx,TypeAutoContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeSumContext<'input> = BaseParserRuleContext<'input,TypeSumContextExt<'input>>;

pub trait TypeSumContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_21
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_21
	fn Surrogate_id_SYMB_21(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_21, 0)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
}

impl<'input> TypeSumContextAttrs<'input> for TypeSumContext<'input>{}

pub struct TypeSumContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub left: Option<Rc<StellatypeContextAll<'input>>>,
	pub right: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeSumContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeSumContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeSumContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeSum(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeSumContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeSumContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeSumContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeSumContext<'input> {}

impl<'input> TypeSumContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeSumContext(
				BaseParserRuleContext::copy_from(ctx,TypeSumContextExt{
        			left:None, right:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeVarContext<'input> = BaseParserRuleContext<'input,TypeVarContextExt<'input>>;

pub trait TypeVarContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token StellaIdent
	/// Returns `None` if there is no child corresponding to token StellaIdent
	fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, 0)
	}
}

impl<'input> TypeVarContextAttrs<'input> for TypeVarContext<'input>{}

pub struct TypeVarContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub name: Option<TokenType<'input>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeVarContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeVarContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeVarContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeVar(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeVarContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeVarContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeVarContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeVarContext<'input> {}

impl<'input> TypeVarContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeVarContext(
				BaseParserRuleContext::copy_from(ctx,TypeVarContextExt{
					name:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeVariantContext<'input> = BaseParserRuleContext<'input,TypeVariantContextExt<'input>>;

pub trait TypeVariantContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_11
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_11
	fn Surrogate_id_SYMB_11(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_11, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_12
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_12
	fn Surrogate_id_SYMB_12(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_12, 0)
	}
	fn variantFieldType_all(&self) ->  Vec<Rc<VariantFieldTypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn variantFieldType(&self, i: usize) -> Option<Rc<VariantFieldTypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeVariantContextAttrs<'input> for TypeVariantContext<'input>{}

pub struct TypeVariantContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub variantFieldType: Option<Rc<VariantFieldTypeContextAll<'input>>>,
	pub fieldTypes:Vec<Rc<VariantFieldTypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeVariantContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeVariantContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeVariantContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeVariant(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeVariantContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeVariantContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeVariantContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeVariantContext<'input> {}

impl<'input> TypeVariantContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeVariantContext(
				BaseParserRuleContext::copy_from(ctx,TypeVariantContextExt{
        			variantFieldType:None, 
        			fieldTypes:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeUnitContext<'input> = BaseParserRuleContext<'input,TypeUnitContextExt<'input>>;

pub trait TypeUnitContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_34
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_34
	fn Surrogate_id_SYMB_34(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_34, 0)
	}
}

impl<'input> TypeUnitContextAttrs<'input> for TypeUnitContext<'input>{}

pub struct TypeUnitContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeUnitContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeUnitContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeUnitContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeUnit(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeUnitContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeUnitContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeUnitContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeUnitContext<'input> {}

impl<'input> TypeUnitContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeUnitContext(
				BaseParserRuleContext::copy_from(ctx,TypeUnitContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeNatContext<'input> = BaseParserRuleContext<'input,TypeNatContextExt<'input>>;

pub trait TypeNatContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_33
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_33
	fn Surrogate_id_SYMB_33(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_33, 0)
	}
}

impl<'input> TypeNatContextAttrs<'input> for TypeNatContext<'input>{}

pub struct TypeNatContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeNatContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeNatContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeNatContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeNat(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeNatContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeNatContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeNatContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeNatContext<'input> {}

impl<'input> TypeNatContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeNatContext(
				BaseParserRuleContext::copy_from(ctx,TypeNatContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeBottomContext<'input> = BaseParserRuleContext<'input,TypeBottomContextExt<'input>>;

pub trait TypeBottomContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token BOTTOM_TYPE
	/// Returns `None` if there is no child corresponding to token BOTTOM_TYPE
	fn BOTTOM_TYPE(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(BOTTOM_TYPE, 0)
	}
}

impl<'input> TypeBottomContextAttrs<'input> for TypeBottomContext<'input>{}

pub struct TypeBottomContextExt<'input>{
	base:StellatypeContextExt<'input>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeBottomContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeBottomContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeBottomContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeBottom(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeBottomContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeBottomContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeBottomContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeBottomContext<'input> {}

impl<'input> TypeBottomContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeBottomContext(
				BaseParserRuleContext::copy_from(ctx,TypeBottomContextExt{
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeParensContext<'input> = BaseParserRuleContext<'input,TypeParensContextExt<'input>>;

pub trait TypeParensContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeParensContextAttrs<'input> for TypeParensContext<'input>{}

pub struct TypeParensContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeParensContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeParensContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeParensContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeParens(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeParensContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeParensContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeParensContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeParensContext<'input> {}

impl<'input> TypeParensContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeParensContext(
				BaseParserRuleContext::copy_from(ctx,TypeParensContextExt{
        			type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeFunContext<'input> = BaseParserRuleContext<'input,TypeFunContextExt<'input>>;

pub trait TypeFunContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_43
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_43
	fn Surrogate_id_SYMB_43(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_43, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_2
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_2
	fn Surrogate_id_SYMB_2(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_2, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_3
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_3
	fn Surrogate_id_SYMB_3(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_3, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_8
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_8
	fn Surrogate_id_SYMB_8(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_8, 0)
	}
	fn stellatype_all(&self) ->  Vec<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn stellatype(&self, i: usize) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeFunContextAttrs<'input> for TypeFunContext<'input>{}

pub struct TypeFunContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub stellatype: Option<Rc<StellatypeContextAll<'input>>>,
	pub paramTypes:Vec<Rc<StellatypeContextAll<'input>>>,
	pub returnType: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeFunContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeFunContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeFunContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeFun(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeFunContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeFunContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeFunContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeFunContext<'input> {}

impl<'input> TypeFunContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeFunContext(
				BaseParserRuleContext::copy_from(ctx,TypeFunContextExt{
        			stellatype:None, returnType:None, 
        			paramTypes:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeForAllContext<'input> = BaseParserRuleContext<'input,TypeForAllContextExt<'input>>;

pub trait TypeForAllContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token FORALL
	/// Returns `None` if there is no child corresponding to token FORALL
	fn FORALL(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(FORALL, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_25
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_25
	fn Surrogate_id_SYMB_25(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_25, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
	/// Retrieves all `TerminalNode`s corresponding to token StellaIdent in current rule
	fn StellaIdent_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token StellaIdent, starting from 0.
	/// Returns `None` if number of children corresponding to token StellaIdent is less or equal than `i`.
	fn StellaIdent(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(StellaIdent, i)
	}
}

impl<'input> TypeForAllContextAttrs<'input> for TypeForAllContext<'input>{}

pub struct TypeForAllContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub StellaIdent: Option<TokenType<'input>>,
	pub types:Vec<TokenType<'input>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeForAllContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeForAllContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeForAllContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeForAll(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeForAllContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeForAllContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeForAllContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeForAllContext<'input> {}

impl<'input> TypeForAllContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeForAllContext(
				BaseParserRuleContext::copy_from(ctx,TypeForAllContextExt{
					StellaIdent:None, 
        			types:Vec::new(), 
        			type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeRecordContext<'input> = BaseParserRuleContext<'input,TypeRecordContextExt<'input>>;

pub trait TypeRecordContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_4
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_4
	fn Surrogate_id_SYMB_4(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_4, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_5
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_5
	fn Surrogate_id_SYMB_5(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_5, 0)
	}
	fn recordFieldType_all(&self) ->  Vec<Rc<RecordFieldTypeContextAll<'input>>> where Self:Sized{
		self.children_of_type()
	}
	fn recordFieldType(&self, i: usize) -> Option<Rc<RecordFieldTypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(i)
	}
	/// Retrieves all `TerminalNode`s corresponding to token Surrogate_id_SYMB_0 in current rule
	fn Surrogate_id_SYMB_0_all(&self) -> Vec<Rc<TerminalNode<'input,stellaParserContextType>>>  where Self:Sized{
		self.children_of_type()
	}
	/// Retrieves 'i's TerminalNode corresponding to token Surrogate_id_SYMB_0, starting from 0.
	/// Returns `None` if number of children corresponding to token Surrogate_id_SYMB_0 is less or equal than `i`.
	fn Surrogate_id_SYMB_0(&self, i: usize) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_0, i)
	}
}

impl<'input> TypeRecordContextAttrs<'input> for TypeRecordContext<'input>{}

pub struct TypeRecordContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub recordFieldType: Option<Rc<RecordFieldTypeContextAll<'input>>>,
	pub fieldTypes:Vec<Rc<RecordFieldTypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeRecordContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeRecordContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeRecordContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeRecord(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeRecordContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeRecordContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeRecordContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeRecordContext<'input> {}

impl<'input> TypeRecordContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeRecordContext(
				BaseParserRuleContext::copy_from(ctx,TypeRecordContextExt{
        			recordFieldType:None, 
        			fieldTypes:Vec::new(), 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

pub type TypeListContext<'input> = BaseParserRuleContext<'input,TypeListContextExt<'input>>;

pub trait TypeListContextAttrs<'input>: stellaParserContext<'input>{
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_13
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_13
	fn Surrogate_id_SYMB_13(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_13, 0)
	}
	/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_14
	/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_14
	fn Surrogate_id_SYMB_14(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
		self.get_token(Surrogate_id_SYMB_14, 0)
	}
	fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
		self.child_of_type(0)
	}
}

impl<'input> TypeListContextAttrs<'input> for TypeListContext<'input>{}

pub struct TypeListContextExt<'input>{
	base:StellatypeContextExt<'input>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
	ph:PhantomData<&'input str>
}

antlr_rust::tid!{TypeListContextExt<'a>}

impl<'input> stellaParserContext<'input> for TypeListContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for TypeListContext<'input>{
	fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
		listener.enter_every_rule(self);
		listener.enter_TypeList(self);
	}
}

impl<'input> CustomRuleContext<'input> for TypeListContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_stellatype }
	//fn type_rule_index() -> usize where Self: Sized { RULE_stellatype }
}

impl<'input> Borrow<StellatypeContextExt<'input>> for TypeListContext<'input>{
	fn borrow(&self) -> &StellatypeContextExt<'input> { &self.base }
}
impl<'input> BorrowMut<StellatypeContextExt<'input>> for TypeListContext<'input>{
	fn borrow_mut(&mut self) -> &mut StellatypeContextExt<'input> { &mut self.base }
}

impl<'input> StellatypeContextAttrs<'input> for TypeListContext<'input> {}

impl<'input> TypeListContextExt<'input>{
	fn new(ctx: &dyn StellatypeContextAttrs<'input>) -> Rc<StellatypeContextAll<'input>>  {
		Rc::new(
			StellatypeContextAll::TypeListContext(
				BaseParserRuleContext::copy_from(ctx,TypeListContextExt{
        			type_:None, 
        			base: ctx.borrow().clone(),
        			ph:PhantomData
				})
			)
		)
	}
}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn  stellatype(&mut self,)
	-> Result<Rc<StellatypeContextAll<'input>>,ANTLRError> {
		self.stellatype_rec(0)
	}

	fn stellatype_rec(&mut self, _p: isize)
	-> Result<Rc<StellatypeContextAll<'input>>,ANTLRError> {
		let recog = self;
		let _parentctx = recog.ctx.take();
		let _parentState = recog.base.get_state();
		let mut _localctx = StellatypeContextExt::new(_parentctx.clone(), recog.base.get_state());
		recog.base.enter_recursion_rule(_localctx.clone(), 30, RULE_stellatype, _p);
	    let mut _localctx: Rc<StellatypeContextAll> = _localctx;
        let mut _prevctx = _localctx.clone();
		let _startState = 30;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {
			let mut _alt: isize;
			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(727);
			recog.err_handler.sync(&mut recog.base)?;
			match  recog.interpreter.adaptive_predict(56,&mut recog.base)? {
				1 =>{
					{
					let mut tmp = TypeBoolContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();


					recog.base.set_state(647);
					recog.base.match_token(Surrogate_id_SYMB_32,&mut recog.err_handler)?;

					}
				}
			,
				2 =>{
					{
					let mut tmp = TypeNatContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(648);
					recog.base.match_token(Surrogate_id_SYMB_33,&mut recog.err_handler)?;

					}
				}
			,
				3 =>{
					{
					let mut tmp = TypeRefContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(649);
					recog.base.match_token(REF_TYPE,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(650);
					let tmp = recog.stellatype_rec(15)?;
					if let StellatypeContextAll::TypeRefContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				4 =>{
					{
					let mut tmp = TypeFunContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(651);
					recog.base.match_token(Surrogate_id_SYMB_43,&mut recog.err_handler)?;

					recog.base.set_state(652);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					recog.base.set_state(661);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13))) != 0) || ((((_la - 33)) & !0x3f) == 0 && ((1usize << (_la - 33)) & ((1usize << (Surrogate_id_SYMB_32 - 33)) | (1usize << (Surrogate_id_SYMB_33 - 33)) | (1usize << (Surrogate_id_SYMB_34 - 33)) | (1usize << (Surrogate_id_SYMB_43 - 33)))) != 0) || ((((_la - 66)) & !0x3f) == 0 && ((1usize << (_la - 66)) & ((1usize << (Surrogate_id_SYMB_65 - 66)) | (1usize << (REF_TYPE - 66)) | (1usize << (TOP_TYPE - 66)) | (1usize << (BOTTOM_TYPE - 66)) | (1usize << (AUTO_TYPE - 66)) | (1usize << (FORALL - 66)) | (1usize << (StellaIdent - 66)))) != 0) {
						{
						/*InvokeRule stellatype*/
						recog.base.set_state(653);
						let tmp = recog.stellatype_rec(0)?;
						if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.paramTypes.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(658);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(654);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(655);
							let tmp = recog.stellatype_rec(0)?;
							if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.paramTypes.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(660);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(663);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					recog.base.set_state(664);
					recog.base.match_token(Surrogate_id_SYMB_8,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(665);
					let tmp = recog.stellatype_rec(13)?;
					if let StellatypeContextAll::TypeFunContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.returnType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				5 =>{
					{
					let mut tmp = TypeForAllContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(666);
					recog.base.match_token(FORALL,&mut recog.err_handler)?;

					recog.base.set_state(670);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==StellaIdent {
						{
						{
						recog.base.set_state(667);
						let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
						if let StellatypeContextAll::TypeForAllContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.StellaIdent = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let StellatypeContextAll::TypeForAllContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.StellaIdent.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let StellatypeContextAll::TypeForAllContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.types.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(672);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(673);
					recog.base.match_token(Surrogate_id_SYMB_25,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(674);
					let tmp = recog.stellatype_rec(12)?;
					if let StellatypeContextAll::TypeForAllContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				6 =>{
					{
					let mut tmp = TypeRecContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(675);
					recog.base.match_token(Surrogate_id_SYMB_65,&mut recog.err_handler)?;

					recog.base.set_state(676);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let StellatypeContextAll::TypeRecContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.var = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(677);
					recog.base.match_token(Surrogate_id_SYMB_25,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(678);
					let tmp = recog.stellatype_rec(11)?;
					if let StellatypeContextAll::TypeRecContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				7 =>{
					{
					let mut tmp = TypeTupleContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(679);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					recog.base.set_state(688);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if (((_la) & !0x3f) == 0 && ((1usize << _la) & ((1usize << Surrogate_id_SYMB_2) | (1usize << Surrogate_id_SYMB_4) | (1usize << Surrogate_id_SYMB_11) | (1usize << Surrogate_id_SYMB_13))) != 0) || ((((_la - 33)) & !0x3f) == 0 && ((1usize << (_la - 33)) & ((1usize << (Surrogate_id_SYMB_32 - 33)) | (1usize << (Surrogate_id_SYMB_33 - 33)) | (1usize << (Surrogate_id_SYMB_34 - 33)) | (1usize << (Surrogate_id_SYMB_43 - 33)))) != 0) || ((((_la - 66)) & !0x3f) == 0 && ((1usize << (_la - 66)) & ((1usize << (Surrogate_id_SYMB_65 - 66)) | (1usize << (REF_TYPE - 66)) | (1usize << (TOP_TYPE - 66)) | (1usize << (BOTTOM_TYPE - 66)) | (1usize << (AUTO_TYPE - 66)) | (1usize << (FORALL - 66)) | (1usize << (StellaIdent - 66)))) != 0) {
						{
						/*InvokeRule stellatype*/
						recog.base.set_state(680);
						let tmp = recog.stellatype_rec(0)?;
						if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.types.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(685);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(681);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule stellatype*/
							recog.base.set_state(682);
							let tmp = recog.stellatype_rec(0)?;
							if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.stellatype = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.stellatype.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let StellatypeContextAll::TypeTupleContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.types.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(687);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(690);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				8 =>{
					{
					let mut tmp = TypeRecordContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(691);
					recog.base.match_token(Surrogate_id_SYMB_4,&mut recog.err_handler)?;

					/*InvokeRule recordFieldType*/
					recog.base.set_state(692);
					let tmp = recog.recordFieldType()?;
					if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.recordFieldType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					let temp = if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.recordFieldType.clone().unwrap() } else {unreachable!("cant cast");} ;
					if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.fieldTypes.push(temp); } else {unreachable!("cant cast");}  
					recog.base.set_state(697);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					while _la==Surrogate_id_SYMB_0 {
						{
						{
						recog.base.set_state(693);
						recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

						/*InvokeRule recordFieldType*/
						recog.base.set_state(694);
						let tmp = recog.recordFieldType()?;
						if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.recordFieldType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.recordFieldType.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let StellatypeContextAll::TypeRecordContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.fieldTypes.push(temp); } else {unreachable!("cant cast");}  
						}
						}
						recog.base.set_state(699);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
					}
					recog.base.set_state(700);
					recog.base.match_token(Surrogate_id_SYMB_5,&mut recog.err_handler)?;

					}
				}
			,
				9 =>{
					{
					let mut tmp = TypeVariantContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(702);
					recog.base.match_token(Surrogate_id_SYMB_11,&mut recog.err_handler)?;

					recog.base.set_state(711);
					recog.err_handler.sync(&mut recog.base)?;
					_la = recog.base.input.la(1);
					if _la==StellaIdent {
						{
						/*InvokeRule variantFieldType*/
						recog.base.set_state(703);
						let tmp = recog.variantFieldType()?;
						if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.variantFieldType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

						let temp = if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.variantFieldType.clone().unwrap() } else {unreachable!("cant cast");} ;
						if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
						ctx.fieldTypes.push(temp); } else {unreachable!("cant cast");}  
						recog.base.set_state(708);
						recog.err_handler.sync(&mut recog.base)?;
						_la = recog.base.input.la(1);
						while _la==Surrogate_id_SYMB_0 {
							{
							{
							recog.base.set_state(704);
							recog.base.match_token(Surrogate_id_SYMB_0,&mut recog.err_handler)?;

							/*InvokeRule variantFieldType*/
							recog.base.set_state(705);
							let tmp = recog.variantFieldType()?;
							if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.variantFieldType = Some(tmp.clone()); } else {unreachable!("cant cast");}  

							let temp = if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.variantFieldType.clone().unwrap() } else {unreachable!("cant cast");} ;
							if let StellatypeContextAll::TypeVariantContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
							ctx.fieldTypes.push(temp); } else {unreachable!("cant cast");}  
							}
							}
							recog.base.set_state(710);
							recog.err_handler.sync(&mut recog.base)?;
							_la = recog.base.input.la(1);
						}
						}
					}

					recog.base.set_state(713);
					recog.base.match_token(Surrogate_id_SYMB_12,&mut recog.err_handler)?;

					}
				}
			,
				10 =>{
					{
					let mut tmp = TypeListContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(714);
					recog.base.match_token(Surrogate_id_SYMB_13,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(715);
					let tmp = recog.stellatype_rec(0)?;
					if let StellatypeContextAll::TypeListContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(716);
					recog.base.match_token(Surrogate_id_SYMB_14,&mut recog.err_handler)?;

					}
				}
			,
				11 =>{
					{
					let mut tmp = TypeUnitContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(718);
					recog.base.match_token(Surrogate_id_SYMB_34,&mut recog.err_handler)?;

					}
				}
			,
				12 =>{
					{
					let mut tmp = TypeTopContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(719);
					recog.base.match_token(TOP_TYPE,&mut recog.err_handler)?;

					}
				}
			,
				13 =>{
					{
					let mut tmp = TypeBottomContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(720);
					recog.base.match_token(BOTTOM_TYPE,&mut recog.err_handler)?;

					}
				}
			,
				14 =>{
					{
					let mut tmp = TypeAutoContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(721);
					recog.base.match_token(AUTO_TYPE,&mut recog.err_handler)?;

					}
				}
			,
				15 =>{
					{
					let mut tmp = TypeVarContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(722);
					let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
					if let StellatypeContextAll::TypeVarContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.name = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
				}
			,
				16 =>{
					{
					let mut tmp = TypeParensContextExt::new(&**_localctx);
					recog.ctx = Some(tmp.clone());
					_localctx = tmp;
					_prevctx = _localctx.clone();
					recog.base.set_state(723);
					recog.base.match_token(Surrogate_id_SYMB_2,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(724);
					let tmp = recog.stellatype_rec(0)?;
					if let StellatypeContextAll::TypeParensContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.type_ = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					recog.base.set_state(725);
					recog.base.match_token(Surrogate_id_SYMB_3,&mut recog.err_handler)?;

					}
				}

				_ => {}
			}

			let tmp = recog.input.lt(-1).cloned();
			recog.ctx.as_ref().unwrap().set_stop(tmp);
			recog.base.set_state(734);
			recog.err_handler.sync(&mut recog.base)?;
			_alt = recog.interpreter.adaptive_predict(57,&mut recog.base)?;
			while { _alt!=2 && _alt!=INVALID_ALT } {
				if _alt==1 {
					recog.trigger_exit_rule_event();
					_prevctx = _localctx.clone();
					{
					{
					/*recRuleLabeledAltStartAction*/
					let mut tmp = TypeSumContextExt::new(&**StellatypeContextExt::new(_parentctx.clone(), _parentState));
					if let StellatypeContextAll::TypeSumContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut tmp){
						ctx.left = Some(_prevctx.clone());
					} else {unreachable!("cant cast");}
					recog.push_new_recursion_context(tmp.clone(), _startState, RULE_stellatype);
					_localctx = tmp;
					recog.base.set_state(729);
					if !({recog.precpred(None, 14)}) {
						Err(FailedPredicateError::new(&mut recog.base, Some("recog.precpred(None, 14)".to_owned()), None))?;
					}
					recog.base.set_state(730);
					recog.base.match_token(Surrogate_id_SYMB_21,&mut recog.err_handler)?;

					/*InvokeRule stellatype*/
					recog.base.set_state(731);
					let tmp = recog.stellatype_rec(15)?;
					if let StellatypeContextAll::TypeSumContext(ctx) = cast_mut::<_,StellatypeContextAll >(&mut _localctx){
					ctx.right = Some(tmp.clone()); } else {unreachable!("cant cast");}  

					}
					} 
				}
				recog.base.set_state(736);
				recog.err_handler.sync(&mut recog.base)?;
				_alt = recog.interpreter.adaptive_predict(57,&mut recog.base)?;
			}
			}
			Ok(())
		})();
		match result {
		Ok(_) => {},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re)=>{
			//_localctx.exception = re;
			recog.err_handler.report_error(&mut recog.base, re);
	        recog.err_handler.recover(&mut recog.base, re)?;}
		}
		recog.base.unroll_recursion_context(_parentctx);

		Ok(_localctx)
	}
}
//------------------- recordFieldType ----------------
pub type RecordFieldTypeContextAll<'input> = RecordFieldTypeContext<'input>;


pub type RecordFieldTypeContext<'input> = BaseParserRuleContext<'input,RecordFieldTypeContextExt<'input>>;

#[derive(Clone)]
pub struct RecordFieldTypeContextExt<'input>{
	pub label: Option<TokenType<'input>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for RecordFieldTypeContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for RecordFieldTypeContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_recordFieldType(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_recordFieldType(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for RecordFieldTypeContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_recordFieldType }
	//fn type_rule_index() -> usize where Self: Sized { RULE_recordFieldType }
}
antlr_rust::tid!{RecordFieldTypeContextExt<'a>}

impl<'input> RecordFieldTypeContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<RecordFieldTypeContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,RecordFieldTypeContextExt{
				label: None, 
				type_: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait RecordFieldTypeContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<RecordFieldTypeContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_7
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_7
fn Surrogate_id_SYMB_7(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_7, 0)
}
/// Retrieves first TerminalNode corresponding to token StellaIdent
/// Returns `None` if there is no child corresponding to token StellaIdent
fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(StellaIdent, 0)
}
fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> RecordFieldTypeContextAttrs<'input> for RecordFieldTypeContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn recordFieldType(&mut self,)
	-> Result<Rc<RecordFieldTypeContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = RecordFieldTypeContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 32, RULE_recordFieldType);
        let mut _localctx: Rc<RecordFieldTypeContextAll> = _localctx;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(737);
			let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
			 cast_mut::<_,RecordFieldTypeContext >(&mut _localctx).label = Some(tmp.clone());
			  

			recog.base.set_state(738);
			recog.base.match_token(Surrogate_id_SYMB_7,&mut recog.err_handler)?;

			/*InvokeRule stellatype*/
			recog.base.set_state(739);
			let tmp = recog.stellatype_rec(0)?;
			 cast_mut::<_,RecordFieldTypeContext >(&mut _localctx).type_ = Some(tmp.clone());
			  

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}
//------------------- variantFieldType ----------------
pub type VariantFieldTypeContextAll<'input> = VariantFieldTypeContext<'input>;


pub type VariantFieldTypeContext<'input> = BaseParserRuleContext<'input,VariantFieldTypeContextExt<'input>>;

#[derive(Clone)]
pub struct VariantFieldTypeContextExt<'input>{
	pub label: Option<TokenType<'input>>,
	pub type_: Option<Rc<StellatypeContextAll<'input>>>,
ph:PhantomData<&'input str>
}

impl<'input> stellaParserContext<'input> for VariantFieldTypeContext<'input>{}

impl<'input,'a> Listenable<dyn stellaParserListener<'input> + 'a> for VariantFieldTypeContext<'input>{
		fn enter(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.enter_every_rule(self);
			listener.enter_variantFieldType(self);
		}fn exit(&self,listener: &mut (dyn stellaParserListener<'input> + 'a)) {
			listener.exit_variantFieldType(self);
			listener.exit_every_rule(self);
		}
}

impl<'input> CustomRuleContext<'input> for VariantFieldTypeContextExt<'input>{
	type TF = LocalTokenFactory<'input>;
	type Ctx = stellaParserContextType;
	fn get_rule_index(&self) -> usize { RULE_variantFieldType }
	//fn type_rule_index() -> usize where Self: Sized { RULE_variantFieldType }
}
antlr_rust::tid!{VariantFieldTypeContextExt<'a>}

impl<'input> VariantFieldTypeContextExt<'input>{
	fn new(parent: Option<Rc<dyn stellaParserContext<'input> + 'input > >, invoking_state: isize) -> Rc<VariantFieldTypeContextAll<'input>> {
		Rc::new(
			BaseParserRuleContext::new_parser_ctx(parent, invoking_state,VariantFieldTypeContextExt{
				label: None, 
				type_: None, 
				ph:PhantomData
			}),
		)
	}
}

pub trait VariantFieldTypeContextAttrs<'input>: stellaParserContext<'input> + BorrowMut<VariantFieldTypeContextExt<'input>>{

/// Retrieves first TerminalNode corresponding to token StellaIdent
/// Returns `None` if there is no child corresponding to token StellaIdent
fn StellaIdent(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(StellaIdent, 0)
}
/// Retrieves first TerminalNode corresponding to token Surrogate_id_SYMB_7
/// Returns `None` if there is no child corresponding to token Surrogate_id_SYMB_7
fn Surrogate_id_SYMB_7(&self) -> Option<Rc<TerminalNode<'input,stellaParserContextType>>> where Self:Sized{
	self.get_token(Surrogate_id_SYMB_7, 0)
}
fn stellatype(&self) -> Option<Rc<StellatypeContextAll<'input>>> where Self:Sized{
	self.child_of_type(0)
}

}

impl<'input> VariantFieldTypeContextAttrs<'input> for VariantFieldTypeContext<'input>{}

impl<'input, I, H> stellaParser<'input, I, H>
where
    I: TokenStream<'input, TF = LocalTokenFactory<'input> > + TidAble<'input>,
    H: ErrorStrategy<'input,BaseParserType<'input,I>>
{
	pub fn variantFieldType(&mut self,)
	-> Result<Rc<VariantFieldTypeContextAll<'input>>,ANTLRError> {
		let mut recog = self;
		let _parentctx = recog.ctx.take();
		let mut _localctx = VariantFieldTypeContextExt::new(_parentctx.clone(), recog.base.get_state());
        recog.base.enter_rule(_localctx.clone(), 34, RULE_variantFieldType);
        let mut _localctx: Rc<VariantFieldTypeContextAll> = _localctx;
		let mut _la: isize = -1;
		let result: Result<(), ANTLRError> = (|| {

			//recog.base.enter_outer_alt(_localctx.clone(), 1);
			recog.base.enter_outer_alt(None, 1);
			{
			recog.base.set_state(741);
			let tmp = recog.base.match_token(StellaIdent,&mut recog.err_handler)?;
			 cast_mut::<_,VariantFieldTypeContext >(&mut _localctx).label = Some(tmp.clone());
			  

			recog.base.set_state(744);
			recog.err_handler.sync(&mut recog.base)?;
			_la = recog.base.input.la(1);
			if _la==Surrogate_id_SYMB_7 {
				{
				recog.base.set_state(742);
				recog.base.match_token(Surrogate_id_SYMB_7,&mut recog.err_handler)?;

				/*InvokeRule stellatype*/
				recog.base.set_state(743);
				let tmp = recog.stellatype_rec(0)?;
				 cast_mut::<_,VariantFieldTypeContext >(&mut _localctx).type_ = Some(tmp.clone());
				  

				}
			}

			}
			Ok(())
		})();
		match result {
		Ok(_)=>{},
        Err(e @ ANTLRError::FallThrough(_)) => return Err(e),
		Err(ref re) => {
				//_localctx.exception = re;
				recog.err_handler.report_error(&mut recog.base, re);
				recog.err_handler.recover(&mut recog.base, re)?;
			}
		}
		recog.base.exit_rule();

		Ok(_localctx)
	}
}

lazy_static! {
    static ref _ATN: Arc<ATN> =
        Arc::new(ATNDeserializer::new(None).deserialize(_serializedATN.chars()));
    static ref _decision_to_DFA: Arc<Vec<antlr_rust::RwLock<DFA>>> = {
        let mut dfa = Vec::new();
        let size = _ATN.decision_to_state.len();
        for i in 0..size {
            dfa.push(DFA::new(
                _ATN.clone(),
                _ATN.get_decision_state(i),
                i as isize,
            ).into())
        }
        Arc::new(dfa)
    };
}



const _serializedATN:&'static str =
	"\x03\u{608b}\u{a72a}\u{8133}\u{b9ed}\u{417c}\u{3be7}\u{7786}\u{5964}\x03\
	\x5b\u{2ed}\x04\x02\x09\x02\x04\x03\x09\x03\x04\x04\x09\x04\x04\x05\x09\
	\x05\x04\x06\x09\x06\x04\x07\x09\x07\x04\x08\x09\x08\x04\x09\x09\x09\x04\
	\x0a\x09\x0a\x04\x0b\x09\x0b\x04\x0c\x09\x0c\x04\x0d\x09\x0d\x04\x0e\x09\
	\x0e\x04\x0f\x09\x0f\x04\x10\x09\x10\x04\x11\x09\x11\x04\x12\x09\x12\x04\
	\x13\x09\x13\x03\x02\x03\x02\x03\x02\x03\x03\x03\x03\x03\x03\x03\x04\x03\
	\x04\x03\x04\x03\x05\x03\x05\x07\x05\x32\x0a\x05\x0c\x05\x0e\x05\x35\x0b\
	\x05\x03\x05\x07\x05\x38\x0a\x05\x0c\x05\x0e\x05\x3b\x0b\x05\x03\x06\x03\
	\x06\x03\x06\x03\x06\x03\x07\x03\x07\x03\x07\x03\x07\x03\x07\x07\x07\x46\
	\x0a\x07\x0c\x07\x0e\x07\x49\x0b\x07\x03\x07\x03\x07\x03\x08\x07\x08\x4e\
	\x0a\x08\x0c\x08\x0e\x08\x51\x0b\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\
	\x08\x03\x08\x07\x08\x59\x0a\x08\x0c\x08\x0e\x08\x5c\x0b\x08\x05\x08\x5e\
	\x0a\x08\x03\x08\x03\x08\x03\x08\x05\x08\x63\x0a\x08\x03\x08\x03\x08\x03\
	\x08\x03\x08\x07\x08\x69\x0a\x08\x0c\x08\x0e\x08\x6c\x0b\x08\x05\x08\x6e\
	\x0a\x08\x03\x08\x03\x08\x07\x08\x72\x0a\x08\x0c\x08\x0e\x08\x75\x0b\x08\
	\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x07\x08\x7c\x0a\x08\x0c\x08\x0e\
	\x08\x7f\x0b\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\
	\x07\x08\u{88}\x0a\x08\x0c\x08\x0e\x08\u{8b}\x0b\x08\x03\x08\x03\x08\x03\
	\x08\x03\x08\x03\x08\x07\x08\u{92}\x0a\x08\x0c\x08\x0e\x08\u{95}\x0b\x08\
	\x05\x08\u{97}\x0a\x08\x03\x08\x03\x08\x03\x08\x05\x08\u{9c}\x0a\x08\x03\
	\x08\x03\x08\x03\x08\x03\x08\x07\x08\u{a2}\x0a\x08\x0c\x08\x0e\x08\u{a5}\
	\x0b\x08\x05\x08\u{a7}\x0a\x08\x03\x08\x03\x08\x07\x08\u{ab}\x0a\x08\x0c\
	\x08\x0e\x08\u{ae}\x0b\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\
	\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\x03\x08\
	\x03\x08\x03\x08\x05\x08\u{c1}\x0a\x08\x03\x09\x03\x09\x03\x0a\x03\x0a\x03\
	\x0a\x03\x0a\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{152}\x0a\x0b\x0c\x0b\x0e\x0b\u{155}\
	\x0b\x0b\x05\x0b\u{157}\x0a\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{163}\x0a\x0b\x0c\x0b\
	\x0e\x0b\u{166}\x0b\x0b\x05\x0b\u{168}\x0a\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x07\x0b\u{16f}\x0a\x0b\x0c\x0b\x0e\x0b\u{172}\x0b\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x05\x0b\u{17a}\x0a\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{183}\x0a\x0b\
	\x0c\x0b\x0e\x0b\u{186}\x0b\x0b\x05\x0b\u{188}\x0a\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{190}\x0a\x0b\x0c\x0b\x0e\x0b\u{193}\
	\x0b\x0b\x05\x0b\u{195}\x0a\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{1a3}\
	\x0a\x0b\x0c\x0b\x0e\x0b\u{1a6}\x0b\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{1af}\x0a\x0b\x0c\x0b\x0e\x0b\u{1b2}\x0b\
	\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x07\
	\x0b\u{1bc}\x0a\x0b\x0c\x0b\x0e\x0b\u{1bf}\x0b\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x05\x0b\u{1c7}\x0a\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x07\x0b\u{1fe}\x0a\x0b\x0c\x0b\
	\x0e\x0b\u{201}\x0b\x0b\x05\x0b\u{203}\x0a\x0b\x03\x0b\x03\x0b\x03\x0b\x03\
	\x0b\x03\x0b\x03\x0b\x07\x0b\u{20b}\x0a\x0b\x0c\x0b\x0e\x0b\u{20e}\x0b\x0b\
	\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\x03\x0b\
	\x03\x0b\x03\x0b\x07\x0b\u{21b}\x0a\x0b\x0c\x0b\x0e\x0b\u{21e}\x0b\x0b\x03\
	\x0c\x03\x0c\x03\x0c\x03\x0c\x03\x0d\x03\x0d\x03\x0d\x03\x0d\x03\x0e\x03\
	\x0e\x03\x0e\x03\x0e\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x05\x0f\u{231}\
	\x0a\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\
	\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x07\x0f\u{242}\
	\x0a\x0f\x0c\x0f\x0e\x0f\u{245}\x0b\x0f\x05\x0f\u{247}\x0a\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x07\x0f\u{24e}\x0a\x0f\x0c\x0f\x0e\x0f\u{251}\
	\x0b\x0f\x05\x0f\u{253}\x0a\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\
	\x07\x0f\u{25a}\x0a\x0f\x0c\x0f\x0e\x0f\u{25d}\x0b\x0f\x05\x0f\u{25f}\x0a\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x05\x0f\u{277}\x0a\x0f\x03\x0f\x03\
	\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x03\x0f\x07\x0f\u{280}\x0a\x0f\x0c\
	\x0f\x0e\x0f\u{283}\x0b\x0f\x03\x10\x03\x10\x03\x10\x03\x10\x03\x11\x03\
	\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x07\
	\x11\u{293}\x0a\x11\x0c\x11\x0e\x11\u{296}\x0b\x11\x05\x11\u{298}\x0a\x11\
	\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x07\x11\u{29f}\x0a\x11\x0c\x11\
	\x0e\x11\u{2a2}\x0b\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\
	\x03\x11\x03\x11\x03\x11\x03\x11\x07\x11\u{2ae}\x0a\x11\x0c\x11\x0e\x11\
	\u{2b1}\x0b\x11\x05\x11\u{2b3}\x0a\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\
	\x11\x07\x11\u{2ba}\x0a\x11\x0c\x11\x0e\x11\u{2bd}\x0b\x11\x03\x11\x03\x11\
	\x03\x11\x03\x11\x03\x11\x03\x11\x07\x11\u{2c5}\x0a\x11\x0c\x11\x0e\x11\
	\u{2c8}\x0b\x11\x05\x11\u{2ca}\x0a\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\
	\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\x11\x03\
	\x11\x05\x11\u{2da}\x0a\x11\x03\x11\x03\x11\x03\x11\x07\x11\u{2df}\x0a\x11\
	\x0c\x11\x0e\x11\u{2e2}\x0b\x11\x03\x12\x03\x12\x03\x12\x03\x12\x03\x13\
	\x03\x13\x03\x13\x05\x13\u{2eb}\x0a\x13\x03\x13\x02\x05\x14\x1c\x20\x14\
	\x02\x04\x06\x08\x0a\x0c\x0e\x10\x12\x14\x16\x18\x1a\x1c\x1e\x20\x22\x24\
	\x02\x02\x02\u{369}\x02\x26\x03\x02\x02\x02\x04\x29\x03\x02\x02\x02\x06\
	\x2c\x03\x02\x02\x02\x08\x2f\x03\x02\x02\x02\x0a\x3c\x03\x02\x02\x02\x0c\
	\x40\x03\x02\x02\x02\x0e\u{c0}\x03\x02\x02\x02\x10\u{c2}\x03\x02\x02\x02\
	\x12\u{c4}\x03\x02\x02\x02\x14\u{1c6}\x03\x02\x02\x02\x16\u{21f}\x03\x02\
	\x02\x02\x18\u{223}\x03\x02\x02\x02\x1a\u{227}\x03\x02\x02\x02\x1c\u{276}\
	\x03\x02\x02\x02\x1e\u{284}\x03\x02\x02\x02\x20\u{2d9}\x03\x02\x02\x02\x22\
	\u{2e3}\x03\x02\x02\x02\x24\u{2e7}\x03\x02\x02\x02\x26\x27\x05\x08\x05\x02\
	\x27\x28\x07\x02\x02\x03\x28\x03\x03\x02\x02\x02\x29\x2a\x05\x14\x0b\x02\
	\x2a\x2b\x07\x02\x02\x03\x2b\x05\x03\x02\x02\x02\x2c\x2d\x05\x20\x11\x02\
	\x2d\x2e\x07\x02\x02\x03\x2e\x07\x03\x02\x02\x02\x2f\x33\x05\x0a\x06\x02\
	\x30\x32\x05\x0c\x07\x02\x31\x30\x03\x02\x02\x02\x32\x35\x03\x02\x02\x02\
	\x33\x31\x03\x02\x02\x02\x33\x34\x03\x02\x02\x02\x34\x39\x03\x02\x02\x02\
	\x35\x33\x03\x02\x02\x02\x36\x38\x05\x0e\x08\x02\x37\x36\x03\x02\x02\x02\
	\x38\x3b\x03\x02\x02\x02\x39\x37\x03\x02\x02\x02\x39\x3a\x03\x02\x02\x02\
	\x3a\x09\x03\x02\x02\x02\x3b\x39\x03\x02\x02\x02\x3c\x3d\x07\x35\x02\x02\
	\x3d\x3e\x07\x29\x02\x02\x3e\x3f\x07\x04\x02\x02\x3f\x0b\x03\x02\x02\x02\
	\x40\x41\x07\x2b\x02\x02\x41\x42\x07\x43\x02\x02\x42\x47\x07\x57\x02\x02\
	\x43\x44\x07\x03\x02\x02\x44\x46\x07\x57\x02\x02\x45\x43\x03\x02\x02\x02\
	\x46\x49\x03\x02\x02\x02\x47\x45\x03\x02\x02\x02\x47\x48\x03\x02\x02\x02\
	\x48\x4a\x03\x02\x02\x02\x49\x47\x03\x02\x02\x02\x4a\x4b\x07\x04\x02\x02\
	\x4b\x0d\x03\x02\x02\x02\x4c\x4e\x05\x10\x09\x02\x4d\x4c\x03\x02\x02\x02\
	\x4e\x51\x03\x02\x02\x02\x4f\x4d\x03\x02\x02\x02\x4f\x50\x03\x02\x02\x02\
	\x50\x52\x03\x02\x02\x02\x51\x4f\x03\x02\x02\x02\x52\x53\x07\x2e\x02\x02\
	\x53\x54\x07\x56\x02\x02\x54\x5d\x07\x05\x02\x02\x55\x5a\x05\x12\x0a\x02\
	\x56\x57\x07\x03\x02\x02\x57\x59\x05\x12\x0a\x02\x58\x56\x03\x02\x02\x02\
	\x59\x5c\x03\x02\x02\x02\x5a\x58\x03\x02\x02\x02\x5a\x5b\x03\x02\x02\x02\
	\x5b\x5e\x03\x02\x02\x02\x5c\x5a\x03\x02\x02\x02\x5d\x55\x03\x02\x02\x02\
	\x5d\x5e\x03\x02\x02\x02\x5e\x5f\x03\x02\x02\x02\x5f\x62\x07\x06\x02\x02\
	\x60\x61\x07\x0b\x02\x02\x61\x63\x05\x20\x11\x02\x62\x60\x03\x02\x02\x02\
	\x62\x63\x03\x02\x02\x02\x63\x6d\x03\x02\x02\x02\x64\x65\x07\x3e\x02\x02\
	\x65\x6a\x05\x20\x11\x02\x66\x67\x07\x03\x02\x02\x67\x69\x05\x20\x11\x02\
	\x68\x66\x03\x02\x02\x02\x69\x6c\x03\x02\x02\x02\x6a\x68\x03\x02\x02\x02\
	\x6a\x6b\x03\x02\x02\x02\x6b\x6e\x03\x02\x02\x02\x6c\x6a\x03\x02\x02\x02\
	\x6d\x64\x03\x02\x02\x02\x6d\x6e\x03\x02\x02\x02\x6e\x6f\x03\x02\x02\x02\
	\x6f\x73\x07\x07\x02\x02\x70\x72\x05\x0e\x08\x02\x71\x70\x03\x02\x02\x02\
	\x72\x75\x03\x02\x02\x02\x73\x71\x03\x02\x02\x02\x73\x74\x03\x02\x02\x02\
	\x74\x76\x03\x02\x02\x02\x75\x73\x03\x02\x02\x02\x76\x77\x07\x3b\x02\x02\
	\x77\x78\x05\x14\x0b\x02\x78\x79\x07\x08\x02\x02\x79\u{c1}\x03\x02\x02\x02\
	\x7a\x7c\x05\x10\x09\x02\x7b\x7a\x03\x02\x02\x02\x7c\x7f\x03\x02\x02\x02\
	\x7d\x7b\x03\x02\x02\x02\x7d\x7e\x03\x02\x02\x02\x7e\u{80}\x03\x02\x02\x02\
	\x7f\x7d\x03\x02\x02\x02\u{80}\u{81}\x07\x52\x02\x02\u{81}\u{82}\x07\x2e\
	\x02\x02\u{82}\u{83}\x07\x56\x02\x02\u{83}\u{84}\x07\x10\x02\x02\u{84}\u{89}\
	\x07\x56\x02\x02\u{85}\u{86}\x07\x03\x02\x02\u{86}\u{88}\x07\x56\x02\x02\
	\u{87}\u{85}\x03\x02\x02\x02\u{88}\u{8b}\x03\x02\x02\x02\u{89}\u{87}\x03\
	\x02\x02\x02\u{89}\u{8a}\x03\x02\x02\x02\u{8a}\u{8c}\x03\x02\x02\x02\u{8b}\
	\u{89}\x03\x02\x02\x02\u{8c}\u{8d}\x07\x11\x02\x02\u{8d}\u{96}\x07\x05\x02\
	\x02\u{8e}\u{93}\x05\x12\x0a\x02\u{8f}\u{90}\x07\x03\x02\x02\u{90}\u{92}\
	\x05\x12\x0a\x02\u{91}\u{8f}\x03\x02\x02\x02\u{92}\u{95}\x03\x02\x02\x02\
	\u{93}\u{91}\x03\x02\x02\x02\u{93}\u{94}\x03\x02\x02\x02\u{94}\u{97}\x03\
	\x02\x02\x02\u{95}\u{93}\x03\x02\x02\x02\u{96}\u{8e}\x03\x02\x02\x02\u{96}\
	\u{97}\x03\x02\x02\x02\u{97}\u{98}\x03\x02\x02\x02\u{98}\u{9b}\x07\x06\x02\
	\x02\u{99}\u{9a}\x07\x0b\x02\x02\u{9a}\u{9c}\x05\x20\x11\x02\u{9b}\u{99}\
	\x03\x02\x02\x02\u{9b}\u{9c}\x03\x02\x02\x02\u{9c}\u{a6}\x03\x02\x02\x02\
	\u{9d}\u{9e}\x07\x3e\x02\x02\u{9e}\u{a3}\x05\x20\x11\x02\u{9f}\u{a0}\x07\
	\x03\x02\x02\u{a0}\u{a2}\x05\x20\x11\x02\u{a1}\u{9f}\x03\x02\x02\x02\u{a2}\
	\u{a5}\x03\x02\x02\x02\u{a3}\u{a1}\x03\x02\x02\x02\u{a3}\u{a4}\x03\x02\x02\
	\x02\u{a4}\u{a7}\x03\x02\x02\x02\u{a5}\u{a3}\x03\x02\x02\x02\u{a6}\u{9d}\
	\x03\x02\x02\x02\u{a6}\u{a7}\x03\x02\x02\x02\u{a7}\u{a8}\x03\x02\x02\x02\
	\u{a8}\u{ac}\x07\x07\x02\x02\u{a9}\u{ab}\x05\x0e\x08\x02\u{aa}\u{a9}\x03\
	\x02\x02\x02\u{ab}\u{ae}\x03\x02\x02\x02\u{ac}\u{aa}\x03\x02\x02\x02\u{ac}\
	\u{ad}\x03\x02\x02\x02\u{ad}\u{af}\x03\x02\x02\x02\u{ae}\u{ac}\x03\x02\x02\
	\x02\u{af}\u{b0}\x07\x3b\x02\x02\u{b0}\u{b1}\x05\x14\x0b\x02\u{b1}\u{b2}\
	\x07\x08\x02\x02\u{b2}\u{c1}\x03\x02\x02\x02\u{b3}\u{b4}\x07\x40\x02\x02\
	\u{b4}\u{b5}\x07\x56\x02\x02\u{b5}\u{b6}\x07\x09\x02\x02\u{b6}\u{c1}\x05\
	\x20\x11\x02\u{b7}\u{b8}\x07\x45\x02\x02\u{b8}\u{b9}\x07\x40\x02\x02\u{b9}\
	\u{ba}\x07\x09\x02\x02\u{ba}\u{c1}\x05\x20\x11\x02\u{bb}\u{bc}\x07\x45\x02\
	\x02\u{bc}\u{bd}\x07\x46\x02\x02\u{bd}\u{be}\x07\x56\x02\x02\u{be}\u{bf}\
	\x07\x0a\x02\x02\u{bf}\u{c1}\x05\x20\x11\x02\u{c0}\x4f\x03\x02\x02\x02\u{c0}\
	\x7d\x03\x02\x02\x02\u{c0}\u{b3}\x03\x02\x02\x02\u{c0}\u{b7}\x03\x02\x02\
	\x02\u{c0}\u{bb}\x03\x02\x02\x02\u{c1}\x0f\x03\x02\x02\x02\u{c2}\u{c3}\x07\
	\x33\x02\x02\u{c3}\x11\x03\x02\x02\x02\u{c4}\u{c5}\x07\x56\x02\x02\u{c5}\
	\u{c6}\x07\x0a\x02\x02\u{c6}\u{c7}\x05\x20\x11\x02\u{c7}\x13\x03\x02\x02\
	\x02\u{c8}\u{c9}\x08\x0b\x01\x02\u{c9}\u{1c7}\x07\x3f\x02\x02\u{ca}\u{1c7}\
	\x07\x2c\x02\x02\u{cb}\u{1c7}\x07\x42\x02\x02\u{cc}\u{1c7}\x07\x59\x02\x02\
	\u{cd}\u{1c7}\x07\x58\x02\x02\u{ce}\u{1c7}\x07\x56\x02\x02\u{cf}\u{1c7}\
	\x07\x4b\x02\x02\u{d0}\u{d1}\x07\x4c\x02\x02\u{d1}\u{d2}\x07\x05\x02\x02\
	\u{d2}\u{d3}\x05\x14\x0b\x02\u{d3}\u{d4}\x07\x06\x02\x02\u{d4}\u{1c7}\x03\
	\x02\x02\x02\u{d5}\u{d6}\x07\x4d\x02\x02\u{d6}\u{d7}\x07\x07\x02\x02\u{d7}\
	\u{d8}\x05\x14\x0b\x02\u{d8}\u{d9}\x07\x08\x02\x02\u{d9}\u{da}\x07\x4e\x02\
	\x02\u{da}\u{db}\x07\x07\x02\x02\u{db}\u{dc}\x05\x1c\x0f\x02\u{dc}\u{dd}\
	\x07\x0c\x02\x02\u{dd}\u{de}\x05\x14\x0b\x02\u{de}\u{df}\x07\x08\x02\x02\
	\u{df}\u{1c7}\x03\x02\x02\x02\u{e0}\u{e1}\x07\x4d\x02\x02\u{e1}\u{e2}\x07\
	\x07\x02\x02\u{e2}\u{e3}\x05\x14\x0b\x02\u{e3}\u{e4}\x07\x08\x02\x02\u{e4}\
	\u{e5}\x07\x47\x02\x02\u{e5}\u{e6}\x07\x27\x02\x02\u{e6}\u{e7}\x05\x20\x11\
	\x02\u{e7}\u{e8}\x07\x07\x02\x02\u{e8}\u{e9}\x05\x1c\x0f\x02\u{e9}\u{ea}\
	\x07\x0c\x02\x02\u{ea}\u{eb}\x05\x14\x0b\x02\u{eb}\u{ec}\x07\x08\x02\x02\
	\u{ec}\u{ed}\x07\x43\x02\x02\u{ed}\u{ee}\x07\x07\x02\x02\u{ee}\u{ef}\x05\
	\x14\x0b\x02\u{ef}\u{f0}\x07\x08\x02\x02\u{f0}\u{1c7}\x03\x02\x02\x02\u{f1}\
	\u{f2}\x07\x4d\x02\x02\u{f2}\u{f3}\x07\x07\x02\x02\u{f3}\u{f4}\x05\x14\x0b\
	\x02\u{f4}\u{f5}\x07\x08\x02\x02\u{f5}\u{f6}\x07\x43\x02\x02\u{f6}\u{f7}\
	\x07\x07\x02\x02\u{f7}\u{f8}\x05\x14\x0b\x02\u{f8}\u{f9}\x07\x08\x02\x02\
	\u{f9}\u{1c7}\x03\x02\x02\x02\u{fa}\u{fb}\x07\x32\x02\x02\u{fb}\u{fc}\x07\
	\x05\x02\x02\u{fc}\u{fd}\x05\x14\x0b\x02\u{fd}\u{fe}\x07\x06\x02\x02\u{fe}\
	\u{1c7}\x03\x02\x02\x02\u{ff}\u{100}\x07\x34\x02\x02\u{100}\u{101}\x07\x05\
	\x02\x02\u{101}\u{102}\x05\x14\x0b\x02\u{102}\u{103}\x07\x06\x02\x02\u{103}\
	\u{1c7}\x03\x02\x02\x02\u{104}\u{105}\x07\x28\x02\x02\u{105}\u{106}\x07\
	\x05\x02\x02\u{106}\u{107}\x05\x14\x0b\x02\u{107}\u{108}\x07\x03\x02\x02\
	\u{108}\u{109}\x05\x14\x0b\x02\u{109}\u{10a}\x07\x06\x02\x02\u{10a}\u{1c7}\
	\x03\x02\x02\x02\u{10b}\u{10c}\x07\x1d\x02\x02\u{10c}\u{10d}\x07\x05\x02\
	\x02\u{10d}\u{10e}\x05\x14\x0b\x02\u{10e}\u{10f}\x07\x06\x02\x02\u{10f}\
	\u{1c7}\x03\x02\x02\x02\u{110}\u{111}\x07\x1e\x02\x02\u{111}\u{112}\x07\
	\x05\x02\x02\u{112}\u{113}\x05\x14\x0b\x02\u{113}\u{114}\x07\x06\x02\x02\
	\u{114}\u{1c7}\x03\x02\x02\x02\u{115}\u{116}\x07\x1f\x02\x02\u{116}\u{117}\
	\x07\x05\x02\x02\u{117}\u{118}\x05\x14\x0b\x02\u{118}\u{119}\x07\x06\x02\
	\x02\u{119}\u{1c7}\x03\x02\x02\x02\u{11a}\u{11b}\x07\x3c\x02\x02\u{11b}\
	\u{11c}\x07\x05\x02\x02\u{11c}\u{11d}\x05\x14\x0b\x02\u{11d}\u{11e}\x07\
	\x06\x02\x02\u{11e}\u{1c7}\x03\x02\x02\x02\u{11f}\u{120}\x07\x39\x02\x02\
	\u{120}\u{121}\x07\x05\x02\x02\u{121}\u{122}\x05\x14\x0b\x02\u{122}\u{123}\
	\x07\x06\x02\x02\u{123}\u{1c7}\x03\x02\x02\x02\u{124}\u{125}\x07\x20\x02\
	\x02\u{125}\u{126}\x07\x05\x02\x02\u{126}\u{127}\x05\x14\x0b\x02\u{127}\
	\u{128}\x07\x06\x02\x02\u{128}\u{1c7}\x03\x02\x02\x02\u{129}\u{12a}\x07\
	\x21\x02\x02\u{12a}\u{12b}\x07\x05\x02\x02\u{12b}\u{12c}\x05\x14\x0b\x02\
	\u{12c}\u{12d}\x07\x06\x02\x02\u{12d}\u{1c7}\x03\x02\x02\x02\u{12e}\u{12f}\
	\x07\x2d\x02\x02\u{12f}\u{130}\x07\x05\x02\x02\u{130}\u{131}\x05\x14\x0b\
	\x02\u{131}\u{132}\x07\x06\x02\x02\u{132}\u{1c7}\x03\x02\x02\x02\u{133}\
	\u{134}\x07\x22\x02\x02\u{134}\u{135}\x07\x05\x02\x02\u{135}\u{136}\x05\
	\x14\x0b\x02\u{136}\u{137}\x07\x03\x02\x02\u{137}\u{138}\x05\x14\x0b\x02\
	\u{138}\u{139}\x07\x03\x02\x02\u{139}\u{13a}\x05\x14\x0b\x02\u{13a}\u{13b}\
	\x07\x06\x02\x02\u{13b}\u{1c7}\x03\x02\x02\x02\u{13c}\u{13d}\x07\x2f\x02\
	\x02\u{13d}\u{13e}\x07\x10\x02\x02\u{13e}\u{13f}\x05\x20\x11\x02\u{13f}\
	\u{140}\x07\x11\x02\x02\u{140}\u{141}\x05\x14\x0b\x24\u{141}\u{1c7}\x03\
	\x02\x02\x02\u{142}\u{143}\x07\x41\x02\x02\u{143}\u{144}\x07\x10\x02\x02\
	\u{144}\u{145}\x05\x20\x11\x02\u{145}\u{146}\x07\x11\x02\x02\u{146}\u{147}\
	\x05\x14\x0b\x23\u{147}\u{1c7}\x03\x02\x02\x02\u{148}\u{149}\x07\x4a\x02\
	\x02\u{149}\u{1c7}\x05\x14\x0b\x1d\u{14a}\u{14b}\x07\x1a\x02\x02\u{14b}\
	\u{1c7}\x05\x14\x0b\x1c\u{14c}\u{14d}\x07\x2e\x02\x02\u{14d}\u{156}\x07\
	\x05\x02\x02\u{14e}\u{153}\x05\x12\x0a\x02\u{14f}\u{150}\x07\x03\x02\x02\
	\u{150}\u{152}\x05\x12\x0a\x02\u{151}\u{14f}\x03\x02\x02\x02\u{152}\u{155}\
	\x03\x02\x02\x02\u{153}\u{151}\x03\x02\x02\x02\u{153}\u{154}\x03\x02\x02\
	\x02\u{154}\u{157}\x03\x02\x02\x02\u{155}\u{153}\x03\x02\x02\x02\u{156}\
	\u{14e}\x03\x02\x02\x02\u{156}\u{157}\x03\x02\x02\x02\u{157}\u{158}\x03\
	\x02\x02\x02\u{158}\u{159}\x07\x06\x02\x02\u{159}\u{15a}\x07\x07\x02\x02\
	\u{15a}\u{15b}\x07\x3b\x02\x02\u{15b}\u{15c}\x05\x14\x0b\x02\u{15c}\u{15d}\
	\x07\x08\x02\x02\u{15d}\u{1c7}\x03\x02\x02\x02\u{15e}\u{167}\x07\x07\x02\
	\x02\u{15f}\u{164}\x05\x14\x0b\x02\u{160}\u{161}\x07\x03\x02\x02\u{161}\
	\u{163}\x05\x14\x0b\x02\u{162}\u{160}\x03\x02\x02\x02\u{163}\u{166}\x03\
	\x02\x02\x02\u{164}\u{162}\x03\x02\x02\x02\u{164}\u{165}\x03\x02\x02\x02\
	\u{165}\u{168}\x03\x02\x02\x02\u{166}\u{164}\x03\x02\x02\x02\u{167}\u{15f}\
	\x03\x02\x02\x02\u{167}\u{168}\x03\x02\x02\x02\u{168}\u{169}\x03\x02\x02\
	\x02\u{169}\u{1c7}\x07\x08\x02\x02\u{16a}\u{16b}\x07\x07\x02\x02\u{16b}\
	\u{170}\x05\x18\x0d\x02\u{16c}\u{16d}\x07\x03\x02\x02\u{16d}\u{16f}\x05\
	\x18\x0d\x02\u{16e}\u{16c}\x03\x02\x02\x02\u{16f}\u{172}\x03\x02\x02\x02\
	\u{170}\u{16e}\x03\x02\x02\x02\u{170}\u{171}\x03\x02\x02\x02\u{171}\u{173}\
	\x03\x02\x02\x02\u{172}\u{170}\x03\x02\x02\x02\u{173}\u{174}\x07\x08\x02\
	\x02\u{174}\u{1c7}\x03\x02\x02\x02\u{175}\u{176}\x07\x0e\x02\x02\u{176}\
	\u{179}\x07\x56\x02\x02\u{177}\u{178}\x07\x09\x02\x02\u{178}\u{17a}\x05\
	\x14\x0b\x02\u{179}\u{177}\x03\x02\x02\x02\u{179}\u{17a}\x03\x02\x02\x02\
	\u{17a}\u{17b}\x03\x02\x02\x02\u{17b}\u{1c7}\x07\x0f\x02\x02\u{17c}\u{17d}\
	\x07\x38\x02\x02\u{17d}\u{17e}\x05\x14\x0b\x02\u{17e}\u{187}\x07\x07\x02\
	\x02\u{17f}\u{184}\x05\x1a\x0e\x02\u{180}\u{181}\x07\x0d\x02\x02\u{181}\
	\u{183}\x05\x1a\x0e\x02\u{182}\u{180}\x03\x02\x02\x02\u{183}\u{186}\x03\
	\x02\x02\x02\u{184}\u{182}\x03\x02\x02\x02\u{184}\u{185}\x03\x02\x02\x02\
	\u{185}\u{188}\x03\x02\x02\x02\u{186}\u{184}\x03\x02\x02\x02\u{187}\u{17f}\
	\x03\x02\x02\x02\u{187}\u{188}\x03\x02\x02\x02\u{188}\u{189}\x03\x02\x02\
	\x02\u{189}\u{18a}\x07\x08\x02\x02\u{18a}\u{1c7}\x03\x02\x02\x02\u{18b}\
	\u{194}\x07\x10\x02\x02\u{18c}\u{191}\x05\x14\x0b\x02\u{18d}\u{18e}\x07\
	\x03\x02\x02\u{18e}\u{190}\x05\x14\x0b\x02\u{18f}\u{18d}\x03\x02\x02\x02\
	\u{190}\u{193}\x03\x02\x02\x02\u{191}\u{18f}\x03\x02\x02\x02\u{191}\u{192}\
	\x03\x02\x02\x02\u{192}\u{195}\x03\x02\x02\x02\u{193}\u{191}\x03\x02\x02\
	\x02\u{194}\u{18c}\x03\x02\x02\x02\u{194}\u{195}\x03\x02\x02\x02\u{195}\
	\u{196}\x03\x02\x02\x02\u{196}\u{1c7}\x07\x11\x02\x02\u{197}\u{198}\x07\
	\x30\x02\x02\u{198}\u{199}\x05\x14\x0b\x02\u{199}\u{19a}\x07\x3d\x02\x02\
	\u{19a}\u{19b}\x05\x14\x0b\x02\u{19b}\u{19c}\x07\x2a\x02\x02\u{19c}\u{19d}\
	\x05\x14\x0b\x09\u{19d}\u{1c7}\x03\x02\x02\x02\u{19e}\u{19f}\x07\x36\x02\
	\x02\u{19f}\u{1a4}\x05\x16\x0c\x02\u{1a0}\u{1a1}\x07\x03\x02\x02\u{1a1}\
	\u{1a3}\x05\x16\x0c\x02\u{1a2}\u{1a0}\x03\x02\x02\x02\u{1a3}\u{1a6}\x03\
	\x02\x02\x02\u{1a4}\u{1a2}\x03\x02\x02\x02\u{1a4}\u{1a5}\x03\x02\x02\x02\
	\u{1a5}\u{1a7}\x03\x02\x02\x02\u{1a6}\u{1a4}\x03\x02\x02\x02\u{1a7}\u{1a8}\
	\x07\x31\x02\x02\u{1a8}\u{1a9}\x05\x14\x0b\x07\u{1a9}\u{1c7}\x03\x02\x02\
	\x02\u{1aa}\u{1ab}\x07\x37\x02\x02\u{1ab}\u{1b0}\x05\x16\x0c\x02\u{1ac}\
	\u{1ad}\x07\x03\x02\x02\u{1ad}\u{1af}\x05\x16\x0c\x02\u{1ae}\u{1ac}\x03\
	\x02\x02\x02\u{1af}\u{1b2}\x03\x02\x02\x02\u{1b0}\u{1ae}\x03\x02\x02\x02\
	\u{1b0}\u{1b1}\x03\x02\x02\x02\u{1b1}\u{1b3}\x03\x02\x02\x02\u{1b2}\u{1b0}\
	\x03\x02\x02\x02\u{1b3}\u{1b4}\x07\x31\x02\x02\u{1b4}\u{1b5}\x05\x14\x0b\
	\x06\u{1b5}\u{1c7}\x03\x02\x02\x02\u{1b6}\u{1b7}\x07\x52\x02\x02\u{1b7}\
	\u{1b8}\x07\x10\x02\x02\u{1b8}\u{1bd}\x07\x56\x02\x02\u{1b9}\u{1ba}\x07\
	\x03\x02\x02\u{1ba}\u{1bc}\x07\x56\x02\x02\u{1bb}\u{1b9}\x03\x02\x02\x02\
	\u{1bc}\u{1bf}\x03\x02\x02\x02\u{1bd}\u{1bb}\x03\x02\x02\x02\u{1bd}\u{1be}\
	\x03\x02\x02\x02\u{1be}\u{1c0}\x03\x02\x02\x02\u{1bf}\u{1bd}\x03\x02\x02\
	\x02\u{1c0}\u{1c1}\x07\x11\x02\x02\u{1c1}\u{1c7}\x05\x14\x0b\x05\u{1c2}\
	\u{1c3}\x07\x05\x02\x02\u{1c3}\u{1c4}\x05\x14\x0b\x02\u{1c4}\u{1c5}\x07\
	\x06\x02\x02\u{1c5}\u{1c7}\x03\x02\x02\x02\u{1c6}\u{c8}\x03\x02\x02\x02\
	\u{1c6}\u{ca}\x03\x02\x02\x02\u{1c6}\u{cb}\x03\x02\x02\x02\u{1c6}\u{cc}\
	\x03\x02\x02\x02\u{1c6}\u{cd}\x03\x02\x02\x02\u{1c6}\u{ce}\x03\x02\x02\x02\
	\u{1c6}\u{cf}\x03\x02\x02\x02\u{1c6}\u{d0}\x03\x02\x02\x02\u{1c6}\u{d5}\
	\x03\x02\x02\x02\u{1c6}\u{e0}\x03\x02\x02\x02\u{1c6}\u{f1}\x03\x02\x02\x02\
	\u{1c6}\u{fa}\x03\x02\x02\x02\u{1c6}\u{ff}\x03\x02\x02\x02\u{1c6}\u{104}\
	\x03\x02\x02\x02\u{1c6}\u{10b}\x03\x02\x02\x02\u{1c6}\u{110}\x03\x02\x02\
	\x02\u{1c6}\u{115}\x03\x02\x02\x02\u{1c6}\u{11a}\x03\x02\x02\x02\u{1c6}\
	\u{11f}\x03\x02\x02\x02\u{1c6}\u{124}\x03\x02\x02\x02\u{1c6}\u{129}\x03\
	\x02\x02\x02\u{1c6}\u{12e}\x03\x02\x02\x02\u{1c6}\u{133}\x03\x02\x02\x02\
	\u{1c6}\u{13c}\x03\x02\x02\x02\u{1c6}\u{142}\x03\x02\x02\x02\u{1c6}\u{148}\
	\x03\x02\x02\x02\u{1c6}\u{14a}\x03\x02\x02\x02\u{1c6}\u{14c}\x03\x02\x02\
	\x02\u{1c6}\u{15e}\x03\x02\x02\x02\u{1c6}\u{16a}\x03\x02\x02\x02\u{1c6}\
	\u{175}\x03\x02\x02\x02\u{1c6}\u{17c}\x03\x02\x02\x02\u{1c6}\u{18b}\x03\
	\x02\x02\x02\u{1c6}\u{197}\x03\x02\x02\x02\u{1c6}\u{19e}\x03\x02\x02\x02\
	\u{1c6}\u{1aa}\x03\x02\x02\x02\u{1c6}\u{1b6}\x03\x02\x02\x02\u{1c6}\u{1c2}\
	\x03\x02\x02\x02\u{1c7}\u{21c}\x03\x02\x02\x02\u{1c8}\u{1c9}\x0c\x20\x02\
	\x02\u{1c9}\u{1ca}\x07\x1a\x02\x02\u{1ca}\u{21b}\x05\x14\x0b\x21\u{1cb}\
	\u{1cc}\x0c\x1f\x02\x02\u{1cc}\u{1cd}\x07\x1b\x02\x02\u{1cd}\u{21b}\x05\
	\x14\x0b\x20\u{1ce}\u{1cf}\x0c\x1e\x02\x02\u{1cf}\u{1d0}\x07\x26\x02\x02\
	\u{1d0}\u{21b}\x05\x14\x0b\x1f\u{1d1}\u{1d2}\x0c\x1b\x02\x02\u{1d2}\u{1d3}\
	\x07\x18\x02\x02\u{1d3}\u{21b}\x05\x14\x0b\x1c\u{1d4}\u{1d5}\x0c\x1a\x02\
	\x02\u{1d5}\u{1d6}\x07\x19\x02\x02\u{1d6}\u{21b}\x05\x14\x0b\x1b\u{1d7}\
	\u{1d8}\x0c\x19\x02\x02\u{1d8}\u{1d9}\x07\x3a\x02\x02\u{1d9}\u{21b}\x05\
	\x14\x0b\x1a\u{1da}\u{1db}\x0c\x10\x02\x02\u{1db}\u{1dc}\x07\x12\x02\x02\
	\u{1dc}\u{21b}\x05\x14\x0b\x11\u{1dd}\u{1de}\x0c\x0f\x02\x02\u{1de}\u{1df}\
	\x07\x13\x02\x02\u{1df}\u{21b}\x05\x14\x0b\x10\u{1e0}\u{1e1}\x0c\x0e\x02\
	\x02\u{1e1}\u{1e2}\x07\x14\x02\x02\u{1e2}\u{21b}\x05\x14\x0b\x0f\u{1e3}\
	\u{1e4}\x0c\x0d\x02\x02\u{1e4}\u{1e5}\x07\x15\x02\x02\u{1e5}\u{21b}\x05\
	\x14\x0b\x0e\u{1e6}\u{1e7}\x0c\x0c\x02\x02\u{1e7}\u{1e8}\x07\x16\x02\x02\
	\u{1e8}\u{21b}\x05\x14\x0b\x0d\u{1e9}\u{1ea}\x0c\x0b\x02\x02\u{1ea}\u{1eb}\
	\x07\x17\x02\x02\u{1eb}\u{21b}\x05\x14\x0b\x0c\u{1ec}\u{1ed}\x0c\x0a\x02\
	\x02\u{1ed}\u{1ee}\x07\x48\x02\x02\u{1ee}\u{21b}\x05\x14\x0b\x0b\u{1ef}\
	\u{1f0}\x0c\x08\x02\x02\u{1f0}\u{1f1}\x07\x04\x02\x02\u{1f1}\u{21b}\x05\
	\x14\x0b\x09\u{1f2}\u{1f3}\x0c\x3d\x02\x02\u{1f3}\u{1f4}\x07\x1c\x02\x02\
	\u{1f4}\u{21b}\x07\x56\x02\x02\u{1f5}\u{1f6}\x0c\x3c\x02\x02\u{1f6}\u{1f7}\
	\x07\x1c\x02\x02\u{1f7}\u{21b}\x07\x59\x02\x02\u{1f8}\u{1f9}\x0c\x22\x02\
	\x02\u{1f9}\u{202}\x07\x05\x02\x02\u{1fa}\u{1ff}\x05\x14\x0b\x02\u{1fb}\
	\u{1fc}\x07\x03\x02\x02\u{1fc}\u{1fe}\x05\x14\x0b\x02\u{1fd}\u{1fb}\x03\
	\x02\x02\x02\u{1fe}\u{201}\x03\x02\x02\x02\u{1ff}\u{1fd}\x03\x02\x02\x02\
	\u{1ff}\u{200}\x03\x02\x02\x02\u{200}\u{203}\x03\x02\x02\x02\u{201}\u{1ff}\
	\x03\x02\x02\x02\u{202}\u{1fa}\x03\x02\x02\x02\u{202}\u{203}\x03\x02\x02\
	\x02\u{203}\u{204}\x03\x02\x02\x02\u{204}\u{21b}\x07\x06\x02\x02\u{205}\
	\u{206}\x0c\x21\x02\x02\u{206}\u{207}\x07\x10\x02\x02\u{207}\u{20c}\x05\
	\x20\x11\x02\u{208}\u{209}\x07\x03\x02\x02\u{209}\u{20b}\x05\x20\x11\x02\
	\u{20a}\u{208}\x03\x02\x02\x02\u{20b}\u{20e}\x03\x02\x02\x02\u{20c}\u{20a}\
	\x03\x02\x02\x02\u{20c}\u{20d}\x03\x02\x02\x02\u{20d}\u{20f}\x03\x02\x02\
	\x02\u{20e}\u{20c}\x03\x02\x02\x02\u{20f}\u{210}\x07\x11\x02\x02\u{210}\
	\u{21b}\x03\x02\x02\x02\u{211}\u{212}\x0c\x18\x02\x02\u{212}\u{213}\x07\
	\x27\x02\x02\u{213}\u{21b}\x05\x20\x11\x02\u{214}\u{215}\x0c\x17\x02\x02\
	\u{215}\u{216}\x07\x47\x02\x02\u{216}\u{217}\x07\x27\x02\x02\u{217}\u{21b}\
	\x05\x20\x11\x02\u{218}\u{219}\x0c\x03\x02\x02\u{219}\u{21b}\x07\x04\x02\
	\x02\u{21a}\u{1c8}\x03\x02\x02\x02\u{21a}\u{1cb}\x03\x02\x02\x02\u{21a}\
	\u{1ce}\x03\x02\x02\x02\u{21a}\u{1d1}\x03\x02\x02\x02\u{21a}\u{1d4}\x03\
	\x02\x02\x02\u{21a}\u{1d7}\x03\x02\x02\x02\u{21a}\u{1da}\x03\x02\x02\x02\
	\u{21a}\u{1dd}\x03\x02\x02\x02\u{21a}\u{1e0}\x03\x02\x02\x02\u{21a}\u{1e3}\
	\x03\x02\x02\x02\u{21a}\u{1e6}\x03\x02\x02\x02\u{21a}\u{1e9}\x03\x02\x02\
	\x02\u{21a}\u{1ec}\x03\x02\x02\x02\u{21a}\u{1ef}\x03\x02\x02\x02\u{21a}\
	\u{1f2}\x03\x02\x02\x02\u{21a}\u{1f5}\x03\x02\x02\x02\u{21a}\u{1f8}\x03\
	\x02\x02\x02\u{21a}\u{205}\x03\x02\x02\x02\u{21a}\u{211}\x03\x02\x02\x02\
	\u{21a}\u{214}\x03\x02\x02\x02\u{21a}\u{218}\x03\x02\x02\x02\u{21b}\u{21e}\
	\x03\x02\x02\x02\u{21c}\u{21a}\x03\x02\x02\x02\u{21c}\u{21d}\x03\x02\x02\
	\x02\u{21d}\x15\x03\x02\x02\x02\u{21e}\u{21c}\x03\x02\x02\x02\u{21f}\u{220}\
	\x05\x1c\x0f\x02\u{220}\u{221}\x07\x09\x02\x02\u{221}\u{222}\x05\x14\x0b\
	\x02\u{222}\x17\x03\x02\x02\x02\u{223}\u{224}\x07\x56\x02\x02\u{224}\u{225}\
	\x07\x09\x02\x02\u{225}\u{226}\x05\x14\x0b\x02\u{226}\x19\x03\x02\x02\x02\
	\u{227}\u{228}\x05\x1c\x0f\x02\u{228}\u{229}\x07\x0c\x02\x02\u{229}\u{22a}\
	\x05\x14\x0b\x02\u{22a}\x1b\x03\x02\x02\x02\u{22b}\u{22c}\x08\x0f\x01\x02\
	\u{22c}\u{22d}\x07\x0e\x02\x02\u{22d}\u{230}\x07\x56\x02\x02\u{22e}\u{22f}\
	\x07\x09\x02\x02\u{22f}\u{231}\x05\x1c\x0f\x02\u{230}\u{22e}\x03\x02\x02\
	\x02\u{230}\u{231}\x03\x02\x02\x02\u{231}\u{232}\x03\x02\x02\x02\u{232}\
	\u{277}\x07\x0f\x02\x02\u{233}\u{234}\x07\x32\x02\x02\u{234}\u{235}\x07\
	\x05\x02\x02\u{235}\u{236}\x05\x1c\x0f\x02\u{236}\u{237}\x07\x06\x02\x02\
	\u{237}\u{277}\x03\x02\x02\x02\u{238}\u{239}\x07\x34\x02\x02\u{239}\u{23a}\
	\x07\x05\x02\x02\u{23a}\u{23b}\x05\x1c\x0f\x02\u{23b}\u{23c}\x07\x06\x02\
	\x02\u{23c}\u{277}\x03\x02\x02\x02\u{23d}\u{246}\x07\x07\x02\x02\u{23e}\
	\u{243}\x05\x1c\x0f\x02\u{23f}\u{240}\x07\x03\x02\x02\u{240}\u{242}\x05\
	\x1c\x0f\x02\u{241}\u{23f}\x03\x02\x02\x02\u{242}\u{245}\x03\x02\x02\x02\
	\u{243}\u{241}\x03\x02\x02\x02\u{243}\u{244}\x03\x02\x02\x02\u{244}\u{247}\
	\x03\x02\x02\x02\u{245}\u{243}\x03\x02\x02\x02\u{246}\u{23e}\x03\x02\x02\
	\x02\u{246}\u{247}\x03\x02\x02\x02\u{247}\u{248}\x03\x02\x02\x02\u{248}\
	\u{277}\x07\x08\x02\x02\u{249}\u{252}\x07\x07\x02\x02\u{24a}\u{24f}\x05\
	\x1e\x10\x02\u{24b}\u{24c}\x07\x03\x02\x02\u{24c}\u{24e}\x05\x1e\x10\x02\
	\u{24d}\u{24b}\x03\x02\x02\x02\u{24e}\u{251}\x03\x02\x02\x02\u{24f}\u{24d}\
	\x03\x02\x02\x02\u{24f}\u{250}\x03\x02\x02\x02\u{250}\u{253}\x03\x02\x02\
	\x02\u{251}\u{24f}\x03\x02\x02\x02\u{252}\u{24a}\x03\x02\x02\x02\u{252}\
	\u{253}\x03\x02\x02\x02\u{253}\u{254}\x03\x02\x02\x02\u{254}\u{277}\x07\
	\x08\x02\x02\u{255}\u{25e}\x07\x10\x02\x02\u{256}\u{25b}\x05\x1c\x0f\x02\
	\u{257}\u{258}\x07\x03\x02\x02\u{258}\u{25a}\x05\x1c\x0f\x02\u{259}\u{257}\
	\x03\x02\x02\x02\u{25a}\u{25d}\x03\x02\x02\x02\u{25b}\u{259}\x03\x02\x02\
	\x02\u{25b}\u{25c}\x03\x02\x02\x02\u{25c}\u{25f}\x03\x02\x02\x02\u{25d}\
	\u{25b}\x03\x02\x02\x02\u{25e}\u{256}\x03\x02\x02\x02\u{25e}\u{25f}\x03\
	\x02\x02\x02\u{25f}\u{260}\x03\x02\x02\x02\u{260}\u{277}\x07\x11\x02\x02\
	\u{261}\u{262}\x07\x28\x02\x02\u{262}\u{263}\x07\x05\x02\x02\u{263}\u{264}\
	\x05\x1c\x0f\x02\u{264}\u{265}\x07\x03\x02\x02\u{265}\u{266}\x05\x1c\x0f\
	\x02\u{266}\u{267}\x07\x06\x02\x02\u{267}\u{277}\x03\x02\x02\x02\u{268}\
	\u{277}\x07\x2c\x02\x02\u{269}\u{277}\x07\x3f\x02\x02\u{26a}\u{277}\x07\
	\x42\x02\x02\u{26b}\u{277}\x07\x59\x02\x02\u{26c}\u{26d}\x07\x3c\x02\x02\
	\u{26d}\u{26e}\x07\x05\x02\x02\u{26e}\u{26f}\x05\x1c\x0f\x02\u{26f}\u{270}\
	\x07\x06\x02\x02\u{270}\u{277}\x03\x02\x02\x02\u{271}\u{277}\x07\x56\x02\
	\x02\u{272}\u{273}\x07\x05\x02\x02\u{273}\u{274}\x05\x1c\x0f\x02\u{274}\
	\u{275}\x07\x06\x02\x02\u{275}\u{277}\x03\x02\x02\x02\u{276}\u{22b}\x03\
	\x02\x02\x02\u{276}\u{233}\x03\x02\x02\x02\u{276}\u{238}\x03\x02\x02\x02\
	\u{276}\u{23d}\x03\x02\x02\x02\u{276}\u{249}\x03\x02\x02\x02\u{276}\u{255}\
	\x03\x02\x02\x02\u{276}\u{261}\x03\x02\x02\x02\u{276}\u{268}\x03\x02\x02\
	\x02\u{276}\u{269}\x03\x02\x02\x02\u{276}\u{26a}\x03\x02\x02\x02\u{276}\
	\u{26b}\x03\x02\x02\x02\u{276}\u{26c}\x03\x02\x02\x02\u{276}\u{271}\x03\
	\x02\x02\x02\u{276}\u{272}\x03\x02\x02\x02\u{277}\u{281}\x03\x02\x02\x02\
	\u{278}\u{279}\x0c\x05\x02\x02\u{279}\u{27a}\x07\x27\x02\x02\u{27a}\u{280}\
	\x05\x20\x11\x02\u{27b}\u{27c}\x0c\x04\x02\x02\u{27c}\u{27d}\x07\x47\x02\
	\x02\u{27d}\u{27e}\x07\x27\x02\x02\u{27e}\u{280}\x05\x20\x11\x02\u{27f}\
	\u{278}\x03\x02\x02\x02\u{27f}\u{27b}\x03\x02\x02\x02\u{280}\u{283}\x03\
	\x02\x02\x02\u{281}\u{27f}\x03\x02\x02\x02\u{281}\u{282}\x03\x02\x02\x02\
	\u{282}\x1d\x03\x02\x02\x02\u{283}\u{281}\x03\x02\x02\x02\u{284}\u{285}\
	\x07\x56\x02\x02\u{285}\u{286}\x07\x09\x02\x02\u{286}\u{287}\x05\x1c\x0f\
	\x02\u{287}\x1f\x03\x02\x02\x02\u{288}\u{289}\x08\x11\x01\x02\u{289}\u{2da}\
	\x07\x23\x02\x02\u{28a}\u{2da}\x07\x24\x02\x02\u{28b}\u{28c}\x07\x49\x02\
	\x02\u{28c}\u{2da}\x05\x20\x11\x11\u{28d}\u{28e}\x07\x2e\x02\x02\u{28e}\
	\u{297}\x07\x05\x02\x02\u{28f}\u{294}\x05\x20\x11\x02\u{290}\u{291}\x07\
	\x03\x02\x02\u{291}\u{293}\x05\x20\x11\x02\u{292}\u{290}\x03\x02\x02\x02\
	\u{293}\u{296}\x03\x02\x02\x02\u{294}\u{292}\x03\x02\x02\x02\u{294}\u{295}\
	\x03\x02\x02\x02\u{295}\u{298}\x03\x02\x02\x02\u{296}\u{294}\x03\x02\x02\
	\x02\u{297}\u{28f}\x03\x02\x02\x02\u{297}\u{298}\x03\x02\x02\x02\u{298}\
	\u{299}\x03\x02\x02\x02\u{299}\u{29a}\x07\x06\x02\x02\u{29a}\u{29b}\x07\
	\x0b\x02\x02\u{29b}\u{2da}\x05\x20\x11\x0f\u{29c}\u{2a0}\x07\x53\x02\x02\
	\u{29d}\u{29f}\x07\x56\x02\x02\u{29e}\u{29d}\x03\x02\x02\x02\u{29f}\u{2a2}\
	\x03\x02\x02\x02\u{2a0}\u{29e}\x03\x02\x02\x02\u{2a0}\u{2a1}\x03\x02\x02\
	\x02\u{2a1}\u{2a3}\x03\x02\x02\x02\u{2a2}\u{2a0}\x03\x02\x02\x02\u{2a3}\
	\u{2a4}\x07\x1c\x02\x02\u{2a4}\u{2da}\x05\x20\x11\x0e\u{2a5}\u{2a6}\x07\
	\x44\x02\x02\u{2a6}\u{2a7}\x07\x56\x02\x02\u{2a7}\u{2a8}\x07\x1c\x02\x02\
	\u{2a8}\u{2da}\x05\x20\x11\x0d\u{2a9}\u{2b2}\x07\x07\x02\x02\u{2aa}\u{2af}\
	\x05\x20\x11\x02\u{2ab}\u{2ac}\x07\x03\x02\x02\u{2ac}\u{2ae}\x05\x20\x11\
	\x02\u{2ad}\u{2ab}\x03\x02\x02\x02\u{2ae}\u{2b1}\x03\x02\x02\x02\u{2af}\
	\u{2ad}\x03\x02\x02\x02\u{2af}\u{2b0}\x03\x02\x02\x02\u{2b0}\u{2b3}\x03\
	\x02\x02\x02\u{2b1}\u{2af}\x03\x02\x02\x02\u{2b2}\u{2aa}\x03\x02\x02\x02\
	\u{2b2}\u{2b3}\x03\x02\x02\x02\u{2b3}\u{2b4}\x03\x02\x02\x02\u{2b4}\u{2da}\
	\x07\x08\x02\x02\u{2b5}\u{2b6}\x07\x07\x02\x02\u{2b6}\u{2bb}\x05\x22\x12\
	\x02\u{2b7}\u{2b8}\x07\x03\x02\x02\u{2b8}\u{2ba}\x05\x22\x12\x02\u{2b9}\
	\u{2b7}\x03\x02\x02\x02\u{2ba}\u{2bd}\x03\x02\x02\x02\u{2bb}\u{2b9}\x03\
	\x02\x02\x02\u{2bb}\u{2bc}\x03\x02\x02\x02\u{2bc}\u{2be}\x03\x02\x02\x02\
	\u{2bd}\u{2bb}\x03\x02\x02\x02\u{2be}\u{2bf}\x07\x08\x02\x02\u{2bf}\u{2da}\
	\x03\x02\x02\x02\u{2c0}\u{2c9}\x07\x0e\x02\x02\u{2c1}\u{2c6}\x05\x24\x13\
	\x02\u{2c2}\u{2c3}\x07\x03\x02\x02\u{2c3}\u{2c5}\x05\x24\x13\x02\u{2c4}\
	\u{2c2}\x03\x02\x02\x02\u{2c5}\u{2c8}\x03\x02\x02\x02\u{2c6}\u{2c4}\x03\
	\x02\x02\x02\u{2c6}\u{2c7}\x03\x02\x02\x02\u{2c7}\u{2ca}\x03\x02\x02\x02\
	\u{2c8}\u{2c6}\x03\x02\x02\x02\u{2c9}\u{2c1}\x03\x02\x02\x02\u{2c9}\u{2ca}\
	\x03\x02\x02\x02\u{2ca}\u{2cb}\x03\x02\x02\x02\u{2cb}\u{2da}\x07\x0f\x02\
	\x02\u{2cc}\u{2cd}\x07\x10\x02\x02\u{2cd}\u{2ce}\x05\x20\x11\x02\u{2ce}\
	\u{2cf}\x07\x11\x02\x02\u{2cf}\u{2da}\x03\x02\x02\x02\u{2d0}\u{2da}\x07\
	\x25\x02\x02\u{2d1}\u{2da}\x07\x4f\x02\x02\u{2d2}\u{2da}\x07\x50\x02\x02\
	\u{2d3}\u{2da}\x07\x51\x02\x02\u{2d4}\u{2da}\x07\x56\x02\x02\u{2d5}\u{2d6}\
	\x07\x05\x02\x02\u{2d6}\u{2d7}\x05\x20\x11\x02\u{2d7}\u{2d8}\x07\x06\x02\
	\x02\u{2d8}\u{2da}\x03\x02\x02\x02\u{2d9}\u{288}\x03\x02\x02\x02\u{2d9}\
	\u{28a}\x03\x02\x02\x02\u{2d9}\u{28b}\x03\x02\x02\x02\u{2d9}\u{28d}\x03\
	\x02\x02\x02\u{2d9}\u{29c}\x03\x02\x02\x02\u{2d9}\u{2a5}\x03\x02\x02\x02\
	\u{2d9}\u{2a9}\x03\x02\x02\x02\u{2d9}\u{2b5}\x03\x02\x02\x02\u{2d9}\u{2c0}\
	\x03\x02\x02\x02\u{2d9}\u{2cc}\x03\x02\x02\x02\u{2d9}\u{2d0}\x03\x02\x02\
	\x02\u{2d9}\u{2d1}\x03\x02\x02\x02\u{2d9}\u{2d2}\x03\x02\x02\x02\u{2d9}\
	\u{2d3}\x03\x02\x02\x02\u{2d9}\u{2d4}\x03\x02\x02\x02\u{2d9}\u{2d5}\x03\
	\x02\x02\x02\u{2da}\u{2e0}\x03\x02\x02\x02\u{2db}\u{2dc}\x0c\x10\x02\x02\
	\u{2dc}\u{2dd}\x07\x18\x02\x02\u{2dd}\u{2df}\x05\x20\x11\x11\u{2de}\u{2db}\
	\x03\x02\x02\x02\u{2df}\u{2e2}\x03\x02\x02\x02\u{2e0}\u{2de}\x03\x02\x02\
	\x02\u{2e0}\u{2e1}\x03\x02\x02\x02\u{2e1}\x21\x03\x02\x02\x02\u{2e2}\u{2e0}\
	\x03\x02\x02\x02\u{2e3}\u{2e4}\x07\x56\x02\x02\u{2e4}\u{2e5}\x07\x0a\x02\
	\x02\u{2e5}\u{2e6}\x05\x20\x11\x02\u{2e6}\x23\x03\x02\x02\x02\u{2e7}\u{2ea}\
	\x07\x56\x02\x02\u{2e8}\u{2e9}\x07\x0a\x02\x02\u{2e9}\u{2eb}\x05\x20\x11\
	\x02\u{2ea}\u{2e8}\x03\x02\x02\x02\u{2ea}\u{2eb}\x03\x02\x02\x02\u{2eb}\
	\x25\x03\x02\x02\x02\x3d\x33\x39\x47\x4f\x5a\x5d\x62\x6a\x6d\x73\x7d\u{89}\
	\u{93}\u{96}\u{9b}\u{a3}\u{a6}\u{ac}\u{c0}\u{153}\u{156}\u{164}\u{167}\u{170}\
	\u{179}\u{184}\u{187}\u{191}\u{194}\u{1a4}\u{1b0}\u{1bd}\u{1c6}\u{1ff}\u{202}\
	\u{20c}\u{21a}\u{21c}\u{230}\u{243}\u{246}\u{24f}\u{252}\u{25b}\u{25e}\u{276}\
	\u{27f}\u{281}\u{294}\u{297}\u{2a0}\u{2af}\u{2b2}\u{2bb}\u{2c6}\u{2c9}\u{2d9}\
	\u{2e0}\u{2ea}";

