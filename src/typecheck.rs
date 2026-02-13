use crate::ast::*;

pub type TypeError = String;

fn typecheck_decl(decl: &Decl) -> Result<(), TypeError> {
    match decl {
        Decl::DeclFun { .. } => Ok(()),
        Decl::DeclGenericFun { .. } => todo!(),
        Decl::DeclTypeAlias { name: _, type_: _ } => todo!(),
        Decl::DeclExceptionType(_) => todo!(),
        Decl::DeclExceptionVariant { .. } => todo!(),
    }
}

pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    program.decls.iter().try_for_each(typecheck_decl)
}
