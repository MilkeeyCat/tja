use crate::ast::{Expr, Pattern};

pub trait Visitor: Sized {
    fn visit_pat(&self, pat: &Pattern) {
        walk_pat(self, pat);
    }

    fn visit_expr(&self, expr: &Expr) {
        walk_expr(self, expr);
    }
}

pub fn walk_pat<T: Visitor>(visitor: &T, pat: &Pattern) {
    match pat {
        Pattern::Application { args, .. } => {
            for pat in args {
                visitor.visit_pat(pat);
            }
        }
        Pattern::Literal(_) | Pattern::Ident(_) | Pattern::Wildcard => (),
    }
}

pub fn walk_expr<T: Visitor>(visitor: &T, expr: &Expr) {
    match expr {
        Expr::Call { args, .. } => {
            for expr in args {
                visitor.visit_expr(expr);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) => (),
    }
}
