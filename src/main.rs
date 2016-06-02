extern crate pad;
extern crate syntex_syntax as syntax;

use std::collections::HashMap;
use std::env;
use std::path::Path;

use pad::PadStr;

use syntax::ast;
use syntax::codemap::{CodeMap, Loc, Span};
use syntax::errors::DiagnosticBuilder;
use syntax::parse::{self, ParseSess};
use syntax::visit::{self, FnKind, Visitor};

fn parse<'a, T: ?Sized + AsRef<Path>>(path: &T,
                                      parse_session: &'a ParseSess)
                                      -> Result<ast::Crate, Option<DiagnosticBuilder<'a>>> {
    let path = path.as_ref();
    let cfgs = vec![];

    match parse::parse_crate_from_file(path, cfgs, parse_session) {
        // There may be parse errors that the parser recovered from, which we want to treat as an
        // error.
        Ok(_) if parse_session.span_diagnostic.has_errors() => Err(None),
        Ok(krate) => Ok(krate),
        Err(e) => Err(Some(e)),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let parse_session = ParseSess::new();
    let krate = parse(args[1].as_str(), &parse_session).unwrap();

    let mut counts: Vec<_> = count_fn_args(&krate, parse_session.codemap()).into_iter().collect();
    // We could just `sort` instead of `sort_by`, since the (name, count) pair would sort by name
    // first. But I wanted there to be a closure in here for running the program against its own
    // source. :-)
    counts.sort_by(|x, y| x.0.cmp(&y.0));

    println!("{} {}", "FUNCTION".with_exact_width(32), "ARGS");
    for (func, num_args) in counts {
        println!("{} {}", func.with_exact_width(32), num_args);
    }
}

struct CountFnArgs<'a> {
    arg_counts: HashMap<String, usize>,
    // The codemap is necessary to go from a `Span` to actual line & column numbers for closures.
    codemap: &'a CodeMap,
}

impl<'a> CountFnArgs<'a> {
    fn format_span(&self, span: Span) -> String {
        format!("{}-{}",
                format_loc(&self.codemap.lookup_char_pos(span.lo)),
                format_loc(&self.codemap.lookup_char_pos(span.hi)))
    }
}

fn format_loc(loc: &Loc) -> String {
    format!("{}:{}", loc.line, loc.col.0)
}

impl<'v, 'a> Visitor<'v> for CountFnArgs<'a> {
    fn visit_fn(&mut self,
                fn_kind: FnKind<'v>,
                fn_decl: &'v ast::FnDecl,
                block: &'v ast::Block,
                span: Span,
                _id: ast::NodeId) {
        let fn_name = match fn_kind {
            FnKind::ItemFn(id, _, _, _, _, _) |
            FnKind::Method(id, _, _) => id.name.as_str().to_string(),
            FnKind::Closure => format!("<closure at {}>", self.format_span(span)),
        };

        self.arg_counts.insert(fn_name, fn_decl.inputs.len());

        // Continue walking the rest of the funciton so we pick up any functions or closures
        // defined in its body.
        visit::walk_fn(self, fn_kind, fn_decl, block, span);
    }

    // The default implementation panics, so this is needed to work on files with macro
    // invocations, eg calls to `format!()` above. A better solution would be to expand macros before
    // walking the AST, but I haven't looked at how to do that. We will miss any functions defined
    // via a macro, but that's fine for this example.
    fn visit_mac(&mut self, _mac: &'v ast::Mac) {}
}

fn count_fn_args(krate: &ast::Crate, codemap: &CodeMap) -> HashMap<String, usize> {
    let mut visitor = CountFnArgs { arg_counts: HashMap::new(), codemap: codemap };
    visitor.visit_mod(&krate.module, krate.span, 0);

    visitor.arg_counts
}
