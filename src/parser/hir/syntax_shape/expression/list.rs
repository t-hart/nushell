use crate::errors::ShellError;
use crate::parser::{
    hir,
    hir::syntax_shape::{
        color_syntax, expand_expr, maybe_spaced, spaced, AnyExpressionShape, ColorSyntax,
        ExpandContext, ExpandSyntax,
    },
    hir::{debug_tokens, TokensIterator},
    FlatShape,
};
use crate::Tagged;

#[derive(Debug, Copy, Clone)]
pub struct ExpressionListShape;

impl ExpandSyntax for ExpressionListShape {
    type Output = Vec<hir::Expression>;

    fn expand_syntax<'a, 'b>(
        &self,
        token_nodes: &mut TokensIterator<'_>,
        context: &ExpandContext,
    ) -> Result<Vec<hir::Expression>, ShellError> {
        let mut exprs = vec![];

        if token_nodes.at_end_possible_ws() {
            return Ok(exprs);
        }

        let expr = expand_expr(&maybe_spaced(AnyExpressionShape), token_nodes, context)?;

        exprs.push(expr);

        loop {
            if token_nodes.at_end_possible_ws() {
                return Ok(exprs);
            }

            let expr = expand_expr(&spaced(AnyExpressionShape), token_nodes, context)?;

            exprs.push(expr);
        }
    }
}

impl ColorSyntax for ExpressionListShape {
    type Info = ();

    fn color_syntax<'a, 'b>(
        &self,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        if token_nodes.at_end_possible_ws() {
            return;
        }

        color_syntax(
            &maybe_spaced(AnyExpressionShape),
            token_nodes,
            context,
            shapes,
        );

        loop {
            if token_nodes.at_end_possible_ws() {
                return;
            }

            color_syntax(&spaced(AnyExpressionShape), token_nodes, context, shapes);
        }
    }
}
