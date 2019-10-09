use crate::errors::ShellError;
use crate::parser::{
    hir,
    hir::syntax_shape::{
        color_syntax, expand_atom, expand_expr, maybe_spaced, spaced, AnyExpressionShape,
        ColorSyntax, ExpandContext, ExpandSyntax, ExpansionRule, SpaceShape,
    },
    hir::TokensIterator,
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
    type Input = ();

    /// The intent of this method is to fully color an expression list shape infallibly.
    /// This means that if we can't expand a token into an expression, we fall back to
    /// a simpler coloring strategy.
    ///
    /// This would apply to something like `where x >`, which includes an incomplete
    /// binary operator. Since we will fail to process it as a binary operator, we'll
    /// fall back to a simpler coloring and move on.
    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        // We encountered a parsing error and will continue with simpler coloring ("backoff
        // coloring mode")
        let mut backoff = false;

        loop {
            // If we reached the very end of the token stream, we're done
            if token_nodes.at_end() {
                return;
            }

            if backoff {
                let len = shapes.len();

                // If we previously encountered a parsing error, switch to backoff coloring mode
                color_syntax(&SimplestExpression, token_nodes, context, shapes);

                if len == shapes.len() && !token_nodes.at_end() {
                    // This should never happen, but if it does, a panic is better than an infinite loop
                    panic!("Unexpected tokens left that couldn't be colored even with SimplestExpression")
                }
            } else {
                match color_syntax(&SpaceShape, token_nodes, context, shapes).1 {
                    Err(_) => backoff = true,
                    Ok(_) => {
                        // Otherwise, try to color the head of the stream as an expression
                        match color_syntax(&AnyExpressionShape, token_nodes, context, shapes).1 {
                            Err(_) => backoff = true,
                            Ok(_) => {}
                        }
                    }
                }
            }
        }
    }
}

/// BackoffColoringMode consumes all of the remaining tokens in an infallible way
#[derive(Debug, Copy, Clone)]
pub struct BackoffColoringMode;

impl ColorSyntax for BackoffColoringMode {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &Self::Input,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        loop {
            if token_nodes.at_end() {
                break;
            }

            let len = shapes.len();
            color_syntax(&SimplestExpression, token_nodes, context, shapes);

            if len == shapes.len() && !token_nodes.at_end() {
                // This shouldn't happen, but if it does, a panic is better than an infinite loop
                panic!("SimplestExpression failed to consume any tokens, but it's not at the end. This is unexpected\n== token nodes==\n{:#?}\n\n== shapes ==\n{:#?}", token_nodes, shapes);
            }
        }
    }
}

/// The point of `SimplestExpression` is to serve as an infallible base case for coloring.
/// As a last ditch effort, if we can't find any way to parse the head of the stream as an
/// expression, fall back to simple coloring.
#[derive(Debug, Copy, Clone)]
pub struct SimplestExpression;

impl ColorSyntax for SimplestExpression {
    type Info = ();
    type Input = ();

    fn color_syntax<'a, 'b>(
        &self,
        _input: &(),
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) {
        let atom = expand_atom(
            token_nodes,
            "any token",
            context,
            ExpansionRule::permissive(),
        );

        match atom {
            Err(_) => {}
            Ok(atom) => atom.color_tokens(shapes),
        }
    }
}
