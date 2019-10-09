use crate::errors::{ArgumentError, ShellError};
use crate::parser::hir::syntax_shape::{
    color_syntax, expand_expr, flat_shape::FlatShape, spaced, BackoffColoringMode, ColorSyntax,
    MaybeSpaceShape,
};
use crate::parser::registry::{NamedType, PositionalType, Signature};
use crate::parser::TokensIterator;
use crate::parser::{
    hir::{self, ExpandContext, NamedArguments},
    Flag,
};
use crate::traits::ToDebug;
use crate::{Tag, Tagged, Text};
use log::trace;

pub fn parse_command_tail(
    config: &Signature,
    context: &ExpandContext,
    tail: &mut TokensIterator,
    command_tag: Tag,
) -> Result<Option<(Option<Vec<hir::Expression>>, Option<NamedArguments>)>, ShellError> {
    let mut named = NamedArguments::new();
    trace_remaining("nodes", tail.clone(), context.source());

    for (name, kind) in &config.named {
        trace!(target: "nu::parse", "looking for {} : {:?}", name, kind);

        match kind {
            NamedType::Switch => {
                let flag = extract_switch(name, tail, context.source());

                named.insert_switch(name, flag);
            }
            NamedType::Mandatory(syntax_type) => {
                match extract_mandatory(config, name, tail, context.source(), command_tag) {
                    Err(err) => return Err(err), // produce a correct diagnostic
                    Ok((pos, flag)) => {
                        tail.move_to(pos);

                        if tail.at_end() {
                            return Err(ShellError::argument_error(
                                config.name.clone(),
                                ArgumentError::MissingValueForName(name.to_string()),
                                flag.tag(),
                            ));
                        }

                        let expr = expand_expr(&spaced(*syntax_type), tail, context)?;

                        tail.restart();
                        named.insert_mandatory(name, expr);
                    }
                }
            }
            NamedType::Optional(syntax_type) => {
                match extract_optional(name, tail, context.source()) {
                    Err(err) => return Err(err), // produce a correct diagnostic
                    Ok(Some((pos, flag))) => {
                        tail.move_to(pos);

                        if tail.at_end() {
                            return Err(ShellError::argument_error(
                                config.name.clone(),
                                ArgumentError::MissingValueForName(name.to_string()),
                                flag.tag(),
                            ));
                        }

                        let expr = expand_expr(&spaced(*syntax_type), tail, context);

                        match expr {
                            Err(_) => named.insert_optional(name, None),
                            Ok(expr) => named.insert_optional(name, Some(expr)),
                        }

                        tail.restart();
                    }

                    Ok(None) => {
                        tail.restart();
                        named.insert_optional(name, None);
                    }
                }
            }
        };
    }

    trace_remaining("after named", tail.clone(), context.source());

    let mut positional = vec![];

    for arg in &config.positional {
        trace!("Processing positional {:?}", arg);

        match arg {
            PositionalType::Mandatory(..) => {
                if tail.at_end() {
                    return Err(ShellError::argument_error(
                        config.name.clone(),
                        ArgumentError::MissingMandatoryPositional(arg.name().to_string()),
                        command_tag,
                    ));
                }
            }

            PositionalType::Optional(..) => {
                if tail.at_end() {
                    break;
                }
            }
        }

        let result = expand_expr(&spaced(arg.syntax_type()), tail, context)?;

        positional.push(result);
    }

    trace_remaining("after positional", tail.clone(), context.source());

    if let Some(syntax_type) = config.rest_positional {
        let mut out = vec![];

        loop {
            if tail.at_end_possible_ws() {
                break;
            }

            let next = expand_expr(&spaced(syntax_type), tail, context)?;

            out.push(next);
        }

        positional.extend(out);
    }

    trace_remaining("after rest", tail.clone(), context.source());

    trace!("Constructed positional={:?} named={:?}", positional, named);

    let positional = if positional.len() == 0 {
        None
    } else {
        Some(positional)
    };

    // TODO: Error if extra unconsumed positional arguments

    let named = if named.named.is_empty() {
        None
    } else {
        Some(named)
    };

    trace!("Normalized positional={:?} named={:?}", positional, named);

    Ok(Some((positional, named)))
}

#[derive(Debug)]
struct ColoringArgs {
    vec: Vec<Option<Vec<Tagged<FlatShape>>>>,
}

impl ColoringArgs {
    fn new(len: usize) -> ColoringArgs {
        let vec = vec![None; len];
        ColoringArgs { vec }
    }

    fn insert(&mut self, pos: usize, shapes: Vec<Tagged<FlatShape>>) {
        self.vec[pos] = Some(shapes);
    }

    fn spread_shapes(self, shapes: &mut Vec<Tagged<FlatShape>>) {
        for item in self.vec {
            match item {
                None => {}
                Some(vec) => {
                    shapes.extend(vec);
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CommandTailShape;

impl ColorSyntax for CommandTailShape {
    type Info = ();
    type Input = Signature;

    fn color_syntax<'a, 'b>(
        &self,
        signature: &Signature,
        token_nodes: &'b mut TokensIterator<'a>,
        context: &ExpandContext,
        shapes: &mut Vec<Tagged<FlatShape>>,
    ) -> Self::Info {
        let mut args = ColoringArgs::new(token_nodes.len());
        trace_remaining("nodes", token_nodes.clone(), context.source());

        for (name, kind) in &signature.named {
            trace!(target: "nu::parse", "looking for {} : {:?}", name, kind);

            match kind {
                NamedType::Switch => {
                    match token_nodes.extract(|t| t.as_flag(name, context.source())) {
                        Some((pos, flag)) => args.insert(pos, vec![flag.color()]),
                        None => {}
                    }
                }
                NamedType::Mandatory(syntax_type) => {
                    match extract_mandatory(
                        signature,
                        name,
                        token_nodes,
                        context.source(),
                        Tag::unknown(),
                    ) {
                        Err(_) => {
                            // The mandatory flag didn't exist at all, so there's nothing to color
                        }
                        Ok((pos, flag)) => {
                            let mut shapes = vec![flag.color()];
                            token_nodes.move_to(pos);

                            if token_nodes.at_end() {
                                args.insert(pos, shapes);
                                token_nodes.restart();
                                continue;
                            }

                            color_syntax(&MaybeSpaceShape, token_nodes, context, &mut shapes);
                            match color_syntax(syntax_type, token_nodes, context, &mut shapes).1 {
                                Err(_) => {
                                    // this is ok; it means the part after a mandatory flag isn't
                                    // a valid expression, but that can happen while typing and
                                    // we can live with it
                                }

                                Ok(_) => {}
                            }

                            args.insert(pos, shapes);
                            token_nodes.restart();
                        }
                    }
                }
                NamedType::Optional(syntax_type) => {
                    match extract_optional(name, token_nodes, context.source()) {
                        Err(_) => {
                            // The optional flag didn't exist at all, so there's nothing to color
                        }
                        Ok(Some((pos, flag))) => {
                            let mut shapes = vec![flag.color()];
                            token_nodes.move_to(pos);

                            if token_nodes.at_end() {
                                args.insert(pos, shapes);
                                token_nodes.restart();
                                continue;
                            }

                            color_syntax(&MaybeSpaceShape, token_nodes, context, &mut shapes);
                            match color_syntax(syntax_type, token_nodes, context, &mut shapes).1 {
                                Err(_) => {
                                    // this is ok; it means the part after an optional flag isn't
                                    // a valid expression, but that can happen while typing and
                                    // we can live with it
                                }

                                Ok(_) => {}
                            }

                            args.insert(pos, shapes);
                            token_nodes.restart();
                        }

                        Ok(None) => {
                            token_nodes.restart();
                        }
                    }
                }
            };
        }

        trace_remaining("after named", token_nodes.clone(), context.source());

        for arg in &signature.positional {
            trace!("Processing positional {:?}", arg);

            match arg {
                PositionalType::Mandatory(..) => {
                    if token_nodes.at_end() {
                        return;
                    }
                }

                PositionalType::Optional(..) => {
                    if token_nodes.at_end() {
                        break;
                    }
                }
            }

            let mut shapes = vec![];
            let pos = token_nodes.pos(false);

            match pos {
                None => break,
                Some(pos) => {
                    color_syntax(&MaybeSpaceShape, token_nodes, context, &mut shapes);
                    match color_syntax(&arg.syntax_type(), token_nodes, context, &mut shapes).1 {
                        Err(_) => {
                            // There are more tokens and we were expecting a positional argument here,
                            // but it didn't match. Hopefully it will be matched by a future token
                        }
                        Ok(_) => {
                            args.insert(pos, shapes);
                        }
                    }
                }
            }
        }

        trace_remaining("after positional", token_nodes.clone(), context.source());

        if let Some(syntax_type) = signature.rest_positional {
            loop {
                if token_nodes.at_end_possible_ws() {
                    break;
                }

                let pos = token_nodes.pos(false);

                match pos {
                    None => break,
                    Some(pos) => {
                        let mut shapes = vec![];
                        color_syntax(&MaybeSpaceShape, token_nodes, context, &mut shapes);
                        match color_syntax(&syntax_type, token_nodes, context, &mut shapes).1 {
                            Err(_) => {
                                // There are more tokens and we were expecting a positional argument here,
                                // but it didn't match. We're going to have to fall back to consuming the
                                // token with backoff coloring mode
                                break;
                            }

                            Ok(_) => {}
                        }
                        args.insert(pos, shapes);
                    }
                }
            }
        }

        args.spread_shapes(shapes);

        // Consume any remaining tokens with backoff coloring mode
        color_syntax(&BackoffColoringMode, token_nodes, context, shapes);
    }
}

fn extract_switch(name: &str, tokens: &mut hir::TokensIterator<'_>, source: &Text) -> Option<Flag> {
    tokens
        .extract(|t| t.as_flag(name, source))
        .map(|(_pos, flag)| flag.item)
}

fn extract_mandatory(
    config: &Signature,
    name: &str,
    tokens: &mut hir::TokensIterator<'_>,
    source: &Text,
    tag: Tag,
) -> Result<(usize, Tagged<Flag>), ShellError> {
    let flag = tokens.extract(|t| t.as_flag(name, source));

    match flag {
        None => Err(ShellError::argument_error(
            config.name.clone(),
            ArgumentError::MissingMandatoryFlag(name.to_string()),
            tag,
        )),

        Some((pos, flag)) => {
            tokens.remove(pos);
            Ok((pos, flag))
        }
    }
}

fn extract_optional(
    name: &str,
    tokens: &mut hir::TokensIterator<'_>,
    source: &Text,
) -> Result<(Option<(usize, Tagged<Flag>)>), ShellError> {
    let flag = tokens.extract(|t| t.as_flag(name, source));

    match flag {
        None => Ok(None),
        Some((pos, flag)) => {
            tokens.remove(pos);
            Ok(Some((pos, flag)))
        }
    }
}

pub fn trace_remaining(desc: &'static str, tail: hir::TokensIterator<'_>, source: &Text) {
    trace!(
        target: "nu::expand_args",
        "{} = {:?}",
        desc,
        itertools::join(
            tail.debug_remaining()
                .iter()
                .map(|i| format!("%{}%", i.debug(&source))),
            " "
        )
    );
}
