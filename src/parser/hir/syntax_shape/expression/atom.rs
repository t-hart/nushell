use crate::parser::hir::syntax_shape::{
    expand_syntax, expression::expand_file_path, parse_single_node, BarePathShape, ExpandContext,
    UnitShape,
};
use crate::parser::{
    hir,
    hir::{Expression, RawNumber, TokensIterator},
    DelimitedNode, Delimiter, FlatShape, Operator, RawToken, TokenNode, Unit,
};
use crate::prelude::*;

#[derive(Debug)]
pub enum AtomicToken<'tokens> {
    Number {
        number: RawNumber,
    },
    Size {
        number: Tagged<RawNumber>,
        unit: Tagged<Unit>,
    },
    String {
        body: Tag,
    },
    ItVariable {
        name: Tag,
    },
    Variable {
        name: Tag,
    },
    ExternalCommand {
        command: Tag,
    },
    ExternalWord {
        text: Tag,
    },
    GlobPattern {
        pattern: Tag,
    },
    FilePath {
        path: Tag,
    },
    Word {
        text: Tag,
    },
    SquareDelimited {
        tags: (Tag, Tag),
        nodes: &'tokens Vec<TokenNode>,
    },
    ParenDelimited {
        tags: (Tag, Tag),
        nodes: &'tokens Vec<TokenNode>,
    },
    BraceDelimited {
        tags: (Tag, Tag),
        nodes: &'tokens Vec<TokenNode>,
    },
    Pipeline {
        pipe: Option<Tag>,
        elements: Tagged<&'tokens Vec<TokenNode>>,
    },
    ShorthandFlag {
        name: Tag,
    },
    LonghandFlag {
        name: Tag,
    },
    Dot {
        text: Tag,
    },
    Whitespace {
        text: Tag,
    },
}

pub type TaggedAtomicToken<'tokens> = Tagged<AtomicToken<'tokens>>;

impl<'tokens> TaggedAtomicToken<'tokens> {
    pub fn into_hir(
        &self,
        context: &ExpandContext,
        expected: &'static str,
    ) -> Result<hir::Expression, ShellError> {
        Ok(match &self.item {
            AtomicToken::ShorthandFlag { .. } => {
                return Err(ShellError::type_error(
                    expected,
                    "shorthand flag".tagged(self.tag),
                ))
            }
            AtomicToken::LonghandFlag { .. } => {
                return Err(ShellError::type_error(expected, "flag".tagged(self.tag)))
            }
            AtomicToken::Whitespace { .. } => {
                return Err(ShellError::unimplemented("whitespace in AtomicToken"))
            }
            AtomicToken::Dot { .. } => {
                return Err(ShellError::type_error(expected, "dot".tagged(self.tag)))
            }
            AtomicToken::Number { number } => {
                Expression::number(number.to_number(context.source), self.tag)
            }
            AtomicToken::FilePath { path } => Expression::file_path(
                expand_file_path(path.slice(context.source), context),
                self.tag,
            ),
            AtomicToken::Size { number, unit } => {
                Expression::size(number.to_number(context.source), **unit, self.tag)
            }
            AtomicToken::String { body } => Expression::string(body, self.tag),
            AtomicToken::ItVariable { name } => Expression::it_variable(name, self.tag),
            AtomicToken::Variable { name } => Expression::variable(name, self.tag),
            AtomicToken::ExternalCommand { command } => {
                Expression::external_command(command, self.tag)
            }
            AtomicToken::ExternalWord { text } => Expression::string(text, self.tag),
            AtomicToken::GlobPattern { pattern } => Expression::pattern(pattern),
            AtomicToken::Word { text } => Expression::string(text, text),
            AtomicToken::SquareDelimited { .. } => unimplemented!("into_hir"),
            AtomicToken::ParenDelimited { .. } => unimplemented!("into_hir"),
            AtomicToken::BraceDelimited { .. } => unimplemented!("into_hir"),
            AtomicToken::Pipeline { .. } => unimplemented!("into_hir"),
        })
    }

    pub(crate) fn color_tokens(&self, shapes: &mut Vec<Tagged<FlatShape>>) {
        match &self.item {
            AtomicToken::ShorthandFlag { name } => {
                return shapes.push(FlatShape::ShorthandFlag.tagged(name));
            }
            AtomicToken::LonghandFlag { name } => {
                return shapes.push(FlatShape::Flag.tagged(name));
            }
            AtomicToken::Whitespace { text } => {
                return shapes.push(FlatShape::Whitespace.tagged(text));
            }
            AtomicToken::FilePath { path } => return shapes.push(FlatShape::Path.tagged(path)),
            AtomicToken::Dot { text } => return shapes.push(FlatShape::Dot.tagged(text)),
            AtomicToken::Number {
                number: RawNumber::Decimal(tag),
            } => {
                return shapes.push(FlatShape::Decimal.tagged(tag));
            }
            AtomicToken::Number {
                number: RawNumber::Int(tag),
            } => {
                return shapes.push(FlatShape::Int.tagged(tag));
            }
            AtomicToken::Size { number, unit } => {
                return shapes.push(
                    FlatShape::Size {
                        number: number.tag,
                        unit: unit.tag,
                    }
                    .tagged(self.tag),
                );
            }
            AtomicToken::String { body } => return shapes.push(FlatShape::String.tagged(body)),
            AtomicToken::ItVariable { name } => {
                return shapes.push(FlatShape::ItVariable.tagged(name))
            }
            AtomicToken::Variable { name } => return shapes.push(FlatShape::Variable.tagged(name)),
            AtomicToken::ExternalCommand { command } => {
                return shapes.push(FlatShape::ExternalCommand.tagged(command));
            }
            AtomicToken::ExternalWord { text } => {
                return shapes.push(FlatShape::ExternalWord.tagged(text))
            }
            AtomicToken::GlobPattern { pattern } => {
                return shapes.push(FlatShape::GlobPattern.tagged(pattern))
            }
            AtomicToken::Word { text } => return shapes.push(FlatShape::Word.tagged(text)),
            AtomicToken::SquareDelimited { .. } => {
                unreachable!("BUG: handle nested tokens before calling into_hir")
            }
            AtomicToken::ParenDelimited { .. } => {
                unreachable!("BUG: handle nested tokens before calling into_hir")
            }
            AtomicToken::BraceDelimited { .. } => {
                unreachable!("BUG: handle nested tokens before calling into_hir")
            }
            AtomicToken::Pipeline { .. } => {
                unreachable!("BUG: handle nested tokens before calling into_hir")
            }
        }
    }
}

#[derive(Debug)]
pub enum WhitespaceHandling {
    #[allow(unused)]
    AllowWhitespace,
    RejectWhitespace,
    #[allow(unused)]
    RequireWhitespace,
}

#[derive(Debug)]
pub struct ExpansionRule {
    pub(crate) expand_bare_path: bool,
    pub(crate) allow_external_command: bool,
    pub(crate) allow_external_word: bool,
    pub(crate) treat_size_as_word: bool,
    pub(crate) whitespace: WhitespaceHandling,
}

impl ExpansionRule {
    pub fn new() -> ExpansionRule {
        ExpansionRule {
            expand_bare_path: false,
            allow_external_command: false,
            allow_external_word: false,
            treat_size_as_word: false,
            whitespace: WhitespaceHandling::RejectWhitespace,
        }
    }

    pub fn permissive() -> ExpansionRule {
        ExpansionRule {
            expand_bare_path: false,
            allow_external_command: true,
            allow_external_word: true,
            treat_size_as_word: false,
            whitespace: WhitespaceHandling::RejectWhitespace,
        }
    }

    #[allow(unused)]
    pub fn expand_bare_path(mut self) -> ExpansionRule {
        self.expand_bare_path = true;
        self
    }

    #[allow(unused)]
    pub fn allow_external_command(mut self) -> ExpansionRule {
        self.allow_external_command = true;
        self
    }

    #[allow(unused)]
    pub fn allow_external_word(mut self) -> ExpansionRule {
        self.allow_external_word = true;
        self
    }

    #[allow(unused)]
    pub fn treat_size_as_word(mut self) -> ExpansionRule {
        self.treat_size_as_word = true;
        self
    }

    #[allow(unused)]
    pub fn allow_whitespace(mut self) -> ExpansionRule {
        self.whitespace = WhitespaceHandling::AllowWhitespace;
        self
    }

    #[allow(unused)]
    pub fn require_whitespace(mut self) -> ExpansionRule {
        self.whitespace = WhitespaceHandling::RequireWhitespace;
        self
    }
}

/// If the caller of expand_atom throws away the returned atomic token returned, it
/// must use a checkpoint to roll it back.
pub fn expand_atom<'me, 'content>(
    token_nodes: &'me mut TokensIterator<'content>,
    expected: &'static str,
    context: &ExpandContext,
    rule: ExpansionRule,
) -> Result<TaggedAtomicToken<'content>, ShellError> {
    match rule.treat_size_as_word {
        true => {}
        false => match expand_syntax(&UnitShape, token_nodes, context) {
            Err(_) => {}
            Ok(Tagged {
                item: (number, unit),
                tag,
            }) => return Ok(AtomicToken::Size { number, unit }.tagged(tag)),
        },
    }

    match rule.expand_bare_path {
        true => return Err(ShellError::unimplemented("expand_atom ExpandBarePath")),
        false => {
            let word = expand_syntax(&BarePathShape, token_nodes, context);

            match word {
                Ok(word) => return Ok(AtomicToken::Word { text: word }.tagged(word)),
                Err(_) => {}
            }
        }
    }

    let peeked = token_nodes.peek_any().not_eof(expected)?;

    match peeked.node {
        TokenNode::Delimited(Tagged {
            item:
                DelimitedNode {
                    delimiter: Delimiter::Square,
                    tags,
                    children,
                },
            tag,
        }) => {
            peeked.commit();
            return Ok(AtomicToken::SquareDelimited {
                nodes: children,
                tags: *tags,
            }
            .tagged(tag));
        }

        _ => {}
    }

    parse_single_node(token_nodes, expected, |token, token_tag, err| {
        Ok(match token {
            RawToken::Number(number) => AtomicToken::Number { number }.tagged(token_tag),
            RawToken::Operator(Operator::Dot) => return Err(err.error()),
            RawToken::Operator(_) => return Err(err.error()),
            RawToken::String(body) => AtomicToken::String { body }.tagged(token_tag),
            RawToken::Variable(name) if name.slice(context.source) == "it" => {
                AtomicToken::ItVariable { name }.tagged(token_tag)
            }
            RawToken::Variable(name) => AtomicToken::Variable { name }.tagged(token_tag),
            RawToken::ExternalCommand(command) if rule.allow_external_command => {
                AtomicToken::ExternalCommand { command }.tagged(token_tag)
            }
            RawToken::ExternalCommand(_) => {
                return Err(ShellError::type_error(
                    expected,
                    token.type_name().tagged(token_tag),
                ))
            }
            RawToken::ExternalWord if rule.allow_external_word => {
                AtomicToken::ExternalWord { text: token_tag }.tagged(token_tag)
            }
            RawToken::ExternalWord => return Err(ShellError::invalid_external_word(token_tag)),
            RawToken::GlobPattern => {
                AtomicToken::GlobPattern { pattern: token_tag }.tagged(token_tag)
            }
            RawToken::Bare => AtomicToken::Word { text: token_tag }.tagged(token_tag),
        })
    })
}
