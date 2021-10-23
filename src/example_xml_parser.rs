use crate::parser::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

pub fn identifier<'input>() -> impl Parser<'input, String> {
    let start = one_or_more(any_char.pred(|c| c.is_alphabetic()));
    let rest = zero_or_more(any_char.pred(|c| c.is_alphanumeric() || *c == '-'));

    pair(start, rest)
        .map(|(first, second)| first
            .iter()
            .chain(second.iter())
            .collect())
}

pub fn quoted_string<'input>() -> impl Parser<'input, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    ).map(|chars| chars.iter().collect())
}

pub fn attribute_pair<'input>() -> impl Parser<'input, (String, String)> {
    pair(
        identifier(),
        right(
            match_literal("="),
            quoted_string(),
        ),
    )
}

pub fn attributes<'input>() -> impl Parser<'input, Vec<(String, String)>> {
    zero_or_more(
        right(
            space1(),
            attribute_pair(),
        )
    )
}

pub fn element_start<'input>() -> impl Parser<'input, (String, Vec<(String, String)>)> {
    right(
        match_literal("<"),
        pair(
            identifier(),
            attributes(),
        ),
    )
}

pub fn element<'input>(end_literal: &'static str) -> impl Parser<'input, Element> {
    left(element_start(), match_literal(end_literal))
        .map(|(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        })
}

pub fn single_element<'input>() -> impl Parser<'input, Element> {
    element("/>")
}

pub fn open_element<'input>() -> impl Parser<'input, Element> {
    element(">")
}

pub fn single_or_open_element<'input>() -> impl Parser<'input, Element> {
    whitespace_wrap(either(single_element(), open_element()))
}

pub fn close_element<'input>(expected_name: String) -> impl Parser<'input, String> {
    let start = match_literal("</");
    let end = left(identifier(), match_literal(">"));
    let all_together = right(start, end);

    all_together.pred(move |name| name == &expected_name)
}

pub fn parent_element<'input>() -> impl Parser<'input, Element> {
    open_element().and_then(|el| {
        let children = left(
            one_or_more(single_or_open_element()),
            close_element(el.name.clone()),
        );

        children.map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}


#[cfg(test)]
mod tests {
    use super::{
        identifier,
        quoted_string,
        attributes,
        single_element,
        open_element,
        Element,
        single_or_open_element,
        Parser
    };

    #[test]
    fn identifier_test() {
        let identifier = identifier();
        assert_eq!(
            Ok(("", "a1239ff".to_string())),
            identifier.parse("a1239ff")
        );
        assert_eq!(
            Ok((" some", "xxx-1-qq".to_string())),
            identifier.parse("xxx-1-qq some")
        );
        assert_eq!(
            Err("123-qqq"),
            identifier.parse("123-qqq")
        );
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            quoted_string().parse("\"Hello Joe!\"")
        );
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string()),
                ]
            )),
            attributes().parse(" one=\"1\" two=\"2\"")
        );
    }

    #[test]
    fn single_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
            single_element().parse("<div class=\"float\"/>")
        );
    }

    #[test]
    fn open_element_parser() {
        assert_eq!(
            Ok((
                "",
                Element {
                    name: "div".to_string(),
                    attributes: vec![("class".to_string(), "float".to_string())],
                    children: vec![],
                }
            )),
            open_element().parse("<div class=\"float\">")
        );
    }

    #[test]
    fn xml_parser() {
        let doc = r#"
        <top label="Top">
            <semi-bottom label="Bottom"/>
            <middle>
                <bottom label="Another bottom"/>
            </middle>
        </top>"#;
        let parsed_doc = Element {
            name: "top".to_string(),
            attributes: vec![("label".to_string(), "Top".to_string())],
            children: vec![
                Element {
                    name: "semi-bottom".to_string(),
                    attributes: vec![("label".to_string(), "Bottom".to_string())],
                    children: vec![],
                },
                Element {
                    name: "middle".to_string(),
                    attributes: vec![],
                    children: vec![Element {
                        name: "bottom".to_string(),
                        attributes: vec![("label".to_string(), "Another bottom".to_string())],
                        children: vec![],
                    }],
                },
            ],
        };
        assert_eq!(Ok(("", parsed_doc)), single_or_open_element().parse(doc));
    }

    #[test]
    fn mismatched_closing_tag() {
        let doc = r#"
        <top>
            <bottom/>
        </middle>"#;
        assert_eq!(Err("</middle>"), single_or_open_element().parse(doc));
    }
}

