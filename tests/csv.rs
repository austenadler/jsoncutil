use std::fmt::Display;

use jsoncutil::{
    csv_parser,
    parser::{self, Mode},
    CsvArgs,
};
use serde_json::json;
use strum::{EnumIter, IntoEnumIterator};

#[test]
fn test_csv() {
    for newline in &["\n", "\r\n", "\r"] {
        for rec1 in RecordValue::iter() {
            for rec2 in RecordValue::iter() {
                for rec3 in RecordValue::iter() {
                    dbg!((&newline, &rec1, &rec2, &rec3));
                    let header = "Abc,\"DeF\",\"GH\nIJ\"";

                    // No header
                    {
                        eprintln!("No header");
                        let input = format!("{rec1},{rec2},{rec3}");
                        let csv_args = CsvArgs {
                            quote_character: b'"',
                            field_separator: b',',
                            skip_header: false,

                            wrap: true,
                            object_format: false,
                        };
                        let output = test_single_csv(csv_args, input.as_bytes());

                        assert_eq!(
                            output,
                            json!([[
                                rec1.to_json_value(),
                                rec2.to_json_value(),
                                rec3.to_json_value(),
                            ]])
                        );
                    }

                    // No header; multiple rows
                    {
                        eprintln!("No header; multiple rows");
                        let input = format!("{rec1},{rec2},{rec3}{newline}{rec1},{rec2},{rec3}");
                        let csv_args = CsvArgs {
                            quote_character: b'"',
                            field_separator: b',',
                            skip_header: false,

                            wrap: true,
                            object_format: false,
                        };
                        let output = test_single_csv(csv_args, input.as_bytes());

                        assert_eq!(
                            output,
                            json!([
                                [
                                    rec1.to_json_value(),
                                    rec2.to_json_value(),
                                    rec3.to_json_value(),
                                ],
                                [
                                    rec1.to_json_value(),
                                    rec2.to_json_value(),
                                    rec3.to_json_value(),
                                ]
                            ])
                        );
                    }

                    // With skipped header
                    {
                        eprintln!("With skipped header");
                        let input = format!("{header}{newline}{rec1},{rec2},{rec3}");
                        let csv_args = CsvArgs {
                            quote_character: b'"',
                            field_separator: b',',
                            skip_header: true,

                            wrap: true,
                            object_format: false,
                        };
                        let output = test_single_csv(csv_args, input.as_bytes());

                        assert_eq!(
                            output,
                            json!([[
                                rec1.to_json_value(),
                                rec2.to_json_value(),
                                rec3.to_json_value(),
                            ]])
                        );
                    }

                    // With header and object mode
                    {
                        eprintln!("With header and object mode");
                        let input = format!("{header}{newline}{rec1},{rec2},{rec3}");
                        let csv_args = CsvArgs {
                            quote_character: b'"',
                            field_separator: b',',
                            skip_header: false,

                            wrap: true,
                            object_format: true,
                        };
                        let output = test_single_csv(csv_args, input.as_bytes());

                        assert_eq!(
                            output,
                            json!([{
                                "Abc": rec1.to_json_value(),
                                "DeF": rec2.to_json_value(),
                                "GH\nIJ": rec3.to_json_value(),
                            }])
                        );
                    }
                }
            }
        }
    }
}

fn test_single_csv(csv_args: CsvArgs, text: &[u8]) -> serde_json::Value {
    eprintln!(
        "Testing ({csv_args:?}): <<<\n{:?}\n>>>",
        String::from_utf8_lossy(text)
    );

    let mut writer = vec![];

    let parser = csv_parser::Parser::new(csv_args);
    parser
        .parse_buf(text, &mut writer)
        .expect("Parse should not panic");

    eprintln!("Got CSV output: {:?}", String::from_utf8_lossy(&writer));

    dbg!(parse_jsonc_as_json_value(&writer))
}

fn parse_jsonc_as_json_value(value: &[u8]) -> serde_json::Value {
    let mut writer = vec![];

    let parser = parser::Parser::new(Mode::CompactJson);
    parser
        .format_buf(value, &mut writer)
        .expect("JSON format should not panic");

    serde_json::from_slice::<serde_json::Value>(&writer[..])
        .expect("JSON should have been parsable")
}

// #[derive(Debug, EnumIter)]
// enum PossibleEncoding {
//     Ascii,
//     // Utf8,
// }

#[derive(Debug, EnumIter)]
enum RecordValue {
    Empty,
    EmptyQuoted,
    Value,
    ValueQuoted,
    NewlineStart,
    NewlineMiddle,
    NewlineEnd,
    CommaStart,
    CommaMiddle,
    CommaEnd,
    QuoteStart,
    QuoteMiddle,
    QuoteEnd,
}

impl Display for RecordValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_csv_value())
    }
}

impl RecordValue {
    fn to_csv_value(&self) -> &'static str {
        match self {
            RecordValue::Empty => "",
            RecordValue::EmptyQuoted => "\"\"",
            RecordValue::Value => " abc ",
            RecordValue::ValueQuoted => "\" abc \"",
            RecordValue::NewlineStart => "\"\nabc\"",
            RecordValue::NewlineEnd => "\"abc\n\"",
            RecordValue::NewlineMiddle => "\"ab\n\n\nc\"",
            RecordValue::CommaStart => "\",abc\"",
            RecordValue::CommaMiddle => "\"a,bc\"",
            RecordValue::CommaEnd => "\"abc,\"",
            RecordValue::QuoteStart => "\"\"\"abc\"",
            RecordValue::QuoteMiddle => "\"a\"\"bc\"",
            RecordValue::QuoteEnd => "\"ab\nc\"\"\"",
        }
    }
    fn to_json_value(&self) -> &'static str {
        match self {
            RecordValue::Empty => "",
            RecordValue::EmptyQuoted => "",
            RecordValue::Value => " abc ",
            RecordValue::ValueQuoted => " abc ",
            RecordValue::NewlineStart => "\nabc",
            RecordValue::NewlineEnd => "abc\n",
            RecordValue::NewlineMiddle => "ab\n\n\nc",
            RecordValue::CommaStart => ",abc",
            RecordValue::CommaMiddle => "a,bc",
            RecordValue::CommaEnd => "abc,",
            RecordValue::QuoteStart => "\"abc",
            RecordValue::QuoteMiddle => "a\"bc",
            RecordValue::QuoteEnd => "ab\nc\"",
        }
    }
}
