use crate::data::{Primitive, Value};
use crate::prelude::*;
use csv::WriterBuilder;

fn from_value_to_delimited_string(
    tagged_value: &Tagged<Value>,
    separator: char,
) -> Result<String, ShellError> {
    let v = &tagged_value.item;

    match v {
        Value::Row(o) => {
            let mut wtr = WriterBuilder::new()
                .delimiter(separator as u8)
                .from_writer(vec![]);
            let mut fields: VecDeque<String> = VecDeque::new();
            let mut values: VecDeque<String> = VecDeque::new();

            for (k, v) in o.entries.iter() {
                fields.push_back(k.clone());

                values.push_back(to_string_tagged_value(&v)?);
            }

            wtr.write_record(fields).expect("can not write.");
            wtr.write_record(values).expect("can not write.");

            return Ok(String::from_utf8(wtr.into_inner().map_err(|_| {
                ShellError::labeled_error(
                    "Could not convert record",
                    "original value",
                    &tagged_value.tag,
                )
            })?)
            .map_err(|_| {
                ShellError::labeled_error(
                    "Could not convert record",
                    "original value",
                    &tagged_value.tag,
                )
            })?);
        }
        Value::Table(list) => {
            let mut wtr = WriterBuilder::new()
                .delimiter(separator as u8)
                .from_writer(vec![]);

            let merged_descriptors = merge_descriptors(&list);
            wtr.write_record(&merged_descriptors)
                .expect("can not write.");

            for l in list {
                let mut row = vec![];
                for desc in &merged_descriptors {
                    match l.item.get_data_by_key(&desc) {
                        Some(s) => {
                            row.push(to_string_tagged_value(s)?);
                        }
                        None => {
                            row.push(String::new());
                        }
                    }
                }
                wtr.write_record(&row).expect("can not write");
            }

            return Ok(String::from_utf8(wtr.into_inner().map_err(|_| {
                ShellError::labeled_error(
                    "Could not convert record",
                    "original value",
                    &tagged_value.tag,
                )
            })?)
            .map_err(|_| {
                ShellError::labeled_error(
                    "Could not convert record",
                    "original value",
                    &tagged_value.tag,
                )
            })?);
        }
        _ => return to_string_tagged_value(tagged_value),
    }
}

// NOTE: could this be useful more widely and implemented on Tagged<Value> ?
pub fn clone_tagged_value(v: &Tagged<Value>) -> Tagged<Value> {
    match &v.item {
        Value::Primitive(Primitive::String(s)) => Value::Primitive(Primitive::String(s.clone())),
        Value::Primitive(Primitive::Nothing) => Value::Primitive(Primitive::Nothing),
        Value::Primitive(Primitive::Boolean(b)) => Value::Primitive(Primitive::Boolean(b.clone())),
        Value::Primitive(Primitive::Decimal(f)) => Value::Primitive(Primitive::Decimal(f.clone())),
        Value::Primitive(Primitive::Int(i)) => Value::Primitive(Primitive::Int(i.clone())),
        Value::Primitive(Primitive::Path(x)) => Value::Primitive(Primitive::Path(x.clone())),
        Value::Primitive(Primitive::Bytes(b)) => Value::Primitive(Primitive::Bytes(b.clone())),
        Value::Primitive(Primitive::Date(d)) => Value::Primitive(Primitive::Date(d.clone())),
        Value::Row(o) => Value::Row(o.clone()),
        Value::Table(l) => Value::Table(l.clone()),
        Value::Block(_) => Value::Primitive(Primitive::Nothing),
        _ => Value::Primitive(Primitive::Nothing),
    }
    .tagged(v.tag.clone())
}

// NOTE: could this be useful more widely and implemented on Tagged<Value> ?
fn to_string_tagged_value(v: &Tagged<Value>) -> Result<String, ShellError> {
    match &v.item {
        Value::Primitive(Primitive::Date(d)) => Ok(d.to_string()),
        Value::Primitive(Primitive::Bytes(b)) => {
            let tmp = format!("{}", b);
            Ok(tmp)
        }
        Value::Primitive(Primitive::Boolean(_)) => Ok(v.as_string()?),
        Value::Primitive(Primitive::Decimal(_)) => Ok(v.as_string()?),
        Value::Primitive(Primitive::Int(_)) => Ok(v.as_string()?),
        Value::Primitive(Primitive::Path(_)) => Ok(v.as_string()?),
        Value::Table(_) => return Ok(String::from("[Table]")),
        Value::Row(_) => return Ok(String::from("[Row]")),
        Value::Primitive(Primitive::String(s)) => return Ok(s.to_string()),
        _ => {
            return Err(ShellError::labeled_error(
                "Unexpected value",
                "",
                v.tag.clone(),
            ))
        }
    }
}

fn merge_descriptors(values: &[Tagged<Value>]) -> Vec<String> {
    let mut ret = vec![];
    for value in values {
        for desc in value.data_descriptors() {
            if !ret.contains(&desc) {
                ret.push(desc);
            }
        }
    }
    ret
}

pub fn to_delimited_data(
    headerless: bool,
    sep: char,
    format_name: &'static str,
    RunnableContext { input, name, .. }: RunnableContext,
) -> Result<OutputStream, ShellError> {
    let name_tag = name;

    let stream = async_stream! {
         let input: Vec<Tagged<Value>> = input.values.collect().await;

         let to_process_input = if input.len() > 1 {
             let tag = input[0].tag.clone();
             vec![Tagged { item: Value::Table(input), tag } ]
         } else if input.len() == 1 {
             input
         } else {
             vec![]
         };

         for value in to_process_input {
             match from_value_to_delimited_string(&clone_tagged_value(&value), sep) {
                 Ok(x) => {
                     let converted = if headerless {
                         x.lines().skip(1).collect()
                     } else {
                         x
                     };
                     yield ReturnSuccess::value(Value::Primitive(Primitive::String(converted)).tagged(&name_tag))
                 }
                 _ => {
                     let expected = format!("Expected a table with {}-compatible structure.tag() from pipeline", format_name);
                     let requires = format!("requires {}-compatible input", format_name);
                     yield Err(ShellError::labeled_error_with_secondary(
                         expected,
                         requires,
                         &name_tag,
                         "originates from here".to_string(),
                         value.tag(),
                     ))
                 }
             }
         }
    };

    Ok(stream.to_output_stream())
}
