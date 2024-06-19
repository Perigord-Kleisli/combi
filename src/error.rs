//
// impl Reason {
//     pub fn expected(s: &str) -> Reason {
//         Self::Expected(s.to_string())
//     }
//     pub fn expecteds(s: &str) -> Vec<Reason> {
//         vec![Self::Expected(s.to_string())]
//     }
//     pub fn message(s: &str) -> Reason {
//         Self::Message(s.to_string())
//     }
// }


// pub fn pretty_print_error(error: &PFailure<'_>) -> String {
//     let mut top_line = format!(
//         "{:?} (line {}, column {}):",
//         error.0.file, error.0.line, error.0.col
//     );
//
//     let unexpected = error
//         .1
//         .iter()
//         .filter_map(|x| match x {
//             Reason::Unexpected(e) if e.is_empty() => Some("EOF"),
//             Reason::Unexpected(e) => Some(e),
//             _ => None,
//         })
//         .dedup()
//         .fold("".to_owned(), |acc, x| {
//             if acc.is_empty() {
//                 format!("unexpected {x:?}")
//             } else {
//                 format!("{acc}\nunexpected {x:?}")
//             }
//         });
//
//     if !unexpected.is_empty() {
//         top_line += "\n";
//         top_line += &unexpected;
//     }
//
//     let expected = error
//         .1
//         .iter()
//         .filter_map(|x| match x {
//             Reason::Expected(e) => Some(format!("{e:?}")),
//             _ => None,
//         })
//         .dedup()
//         .collect::<Vec<String>>()
//         .join(" or ");
//
//     if !expected.is_empty() {
//         top_line += "\nexpected ";
//         top_line += &expected;
//     }
//
//     let messages = error
//         .1
//         .iter()
//         .filter_map(|x| match x {
//             Reason::Message(e) => Some(e),
//             _ => None,
//         })
//         .dedup()
//         .fold("".to_owned(), |acc, x| {
//             if acc.is_empty() {
//                 x.to_string()
//             } else {
//                 acc + "\n" + x
//             }
//         });
//
//     if !messages.is_empty() {
//         top_line += "\n";
//         top_line += &messages;
//     }
//
//     top_line
// }
