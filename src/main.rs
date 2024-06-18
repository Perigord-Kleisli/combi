use combi::parser::repeating::Repeating;
use combi::parser::sequential::Sequential;
use combi::parser::Parser;
use combi::parsers::char::char;

fn main() {
    // char('(').seq_r(char('b')).many().test_parse("(b(b(b(a");
    // char('b').sep_by(char(',')).test_parse("b,b,b");
    // char('a').replicate(4).test_parse("aaab");
    char('a').seq_r(char('b')).many_till(char(')')).test_parse("aba)");

    // char('a').ignore_left(char('b'))
    // char('a').or(char('b')).test_parse("");
    // char('a').test_parse("b");
    // char('f').or(char('o')).test_parse("foo");
    // println!("-----");
    // char('f').or(char('o')).test_parse("oo");
    // println!("-----");
    // char('f').or(char('o')).test_parse("loo");
    // println!("-----");
    // char('f')
    //     .and_then(char('o'))
    //     .and_then(char('o'))
    //     .test_parse("foo");
    // println!("-----");
    // char('f')
    //     .and_then(char('o'))
    //     .and_then(char('o'))
    //     .test_parse("bar");
    // println!("-----");
    // char('f')
    //     .and_then(char('o'))
    //     .and_then(char('o'))
    //     .test_parse("far");
    // println!("====");
    // let xs: &[u8] = &[23, 34, 54];
    // byte_p(23).or(byte_p(34)).test_parse(xs);
    // println!("-----");
    // let xs: &[u8] = &[34, 23, 54];
    // byte_p(23).or(byte_p(34)).test_parse(xs);
    // println!("-----");
    // let xs: &[u8] = &[22, 23, 54];
    // byte_p(23).or(byte_p(34)).test_parse(xs);
    // println!("-----");
    // int.test_parse("1234");
    // println!("-----");
    // int.test_parse("-1234");
    // println!("-----");
    // int.test_parse("-a34");
    // println!("-----");
    // string("foo").test_parse("foob");
    // println!("-----");
    // string("foo").test_parse("fofb");
    // println!("-----");
    // eof.test_parse("foob");
    // println!("-----");
    // eof.test_parse("");
    // println!("-----");
    // char_literal.test_parse("\\\"");
}
