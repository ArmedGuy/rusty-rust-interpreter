use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

use grammar::*;

fn main() {
    println!("Parse an Id {:?}", IdParser::new().parse("abcd"));
}

#[test]
fn hello() {
    println!("Parse an Id {:?}", IdParser::new().parse("abcd"));
}
