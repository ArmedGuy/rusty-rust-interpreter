use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser, "/ast/parser.rs");

use parser::*;

pub mod ast;

fn main() {
    println!("minimal");
    println!("{:?}", NumOrIdParser::new().parse("123"));
    println!("{:?}", NumOrIdParser::new().parse("a1_a"));
}

#[test]
fn parse_num_or_id() {
    // println!("{:?}", NumOrIdParser::new().parse("123"));
    // println!("{:?}", NumOrIdParser::new().parse("a1_a"));
}
