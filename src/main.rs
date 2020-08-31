use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);

use grammar::*;

fn main() {
    println!("hello world");
}

#[test] 
fn hello() {
    println!("Hello in test")
}

