use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser, "/ast/parser.rs");

use parser::*;

pub mod ast;
pub mod type_checking;

fn main() {
    let prg = "fn _if_then_else_and_while() {
        // a function taking two bool arguments returning the bool type
        // with some let statements and function calls
        fn a(x: bool, y: bool) -> bool {
            if x && y {
                let a: bool = true;
                y || a
            } else {
                x && false
            }
        }
    
        // a function taking two bool arguments returning the i32 type
        // with some let statements and function calls
        fn b(x: bool, y: bool) -> i32 {
            let a: bool = a(x, y || false);
            let mut b: i32 = 0;
            if a && y {
                let a: bool = true; // shadowing
                let mut b: bool = false;
                if y || a {
                    b = b + 1;
                };
            } else {
                if !(x && false) {
                    b = b - 1;
                }
            };
            b + 3
        }
    
        // a function taking two bool arguments returning the i32 type
        // while
        fn c(x: bool, y: bool) -> i32 {
            let mut b: i32 = 0;
            let mut c: i32 = 1;
            while (b < 10) {
                c = c * 2;
            }
            c
        }
    }";
    println!("{:?}", ProgramParser::new().parse(prg));

    println!("{:?}", FuncStatementParser::new().parse("fn a(b: i32) { return false }"));
    let test = ProgramParser::new().parse("fn a(b: i32) { return false }").unwrap();
    println!("statement_type_check {:?}", type_checking::statement_type_check(vec![test]));
}


