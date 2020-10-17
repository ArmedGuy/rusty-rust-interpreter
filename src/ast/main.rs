use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub parser, "/ast/parser.rs");

use parser::*;

pub mod ast;
pub mod type_checking;
pub mod inderpaderper;

fn main() {
    let prg = "fn _let_and_return() {
        // a function taking no arguments returning the unit type
        fn a() -> () {
            let _a: i32 = 5; // this returns a unit type
        }
    
        // a function taking two i32 arguments returning the i32 type
        fn b(_x: i32, _y: i32) -> i32 {
            3 // this returns 3 (as i32)
        }
    
        // a function taking two i32 arguments returning the i32 type
        // with some let statements
        fn c(x: i32, y: i32) -> i32 {
            let a: i32 = 5;
            let b: i32 = x + y; // this will be an infix operator +
            -a - (-b) * y // here we have prefix operator '-'
        }
    } 
    fn _if_then_else_and_while() {
        // a function taking two bool arguments returning the bool type
        // with some let statements and function calls
        fn a(x: bool, y: bool) -> bool {
            let k = c(x, y);
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
            let k = 'hi hello';
            let m = 'swag';
            if (m == k) {
                
            }
            let mut b: i32 = 0;
            let mut c: i32 = 1;
            while (b < 10) {
                c = c * 2;
            }
            c
        }

    }";

    let test = ProgramParser::new().parse(prg).unwrap();
    //println!("{:?}", test);
    let mut scope = type_checking::Scope::new(prg.to_string());
    let result  = type_checking::statement_type_check(&mut scope, vec![test]);
    if result.is_err() {
        println!("{}", result.unwrap_err());
    } else {
        println!("statement_type_check Ok for first");
    }

    let prg2 = "fn test(a: &mut String) {
        fn swag(k: &String) {

        }
        let mut b = 'hi';
        swag(&b);
        let mut e = &mut b;
    }
    fn test2(b: &i32) -> i32 {
        return *b + 4;
    }
    ";

    let test = ProgramParser::new().parse(prg2).unwrap();
    //println!("{:?}", test);
    let mut scope = type_checking::Scope::new(prg2.to_string());
    let result  = type_checking::statement_type_check(&mut scope, vec![test]);
    if result.is_err() {
        println!("{}", result.unwrap_err());
    } else {
        println!("statement_type_check Ok for second");
    }
    //inderpaderper::prompt();
   
}


