use std::io;
use std::io::*;
use crate::parser::*;
use crate::type_checking;

// shitty interpreter shell
pub fn prompt() {
    let mut input = String::new();
    let mut br = false;
    let mut lines = vec![];
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut depth = 0;
    while (!br) {
        if depth > 0 {
            print!("... ");
        } else {
            print!(">>> ");
        }
        stdout.flush();
        stdin.read_line(&mut input).expect("error: unable to read user input");
        depth += input.matches("{").count() as i32 - input.matches("}").count() as i32;
        if depth < 0 {
            panic!("mismatched parentheses");
        }
        if depth == 0 {
            lines.push(format!("{};", input.clone()));
            let src = format!("fn main() {{ {} }}", lines.join("\n"));
            let test = ProgramParser::new().parse(&src).unwrap();
            let mut scope = type_checking::Scope::new(src.to_string());
            let result  = type_checking::statement_type_check(&mut scope, vec![test]);
            if result.is_err() {
                println!("{}", result.unwrap_err());
                lines.clear();
            } else {
                println!("Ok");
            }
        } else {
            lines.push(input.clone());
        }
        input.clear();
    }
}