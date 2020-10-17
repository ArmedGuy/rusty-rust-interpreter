use crate::ast;
use std::collections::HashMap;

type Error = String;


struct FuncSignature {
    args: Vec<ast::Typedef>,
    ret: ast::Typedef,
}

#[derive(Clone)]
struct VarFeatures {
    base: ast::Typedef,
    mutable: bool,
    reference: bool,
    borrowed: bool,
    mutborrowed: bool,
    transferred: bool,
}

pub struct Scope {
    functable: HashMap<i32, HashMap<String, FuncSignature>>,
    vartable: HashMap<i32, HashMap<String, VarFeatures>>,
    fnscopes: Vec<i32>,
    scope: i32,
    source: String,
}

impl Scope {
    pub fn new(source: String) -> Scope {
        let mut s = Scope {functable: HashMap::new(), vartable: HashMap::new(), fnscopes: vec![0], scope: 0, source: source };
        s.functable.insert(0, HashMap::new());
        s.vartable.insert(0, HashMap::new());
        s
    }
    fn push(&mut self, fnscope: bool) {
        self.scope += 1;
        if fnscope {
            self.fnscopes.push(self.scope)
        }
        self.functable.insert(self.scope, HashMap::new());
        self.vartable.insert(self.scope, HashMap::new());
    }
    
    fn pop(&mut self) {
        self.functable.remove(&self.scope);
        self.vartable.remove(&self.scope);
        if self.fnscopes.contains(&self.scope) {
            self.fnscopes.pop();
        }
        self.scope -= 1;
    }

    fn register_function(&mut self, id: &String, args: &Vec<ast::Typedef>, ret: ast::Typedef) {
        let scope = self.functable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), FuncSignature{args: args.to_vec(), ret: ret });
    }

    fn register_variable(&mut self, id: &String, ret: ast::Typedef, mutable: bool) {
        let scope = self.vartable.get_mut(&self.scope).unwrap();
        scope.insert(id.to_string(), VarFeatures{
            base: ret,
            borrowed: false,
            mutable: mutable,
            reference: false,
            mutborrowed: false,
            transferred: false,
        });
    }

    fn get_var(&mut self, id: &String) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        let fnscope = *self.fnscopes.last().unwrap();
        while current >= fnscope {
            let scope = self.vartable.get(&current).unwrap();
            if scope.contains_key(id) {
                let var = scope.get(id).unwrap();
                if var.transferred {
                    return Err(format!("The data of {} has been moved, cannot use here", id))
                }
                return Ok(var.base.clone());
            }
            current -= 1;
        }
        Err(format!("variable {} not found in scope", id))
    }

    fn borrow_var(&mut self, id: &String, mutable: bool) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        let fnscope = *self.fnscopes.last().unwrap();
        while current >= fnscope {
            let mut scope = self.vartable.get_mut(&current).unwrap();
            if scope.contains_key(id) {
                let var = scope.get(id).unwrap();
                if mutable && var.mutborrowed {
                    return Err(format!("Variable {} already borrowed as mutable", id))
                }
                if mutable && var.borrowed {
                    return Err(format!("Variable {} already borrowed as immutable", id))
                }
                if mutable && !var.mutable {
                    return Err(format!("cannot borrow {} as mutable, as it is not declared as mutable", id))
                }
                let mut vartype = var.base.clone();
                if let ast::Typedef::Ref(ref_mutable, inner_type) = vartype {
                    if mutable && !ref_mutable {
                        return Err(format!("cannot borrow {}'s data as mutable", id))
                    }
                    vartype = *inner_type;
                }
                let mut newvar = var.clone();
                if mutable {
                    newvar.mutborrowed = true;
                } else {
                    newvar.borrowed = true;
                }
                let t = ast::Typedef::Ref(mutable, Box::new(vartype));
                newvar.base = t.clone();
                self.vartable.get_mut(&self.scope).unwrap().insert(id.to_string(), newvar);
                return Ok(t);
            }
            current -= 1;
        }
        Err(format!("variable {} not found in scope", id))
    }

    fn check_move(&mut self, expr: ast::Expr) -> Result<bool, Error> {
        if let ast::Expr::Identifier(id, span) = expr {
            self.transfer_ownership(&id)
        } else {
            return Ok(false);
        }
    }

    fn transfer_ownership(&mut self, id: &String) -> Result<bool, Error> {
        let mut current = self.scope;
        let fnscope = *self.fnscopes.last().unwrap();
        while current >= fnscope {
            let mut scope = self.vartable.get_mut(&current).unwrap();
            if scope.contains_key(id) {
                let mut var = scope.get_mut(id).unwrap();
                return match var.base {
                    ast::Typedef::Ref(_, _) => {
                        Ok(false)
                    },
                    ast::Typedef::Str => {
                        if var.transferred {
                            Err(format!("{} has already been moved", id))
                        } else {
                            var.transferred = true;
                            Ok(true)
                        }
                    },
                    _ => Ok(false) // All others we just copy
                }
            }
            current -= 1;
        }
        Err(format!("variable {} not found in scope", id))
    }

    fn get_fn(&mut self, id: &String, args: Vec<ast::Typedef>) -> Result<ast::Typedef, Error> {
        let mut current = self.scope;
        while current >= 0 {
            let scope = self.functable.get(&current).unwrap();
            if scope.contains_key(id) {
                let sign = scope.get(id).unwrap();
                let matching = args.iter().zip(sign.args.iter()).filter(|&(a, b)| a == b).count();
                if matching == args.len() && matching == sign.args.len() {
                    return Ok(sign.ret.clone());
                }
            }
            current -= 1;
        }
        Err(format!("function {}({:?}) not found in scope", id, args))
    }

    fn format_error(&self, err: String, span: ast::CodeSpan) -> Result<ast::Typedef, Error> {
        let mut len = 0;
        let starts = self.source.to_string().into_bytes().into_iter()
            .inspect(|_| len+=1)
            .enumerate()
            .filter(|&(_, b)| b == b'\n')
            .map(|(i, _)| i+1);
        
        let mut last = 0;
        for (line, start) in starts.enumerate() {
            // line starts at 0, so using "next" line is perfectly fine
            if span.l < start {
                let column = span.l - last;
                let src = &self.source[last..start-1];
                let bad_part_pointer = format!("{}{}", " ".repeat(column), "^".repeat(span.r - last - column));
                return Err(format!(" - On line {}:{}\n{}\n{} {}\n", line, column + 1, src, bad_part_pointer, err))
            }
            last = start
        }
        Err(format!("Could not locate source position for error"))
    }

    fn format_function_if_err(&self, res: Result<ast::Typedef, Error>, span: ast::CodeSpan) -> Result<ast::Typedef, Error> {
        if res.is_ok() {
            return res
        }
        let mut len = 0;
        let starts = self.source.to_string().into_bytes().into_iter()
            .inspect(|_| len+=1)
            .enumerate()
            .filter(|&(_, b)| b == b'\n')
            .map(|(i, _)| i+1);
        
        let mut last = 0;
        for (line, start) in starts.enumerate() {
            // line starts at 0, so using "next" line is perfectly fine
            if span.l < start {
                let src = &self.source[last..start-1];
                return Err(format!(" | In function on line {}\n | {}\n | \n{}", line, src, res.unwrap_err()))
            }
            last = start
        }
        Err(format!("Could not locate source position for error"))
    }
}


pub fn expr_type_check(scope: &mut Scope, expr: Box<ast::Expr>) -> Result<ast::Typedef, Error> {
    match *expr {
        ast::Expr::Boolean(_) => Ok(ast::Typedef::Bool),
        ast::Expr::Number(_) => Ok(ast::Typedef::I32),
        ast::Expr::Str(s) => Ok(ast::Typedef::Str),
        ast::Expr::Op(e1, op, e2, span) => {
            let r1 = expr_type_check(scope, e1);
            let r2 = expr_type_check(scope, e2);
            if r1.is_err() {
                return r1;
            }
            if r2.is_err() {
                return r2;
            }
            let t1 = r1.unwrap();
            let t2 = r2.unwrap();
            match op {
                ast::Opcode::Add | ast::Opcode::Sub | ast::Opcode::Div | ast::Opcode::Mul => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::I32)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::GreaterThen | ast::Opcode::LessThen => {
                    if t1 == ast::Typedef::I32 && t2 == ast::Typedef::I32 {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::Is => {
                    if t1 == t2 {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                ast::Opcode::And | ast::Opcode::Or => {
                    if t1 == ast::Typedef::Bool && t2 == ast::Typedef::Bool {
                        return Ok(ast::Typedef::Bool)
                    }
                    return scope.format_error(format!("Could not apply {:?} between {:?} and {:?}", op, t1, t2), span)
                },
                _ => Err("swag".to_string())
            }
        },
        ast::Expr::ModOp(ast::Opcode::Neg, e, span) => {
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::Bool {
                return Ok(ast::Typedef::Bool)
            }
            return scope.format_error(format!("{:?} does not support negation", t1), span)
        },
        ast::Expr::ModOp(ast::Opcode::Sub, e, span) => {
            let r1 = expr_type_check(scope, e);
            if r1.is_err() {
                return r1;
            }
            let t1 = r1.unwrap();
            if t1 == ast::Typedef::I32 {
                return Ok(ast::Typedef::I32)
            }
            return scope.format_error(format!("{:?} does not support sign inversion", t1), span)
        },
        ast::Expr::Function(id, exprs, span) => {
            // TODO typecheck expressions
            let mut args = vec![];
            scope.push(false); // this is cheating, but yolo
            for expr in exprs {
                let expr2 = expr.clone();
                let r = expr_type_check(scope, expr);
                if r.is_err() {
                    scope.pop();
                    return r;
                } else {
                    let mv = scope.check_move(*expr2);
                    if mv.is_err() {
                        scope.pop();
                        return Err(format!("{}", mv.unwrap_err()));
                    }
                    args.push(r.unwrap());
                }
            }
            
            let r = scope.get_fn(&id, args);
            scope.pop();
            return if r.is_err() {
                scope.format_error(format!("{}", r.unwrap_err()), span)
            } else {
                Ok(r.unwrap())
            }
        },
        ast::Expr::Identifier(id, span) => {
            let r = scope.get_var(&id);
            if r.is_err() {
                scope.format_error(format!("{}", r.unwrap_err()), span)
            } else {
                Ok(r.unwrap())
            }
        },
        ast::Expr::ConditionalExpr(stmt, _) => {
            statement_type_check(scope, vec![stmt])
        },
        ast::Expr::Borrow(mutable, inner) => {
            if let ast::Expr::Identifier(id, span) = *inner {
                let r = scope.get_var(&id);
                if r.is_err() {
                    scope.format_error(format!("{}", r.unwrap_err()), span)
                } else {
                    let r = scope.borrow_var(&id, mutable);
                    if r.is_err() {
                        scope.format_error(r.unwrap_err(), span)
                    } else {
                        r
                    }
                }
            } else {
                Err(format!("Can only borrow variables"))
            }
        },
        ast::Expr::Dereference(inner) => {
            if let ast::Expr::Identifier(id, span) = *inner {
                let r = scope.get_var(&id);
                if r.is_err() {
                    scope.format_error(format!("{}", r.unwrap_err()), span)
                } else {
                    if let ast::Typedef::Ref(_, inner_type) = r.unwrap() {
                        Ok(*inner_type)
                    } else {
                        scope.format_error(format!("Cannot dereference {}, is not a reference", id), span)
                    }
                }
            } else {
                Err(format!("Can only dereference variables"))
            }
        },
        _ => Err(format!("Unrecognized expression {:?}", *expr)),
    }
}

pub fn block_type_check(scope: &mut Scope, body: Box<ast::Statement>) -> (Result<ast::Typedef, Error>, ast::CodeSpan) {
    scope.push(false);
    let ret = match *body {
        ast::Statement::Block(stmts, None, span) => {
            (statement_type_check(scope, stmts), span)
        },
        ast::Statement::Block(stmts, Some(ret2), span) => {
            let chk = statement_type_check(scope, stmts);
            if chk.is_err() {
                (chk, span)
            } else {
                if let ast::Statement::Return(e, inner_span) = *ret2 {
                    (expr_type_check(scope, e), inner_span)
                } else {
                    (Err(format!("Unrecoverable error")), span)
                }
            }
        },
        _ => (Err(format!("Unrecoverable error")), ast::CodeSpan::new(0, 0)),
    };
    scope.pop();
    ret
}

pub fn func_type_check(scope: &mut Scope, ret: ast::Typedef, body: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    let (bret, span) = block_type_check(scope, body);
    if bret.is_err() {
        return bret
    } else {
        let r = bret.unwrap();
        if r != ret {
            return scope.format_error(format!("Invalid return type {:?}, expected {:?}", r, ret), span)
        }
    }
    return Ok(ast::Typedef::Unit)
}

fn cond_type_check(scope: &mut Scope, next: Box<ast::Statement>) -> Result<ast::Typedef, Error> {
    match *next {
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, None, _) => {
            let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    block_type_check(scope, body).0
                }
        },
        ast::Statement::Conditional(ast::ConditionalType::ElseIf, Some(e), body, Some(next), _) => {
            let r = expr_type_check(scope, e);
            if r.is_err() {
                r
            } else {
                let (r2, inner_span) = block_type_check(scope, body);
                if r2.is_err() {
                    r2
                } else {
                    let r3 = cond_type_check(scope, next);
                    if r3.is_err() {
                        r3
                    } else {
                        let t2 = r2.unwrap();
                        let t3 = r3.unwrap();
                        if t2 != t3 {
                            scope.format_error(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t3, t2), inner_span)
                        } else {
                            Ok(t3)
                        }
                    }
                }
            }
        },
        ast::Statement::Conditional(ast::ConditionalType::Else, None, body, None, _) => {
            block_type_check(scope, body).0
        },
        _ => Err(format!("Unrecoverable error in cond")),
    }
}

pub fn statement_type_check(scope: &mut Scope, stmts: Vec<Box<ast::Statement>>) -> Result<ast::Typedef, Error> {
    let stmt_len = stmts.len();
    let mut i = 1;
    let mut unboxed = vec![];
    for stmt in stmts {
        unboxed.push(*stmt);
    }
    // Breadth first, find all functions and register their signatures
    for stmt in &unboxed {
        if let ast::Statement::Function(id, vars, ret, _, _) = stmt {
            let mut args = vec![];
            for var in vars {
                if let ast::Statement::VarDef(_, t) = &**var {
                    args.push(t.clone());
                } else {
                    
                }
            }
            let ret = if ret.is_some() { ret.as_ref().unwrap().clone() } else { ast::Typedef::Unit };
            scope.register_function(&id, &args, ret);
        } else {

        }
    }
    // Depth first, all functions in current scope registered
    for stmt in unboxed {
        let is_last = i == stmt_len;
        let res: Result<ast::Typedef, Error> = match stmt {
            ast::Statement::Program(statements, _) => {
                statement_type_check(scope, statements)
            },
            ast::Statement::Function(_, vars, Some(ret), body, span) => {
                scope.push(true);
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt, false);
                    } else {

                    }
                }
                let ret = func_type_check(scope, ret, body);
                scope.pop();
                scope.format_function_if_err(ret, span)
            },
            ast::Statement::Function(_, vars, None, body, span) => {
                scope.push(true);
                for var in vars {
                    if let ast::Statement::VarDef(id, rt) = *var {
                        scope.register_variable(&id, rt, false);
                    } else {

                    }
                }
                let ret = func_type_check(scope, ast::Typedef::Unit, body);
                scope.pop();
                scope.format_function_if_err(ret, span)
            },
            ast::Statement::Block(_, _, span) => block_type_check(scope, Box::new(stmt)).0,
            ast::Statement::Expr(e, _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    Ok(ast::Typedef::Unit)
                }
            },
            ast::Statement::Definition(mutable, vardef, e, span) => {
                if let ast::Statement::VarDef(id, def) = *vardef {
                    let expr = *e;
                    let expr2 = expr.clone();
                    let r = expr_type_check(scope, Box::new(expr));
                    if r.is_err() {
                        r
                    } else {
                        let mv = scope.check_move(expr2);
                        if mv.is_err() {
                            return Err(format!("{}", mv.unwrap_err()));
                        }
                        let t = r.unwrap();
                        if def == t || def == ast::Typedef::Implicit {
                            scope.register_variable(&id, t, mutable);
                            Ok(ast::Typedef::Unit)
                        } else {
                            scope.format_error(format!("Expression evaluated to {:?}, expected {:?}", t, def), span)
                        }
                    }
                } else {
                    Err(format!("Unrecoverable error"))
                }
            },
            ast::Statement::Assignment(id, e, span) => {
                let r1 = scope.get_var(&id);
                if r1.is_err() {
                    scope.format_error(format!("{}", r1.unwrap_err()), span)
                } else {
                    let r2 = expr_type_check(scope, e);
                    if r2.is_err() {
                       r2
                    } else {
                        Ok(ast::Typedef::Unit)
                    }
                }
                //Ok("".to_string()) // todo when lookup table
            },
            ast::Statement::WhileLoop(e, body, span) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    let t = r.unwrap();
                    if t != ast::Typedef::Bool {
                        scope.format_error(format!("Expression evaluated to {:?}, expected bool", t), span)
                    } else {
                        let r2 = block_type_check(scope, body).0;
                        if r2.is_err() {
                            r2
                        } else {
                            let t2 = r2.unwrap();
                            if t2 != ast::Typedef::Unit {
                                scope.format_error(format!("Expected {:?} from while, got {:?}", ast::Typedef::Unit, t2), span)
                            } else {
                                Ok(ast::Typedef::Unit)
                            }
                        }
                    }
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, None, _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    block_type_check(scope, body).0
                }
            },
            ast::Statement::Conditional(ast::ConditionalType::If, Some(e), body, Some(next), _) => {
                let r = expr_type_check(scope, e);
                if r.is_err() {
                    r
                } else {
                    let (r2, inner_span) = block_type_check(scope, body);
                    if r2.is_err() {
                        r2
                    } else {
                        let r3 = cond_type_check(scope, next);
                        if r3.is_err() {
                            r3
                        } else {
                            let t2 = r2.unwrap();
                            let t3 = r3.unwrap();
                            if t2 != t3 {
                                scope.format_error(format!("Mismatching return type in conditional, expected {:?}, got {:?}", t3, t2), inner_span)
                            } else {
                                Ok(t3)
                            }
                        }
                    }
                }
            },
            _ => Err(format!("Unrecoverable error in stmt")),
        };
        if res.is_err() {
            return res
        }
        if is_last {
            return res
        } else {
            let r = res.unwrap();
            if r != ast::Typedef::Unit {
                return Err(format!("Unexpected return type {:?}, expected Unit", r))
            }
        }
        i += 1;
    }
    return Ok(ast::Typedef::Unit)
}
