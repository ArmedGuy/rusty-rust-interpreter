// ast

#[derive(Debug)]
pub enum NumOrId {
    Num(usize),
    Id(String),
}