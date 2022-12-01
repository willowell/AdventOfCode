use core::fmt;
use nom::{Parser, IResult};

pub mod solutions;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Part {
    PartA,
    PartB
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Part::PartA => write!(f, "a"),
            Part::PartB => write!(f, "b"),
        }
    }
}

fn get_all_parts() -> [Part; 2] {
    [Part::PartA, Part::PartB]
}

pub struct Test {
    pub input: &'static str,
    pub outputs: Vec<(Part, &'static str)>,
}

pub trait Solution<R, I, A, B> {
    fn decode_input(&self, input: I) -> IResult<R, I>;

    fn part_a(&self, input: I) -> Option<A>;

    fn part_b(&self, input: I) -> Option<B>;

    fn tests(&self) -> Vec<Test>;
}

pub fn run_solver<R, I, A, B, S>(solution: S, part: Part, input: I) -> Option<String>
where
    S: Solution<R, I, A, B>,
    String: From<A> + From<B>,
{
    match part {
        Part::PartA => {
            if let Some(out) = solution.part_a(input) {
                return Some(String::from(out))
            }

            None
        },
        Part::PartB => {
            if let Some(out) = solution.part_b(input) {
                return Some(String::from(out))
            }

            None
        },
    }
}
