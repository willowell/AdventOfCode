use crate::*;

use itertools::Itertools;
use nom::bytes::complete::tag;

pub struct Day01;

impl Solution<&'static str, &'static str, i32, i32> for Day01 {
    fn decode_input(&self, input: &'static str) -> nom::IResult<&'static str, &'static str> {
        tag("foo")(input)
    }

    fn part_a(&self, input: &'static str) -> Option<i32> {
        Some(input.chars().count() as i32)
    }

    fn part_b(&self, input: &'static str) -> Option<i32> {
        Some(input.chars().into_iter().unique().count() as i32)
    }

    fn tests(&self) -> Vec<Test> {
        vec![
            Test {
                input: "foo",
                outputs: vec![
                    (Part::PartA, "3"),
                    (Part::PartB, "2")
                ],
            }
        ]
    }
}
