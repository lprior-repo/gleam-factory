import arith
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn add_two_numbers_test() {
  let result = arith.add(2, 3)
  result |> should.equal(5)
}

pub fn add_zero_returns_same_number_test() {
  let result = arith.add(10, 0)
  result |> should.equal(10)
}

pub fn add_negative_numbers_test() {
  let result = arith.add(-5, -3)
  result |> should.equal(-8)
}
