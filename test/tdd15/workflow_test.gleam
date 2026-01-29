import gleeunit
import gleeunit/should
import tdd15/phases
import tdd15/types

pub fn main() {
  gleeunit.main()
}

pub fn route_for_complexty_test() {
  let simple_route = phases.route_for_complexty(phases.Simple)
  let phases.Route(numbers) = simple_route
  numbers
  |> should.equal([0, 4, 5, 6, 7, 10, 12, 15])

  let medium_route = phases.route_for_complexty(phases.Medium)
  let phases.Route(medium_numbers) = medium_route
  medium_numbers
  |> should.equal([0, 4, 5, 6, 7, 10, 12, 15])

  let complex_route = phases.route_for_complexty(phases.Complex)
  let phases.Route(complex_numbers) = complex_route
  complex_numbers
  |> should.equal([0, 4, 5, 6, 7, 10, 12, 15])
}

pub fn route_start_test() {
  let route = phases.Route([0, 4, 5, 6, 7, 10, 12, 15])
  let assert Ok(start_num) = phases.route_start(route)
  start_num
  |> should.equal(0)

  let empty_route = phases.Route([])
  let result = phases.route_start(empty_route)
  result
  |> should.be_error()
}

pub fn phase_meta_test() {
  let phase0 = phases.Phase(number: 0, name: "Triage")
  let phases.PhaseMeta(number: num, name: name) = phases.phase_meta(phase0)
  num
  |> should.equal(0)
  name
  |> should.equal("Triage")

  let phase15 = phases.Phase(number: 15, name: "Landing")
  let phases.PhaseMeta(number: num15, name: name15) = phases.phase_meta(phase15)
  num15
  |> should.equal(15)
  name15
  |> should.equal("Landing")
}

pub fn phase_by_number_test() {
  let assert Ok(phase0) = phases.phase_by_number(0)
  let phases.Phase(number: num0, name: name0) = phase0
  num0
  |> should.equal(0)
  name0
  |> should.equal("0")

  let assert Ok(phase10) = phases.phase_by_number(10)
  let phases.Phase(number: num10, name: name10) = phase10
  num10
  |> should.equal(10)
  name10
  |> should.equal("10")
}

pub fn next_phase_test() {
  let route = phases.Route([0, 4, 5, 6, 7, 10, 12, 15])
  let phase0 = phases.Phase(number: 0, name: "Triage")
  let assert Ok(next) = phases.next_phase(phase0, route)
  let phases.Phase(number: num, ..) = next
  num
  |> should.equal(4)

  let phase15 = phases.Phase(number: 15, name: "Landing")
  let result = phases.next_phase(phase15, route)
  result
  |> should.be_error()
}
