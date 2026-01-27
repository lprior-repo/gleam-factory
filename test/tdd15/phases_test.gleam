import gleam/list
import gleeunit/should
import tdd15/phases

pub fn phase_meta_test() {
  let meta = phases.phase_meta(phases.Phase0Triage)
  meta.number
  |> should.equal(0)
  meta.name
  |> should.equal("TRIAGE")
  meta.gate
  |> should.equal("complexity_assessed")
}

pub fn phase_by_number_test() {
  phases.phase_by_number(0)
  |> should.equal(Ok(phases.Phase0Triage))

  phases.phase_by_number(15)
  |> should.equal(Ok(phases.Phase15Landing))

  phases.phase_by_number(99)
  |> should.be_error
}

pub fn route_for_complexity_test() {
  let phases.Route(simple) = phases.route_for_complexity(phases.Simple)
  simple
  |> should.equal([0, 4, 5, 6, 14, 15])

  let phases.Route(medium) = phases.route_for_complexity(phases.Medium)
  medium
  |> should.equal([0, 1, 2, 4, 5, 6, 7, 9, 11, 15])

  let phases.Route(complex) = phases.route_for_complexity(phases.Complex)
  complex
  |> list.length
  |> should.equal(16)
}

pub fn next_phase_test() {
  let route = phases.route_for_complexity(phases.Simple)

  phases.next_phase(phases.Phase0Triage, route)
  |> should.equal(Ok(phases.Phase4Red))

  phases.next_phase(phases.Phase15Landing, route)
  |> should.be_error
}

pub fn route_phases_test() {
  let route = phases.route_for_complexity(phases.Simple)
  let phase_list = phases.route_phases(route)

  phase_list
  |> list.length
  |> should.equal(6)

  phase_list
  |> list.first
  |> should.equal(Ok(phases.Phase0Triage))
}

pub fn parse_complexity_test() {
  phases.parse_complexity("SIMPLE")
  |> should.equal(Ok(phases.Simple))

  phases.parse_complexity("INVALID")
  |> should.be_error
}

pub fn phase_in_route_test() {
  let route = phases.route_for_complexity(phases.Simple)

  phases.phase_in_route(phases.Phase0Triage, route)
  |> should.be_true

  phases.phase_in_route(phases.Phase1Research, route)
  |> should.be_false
}

pub fn route_length_test() {
  let route = phases.route_for_complexity(phases.Simple)
  phases.route_length(route)
  |> should.equal(6)
}

pub fn phase_position_test() {
  let route = phases.route_for_complexity(phases.Simple)

  phases.phase_position(phases.Phase0Triage, route)
  |> should.equal(Ok(0))

  phases.phase_position(phases.Phase4Red, route)
  |> should.equal(Ok(1))
}

pub fn route_start_end_test() {
  let route = phases.route_for_complexity(phases.Simple)

  phases.route_start(route)
  |> should.equal(Ok(phases.Phase0Triage))

  phases.route_end(route)
  |> should.equal(Ok(phases.Phase15Landing))
}
