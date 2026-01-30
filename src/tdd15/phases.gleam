import gleam/int

pub type Complexty {
  Simple
  Medium
  Complex
}

pub type Route {
  Route(numbers: List(Int))
}

pub type Phase {
  Phase(number: Int, name: String)
}

pub type PhaseMeta {
  PhaseMeta(number: Int, name: String)
}

pub type PhaseError {
  InvalidPhaseNumber(Int)
}

pub fn route_for_complexty(complexty: Complexty) -> Route {
  case complexty {
    Simple -> Route([0, 4, 5, 6, 7, 10, 12, 15])
    Medium -> Route([0, 4, 5, 6, 7, 10, 12, 15])
    Complex -> Route([0, 4, 5, 6, 7, 10, 12, 15])
  }
}

pub fn route_start(route: Route) -> Result(Int, Nil) {
  case route {
    Route(numbers) -> {
      case numbers {
        [n, ..] -> Ok(n)
        [] -> Error(Nil)
      }
    }
  }
}

pub fn phase_meta(phase: Phase) -> Result(PhaseMeta, PhaseError) {
  case phase {
    Phase(number: 0, name: _) -> Ok(PhaseMeta(number: 0, name: "Triage"))
    Phase(number: 4, name: _) -> Ok(PhaseMeta(number: 4, name: "Red"))
    Phase(number: 5, name: _) -> Ok(PhaseMeta(number: 5, name: "Green"))
    Phase(number: 6, name: _) -> Ok(PhaseMeta(number: 6, name: "Refactor"))
    Phase(number: 7, name: _) -> Ok(PhaseMeta(number: 7, name: "Martin Fowler"))
    Phase(number: 10, name: _) -> Ok(PhaseMeta(number: 10, name: "FP Gates"))
    Phase(number: 12, name: _) ->
      Ok(PhaseMeta(number: 12, name: "Martin Fowler (Phase 12)"))
    Phase(number: 15, name: _) -> Ok(PhaseMeta(number: 15, name: "Landing"))
    Phase(number: n, name: _) -> Error(InvalidPhaseNumber(n))
  }
}

pub fn phase_by_number(number: Int) -> Result(Phase, Nil) {
  Ok(Phase(number: number, name: int.to_string(number)))
}

pub fn next_phase(_current: Phase, route: Route) -> Result(Phase, Nil) {
  case route {
    Route(numbers) -> {
      case numbers {
        [] -> Error(Nil)
        [_n] -> Error(Nil)
        [_, next_n, ..] ->
          Ok(Phase(number: next_n, name: int.to_string(next_n)))
      }
    }
  }
}
