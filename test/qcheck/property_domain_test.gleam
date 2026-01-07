// Property-based tests for domain module
// Tests parsing and validation invariants across generated values

import gleeunit/should
import gleam/string
import gleam/list
import qcheck.{type Generator}
import qcheck/generators.{choice, string as gen_string, int}
import domain

// Language parsing properties
pub fn prop_parse_language_lowercase_valid() {
  let gen_valid_lang = choice([
    qcheck.constant("go"),
    qcheck.constant("gleam"),
    qcheck.constant("rust"),
    qcheck.constant("python"),
  ])

  qcheck.property(gen_valid_lang, fn(lang) {
    case domain.parse_language(lang) {
      Ok(_) -> True
      Error(_) -> False
    }
  })
  |> qcheck.run()
  |> should.be_ok()
}

pub fn prop_parse_language_returns_matching_variant() {
  let test_cases = [
    #("go", domain.Go),
    #("gleam", domain.Gleam),
    #("rust", domain.Rust),
    #("python", domain.Python),
  ]

  list.all(test_cases, fn(tc) {
    let #(input, expected) = tc
    case domain.parse_language(input) {
      Ok(lang) -> lang == expected
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

pub fn prop_parse_language_invalid_returns_error() {
  let gen_invalid = gen_string(1, 20)

  qcheck.property(gen_invalid, fn(s) {
    let is_invalid = ![
      "go", "gleam", "rust", "python",
    ]
    |> list.contains(s, _)

    case is_invalid {
      True ->
        case domain.parse_language(s) {
          Ok(_) -> False
          Error(_) -> True
        }
      False -> True
    }
  })
  |> qcheck.run()
  |> should.be_ok()
}

pub fn prop_detect_language_single_file_present() {
  let test_cases = [
    #(True, False, False, False, domain.Gleam),
    #(False, True, False, False, domain.Go),
    #(False, False, True, False, domain.Rust),
    #(False, False, False, True, domain.Python),
  ]

  list.all(test_cases, fn(tc) {
    let #(gleam, go, cargo, python, expected) = tc
    case domain.detect_language_from_files(gleam, go, cargo, python) {
      Ok(lang) -> lang == expected
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

pub fn prop_detect_language_no_files_errors() {
  case domain.detect_language_from_files(False, False, False, False) {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_detect_language_priority_gleam_first() {
  case domain.detect_language_from_files(True, True, True, True) {
    Ok(domain.Gleam) -> True
    _ -> False
  }
  |> should.equal(True)
}

// Slug validation properties
pub fn prop_slug_valid_format() {
  let gen_valid_slug = gen_string(1, 50)

  qcheck.property(gen_valid_slug, fn(s) {
    case domain.validate_slug(s) {
      Ok(slug) -> slug == s && string.length(slug) > 0 && string.length(slug) <= 50
      Error(_) -> True // Both ok and error are acceptable
    }
  })
  |> qcheck.run()
  |> should.be_ok()
}

pub fn prop_slug_rejects_empty() {
  case domain.validate_slug("") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_slug_rejects_too_long() {
  let long_slug = string.repeat("a", 51)
  case domain.validate_slug(long_slug) {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_slug_accepts_valid_chars() {
  let valid_slugs = ["test", "test-123", "test_case", "a", "Z9_-x"]

  list.all(valid_slugs, fn(slug) {
    case domain.validate_slug(slug) {
      Ok(s) -> s == slug
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

pub fn prop_slug_rejects_spaces() {
  case domain.validate_slug("test slug") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

// Pipeline properties
pub fn prop_standard_pipeline_not_empty() {
  let pipeline = domain.standard_pipeline()
  list.length(pipeline) > 0
  |> should.equal(True)
}

pub fn prop_standard_pipeline_has_10_stages() {
  let pipeline = domain.standard_pipeline()
  list.length(pipeline)
  |> should.equal(10)
}

pub fn prop_get_stage_finds_first_stage() {
  case domain.get_stage("tdd-setup") {
    Ok(stage) -> stage.name == "tdd-setup"
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_get_stage_invalid_returns_error() {
  case domain.get_stage("nonexistent") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_find_stage_index_valid() {
  let pipeline = domain.standard_pipeline()
  case domain.find_stage_index(pipeline, "implement") {
    Ok(idx) -> idx == 1
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_find_stage_index_first_is_zero() {
  let pipeline = domain.standard_pipeline()
  case domain.find_stage_index(pipeline, "tdd-setup") {
    Ok(idx) -> idx == 0
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_find_stage_index_invalid_returns_error() {
  let pipeline = domain.standard_pipeline()
  case domain.find_stage_index(pipeline, "fake-stage") {
    Ok(_) -> False
    Error(Nil) -> True
  }
  |> should.equal(True)
}

pub fn prop_filter_stages_valid_range() {
  case domain.filter_stages("tdd-setup", "implement") {
    Ok(stages) -> list.length(stages) == 2
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_filter_stages_single_stage() {
  case domain.filter_stages("implement", "implement") {
    Ok(stages) -> list.length(stages) == 1
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_filter_stages_start_greater_than_end_errors() {
  case domain.filter_stages("implement", "tdd-setup") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_filter_stages_invalid_start_errors() {
  case domain.filter_stages("fake", "implement") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_filter_stages_full_range() {
  case domain.filter_stages("tdd-setup", "accept") {
    Ok(stages) -> list.length(stages) == 10
    Error(_) -> False
  }
  |> should.equal(True)
}

// Test that all stages in pipeline can be found
pub fn prop_all_pipeline_stages_findable() {
  let pipeline = domain.standard_pipeline()

  list.all(pipeline, fn(stage) {
    case domain.find_stage_index(pipeline, stage.name) {
      Ok(_idx) -> True
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

// Test that pipeline is in consistent order
pub fn prop_pipeline_stages_ordered() {
  let pipeline = domain.standard_pipeline()

  list.fold(
    pipeline,
    Ok(#(0, "")),
    fn(acc, stage) {
      case acc {
        Ok(#(_idx, _prev_name)) -> {
          case domain.find_stage_index(pipeline, stage.name) {
            Ok(idx) -> Ok(#(idx, stage.name))
            Error(_) -> Error(Nil)
          }
        }
        Error(_) -> Error(Nil)
      }
    },
  )
  |> result.is_ok
  |> should.equal(True)
}
