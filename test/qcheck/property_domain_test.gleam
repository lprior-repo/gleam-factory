// Property-based tests for domain module using qcheck
// Tests that expose bugs and force code quality

import domain
import gleam/list
import gleam/string
import qcheck

fn codepoint_to_string(code: Int) -> String {
  case string.utf_codepoint(code) {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(_) -> "a"
  }
}

// SLUG VALIDATION - Property: MUST reject uppercase letters
// The spec says "only [a-z0-9_-]" - uppercase A-Z must fail
pub fn prop_slug_rejects_uppercase__test() {
  use code <- qcheck.given(qcheck.bounded_int(65, 90))
  let upper_char = codepoint_to_string(code)
  assert case domain.validate_slug(upper_char) {
    Ok(_) -> False
    Error(_) -> True
  }
}

// SLUG VALIDATION - Property: MUST reject accented/unicode chars
// Non-ASCII like é, ñ, 中 must all fail
pub fn prop_slug_rejects_unicode__test() {
  use code <- qcheck.given(qcheck.bounded_int(256, 500))
  let unicode_char = codepoint_to_string(code)
  assert case domain.validate_slug(unicode_char) {
    Ok(_) -> False
    Error(_) -> True
  }
}

// SLUG VALIDATION - Property: accept lowercase letters
pub fn prop_slug_accepts_lowercase__test() {
  use code <- qcheck.given(qcheck.bounded_int(97, 122))
  let lower_char = codepoint_to_string(code)
  assert case domain.validate_slug(lower_char) {
    Ok(_) -> True
    Error(_) -> False
  }
}

// SLUG VALIDATION - Property: accept digits
pub fn prop_slug_accepts_digits__test() {
  use code <- qcheck.given(qcheck.bounded_int(48, 57))
  let digit_char = codepoint_to_string(code)
  assert case domain.validate_slug(digit_char) {
    Ok(_) -> True
    Error(_) -> False
  }
}

// SLUG VALIDATION - Property: valid multi-char slugs always pass
pub fn prop_slug_valid_composition__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 50))
  let slug = string.repeat("a", len)
  assert case domain.validate_slug(slug) {
    Ok(v) -> domain.slug_to_string(v) == slug
    Error(_) -> False
  }
}

// SLUG VALIDATION - Property: reject empty and over-length
pub fn prop_slug_length_boundaries__test() {
  use len <- qcheck.given(qcheck.bounded_int(51, 100))
  let long_slug = string.repeat("a", len)
  assert case domain.validate_slug(long_slug) {
    Ok(_) -> False
    Error(_) -> True
  }
  assert case domain.validate_slug("") {
    Ok(_) -> False
    Error(_) -> True
  }
}

// LANGUAGE DETECTION - Property: Gleam manifest always wins
pub fn prop_language_gleam_priority__test() {
  use vals <- qcheck.given(qcheck.tuple3(
    qcheck.bool(),
    qcheck.bool(),
    qcheck.bool(),
  ))
  let #(has_go, has_rust, has_python) = vals
  assert case
    domain.detect_language_from_files(True, has_go, has_rust, has_python)
  {
    Ok(domain.Gleam) -> True
    _ -> False
  }
}

// LANGUAGE DETECTION - Property: needs at least one manifest
pub fn prop_language_requires_manifest__test() {
  assert case domain.detect_language_from_files(False, False, False, False) {
    Ok(_) -> False
    Error(_) -> True
  }
}

// PIPELINE - Property: always 9 stages (pure TCR pipeline)
pub fn prop_pipeline_size__test() {
  use _n <- qcheck.given(qcheck.small_non_negative_int())
  let p = domain.standard_pipeline()
  assert list.length(p) == 9
}

// PIPELINE - Property: all stage names unique
pub fn prop_pipeline_unique_names__test() {
  let p = domain.standard_pipeline()
  let names = list.map(p, fn(s) { s.name })
  let unique = list.unique(names)
  assert list.length(names) == list.length(unique)
}

// PIPELINE - Property: find_stage works for all valid stages
pub fn prop_find_stage_works__test() {
  use idx <- qcheck.given(qcheck.bounded_int(0, 8))
  let p = domain.standard_pipeline()
  assert case list.drop(p, idx) |> list.first {
    Ok(stage) ->
      case domain.find_stage_index(p, stage.name) {
        Ok(found) -> found == idx
        Error(_) -> False
      }
    Error(_) -> False
  }
}

// PIPELINE - Property: filter always returns contiguous slice
pub fn prop_filter_stages_contiguous__test() {
  use start <- qcheck.given(qcheck.bounded_int(0, 7))
  use end <- qcheck.given(qcheck.bounded_int(start, 8))
  let p = domain.standard_pipeline()
  let expected_len = end - start + 1
  assert case list.drop(p, start) |> list.first {
    Ok(start_stage) ->
      case list.drop(p, end) |> list.first {
        Ok(end_stage) ->
          case domain.filter_stages(start_stage.name, end_stage.name) {
            Ok(filtered) ->
              list.length(filtered) == expected_len
              && case list.first(filtered) {
                Ok(s) -> s.name == start_stage.name
                Error(_) -> False
              }
              && case list.last(filtered) {
                Ok(s) -> s.name == end_stage.name
                Error(_) -> False
              }
            Error(_) -> False
          }
        Error(_) -> False
      }
    Error(_) -> False
  }
}

// PIPELINE - Property: reversed ranges always fail
pub fn prop_filter_enforces_order__test() {
  use start <- qcheck.given(qcheck.bounded_int(1, 8))
  use end <- qcheck.given(qcheck.bounded_int(0, start - 1))
  let p = domain.standard_pipeline()
  assert case list.drop(p, start) |> list.first {
    Ok(start_stage) ->
      case list.drop(p, end) |> list.first {
        Ok(end_stage) ->
          case domain.filter_stages(start_stage.name, end_stage.name) {
            Ok(_) -> False
            Error(_) -> True
          }
        Error(_) -> False
      }
    Error(_) -> False
  }
}
