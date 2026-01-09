// Property-based tests for validation module using qcheck
// Tests core validation invariants

import gleam/string
import qcheck
import validation

// EMAIL VALIDATION - Property: MUST reject strings without @
pub fn prop_email_requires_at_symbol__test() {
  use code <- qcheck.given(qcheck.bounded_int(32, 126))
  let codepoint = string.utf_codepoint(code)
  let char = case codepoint {
    Ok(cp) -> string.from_utf_codepoints([cp])
    Error(_) -> "a"
  }
  assert case char == "@" {
    True -> True
    False ->
      case validation.validate_email(char) {
        Ok(_) -> False
        Error(_) -> True
      }
  }
}

// EMAIL VALIDATION - Property: strings with @ pass basic email validation
pub fn prop_email_accepts_at_symbol__test() {
  use vals <- qcheck.given(qcheck.tuple2(
    qcheck.bounded_int(1, 20),
    qcheck.bounded_int(1, 20),
  ))
  let #(local_len, domain_len) = vals
  let local = string.repeat("a", local_len)
  let domain = string.repeat("b", domain_len)
  let email = local <> "@" <> domain
  assert case validation.validate_email(email) {
    Ok(_) -> True
    Error(_) -> False
  }
}

// EMAIL VALIDATION - Property: format validation requires dot in domain
pub fn prop_email_format_requires_dot_in_domain__test() {
  use vals <- qcheck.given(qcheck.tuple2(
    qcheck.bounded_int(1, 20),
    qcheck.bounded_int(1, 20),
  ))
  let #(local_len, domain_len) = vals
  let local = string.repeat("a", local_len)
  let domain_no_dot = string.repeat("b", domain_len)
  let email = local <> "@" <> domain_no_dot
  assert case validation.validate_email_format(email) {
    Ok(_) -> False
    Error(_) -> True
  }
}

// EMAIL VALIDATION - Property: format validation accepts valid format
pub fn prop_email_format_accepts_valid__test() {
  use vals <- qcheck.given(qcheck.tuple2(
    qcheck.bounded_int(1, 20),
    qcheck.bounded_int(1, 20),
  ))
  let #(local_len, domain_len) = vals
  let local = string.repeat("a", local_len)
  let domain = string.repeat("b", domain_len) <> ".com"
  let email = local <> "@" <> domain
  assert case validation.validate_email_format(email) {
    Ok(result) -> result == email
    Error(_) -> False
  }
}

// NON-EMPTY VALIDATION - Property: empty string always fails
pub fn prop_non_empty_rejects_empty__test() {
  assert case validation.validate_non_empty("", "test_field") {
    Ok(_) -> False
    Error(_) -> True
  }
}

// NON-EMPTY VALIDATION - Property: non-empty strings pass
pub fn prop_non_empty_accepts_non_empty__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let value = string.repeat("a", len)
  assert case validation.validate_non_empty(value, "test_field") {
    Ok(result) -> result == value
    Error(_) -> False
  }
}

// NON-EMPTY VALIDATION - Property: error message includes field name
pub fn prop_non_empty_error_includes_field__test() {
  let field_name = "my_field"
  assert case validation.validate_non_empty("", field_name) {
    Ok(_) -> False
    Error(msg) -> string.contains(msg, field_name)
  }
}

// LENGTH VALIDATION - Property: strings within bounds pass
pub fn prop_length_within_bounds_passes__test() {
  use len <- qcheck.given(qcheck.bounded_int(3, 7))
  let value = string.repeat("a", len)
  assert case validation.validate_length(value, 3, 7, "test_field") {
    Ok(result) -> result == value
    Error(_) -> False
  }
}

// LENGTH VALIDATION - Property: strings below min fail
pub fn prop_length_below_min_fails__test() {
  use len <- qcheck.given(qcheck.bounded_int(0, 2))
  let value = string.repeat("a", len)
  assert case validation.validate_length(value, 3, 10, "test_field") {
    Ok(_) -> False
    Error(_) -> True
  }
}

// LENGTH VALIDATION - Property: strings above max fail
pub fn prop_length_above_max_fails__test() {
  use len <- qcheck.given(qcheck.bounded_int(11, 50))
  let value = string.repeat("a", len)
  assert case validation.validate_length(value, 3, 10, "test_field") {
    Ok(_) -> False
    Error(_) -> True
  }
}

// LENGTH VALIDATION - Property: boundary at min works
pub fn prop_length_boundary_at_min__test() {
  let value = string.repeat("a", 5)
  assert case validation.validate_length(value, 5, 10, "test_field") {
    Ok(result) -> result == value
    Error(_) -> False
  }
}

// LENGTH VALIDATION - Property: boundary at max works
pub fn prop_length_boundary_at_max__test() {
  let value = string.repeat("a", 10)
  assert case validation.validate_length(value, 5, 10, "test_field") {
    Ok(result) -> result == value
    Error(_) -> False
  }
}

// LENGTH VALIDATION - Property: error message includes bounds
pub fn prop_length_error_includes_bounds__test() {
  let min = 5
  let max = 10
  let value = string.repeat("a", 15)
  assert case validation.validate_length(value, min, max, "test_field") {
    Ok(_) -> False
    Error(msg) -> string.contains(msg, "5") && string.contains(msg, "10")
  }
}
