// Property-based tests for validation module
// Tests validation invariants across generated input values

import gleeunit/should
import gleam/string
import gleam/list
import qcheck
import qcheck/generators.{string as gen_string}
import validation

// Email validation properties
pub fn prop_validate_email_simple_requires_at() {
  let gen_without_at = gen_string(1, 50)

  qcheck.property(gen_without_at, fn(s) {
    // Only test strings without @ symbol
    case string.contains(s, "@") {
      True -> True // Skip this case
      False ->
        case validation.validate_email(s) {
          Ok(_) -> False
          Error(_) -> True
        }
    }
  })
  |> qcheck.run()
  |> should.be_ok()
}

pub fn prop_validate_email_simple_accepts_at() {
  let valid_emails = [
    "user@example.com",
    "test@test",
    "a@b",
    "@",
  ]

  list.all(valid_emails, fn(email) {
    case validation.validate_email(email) {
      Ok(_) -> True
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

// Strict email validation properties
pub fn prop_validate_email_format_requires_single_at() {
  let test_cases = [
    #("user@example.com", True),
    #("test@test.co", True),
    #("a@b.c", True),
    #("noatsign", False),
    #("double@@at.com", False),
    #("@nodomain", False),
    #("nolocal@", False),
    #("no@domain", False), // No dot in domain
  ]

  list.all(test_cases, fn(tc) {
    let #(email, should_be_ok) = tc
    case validation.validate_email_format(email) {
      Ok(_) -> should_be_ok
      Error(_) -> !should_be_ok
    }
  })
  |> should.equal(True)
}

pub fn prop_validate_email_format_needs_dot_in_domain() {
  case validation.validate_email_format("user@example") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_validate_email_format_needs_local_part() {
  case validation.validate_email_format("@example.com") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_validate_email_format_needs_domain_part() {
  case validation.validate_email_format("user@") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

// Non-empty validation properties
pub fn prop_validate_non_empty_rejects_empty_string() {
  case validation.validate_non_empty("", "field") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_validate_non_empty_accepts_any_non_empty() {
  let gen_non_empty = gen_string(1, 100)

  qcheck.property(gen_non_empty, fn(s) {
    case validation.validate_non_empty(s, "test_field") {
      Ok(result) -> result == s
      Error(_) -> False
    }
  })
  |> qcheck.run()
  |> should.be_ok()
}

pub fn prop_validate_non_empty_returns_input_unchanged() {
  let test_values = ["a", "test", "123", "  ", "\t"]

  list.all(test_values, fn(val) {
    case validation.validate_non_empty(val, "field") {
      Ok(result) -> result == val
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

pub fn prop_validate_non_empty_error_includes_field_name() {
  let field_name = "username"
  case validation.validate_non_empty("", field_name) {
    Ok(_) -> False
    Error(msg) -> string.contains(msg, field_name)
  }
  |> should.equal(True)
}

// Length validation properties
pub fn prop_validate_length_exact_min_boundary() {
  case validation.validate_length("abc", 3, 5, "field") {
    Ok(s) -> s == "abc"
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_validate_length_exact_max_boundary() {
  case validation.validate_length("abcde", 3, 5, "field") {
    Ok(s) -> s == "abcde"
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_validate_length_below_min_fails() {
  case validation.validate_length("ab", 3, 5, "field") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_validate_length_above_max_fails() {
  case validation.validate_length("abcdef", 3, 5, "field") {
    Ok(_) -> False
    Error(_) -> True
  }
  |> should.equal(True)
}

pub fn prop_validate_length_middle_range_passes() {
  case validation.validate_length("abcd", 3, 5, "field") {
    Ok(s) -> s == "abcd"
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_validate_length_returns_original() {
  let test_cases = [
    #("a", 0, 10),
    #("hello", 1, 10),
    #("x", 1, 1),
  ]

  list.all(test_cases, fn(tc) {
    let #(str, min, max) = tc
    case validation.validate_length(str, min, max, "test") {
      Ok(result) -> result == str
      Error(_) -> False
    }
  })
  |> should.equal(True)
}

pub fn prop_validate_length_error_includes_field_name() {
  let field_name = "password"
  case validation.validate_length("a", 5, 10, field_name) {
    Ok(_) -> False
    Error(msg) -> string.contains(msg, field_name)
  }
  |> should.equal(True)
}

pub fn prop_validate_length_error_includes_bounds() {
  case validation.validate_length("a", 5, 10, "password") {
    Ok(_) -> False
    Error(msg) ->
      string.contains(msg, "5")
      && string.contains(msg, "10")
  }
  |> should.equal(True)
}

pub fn prop_validate_length_single_char() {
  case validation.validate_length("x", 1, 1, "field") {
    Ok(s) -> s == "x"
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_validate_length_zero_length_string() {
  case validation.validate_length("", 0, 5, "field") {
    Ok(s) -> s == ""
    Error(_) -> False
  }
  |> should.equal(True)
}

pub fn prop_validate_length_zero_length_max() {
  case validation.validate_length("", 0, 0, "field") {
    Ok(s) -> s == ""
    Error(_) -> False
  }
  |> should.equal(True)
}

// Integration property: combining validations
pub fn prop_validate_email_then_length() {
  let valid_emails = [
    "a@b.c",
    "test@example.com",
  ]

  list.all(valid_emails, fn(email) {
    case
      validation.validate_email_format(email)
      |> result.try(fn(e) {
        validation.validate_length(e, 5, 50, "email")
      })
    {
      Ok(e) -> e == email
      Error(_) -> False
    }
  })
  |> should.equal(True)
}
