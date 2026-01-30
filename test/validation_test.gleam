import gleam/string
import gleeunit/should
import validation

pub fn validate_email_rejects_at_alone_test() {
  validation.validate_email("@")
  |> should.be_error
}

pub fn validate_email_rejects_no_local_part_test() {
  validation.validate_email("@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_no_domain_test() {
  validation.validate_email("user@")
  |> should.be_error
}

pub fn validate_email_rejects_double_at_test() {
  validation.validate_email("user@@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_spaces_test() {
  validation.validate_email("user name@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_leading_dot_test() {
  validation.validate_email(".user@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_trailing_dot_test() {
  validation.validate_email("user.@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_consecutive_dots_test() {
  validation.validate_email("user..name@domain.com")
  |> should.be_error
}

pub fn validate_email_rejects_special_chars_test() {
  validation.validate_email("user<>@domain.com")
  |> should.be_error
}

pub fn validate_email_accepts_valid_test() {
  validation.validate_email("user@domain.com")
  |> should.be_ok
}

pub fn validate_email_accepts_with_dot_test() {
  validation.validate_email("user.name@domain.com")
  |> should.be_ok
}

pub fn validate_email_accepts_with_plus_test() {
  validation.validate_email("user+tag@domain.com")
  |> should.be_ok
}

pub fn validate_email_accepts_with_dash_test() {
  validation.validate_email("user-name@domain.com")
  |> should.be_ok
}

pub fn validate_email_accepts_with_underscore_test() {
  validation.validate_email("user_name@domain.com")
  |> should.be_ok
}

// === validate_email_format Stricter Validation ===

pub fn validate_email_format_requires_dot_in_domain_test() {
  validation.validate_email_format("user@domain")
  |> should.be_error
}

pub fn validate_email_format_accepts_valid_with_dot_test() {
  validation.validate_email_format("user@domain.com")
  |> should.be_ok
}

pub fn validate_email_format_rejects_empty_local_test() {
  validation.validate_email_format("@domain.com")
  |> should.be_error
}

pub fn validate_email_format_rejects_empty_domain_test() {
  validation.validate_email_format("user@")
  |> should.be_error
}

pub fn validate_email_format_rejects_no_at_symbol_test() {
  validation.validate_email_format("userdomain.com")
  |> should.be_error
}

pub fn validate_email_format_rejects_multiple_at_symbols_test() {
  validation.validate_email_format("user@name@domain.com")
  |> should.be_error
}

// === validate_non_empty ===

pub fn validate_non_empty_accepts_non_empty_string_test() {
  validation.validate_non_empty("hello", "field")
  |> should.be_ok
  |> should.equal("hello")
}

pub fn validate_non_empty_rejects_empty_string_test() {
  validation.validate_non_empty("", "name")
  |> should.be_error
}

pub fn validate_non_empty_error_includes_field_name_test() {
  case validation.validate_non_empty("", "username") {
    Error(msg) -> {
      msg
      |> string.contains("username")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_non_empty_accepts_whitespace_only_test() {
  // Whitespace counts as non-empty (not trimmed)
  validation.validate_non_empty("   ", "field")
  |> should.be_ok
}

// === validate_length ===

pub fn validate_length_accepts_string_within_bounds_test() {
  validation.validate_length("hello", 1, 10, "field")
  |> should.be_ok
  |> should.equal("hello")
}

pub fn validate_length_accepts_string_at_min_bound_test() {
  validation.validate_length("a", 1, 10, "field")
  |> should.be_ok
}

pub fn validate_length_accepts_string_at_max_bound_test() {
  validation.validate_length("1234567890", 1, 10, "field")
  |> should.be_ok
}

pub fn validate_length_rejects_string_below_min_test() {
  validation.validate_length("", 1, 10, "field")
  |> should.be_error
}

pub fn validate_length_rejects_string_above_max_test() {
  validation.validate_length("12345678901", 1, 10, "field")
  |> should.be_error
}

pub fn validate_length_error_includes_field_name_test() {
  case validation.validate_length("x", 5, 10, "password") {
    Error(msg) -> {
      msg
      |> string.contains("password")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}

pub fn validate_length_error_includes_bounds_test() {
  case validation.validate_length("x", 5, 10, "field") {
    Error(msg) -> {
      msg
      |> string.contains("5")
      |> should.be_true
      msg
      |> string.contains("10")
      |> should.be_true
    }
    Ok(_) -> should.fail()
  }
}
