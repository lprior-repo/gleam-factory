// Validation module - Input validation utilities
// Provides common validation functions for user input

import gleam/string
import utils

/// Validate email format (simple check for @ symbol)
pub fn validate_email(email: String) -> Result(String, String) {
  case string.contains(email, "@") {
    True -> Ok(email)
    False -> Error("Invalid email: must contain @ symbol")
  }
}

/// Validate email format (stricter check: exactly one @, text before/after, dot after @)
pub fn validate_email_format(email: String) -> Result(String, String) {
  case string.split(email, "@") {
    [local, domain] ->
      case string.length(local) > 0 && string.length(domain) > 0 && string.contains(domain, ".") {
        True -> Ok(email)
        False -> Error("Invalid email format")
      }
    _ -> Error("Invalid email format")
  }
}

/// Validate non-empty string
pub fn validate_non_empty(value: String, field_name: String) -> Result(String, String) {
  case string.length(value) > 0 {
    True -> Ok(value)
    False -> Error(field_name <> " cannot be empty")
  }
}

/// Validate string length within bounds
pub fn validate_length(
  value: String,
  min: Int,
  max: Int,
  field_name: String,
) -> Result(String, String) {
  let len = string.length(value)
  case len >= min && len <= max {
    True -> Ok(value)
    False ->
      Error(
        field_name
        <> " must be between "
        <> utils.int_to_string(min)
        <> " and "
        <> utils.int_to_string(max)
        <> " characters",
      )
  }
}
