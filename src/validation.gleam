// Validation module - Input validation utilities
// Provides common validation functions for user input

import gleam/string

/// Validate email format (simple check for @ symbol)
pub fn validate_email(email: String) -> Result(String, String) {
  case string.contains(email, "@") {
    True -> Ok(email)
    False -> Error("Invalid email: must contain @ symbol")
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
        <> int_to_string(min)
        <> " and "
        <> int_to_string(max)
        <> " characters",
      )
  }
}

/// Helper to convert int to string
fn int_to_string(n: Int) -> String {
  case n < 0 {
    True -> "-" <> int_to_string(-n)
    False ->
      case n {
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        _ -> int_to_string(n / 10) <> int_to_string(n % 10)
      }
  }
}
