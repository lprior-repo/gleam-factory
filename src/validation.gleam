import gleam/int
import gleam/list
import gleam/string

pub fn validate_email(email: String) -> Result(String, String) {
  case string.split(email, "@") {
    [local, domain] -> {
      case is_valid_local_part(local) && is_valid_domain_part(domain) {
        True -> Ok(email)
        False -> Error("Invalid email format")
      }
    }
    _ -> Error("Invalid email: must contain exactly one @ symbol")
  }
}

fn is_valid_local_part(local: String) -> Bool {
  case string.length(local) {
    0 -> False
    _ -> {
      let chars = string.to_graphemes(local)
      case chars {
        ["."] -> False
        _ ->
          !string.starts_with(local, ".")
          && !string.ends_with(local, ".")
          && !string.contains(local, "..")
          && !string.contains(local, " ")
          && list.all(chars, is_valid_local_char)
      }
    }
  }
}

fn is_valid_domain_part(domain: String) -> Bool {
  string.length(domain) > 0 && !string.contains(domain, " ")
}

fn is_valid_local_char(char: String) -> Bool {
  case string.to_utf_codepoints(char) {
    [cp] -> {
      let code = string.utf_codepoint_to_int(cp)
      { code >= 97 && code <= 122 }
      || { code >= 65 && code <= 90 }
      || { code >= 48 && code <= 57 }
      || code == 46
      || code == 95
      || code == 45
      || code == 43
    }
    _ -> False
  }
}

/// Validate email format (stricter check: exactly one @, text before/after, dot after @)
pub fn validate_email_format(email: String) -> Result(String, String) {
  case string.split(email, "@") {
    [local, domain] ->
      case
        string.length(local) > 0
        && string.length(domain) > 0
        && string.contains(domain, ".")
      {
        True -> Ok(email)
        False -> Error("Invalid email format")
      }
    _ -> Error("Invalid email format")
  }
}

/// Validate non-empty string
pub fn validate_non_empty(
  value: String,
  field_name: String,
) -> Result(String, String) {
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
        <> int.to_string(min)
        <> " and "
        <> int.to_string(max)
        <> " characters",
      )
  }
}
