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

fn is_valid_local_char(c: String) -> Bool {
  case c {
    "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f"
    | "g"
    | "h"
    | "i"
    | "j"
    | "k"
    | "l"
    | "m"
    | "n"
    | "o"
    | "p"
    | "q"
    | "r"
    | "s"
    | "t"
    | "u"
    | "v"
    | "w"
    | "x"
    | "y"
    | "z" -> True
    "A"
    | "B"
    | "C"
    | "D"
    | "E"
    | "F"
    | "G"
    | "H"
    | "I"
    | "J"
    | "K"
    | "L"
    | "M"
    | "N"
    | "O"
    | "P"
    | "Q"
    | "R"
    | "S"
    | "T"
    | "U"
    | "V"
    | "W"
    | "X"
    | "Y"
    | "Z" -> True
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    "." | "_" | "-" | "+" -> True
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
