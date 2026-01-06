import gleam/string
import gleam/list

pub type Priority {
  P1
  P2
  P3
}

pub fn validate_priority(p: String) -> Result(Priority, String) {
  case string.lowercase(p) {
    "p1" | "1" -> Ok(P1)
    "p2" | "2" -> Ok(P2)
    "p3" | "3" -> Ok(P3)
    "" -> Error("Priority cannot be empty")
    _ -> Error("Invalid priority")
  }
}

pub fn validate_slug_format(slug: String) -> Result(String, String) {
  let len = string.length(slug)
  case len {
    0 -> Error("Slug cannot be empty")
    n if n > 50 -> Error("Slug exceeds maximum length of 50 characters")
    _ -> validate_slug_chars(slug)
  }
}

fn validate_slug_chars(slug: String) -> Result(String, String) {
  case string.first(slug) {
    Error(_) -> Error("Slug must start with a letter")
    Ok(first) ->
      case is_letter(first) {
        False -> Error("Slug must start with a letter")
        True ->
          case
            slug
            |> string.to_graphemes
            |> list.all(fn(c) {
              is_letter(c) || is_digit(c) || c == "_"
            })
          {
            False ->
              Error(
                "Slug can only contain letters, numbers, and underscores",
              )
            True -> Ok(slug)
          }
      }
  }
}

fn is_letter(c: String) -> Bool {
  let lower = string.lowercase(c)
  case string.length(lower) {
    1 -> string.contains("abcdefghijklmnopqrstuvwxyz", lower)
    _ -> False
  }
}

fn is_digit(c: String) -> Bool {
  string.contains("0123456789", c)
}
