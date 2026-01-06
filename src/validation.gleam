import gleam/string

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
  let slug_length = string.length(slug)

  case slug_length {
    0 -> Error("Slug cannot be empty")
    _ ->
      case slug_length > 50 {
        True -> Error("Slug exceeds maximum length of 50 characters")
        False ->
          case string.starts_with(slug, first_char(slug)) {
            False -> Error("Slug must start with a letter")
            True ->
              case is_valid_chars(slug) {
                False -> Error("Slug can only contain letters, numbers, and underscores")
                True -> Ok(slug)
              }
          }
      }
  }
}

fn first_char(s: String) -> String {
  case string.first(s) {
    Ok(c) -> c
    Error(_) -> ""
  }
}

fn is_valid_chars(slug: String) -> Bool {
  case string.first(slug) {
    Error(_) -> False
    Ok(first) ->
      case is_letter(first) {
        False -> False
        True ->
          slug
          |> string.to_graphemes
          |> check_all_valid_chars
      }
  }
}

fn check_all_valid_chars(chars: List(String)) -> Bool {
  case chars {
    [] -> True
    [char, ..rest] ->
      case is_alphanumeric_or_underscore(char) {
        False -> False
        True -> check_all_valid_chars(rest)
      }
  }
}

fn is_letter(char: String) -> Bool {
  let code = case string.to_graphemes(char) {
    [c] ->
      case c {
        "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
        | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w"
        | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
        | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S"
        | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" -> True
        _ -> False
      }
    _ -> False
  }
  code
}

fn is_alphanumeric_or_underscore(char: String) -> Bool {
  case char {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
    | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w"
    | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H"
    | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S"
    | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "0" | "1" | "2" | "3"
    | "4" | "5" | "6" | "7" | "8" | "9" | "_" -> True
    _ -> False
  }
}
