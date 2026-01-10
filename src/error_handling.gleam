import gleam/option.{type Option, None, Some}
import gleam/string

// Error context captures source location and root cause
pub type ErrorContext {
  ErrorContext(module: String, function: String, cause: Option(String))
}

// Error wraps a reason with context for diagnosis
pub type Error {
  Error(reason: String, context: ErrorContext)
}

// wrap_error attaches context to an error value, preserving the original error
pub fn wrap_error(e: a, context: ErrorContext) -> Error {
  let reason = string.inspect(e)
  Error(reason:, context:)
}

// wrap_error_string wraps a string error with context
pub fn wrap_error_string(reason: String, context: ErrorContext) -> Error {
  Error(reason:, context:)
}

// format_error_details produces technical output including all context
pub fn format_error_details(context: ErrorContext) -> String {
  let cause_str = case context.cause {
    Some(c) -> " (" <> c <> ")"
    None -> ""
  }

  context.module <> "::" <> context.function <> cause_str
}

// format_user_message produces friendly output safe for end users
pub fn format_user_message(error: Error) -> String {
  case error.context.module {
    "auth" -> "Authentication failed. Please check your credentials."
    "validation" ->
      case error.context.cause {
        Some(cause) -> "Invalid input: " <> cause
        None -> "Something went wrong. Please try again later."
      }
    _ ->
      case error.context.cause {
        Some(cause) ->
          case
            string.contains(cause, "network")
            || string.contains(cause, "timeout")
          {
            True -> "Network error. Please check your connection and retry."
            False -> "Something went wrong. Please try again later."
          }
        None -> "Something went wrong. Please try again later."
      }
  }
}

// extract_cause gets the root cause from an error
pub fn extract_cause(error: Error) -> Option(String) {
  case error {
    Error(_, context) -> context.cause
  }
}
