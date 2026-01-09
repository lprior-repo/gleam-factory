import gleam/option.{type Option, None, Some}

// Error context captures source location and root cause
pub type ErrorContext {
  ErrorContext(
    module: String,
    function: String,
    cause: Option(String),
  )
}

// Error wraps a reason with context for diagnosis
pub type Error {
  Error(reason: String, context: ErrorContext)
}

// wrap_error attaches context to an error value
pub fn wrap_error(_e: a, context: ErrorContext) -> Error {
  Error(reason: "Error occurred", context: context)
}

// format_error_details produces technical output including all context
pub fn format_error_details(context: ErrorContext) -> String {
  let cause_str = case context.cause {
    Some(c) -> " (" <> c <> ")"
    None -> ""
  }

  context.module
  <> "::"
  <> context.function
  <> cause_str
}

// format_user_message produces friendly output safe for end users
pub fn format_user_message(_error: Error) -> String {
  "Something went wrong. Please try again later."
}

// extract_cause gets the root cause from an error
pub fn extract_cause(error: Error) -> Option(String) {
  case error {
    Error(_, context) -> context.cause
  }
}
