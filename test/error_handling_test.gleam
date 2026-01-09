import gleam/option.{Some}
import gleam/string
import gleeunit
import gleeunit/should
import error_handling.{
  Error, ErrorContext, wrap_error, format_error_details,
  format_user_message, extract_cause,
}

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// ERROR HANDLING TESTS
// ============================================================================

pub fn wrap_error_includes_context_test() {
  let context =
    ErrorContext(
      module: "validation",
      function: "validate_input",
      cause: Some("invalid format"),
    )

  let error = wrap_error(Nil, context)

  case error {
    Error(_, ctx) -> {
      ctx.module
      |> should.equal("validation")

      ctx.function
      |> should.equal("validate_input")

      ctx.cause
      |> should.equal(Some("invalid format"))
    }
  }
}

pub fn error_logged_with_details_test() {
  let context =
    ErrorContext(
      module: "database",
      function: "connect",
      cause: Some("connection timeout"),
    )

  let error = wrap_error(Nil, context)

  case error {
    Error(_, ctx) -> {
      let error_details = format_error_details(ctx)

      error_details
      |> string.contains("database")
      |> should.be_true()

      error_details
      |> string.contains("connect")
      |> should.be_true()

      error_details
      |> string.contains("connection timeout")
      |> should.be_true()
    }
  }
}

pub fn user_facing_error_is_friendly_test() {
  let context =
    ErrorContext(
      module: "api",
      function: "fetch_data",
      cause: Some("HTTP 500"),
    )

  let error = wrap_error(Nil, context)
  let user_message = format_user_message(error)

  user_message
  |> string.contains("panic")
  |> should.be_false()

  user_message
  |> string.contains("stack")
  |> should.be_false()

  user_message
  |> string.contains("trace")
  |> should.be_false()

  user_message
  |> string.contains("Something went wrong")
  |> should.be_true()
}

pub fn error_chain_preserves_cause_test() {
  let original_cause = "file not found"

  let context1 =
    ErrorContext(
      module: "file_system",
      function: "read_file",
      cause: Some(original_cause),
    )

  let error1 = wrap_error(Nil, context1)

  let context2 =
    ErrorContext(
      module: "config",
      function: "load_config",
      cause: extract_cause(error1),
    )

  let error2 = wrap_error(Nil, context2)

  case error2 {
    Error(_, ctx) -> {
      ctx.cause
      |> should.equal(Some(original_cause))
    }
  }
}

