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
