// Unit tests for validation module - edge cases for validate_email
// Tests malformed email formats that should be rejected

import gleeunit/should
import validation

// Test @ alone fails
pub fn validate_email_rejects_at_alone_test() {
  validation.validate_email("@")
  |> should.be_error
}

// Test @domain.com fails (no local part)
pub fn validate_email_rejects_no_local_part_test() {
  validation.validate_email("@domain.com")
  |> should.be_error
}

// Test user@ fails (no domain)
pub fn validate_email_rejects_no_domain_test() {
  validation.validate_email("user@")
  |> should.be_error
}

// Test user@@domain.com fails (double @)
pub fn validate_email_rejects_double_at_test() {
  validation.validate_email("user@@domain.com")
  |> should.be_error
}

// Test user@domain.com passes
pub fn validate_email_accepts_valid_test() {
  validation.validate_email("user@domain.com")
  |> should.be_ok
}

// Test simple user@domain passes (no dot required for basic validation)
pub fn validate_email_accepts_simple_valid_test() {
  validation.validate_email("user@domain")
  |> should.be_ok
}
