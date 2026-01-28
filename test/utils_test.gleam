import gleeunit
import gleeunit/should
import utils

pub fn main() {
  gleeunit.main()
}

pub fn format_success_adds_checkmark_test() {
  let result = utils.format_success("Test passed")
  result |> should.equal("✓ Test passed")
}

pub fn format_error_adds_x_mark_test() {
  let result = utils.format_error("Test failed")
  result |> should.equal("✗ Test failed")
}

pub fn format_info_adds_i_mark_test() {
  let result = utils.format_info("Information")
  result |> should.equal("ℹ Information")
}

pub fn format_warning_adds_warning_mark_test() {
  let result = utils.format_warning("Warning")
  result |> should.equal("⚠ Warning")
}

pub fn progress_bar_zero_percent_test() {
  let result = utils.progress_bar(0, 100)
  result |> should.equal("[░░░░░░░░░░░░░░░░░░░] 0%")
}

pub fn progress_bar_half_complete_test() {
  let result = utils.progress_bar(5, 10)
  result |> should.equal("[██████████░░░░░░░░] 50%")
}

pub fn progress_bar_fully_complete_test() {
  let result = utils.progress_bar(100, 100)
  result |> should.equal("[████████████████████] 100%")
}

pub fn progress_bar_div_by_zero_returns_zero_test() {
  let result = utils.progress_bar(50, 0)
  result |> should.equal("[░░░░░░░░░░░░░░░░░░░] 0%")
}
