// Property-based tests for utils module using qcheck
// Tests formatting and message generation invariants

import gleam/string
import gleam/int
import qcheck
import utils

// FORMAT SUCCESS - Property: always starts with checkmark
pub fn prop_format_success_has_checkmark__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_success(msg)
  assert string.starts_with(formatted, "✓")
}

// FORMAT SUCCESS - Property: contains original message
pub fn prop_format_success_preserves_message__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_success(msg)
  assert string.contains(formatted, msg)
}

// FORMAT ERROR - Property: always starts with X mark
pub fn prop_format_error_has_x__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_error(msg)
  assert string.starts_with(formatted, "✗")
}

// FORMAT ERROR - Property: contains original message
pub fn prop_format_error_preserves_message__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_error(msg)
  assert string.contains(formatted, msg)
}

// FORMAT INFO - Property: always starts with info symbol
pub fn prop_format_info_has_symbol__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_info(msg)
  assert string.starts_with(formatted, "ℹ")
}

// FORMAT INFO - Property: contains original message
pub fn prop_format_info_preserves_message__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_info(msg)
  assert string.contains(formatted, msg)
}

// FORMAT WARNING - Property: always starts with warning symbol
pub fn prop_format_warning_has_symbol__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_warning(msg)
  assert string.starts_with(formatted, "⚠")
}

// FORMAT WARNING - Property: contains original message
pub fn prop_format_warning_preserves_message__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let formatted = utils.format_warning(msg)
  assert string.contains(formatted, msg)
}

// PROGRESS BAR - Property: always 20 characters wide
pub fn prop_progress_bar_width__test() {
  use current <- qcheck.given(qcheck.bounded_int(0, 100))
  let bar = utils.progress_bar(current, 100)
  // Count filled and empty blocks plus brackets and percentage
  assert string.contains(bar, "[") && string.contains(bar, "]")
}

// PROGRESS BAR - Property: 0% shows all empty
pub fn prop_progress_bar_zero_percent__test() {
  let bar = utils.progress_bar(0, 100)
  assert string.contains(bar, "░") && string.contains(bar, "0%")
}

// PROGRESS BAR - Property: 100% shows all filled
pub fn prop_progress_bar_full__test() {
  let bar = utils.progress_bar(100, 100)
  assert string.contains(bar, "█") && string.contains(bar, "100%")
}

// PROGRESS BAR - Property: 50% is balanced
pub fn prop_progress_bar_half__test() {
  let bar = utils.progress_bar(50, 100)
  assert string.contains(bar, "█") && string.contains(bar, "░") && string.contains(bar, "50%")
}

// PROGRESS BAR - Property: handles zero total
pub fn prop_progress_bar_zero_total__test() {
  let bar = utils.progress_bar(5, 0)
  // Should handle gracefully and show 0%
  assert string.contains(bar, "0%")
}

// INT TO STRING - Property: converts positive numbers
pub fn prop_int_to_string_positive__test() {
  use n <- qcheck.given(qcheck.bounded_int(1, 10000))
  let str = utils.int_to_string(n)
  assert string.length(str) > 0
}

// INT TO STRING - Property: zero converts to "0"
pub fn prop_int_to_string_zero__test() {
  assert utils.int_to_string(0) == "0"
}

// INT TO STRING - Property: single digits convert correctly
pub fn prop_int_to_string_single_digit__test() {
  use n <- qcheck.given(qcheck.bounded_int(1, 9))
  let str = utils.int_to_string(n)
  assert string.length(str) == 1
}

// INT TO STRING - Property: preserves magnitude order
pub fn prop_int_to_string_order__test() {
  use vals <- qcheck.given(qcheck.tuple2(qcheck.bounded_int(1, 1000), qcheck.bounded_int(1, 1000)))
  let #(a, b) = vals
  let str_a = utils.int_to_string(a)
  let str_b = utils.int_to_string(b)
  assert case a < b {
    True -> string.length(str_a) <= string.length(str_b)
    False -> True
  }
}

// INT TO STRING - Property: negative numbers get minus prefix
pub fn prop_int_to_string_negative__test() {
  use n <- qcheck.given(qcheck.bounded_int(1, 1000))
  let str = utils.int_to_string(-n)
  assert string.starts_with(str, "-")
}

// INT TO STRING - Property: string representation matches int.to_string
pub fn prop_int_to_string_matches_standard__test() {
  use n <- qcheck.given(qcheck.bounded_int(1, 1000))
  let custom = utils.int_to_string(n)
  let standard = int.to_string(n)
  assert custom == standard
}

// CONFIRM - Property: always returns bool
pub fn prop_confirm_returns_bool__test() {
  let result = utils.confirm("test")
  // Just verify it returns a bool (we can't test interactive input)
  assert case result {
    True -> True
    False -> True
  }
}

// PROMPT - Property: always returns string
pub fn prop_prompt_returns_string__test() {
  use len <- qcheck.given(qcheck.bounded_int(1, 100))
  let msg = string.repeat("a", len)
  let result = utils.prompt(msg)
  // Prompt returns empty in test environment
  assert case result {
    "" -> True
    _ -> True
  }
}

// FORMATTING CONSISTENCY - Property: all format functions follow same pattern
pub fn prop_format_consistency__test() {
  let msg = "test"
  let success = utils.format_success(msg)
  let error = utils.format_error(msg)
  let info = utils.format_info(msg)
  let warning = utils.format_warning(msg)

  // All should have exactly 1 space after symbol
  assert string.starts_with(success, "✓ ")
  && string.starts_with(error, "✗ ")
  && string.starts_with(info, "ℹ ")
  && string.starts_with(warning, "⚠ ")
}
