import domain
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Language parsing tests
pub fn parse_language_go_test() {
  domain.parse_language("go")
  |> should.equal(Ok(domain.Go))
}

pub fn parse_language_gleam_test() {
  domain.parse_language("gleam")
  |> should.equal(Ok(domain.Gleam))
}

pub fn parse_language_rust_test() {
  domain.parse_language("rust")
  |> should.equal(Ok(domain.Rust))
}

pub fn parse_language_python_test() {
  domain.parse_language("python")
  |> should.equal(Ok(domain.Python))
}

pub fn parse_language_invalid_test() {
  domain.parse_language("javascript")
  |> should.be_error()
}

pub fn parse_language_empty_string_test() {
  domain.parse_language("")
  |> should.be_error()
}

pub fn parse_language_uppercase_test() {
  domain.parse_language("GO")
  |> should.be_error()
}

pub fn parse_language_mixed_case_test() {
  domain.parse_language("Go")
  |> should.be_error()
}

pub fn parse_language_typo_test() {
  domain.parse_language("go ")
  |> should.be_error()
}

pub fn parse_language_similar_name_test() {
  domain.parse_language("golang")
  |> should.be_error()
}

pub fn parse_language_c_test() {
  domain.parse_language("c")
  |> should.be_error()
}

pub fn parse_language_java_test() {
  domain.parse_language("java")
  |> should.be_error()
}

// Language detection tests
pub fn detect_language_gleam_test() {
  domain.detect_language_from_files(True, False, False, False)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_go_test() {
  domain.detect_language_from_files(False, True, False, False)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_rust_test() {
  domain.detect_language_from_files(False, False, True, False)
  |> should.equal(Ok(domain.Rust))
}

pub fn detect_language_python_test() {
  domain.detect_language_from_files(False, False, False, True)
  |> should.equal(Ok(domain.Python))
}

pub fn detect_language_priority_test() {
  domain.detect_language_from_files(True, True, True, True)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_none_test() {
  domain.detect_language_from_files(False, False, False, False)
  |> should.be_error()
}

pub fn detect_language_first_match_priority_test() {
  domain.detect_language_from_files(True, True, False, False)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_multiple_markers_go_priority_test() {
  domain.detect_language_from_files(False, True, True, False)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_multiple_markers_rust_priority_test() {
  domain.detect_language_from_files(False, False, True, True)
  |> should.equal(Ok(domain.Rust))
}

pub fn detect_language_only_python_test() {
  domain.detect_language_from_files(False, False, False, True)
  |> should.equal(Ok(domain.Python))
}

pub fn detect_language_all_false_test() {
  domain.detect_language_from_files(False, False, False, False)
  |> should.be_error()
}

// Slug validation tests
pub fn validate_slug_valid_test() {
  domain.validate_slug("valid-slug")
  |> should.be_ok()
}

pub fn validate_slug_empty_test() {
  domain.validate_slug("")
  |> should.be_error()
}

pub fn validate_slug_too_long_test() {
  let long_slug = "this-is-a-very-long-slug-that-exceeds-the-maximum-length"
  domain.validate_slug(long_slug)
  |> should.be_error()
}

pub fn validate_slug_invalid_chars_test() {
  domain.validate_slug("invalid slug")
  |> should.be_error()
}

pub fn validate_slug_uppercase_test() {
  domain.validate_slug("Invalid-Slug")
  |> should.be_error()
}

pub fn validate_slug_underscore_test() {
  domain.validate_slug("valid_slug")
  |> should.be_ok()
}

pub fn validate_slug_hyphen_test() {
  domain.validate_slug("valid-slug")
  |> should.be_ok()
}

pub fn validate_slug_digits_test() {
  domain.validate_slug("valid123")
  |> should.be_ok()
}

pub fn validate_slug_single_char_test() {
  domain.validate_slug("a")
  |> should.be_ok()
}

pub fn validate_slug_single_digit_test() {
  domain.validate_slug("0")
  |> should.be_ok()
}

pub fn validate_slug_single_hyphen_test() {
  domain.validate_slug("-")
  |> should.be_ok()
}

pub fn validate_slug_single_underscore_test() {
  domain.validate_slug("_")
  |> should.be_ok()
}

pub fn validate_slug_max_length_test() {
  let max_slug = "a-0_b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x"
  domain.validate_slug(max_slug)
  |> should.be_ok()
}

pub fn validate_slug_exactly_51_chars_test() {
  let too_long = "a-0_b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x-y"
  domain.validate_slug(too_long)
  |> should.be_error()
}

pub fn validate_slug_spaces_test() {
  domain.validate_slug("has space")
  |> should.be_error()
}

pub fn validate_slug_tabs_test() {
  domain.validate_slug("has\ttab")
  |> should.be_error()
}

pub fn validate_slug_special_chars_exclamation_test() {
  domain.validate_slug("has!")
  |> should.be_error()
}

pub fn validate_slug_special_chars_dot_test() {
  domain.validate_slug("has.dot")
  |> should.be_error()
}

pub fn validate_slug_special_chars_comma_test() {
  domain.validate_slug("has,comma")
  |> should.be_error()
}

pub fn validate_slug_special_chars_slash_test() {
  domain.validate_slug("has/slash")
  |> should.be_error()
}

pub fn validate_slug_special_chars_backslash_test() {
  domain.validate_slug("has\\backslash")
  |> should.be_error()
}

pub fn validate_slug_special_chars_at_test() {
  domain.validate_slug("has@at")
  |> should.be_error()
}

pub fn validate_slug_special_chars_hash_test() {
  domain.validate_slug("has#hash")
  |> should.be_error()
}

pub fn validate_slug_special_chars_dollar_test() {
  domain.validate_slug("has$dollar")
  |> should.be_error()
}

pub fn validate_slug_special_chars_percent_test() {
  domain.validate_slug("has%percent")
  |> should.be_error()
}

pub fn validate_slug_uppercase_a_test() {
  domain.validate_slug("A")
  |> should.be_error()
}

pub fn validate_slug_uppercase_mid_test() {
  domain.validate_slug("aB")
  |> should.be_error()
}

pub fn validate_slug_all_digits_test() {
  domain.validate_slug("123456789")
  |> should.be_ok()
}

pub fn validate_slug_all_hyphens_test() {
  domain.validate_slug("-----")
  |> should.be_ok()
}

pub fn validate_slug_all_underscores_test() {
  domain.validate_slug("_____")
  |> should.be_ok()
}

pub fn validate_slug_mixed_separators_test() {
  domain.validate_slug("a-b_c-d_e")
  |> should.be_ok()
}

pub fn validate_slug_starting_with_hyphen_test() {
  domain.validate_slug("-start")
  |> should.be_ok()
}

pub fn validate_slug_ending_with_underscore_test() {
  domain.validate_slug("end_")
  |> should.be_ok()
}

pub fn validate_slug_consecutive_separators_test() {
  domain.validate_slug("a--b__c")
  |> should.be_ok()
}

pub fn slug_validator_rejects_uppercase_consistently_test() {
  domain.validate_slug("Test")
  |> should.be_error()
}

pub fn slug_validator_rejects_spaces_consistently_test() {
  domain.validate_slug("test slug")
  |> should.be_error()
}

// Slug opaque type tests
pub fn slug_preserves_value_test() {
  let assert Ok(slug) = domain.validate_slug("test-123")
  domain.slug_to_string(slug)
  |> should.equal("test-123")
}

pub fn slug_roundtrip_test() {
  let original = "a-b_c-1-2-3"
  let assert Ok(slug) = domain.validate_slug(original)
  domain.slug_to_string(slug)
  |> should.equal(original)
}

pub fn slug_accessor_type_safety_test() {
  let assert Ok(slug) = domain.validate_slug("my-slug")
  let result = domain.slug_to_string(slug)
  result
  |> should.equal("my-slug")
}

pub fn slug_roundtrip_various_values_test() {
  let values = ["a", "test", "test-123", "test_456", "a-b_c-d"]
  list.all(values, fn(val) {
    let assert Ok(slug) = domain.validate_slug(val)
    domain.slug_to_string(slug) == val
  })
  |> should.be_true()
}

// Stage retrieval tests
pub fn get_stage_implement_test() {
  domain.get_stage("implement")
  |> should.be_ok()
}

pub fn get_stage_unknown_test() {
  domain.get_stage("unknown")
  |> should.be_error()
}

pub fn find_stage_index_first_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "implement")
  |> should.equal(Ok(0))
}

pub fn find_stage_index_second_stage_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "unit-test")
  |> should.equal(Ok(1))
}

pub fn find_stage_index_middle_stage_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "coverage")
  |> should.equal(Ok(2))
}

pub fn find_stage_index_last_stage_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "accept")
  |> should.equal(Ok(8))
}

pub fn find_stage_index_last_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "accept")
  |> should.be_ok()
}

pub fn find_stage_index_missing_test() {
  let pipeline = domain.standard_pipeline()
  domain.find_stage_index(pipeline, "nonexistent")
  |> should.be_error()
}

// Stage filtering tests
pub fn filter_stages_full_test() {
  domain.filter_stages("implement", "accept")
  |> should.be_ok()
}

pub fn filter_stages_single_test() {
  domain.filter_stages("implement", "implement")
  |> should.be_ok()
}

pub fn filter_stages_reversed_test() {
  domain.filter_stages("accept", "implement")
  |> should.be_error()
}

pub fn filter_stages_unknown_start_test() {
  domain.filter_stages("unknown", "accept")
  |> should.be_error()
}

pub fn filter_stages_middle_range_test() {
  domain.filter_stages("unit-test", "static")
  |> should.be_ok()
}

pub fn filter_stages_last_three_test() {
  domain.filter_stages("security", "accept")
  |> should.be_ok()
}

pub fn filter_stages_first_two_test() {
  domain.filter_stages("implement", "unit-test")
  |> should.be_ok()
}

pub fn filter_stages_invalid_start_test() {
  domain.filter_stages("nonexistent", "accept")
  |> should.be_error()
}

pub fn filter_stages_invalid_end_test() {
  domain.filter_stages("implement", "nonexistent")
  |> should.be_error()
}

pub fn filter_stages_both_invalid_test() {
  domain.filter_stages("bad1", "bad2")
  |> should.be_error()
}

// TaskStatus tests - Created variant
pub fn task_status_created_test() {
  let status = domain.Created
  domain.is_transient_status(status)
  |> should.equal(False)
}

pub fn task_status_created_not_failed_test() {
  domain.is_failed(domain.Created)
  |> should.equal(False)
}

pub fn task_status_created_not_completed_test() {
  domain.is_completed(domain.Created)
  |> should.equal(False)
}

// TaskStatus tests - InProgress variant
pub fn is_transient_status_in_progress_test() {
  domain.is_transient_status(domain.InProgress("implement"))
  |> should.equal(True)
}

pub fn is_transient_status_created_test() {
  domain.is_transient_status(domain.Created)
  |> should.equal(False)
}

pub fn task_status_in_progress_transient_test() {
  domain.is_transient_status(domain.InProgress("stage1"))
  |> should.equal(True)
}

pub fn task_status_in_progress_not_failed_test() {
  domain.is_failed(domain.InProgress("stage1"))
  |> should.equal(False)
}

pub fn task_status_in_progress_not_completed_test() {
  domain.is_completed(domain.InProgress("stage1"))
  |> should.equal(False)
}

pub fn task_status_in_progress_get_stage_test() {
  domain.get_current_stage(domain.InProgress("unit-test"))
  |> should.equal(Ok("unit-test"))
}

pub fn task_status_in_progress_multiple_stages_test() {
  let s1 = domain.InProgress("implement")
  let s2 = domain.InProgress("unit-test")
  let s1_stage = domain.get_current_stage(s1)
  let s2_stage = domain.get_current_stage(s2)
  let assert Ok(s1_val) = s1_stage
  let assert Ok(s2_val) = s2_stage
  { s1_val == s2_val }
  |> should.be_false()
}

// TaskStatus tests - PassedPipeline variant
pub fn task_status_passed_pipeline_not_transient_test() {
  domain.is_transient_status(domain.PassedPipeline)
  |> should.equal(False)
}

pub fn task_status_passed_pipeline_not_failed_test() {
  domain.is_failed(domain.PassedPipeline)
  |> should.equal(False)
}

pub fn is_completed_passed_test() {
  domain.is_completed(domain.PassedPipeline)
  |> should.equal(True)
}

pub fn task_status_passed_pipeline_completed_test() {
  domain.is_completed(domain.PassedPipeline)
  |> should.equal(True)
}

pub fn task_status_passed_pipeline_get_stage_error_test() {
  domain.get_current_stage(domain.PassedPipeline)
  |> should.be_error()
}

// TaskStatus tests - FailedPipeline variant
pub fn is_failed_test() {
  domain.is_failed(domain.FailedPipeline("implement", "compilation error"))
  |> should.equal(True)
}

pub fn is_not_failed_test() {
  domain.is_failed(domain.PassedPipeline)
  |> should.equal(False)
}

pub fn get_failure_reason_test() {
  domain.get_failure_reason(domain.FailedPipeline("implement", "error"))
  |> should.equal(Ok("error"))
}

pub fn get_failure_reason_not_failed_test() {
  domain.get_failure_reason(domain.PassedPipeline)
  |> should.be_error()
}

pub fn task_status_failed_pipeline_not_transient_test() {
  domain.is_transient_status(domain.FailedPipeline("implement", "error"))
  |> should.equal(False)
}

pub fn task_status_failed_pipeline_is_failed_test() {
  domain.is_failed(domain.FailedPipeline("implement", "error"))
  |> should.equal(True)
}

pub fn task_status_failed_pipeline_not_completed_test() {
  domain.is_completed(domain.FailedPipeline("implement", "error"))
  |> should.equal(False)
}

pub fn task_status_failed_pipeline_get_reason_test() {
  domain.get_failure_reason(domain.FailedPipeline(
    "implement",
    "compilation failed",
  ))
  |> should.equal(Ok("compilation failed"))
}

pub fn task_status_failed_pipeline_get_stage_error_test() {
  domain.get_current_stage(domain.FailedPipeline("implement", "error"))
  |> should.be_error()
}

pub fn task_status_failed_pipeline_multiple_failures_test() {
  let f1 = domain.FailedPipeline("stage1", "reason1")
  let f2 = domain.FailedPipeline("stage2", "reason2")
  let r1 = domain.get_failure_reason(f1)
  let r2 = domain.get_failure_reason(f2)
  let assert Ok(r1_val) = r1
  let assert Ok(r2_val) = r2
  { r1_val == r2_val }
  |> should.be_false()
}

// TaskStatus tests - Integrated variant
pub fn task_status_integrated_not_transient_test() {
  domain.is_transient_status(domain.Integrated)
  |> should.equal(False)
}

pub fn task_status_integrated_not_failed_test() {
  domain.is_failed(domain.Integrated)
  |> should.equal(False)
}

pub fn is_completed_integrated_test() {
  domain.is_completed(domain.Integrated)
  |> should.equal(True)
}

pub fn task_status_integrated_completed_test() {
  domain.is_completed(domain.Integrated)
  |> should.equal(True)
}

pub fn task_status_integrated_get_stage_error_test() {
  domain.get_current_stage(domain.Integrated)
  |> should.be_error()
}

pub fn task_status_integrated_get_reason_error_test() {
  domain.get_failure_reason(domain.Integrated)
  |> should.be_error()
}

// TaskStatus test - general cases
pub fn is_completed_in_progress_test() {
  domain.is_completed(domain.InProgress("implement"))
  |> should.equal(False)
}

pub fn get_current_stage_test() {
  domain.get_current_stage(domain.InProgress("implement"))
  |> should.equal(Ok("implement"))
}

pub fn get_current_stage_not_in_progress_test() {
  domain.get_current_stage(domain.PassedPipeline)
  |> should.be_error()
}

// Language enum tests - Display names
pub fn language_all_variants_go_test() {
  let lang = domain.Go
  domain.language_display_name(lang)
  |> should.equal("Go")
}

pub fn language_all_variants_gleam_test() {
  let lang = domain.Gleam
  domain.language_display_name(lang)
  |> should.equal("Gleam")
}

pub fn language_all_variants_rust_test() {
  let lang = domain.Rust
  domain.language_display_name(lang)
  |> should.equal("Rust")
}

pub fn language_all_variants_python_test() {
  let lang = domain.Python
  domain.language_display_name(lang)
  |> should.equal("Python")
}

pub fn language_display_name_test() {
  domain.language_display_name(domain.Gleam)
  |> should.equal("Gleam")
}

// Language enum tests - Compiled check
pub fn language_compiled_go_test() {
  domain.is_compiled_language(domain.Go)
  |> should.equal(True)
}

pub fn language_compiled_gleam_test() {
  domain.is_compiled_language(domain.Gleam)
  |> should.equal(True)
}

pub fn language_compiled_rust_test() {
  domain.is_compiled_language(domain.Rust)
  |> should.equal(True)
}

pub fn language_compiled_python_test() {
  domain.is_compiled_language(domain.Python)
  |> should.equal(False)
}

pub fn is_compiled_language_gleam_test() {
  domain.is_compiled_language(domain.Gleam)
  |> should.equal(True)
}

pub fn is_compiled_language_python_test() {
  domain.is_compiled_language(domain.Python)
  |> should.equal(False)
}

// Language enum tests - Dynamic check
pub fn language_dynamic_go_test() {
  domain.is_dynamic_language(domain.Go)
  |> should.equal(False)
}

pub fn language_dynamic_gleam_test() {
  domain.is_dynamic_language(domain.Gleam)
  |> should.equal(False)
}

pub fn language_dynamic_rust_test() {
  domain.is_dynamic_language(domain.Rust)
  |> should.equal(False)
}

pub fn language_dynamic_python_test() {
  domain.is_dynamic_language(domain.Python)
  |> should.equal(True)
}

pub fn is_dynamic_language_python_test() {
  domain.is_dynamic_language(domain.Python)
  |> should.equal(True)
}

pub fn is_dynamic_language_gleam_test() {
  domain.is_dynamic_language(domain.Gleam)
  |> should.equal(False)
}

// Pipeline stage definitions - Existence
pub fn standard_pipeline_count_test() {
  domain.standard_pipeline()
  |> list.length()
  |> should.equal(9)
}

pub fn standard_pipeline_implement_exists_test() {
  domain.get_stage("implement")
  |> should.be_ok()
}

pub fn standard_pipeline_unit_test_exists_test() {
  domain.get_stage("unit-test")
  |> should.be_ok()
}

pub fn standard_pipeline_coverage_exists_test() {
  domain.get_stage("coverage")
  |> should.be_ok()
}

pub fn standard_pipeline_lint_exists_test() {
  domain.get_stage("lint")
  |> should.be_ok()
}

pub fn standard_pipeline_static_exists_test() {
  domain.get_stage("static")
  |> should.be_ok()
}

pub fn standard_pipeline_integration_exists_test() {
  domain.get_stage("integration")
  |> should.be_ok()
}

pub fn standard_pipeline_security_exists_test() {
  domain.get_stage("security")
  |> should.be_ok()
}

pub fn standard_pipeline_review_exists_test() {
  domain.get_stage("review")
  |> should.be_ok()
}

pub fn standard_pipeline_accept_exists_test() {
  domain.get_stage("accept")
  |> should.be_ok()
}

// Pipeline stage properties - TCR enabled
pub fn stage_all_tcr_enabled_test() {
  domain.standard_pipeline()
  |> list.all(domain.is_tcr_stage)
  |> should.equal(True)
}

pub fn is_tcr_stage_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  domain.is_tcr_stage(stage)
  |> should.equal(True)
}

pub fn is_not_tcr_stage_test() {
  let stage = domain.Stage("manual", "Manual check", 1, False)
  domain.is_tcr_stage(stage)
  |> should.equal(False)
}

// Pipeline stage properties - Retries
pub fn stage_retries_implement_test() {
  let assert Ok(stage) = domain.get_stage("implement")
  domain.get_stage_retries(stage)
  |> should.equal(5)
}

pub fn stage_retries_unit_test_test() {
  let assert Ok(stage) = domain.get_stage("unit-test")
  domain.get_stage_retries(stage)
  |> should.equal(3)
}

pub fn stage_retries_coverage_test() {
  let assert Ok(stage) = domain.get_stage("coverage")
  domain.get_stage_retries(stage)
  |> should.equal(5)
}

pub fn stage_retries_lint_test() {
  let assert Ok(stage) = domain.get_stage("lint")
  domain.get_stage_retries(stage)
  |> should.equal(3)
}

pub fn stage_retries_static_test() {
  let assert Ok(stage) = domain.get_stage("static")
  domain.get_stage_retries(stage)
  |> should.equal(3)
}

pub fn stage_retries_integration_test() {
  let assert Ok(stage) = domain.get_stage("integration")
  domain.get_stage_retries(stage)
  |> should.equal(3)
}

pub fn stage_retries_security_test() {
  let assert Ok(stage) = domain.get_stage("security")
  domain.get_stage_retries(stage)
  |> should.equal(2)
}

pub fn stage_retries_review_test() {
  let assert Ok(stage) = domain.get_stage("review")
  domain.get_stage_retries(stage)
  |> should.equal(3)
}

pub fn stage_retries_accept_test() {
  let assert Ok(stage) = domain.get_stage("accept")
  domain.get_stage_retries(stage)
  |> should.equal(1)
}

pub fn get_stage_retries_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  domain.get_stage_retries(stage)
  |> should.equal(5)
}

// Pipeline tests - TCR stages
pub fn count_tcr_stages_test() {
  domain.standard_pipeline()
  |> domain.count_tcr_stages()
  |> should.equal(9)
}

pub fn first_tcr_stage_test() {
  domain.standard_pipeline()
  |> domain.first_tcr_stage()
  |> should.be_ok()
}

pub fn non_tcr_stages_test() {
  domain.standard_pipeline()
  |> domain.non_tcr_stages()
  |> should.equal([])
}

// Slug property tests
pub fn is_slug_lowercase_test() {
  let assert Ok(slug) = domain.validate_slug("lowercase")
  domain.is_slug_lowercase(slug)
  |> should.equal(True)
}

pub fn has_separators_hyphen_test() {
  let assert Ok(slug) = domain.validate_slug("has-separator")
  domain.has_separators(slug)
  |> should.equal(True)
}

pub fn has_separators_underscore_test() {
  let assert Ok(slug) = domain.validate_slug("has_separator")
  domain.has_separators(slug)
  |> should.equal(True)
}

pub fn has_separators_none_test() {
  let assert Ok(slug) = domain.validate_slug("noseparator")
  domain.has_separators(slug)
  |> should.equal(False)
}

// Slug to string tests
pub fn slug_to_string_test() {
  let assert Ok(slug) = domain.validate_slug("test-slug")
  domain.slug_to_string(slug)
  |> should.equal("test-slug")
}

// Pipeline property tests
pub fn max_pipeline_retries_test() {
  domain.standard_pipeline()
  |> domain.max_pipeline_retries()
  |> should.equal(5)
}

pub fn count_stages_by_gate_test() {
  let pipeline = [
    domain.Stage("s1", "gate1", 1, True),
    domain.Stage("s2", "gate1", 1, True),
    domain.Stage("s3", "gate2", 1, True),
  ]
  domain.count_stages_by_gate(pipeline, "gate1")
  |> should.equal(2)
}

pub fn gate_names_test() {
  let pipeline = [
    domain.Stage("s1", "gate1", 1, True),
    domain.Stage("s2", "gate2", 1, True),
  ]
  domain.gate_names(pipeline)
  |> should.equal(["gate1", "gate2"])
}

// Integration readiness tests
pub fn is_ready_for_integration_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Gleam)
  |> should.equal(True)
}

pub fn is_not_ready_for_integration_python_test() {
  domain.is_ready_for_integration(domain.PassedPipeline, domain.Python)
  |> should.equal(False)
}

pub fn is_not_ready_for_integration_status_test() {
  domain.is_ready_for_integration(domain.InProgress("implement"), domain.Gleam)
  |> should.equal(False)
}

// Illegal state prevention tests
pub fn no_empty_slug_construction_test() {
  domain.validate_slug("")
  |> should.be_error()
}

pub fn no_invalid_char_slug_construction_test() {
  domain.validate_slug("INVALID")
  |> should.be_error()
}

pub fn no_oversized_slug_construction_test() {
  let huge = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  domain.validate_slug(huge)
  |> should.be_error()
}
