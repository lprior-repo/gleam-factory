import domain
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

// TaskStatus tests
pub fn is_transient_status_in_progress_test() {
  domain.is_transient_status(domain.InProgress("implement"))
  |> should.equal(True)
}

pub fn is_transient_status_created_test() {
  domain.is_transient_status(domain.Created)
  |> should.equal(False)
}

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

pub fn is_completed_passed_test() {
  domain.is_completed(domain.PassedPipeline)
  |> should.equal(True)
}

pub fn is_completed_integrated_test() {
  domain.is_completed(domain.Integrated)
  |> should.equal(True)
}

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

// Stage property tests
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

pub fn get_stage_retries_test() {
  let stage = domain.Stage("implement", "Code compiles", 5, True)
  domain.get_stage_retries(stage)
  |> should.equal(5)
}

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

// Language property tests
pub fn is_compiled_language_gleam_test() {
  domain.is_compiled_language(domain.Gleam)
  |> should.equal(True)
}

pub fn is_compiled_language_python_test() {
  domain.is_compiled_language(domain.Python)
  |> should.equal(False)
}

pub fn is_dynamic_language_python_test() {
  domain.is_dynamic_language(domain.Python)
  |> should.equal(True)
}

pub fn is_dynamic_language_gleam_test() {
  domain.is_dynamic_language(domain.Gleam)
  |> should.equal(False)
}

pub fn language_display_name_test() {
  domain.language_display_name(domain.Gleam)
  |> should.equal("Gleam")
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

// Integration readiness test
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

// Slug to string test
pub fn slug_to_string_test() {
  let assert Ok(slug) = domain.validate_slug("test-slug")
  domain.slug_to_string(slug)
  |> should.equal("test-slug")
}
