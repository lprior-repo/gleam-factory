import gleeunit
import gleeunit/should
import phase_handlers
import llm
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn implementer_can_write_src_test() {
  let _ = simplifile.create_directory_all("src/test_dir")
  let result = phase_handlers.handle_write_file("src/test_file.gleam", "test", llm.Implementer)
  should.be_ok(result)
  let _ = simplifile.delete("src/test_file.gleam")
  Nil
}

pub fn implementer_cannot_write_test_test() {
  let result = phase_handlers.handle_write_file("test/test_file.gleam", "test", llm.Implementer)
  should.be_error(result)
}

pub fn architect_cannot_write_test() {
  let result = phase_handlers.handle_write_file("src/test_file.gleam", "test", llm.Architect)
  should.be_error(result)
}

pub fn reviewer_can_write_test_test() {
  let _ = simplifile.create_directory_all("test/test_dir")
  let result = phase_handlers.handle_write_file("test/test_file.gleam", "test", llm.Reviewer)
  should.be_ok(result)
  let _ = simplifile.delete("test/test_file.gleam")
  Nil
}

pub fn reviewer_cannot_write_src_test() {
  let result = phase_handlers.handle_write_file("src/test_file.gleam", "test", llm.Reviewer)
  should.be_error(result)
}

pub fn rejects_path_escape_test() {
  let result = phase_handlers.handle_write_file("../escape.txt", "test", llm.Implementer)
  should.be_error(result)
}

pub fn rejects_absolute_path_test() {
  let result = phase_handlers.handle_write_file("/etc/passwd", "test", llm.Implementer)
  should.be_error(result)
}
