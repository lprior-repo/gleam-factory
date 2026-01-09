import gleeunit
import gleeunit/should
import process

pub fn main() {
  gleeunit.main()
}

// shell_escape tests (via build_shell_command)
pub fn run_command_simple_test() {
  case process.run_command("echo", ["hello"], "") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("hello")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_with_args_test() {
  case process.run_command("echo", ["foo", "bar"], "") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("foo bar")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_with_cwd_test() {
  case process.run_command("pwd", [], "/tmp") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("/tmp")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_empty_args_test() {
  case process.run_command("echo", [], "") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_special_chars_test() {
  case process.run_command("echo", ["it's"], "") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("it's")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_nonzero_exit_test() {
  case process.run_command("sh", ["-c", "exit 42"], "") {
    Ok(process.Failure(_, 42)) -> Nil
    Ok(process.Failure(_, _)) -> should.fail()
    Ok(process.Success(_, _, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_missing_command_test() {
  case process.run_command("nonexistent_cmd_xyz", [], "") {
    Ok(process.Failure(_, code)) -> {
      code
      |> should.not_equal(0)
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

// command_exists tests
pub fn command_exists_true_test() {
  case process.command_exists("echo") {
    Ok(True) -> Nil
    Ok(False) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn command_exists_false_test() {
  case process.command_exists("nonexistent_cmd_abc") {
    Ok(_) -> should.fail()
    Error(msg) -> {
      msg
      |> should.equal("Command not found in PATH: nonexistent_cmd_abc")
    }
  }
}

// check_success tests
pub fn check_success_on_success_test() {
  let result = process.Success("out", "", 0)
  process.check_success(result)
  |> should.be_ok()
}

pub fn check_success_on_failure_test() {
  let result = process.Failure("err", 1)
  case process.check_success(result) {
    Ok(_) -> should.fail()
    Error(msg) -> {
      msg
      |> should.equal("Command failed with exit code 1: err")
    }
  }
}

// is_success tests
pub fn is_success_true_test() {
  let result = process.Success("", "", 0)
  process.is_success(result)
  |> should.be_true()
}

pub fn is_success_false_test() {
  let result = process.Failure("", 1)
  process.is_success(result)
  |> should.be_false()
}

// get_stdout tests
pub fn get_stdout_success_test() {
  let result = process.Success("output", "", 0)
  process.get_stdout(result)
  |> should.equal(Ok("output"))
}

pub fn get_stdout_failure_test() {
  let result = process.Failure("error", 2)
  case process.get_stdout(result) {
    Ok(_) -> should.fail()
    Error(msg) -> {
      msg
      |> should.equal("Command failed with exit code 2: error")
    }
  }
}

// get_error tests
pub fn get_error_on_success_test() {
  let result = process.Success("", "", 0)
  process.get_error(result)
  |> should.be_ok()
}

pub fn get_error_on_failure_test() {
  let result = process.Failure("failed", 3)
  case process.get_error(result) {
    Ok(_) -> should.fail()
    Error(msg) -> {
      msg
      |> should.equal("Command failed with code 3: failed")
    }
  }
}

// parse_result tests
pub fn parse_result_success_test() {
  process.parse_result("out", "err", 0)
  |> should.equal(process.Success("out", "err", 0))
}

pub fn parse_result_failure_test() {
  process.parse_result("", "error", 1)
  |> should.equal(process.Failure("error", 1))
}

// run_command_safe tests
pub fn run_command_safe_exists_test() {
  case process.run_command_safe("echo", ["test"], "") {
    Ok(process.Success(stdout, _, 0)) -> {
      stdout
      |> should.equal("test")
    }
    Ok(process.Success(_, _, _)) -> should.fail()
    Ok(process.Failure(_, _)) -> should.fail()
    Error(_) -> should.fail()
  }
}

pub fn run_command_safe_missing_test() {
  case process.run_command_safe("nonexistent_xyz", [], "") {
    Ok(_) -> should.fail()
    Error(msg) -> {
      msg
      |> should.equal("Command not found in PATH: nonexistent_xyz")
    }
  }
}
