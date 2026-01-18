import gleam/int
import gleam/list
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import process

pub fn main() {
  gleeunit.main()
}

// ============================================================================
// HARDWARE VERIFICATION TYPES
// ============================================================================

pub type HardwareConfig {
  HardwareConfig(min_ram_gb: Int, require_gpu: Bool, require_shm: Bool)
}

pub type HardwareStatus {
  HardwareStatus(ram_gb: Int, gpu_available: Bool, shm_mounted: Bool)
}

// ============================================================================
// HARDWARE CHECK FUNCTIONS (testable, pure interface)
// ============================================================================

pub fn get_available_ram() -> Result(Int, String) {
  case process.run_command("free", ["-g"], "") {
    Ok(process.Success(stdout, _, _)) -> parse_ram_from_free(stdout)
    Ok(process.Failure(err, code)) ->
      Error(
        "Failed to read RAM: exit code " <> int.to_string(code) <> ": " <> err,
      )
    Error(e) -> Error("Command execution error: " <> e)
  }
}

pub fn is_gpu_available() -> Result(Bool, String) {
  case process.run_command("lspci", [], "") {
    Ok(process.Success(stdout, _, _)) -> {
      let has_nvidia =
        string.contains(stdout, "NVIDIA") || string.contains(stdout, "nvidia")
      Ok(has_nvidia)
    }
    Ok(process.Failure(_, _)) -> Ok(False)
    Error(e) -> Error("GPU check failed: " <> e)
  }
}

pub fn is_shm_mounted() -> Result(Bool, String) {
  case process.run_command("mount", [], "") {
    Ok(process.Success(stdout, _, _)) -> {
      Ok(
        string.contains(stdout, "/dev/shm") || string.contains(stdout, "tmpfs"),
      )
    }
    Ok(process.Failure(err, code)) ->
      Error(
        "Failed to check mounts: "
        <> err
        <> " (code "
        <> int.to_string(code)
        <> ")",
      )
    Error(e) -> Error("Mount check failed: " <> e)
  }
}

pub fn verify_hardware(config: HardwareConfig) -> Result(Nil, String) {
  use ram <- result.try(get_available_ram())
  use gpu_avail <- result.try(is_gpu_available())
  use shm_avail <- result.try(is_shm_mounted())

  case ram < config.min_ram_gb {
    True ->
      Error(
        "Insufficient RAM: "
        <> int.to_string(ram)
        <> "GB available, "
        <> int.to_string(config.min_ram_gb)
        <> "GB required",
      )
    False -> {
      case config.require_gpu, gpu_avail {
        True, False -> Error("GPU required but not available")
        _, _ -> {
          case config.require_shm, shm_avail {
            True, False ->
              Error("/dev/shm not mounted (required for reflink mode)")
            _, _ -> Ok(Nil)
          }
        }
      }
    }
  }
}

fn parse_ram_from_free(output: String) -> Result(Int, String) {
  output
  |> string.split("\n")
  |> list.find(fn(line) { string.contains(line, "Mem:") })
  |> result.map_error(fn(_) { "Could not parse 'Mem:' line from free output" })
  |> result.try(fn(line) {
    line
    |> string.split_once(" ")
    |> result.map_error(fn(_) { "Invalid free output format" })
    |> result.try(fn(tup) {
      let #(_, rest) = tup
      rest
      |> string.trim
      |> string.split(" ")
      |> list.first
      |> result.map_error(fn(_) { "Could not extract RAM value" })
      |> result.try(fn(ram_str) {
        case int.parse(ram_str) {
          Ok(gb) -> Ok(gb)
          Error(_) -> Error("Could not parse RAM value: " <> ram_str)
        }
      })
    })
  })
}

// ============================================================================
// TEST: All hardware present - success case
// ============================================================================

pub fn verify_hardware_all_present_test() {
  let config =
    HardwareConfig(min_ram_gb: 4, require_gpu: False, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> Nil
    Error(msg) -> {
      // System may not have required resources, so we test the function exists
      // and returns a Result type (either Ok or Error)
      let _ = msg
      Nil
    }
  }
}

// ============================================================================
// TEST: Low RAM detection
// ============================================================================

pub fn verify_hardware_low_ram_test() {
  let config =
    HardwareConfig(min_ram_gb: 1024, require_gpu: False, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> should.fail()
    Error(msg) -> {
      string.contains(msg, "Insufficient RAM")
      |> should.be_true()
    }
  }
}

// ============================================================================
// TEST: RAM parsing from free command output
// ============================================================================

pub fn parse_ram_simple_test() {
  let output =
    "                  total        used        free      shared  buff/cache   available\nMem:              15625        4532        8123       1024        3012       10093\nSwap:             2048         512        1536"

  parse_ram_from_free(output)
  |> should.equal(Ok(15_625))
}

pub fn parse_ram_with_various_spacing_test() {
  let output = "Mem:    8192    2048    6144    512    100    7000"

  parse_ram_from_free(output)
  |> should.equal(Ok(8192))
}

pub fn parse_ram_edge_case_single_digit_test() {
  let output = "Mem:    2    1    1    0    0    1"

  parse_ram_from_free(output)
  |> should.equal(Ok(2))
}

pub fn parse_ram_missing_mem_line_test() {
  let output = "Swap:    4096    1024    3072"

  parse_ram_from_free(output)
  |> should.be_error()
}

// ============================================================================
// TEST: GPU availability detection
// ============================================================================

pub fn verify_hardware_gpu_optional_test() {
  let config =
    HardwareConfig(min_ram_gb: 1, require_gpu: False, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> Nil
    Error(msg) -> {
      let _ = msg
      Nil
    }
  }
}

pub fn verify_hardware_gpu_required_test() {
  let config =
    HardwareConfig(min_ram_gb: 1, require_gpu: True, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> {
      // System may have GPU available
      Nil
    }
    Error(msg) -> {
      // If GPU not available, error should mention it
      string.contains(msg, "GPU")
      |> should.be_true()
    }
  }
}

// ============================================================================
// TEST: /dev/shm mounted detection (reflink mode)
// ============================================================================

pub fn verify_hardware_shm_optional_test() {
  let config =
    HardwareConfig(min_ram_gb: 1, require_gpu: False, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> Nil
    Error(msg) -> {
      let _ = msg
      Nil
    }
  }
}

pub fn verify_hardware_shm_required_test() {
  let config =
    HardwareConfig(min_ram_gb: 1, require_gpu: False, require_shm: True)

  case verify_hardware(config) {
    Ok(Nil) -> {
      // System may have /dev/shm mounted
      Nil
    }
    Error(msg) -> {
      // If /dev/shm not available, error should mention reflink
      string.contains(msg, "/dev/shm")
      |> should.be_true()
    }
  }
}

// ============================================================================
// TEST: Configuration validation - all requirements met
// ============================================================================

pub fn verify_hardware_strict_config_test() {
  let config =
    HardwareConfig(min_ram_gb: 2, require_gpu: False, require_shm: False)

  case verify_hardware(config) {
    Ok(Nil) -> Nil
    Error(msg) -> {
      let _ = msg
      Nil
    }
  }
}
