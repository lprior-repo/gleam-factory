//// Hardware verification module for checking system resources.

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import process

pub type HardwareConfig {
  HardwareConfig(min_ram_gb: Int, require_gpu: Bool, require_shm: Bool)
}

pub type HardwareStatus {
  HardwareStatus(ram_gb: Int, gpu_available: Bool, shm_mounted: Bool)
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
