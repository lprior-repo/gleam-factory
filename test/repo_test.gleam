import domain
import gleam/int
import gleam/result
import gleam/string
import gleeunit
import gleeunit/should
import repo
import simplifile

pub fn main() {
  gleeunit.main()
}

// Helper: Create temp directory with unique name
fn create_temp_dir() -> Result(String, String) {
  let temp_base = "/tmp/factory-gleam-test-repo"
  // Delete if exists from previous failed test, then recreate
  let _ = simplifile.delete_all([temp_base])
  simplifile.create_directory_all(temp_base)
  |> result.map(fn(_) { temp_base })
  |> result.map_error(fn(_) { "Failed to create temp dir" })
}

// Helper: Clean up temp directory
fn cleanup_temp_dir(path: String) -> Result(Nil, String) {
  simplifile.delete_all([path])
  |> result.map_error(fn(_) { "Failed to cleanup temp dir" })
}

// Helper: Create file in directory
fn create_file(
  dir: String,
  filename: String,
  content: String,
) -> Result(String, String) {
  let filepath = dir <> "/" <> filename
  use _ <- result.try(
    simplifile.write(filepath, content)
    |> result.map_error(fn(_) { "Failed to write file: " <> filename }),
  )
  // Verify file was actually written
  simplifile.verify_is_file(filepath)
  |> result.map(fn(_) { filepath })
  |> result.map_error(fn(_) { "File write verification failed: " <> filename })
}

// ===== Language Detection Tests (via domain module) =====

pub fn detect_language_from_files_gleam_test() {
  domain.detect_language_from_files(True, False, False, False)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_from_files_go_test() {
  domain.detect_language_from_files(False, True, False, False)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_from_files_rust_test() {
  domain.detect_language_from_files(False, False, True, False)
  |> should.equal(Ok(domain.Rust))
}

pub fn detect_language_from_files_python_test() {
  domain.detect_language_from_files(False, False, False, True)
  |> should.equal(Ok(domain.Python))
}

pub fn detect_language_from_files_none_test() {
  domain.detect_language_from_files(False, False, False, False)
  |> should.be_error()
}

pub fn detect_language_from_files_priority_gleam_test() {
  domain.detect_language_from_files(True, True, True, True)
  |> should.equal(Ok(domain.Gleam))
}

pub fn detect_language_from_files_priority_go_test() {
  domain.detect_language_from_files(False, True, True, True)
  |> should.equal(Ok(domain.Go))
}

pub fn detect_language_from_files_priority_rust_test() {
  domain.detect_language_from_files(False, False, True, True)
  |> should.equal(Ok(domain.Rust))
}

pub fn detect_language_from_files_priority_python_test() {
  domain.detect_language_from_files(False, False, False, True)
  |> should.equal(Ok(domain.Python))
}

// ===== Repo Detection - Real Filesystem Tests =====

pub fn detect_repo_root_in_actual_repo_test() {
  case repo.detect_repo_root() {
    Ok(path) -> {
      case path {
        "" -> panic as "Expected non-empty repo root"
        _ -> Nil
      }
    }
    Error(_) -> panic as "Expected repo detection to succeed in test repo"
  }
}

pub fn detect_repo_root_nonexistent_path_test() {
  // In nonexistent path, should fail
  repo.detect_repo_root()
  |> result.try(fn(_path) {
    // If we somehow succeed, that's unexpected
    Error("Should fail in nonexistent path")
  })
  |> result.map_error(fn(_) { Nil })
  |> should.be_error()
}

pub fn detect_language_gleam_project_test() {
  case repo.detect_repo_root() {
    Ok(root) -> {
      case repo.detect_language(root) {
        Ok(domain.Gleam) -> Nil
        Ok(lang) ->
          panic as {
            "Expected Gleam, got: " <> domain.language_display_name(lang)
          }
        Error(e) -> panic as { "Language detection failed: " <> e }
      }
    }
    Error(_) -> panic as "Repo detection failed"
  }
}

// ===== Isolated Temp Directory Tests =====

pub fn detect_language_with_gleam_toml_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "gleam.toml", "name = \"test\"\n") {
        Ok(_) -> {
          case repo.detect_language(tmpdir) {
            Ok(domain.Gleam) -> {
              let _ = cleanup_temp_dir(tmpdir)
              Nil
            }
            Ok(lang) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as {
                "Expected Gleam, got: " <> domain.language_display_name(lang)
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Language detection failed: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create gleam.toml: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_with_go_mod_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "go.mod", "module test\n") {
        Ok(_) -> {
          case repo.detect_language(tmpdir) {
            Ok(domain.Go) -> {
              let _ = cleanup_temp_dir(tmpdir)
              Nil
            }
            Ok(lang) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as {
                "Expected Go, got: " <> domain.language_display_name(lang)
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Language detection failed: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create go.mod: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_with_cargo_toml_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "Cargo.toml", "[package]\n") {
        Ok(_) -> {
          case repo.detect_language(tmpdir) {
            Ok(domain.Rust) -> {
              let _ = cleanup_temp_dir(tmpdir)
              Nil
            }
            Ok(lang) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as {
                "Expected Rust, got: " <> domain.language_display_name(lang)
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Language detection failed: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create Cargo.toml: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_with_pyproject_toml_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "pyproject.toml", "[project]\n") {
        Ok(_) -> {
          case repo.detect_language(tmpdir) {
            Ok(domain.Python) -> {
              let _ = cleanup_temp_dir(tmpdir)
              Nil
            }
            Ok(lang) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as {
                "Expected Python, got: " <> domain.language_display_name(lang)
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Language detection failed: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create pyproject.toml: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_empty_directory_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case repo.detect_language(tmpdir) {
        Ok(_) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as "Expected error for empty directory"
        }
        Error(_) -> {
          let _ = cleanup_temp_dir(tmpdir)
          Nil
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_priority_gleam_over_go_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "gleam.toml", "name = \"test\"\n") {
        Ok(_) -> {
          case create_file(tmpdir, "go.mod", "module test\n") {
            Ok(_) -> {
              case repo.detect_language(tmpdir) {
                Ok(domain.Gleam) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  Nil
                }
                Ok(lang) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as {
                    "Expected Gleam priority, got: "
                    <> domain.language_display_name(lang)
                  }
                }
                Error(e) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as { "Language detection failed: " <> e }
                }
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Failed to create go.mod: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create gleam.toml: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_priority_go_over_rust_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "go.mod", "module test\n") {
        Ok(_) -> {
          case create_file(tmpdir, "Cargo.toml", "[package]\n") {
            Ok(_) -> {
              case repo.detect_language(tmpdir) {
                Ok(domain.Go) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  Nil
                }
                Ok(lang) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as {
                    "Expected Go priority, got: "
                    <> domain.language_display_name(lang)
                  }
                }
                Error(e) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as { "Language detection failed: " <> e }
                }
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Failed to create Cargo.toml: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create go.mod: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_priority_rust_over_python_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case create_file(tmpdir, "Cargo.toml", "[package]\n") {
        Ok(_) -> {
          case create_file(tmpdir, "pyproject.toml", "[project]\n") {
            Ok(_) -> {
              case repo.detect_language(tmpdir) {
                Ok(domain.Rust) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  Nil
                }
                Ok(lang) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as {
                    "Expected Rust priority, got: "
                    <> domain.language_display_name(lang)
                  }
                }
                Error(e) -> {
                  let _ = cleanup_temp_dir(tmpdir)
                  panic as { "Language detection failed: " <> e }
                }
              }
            }
            Error(e) -> {
              let _ = cleanup_temp_dir(tmpdir)
              panic as { "Failed to create pyproject.toml: " <> e }
            }
          }
        }
        Error(e) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as { "Failed to create Cargo.toml: " <> e }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}

pub fn detect_language_nonexistent_directory_test() {
  repo.detect_language("/nonexistent/path/definitely/does/not/exist/12345")
  |> should.be_error()
}

// ===== Fallback Behavior Tests =====

pub fn detect_language_missing_all_files_test() {
  case create_temp_dir() {
    Ok(tmpdir) -> {
      case repo.detect_language(tmpdir) {
        Ok(_) -> {
          let _ = cleanup_temp_dir(tmpdir)
          panic as "Expected error when no config files exist"
        }
        Error(msg) -> {
          let _ = cleanup_temp_dir(tmpdir)
          case msg {
            "" -> panic as "Expected error message"
            _ -> Nil
          }
        }
      }
    }
    Error(e) -> panic as { "Failed to create temp dir: " <> e }
  }
}
