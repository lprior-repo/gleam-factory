import gleeunit/should
import resource_governor

// === ResourceLimits Construction ===

pub fn resource_limits_captures_max_mutators_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 4,
    max_loops: 2,
    max_workspaces: 10,
    min_free_ram_mb: 1024,
    gpu_tickets: 1,
  )
  case limits {
    resource_governor.ResourceLimits(max_mutators: max_mutators, ..) ->
      max_mutators |> should.equal(4)
  }
}

pub fn resource_limits_captures_max_loops_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 4,
    max_loops: 8,
    max_workspaces: 10,
    min_free_ram_mb: 1024,
    gpu_tickets: 1,
  )
  case limits {
    resource_governor.ResourceLimits(max_loops: max_loops, ..) ->
      max_loops |> should.equal(8)
  }
}

pub fn resource_limits_captures_max_workspaces_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 4,
    max_loops: 2,
    max_workspaces: 20,
    min_free_ram_mb: 1024,
    gpu_tickets: 1,
  )
  case limits {
    resource_governor.ResourceLimits(max_workspaces: max_workspaces, ..) ->
      max_workspaces |> should.equal(20)
  }
}

pub fn resource_limits_captures_min_free_ram_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 4,
    max_loops: 2,
    max_workspaces: 10,
    min_free_ram_mb: 2048,
    gpu_tickets: 1,
  )
  case limits {
    resource_governor.ResourceLimits(min_free_ram_mb: min_free_ram_mb, ..) ->
      min_free_ram_mb |> should.equal(2048)
  }
}

pub fn resource_limits_captures_gpu_tickets_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 4,
    max_loops: 2,
    max_workspaces: 10,
    min_free_ram_mb: 1024,
    gpu_tickets: 3,
  )
  case limits {
    resource_governor.ResourceLimits(gpu_tickets: gpu_tickets, ..) ->
      gpu_tickets |> should.equal(3)
  }
}

// === Ticket Types ===

pub fn ticket_mutator_ticket_exists_test() {
  let ticket = resource_governor.MutatorTicket
  case ticket {
    resource_governor.MutatorTicket -> should.be_true(True)
  }
}

pub fn ticket_loop_ticket_exists_test() {
  let ticket = resource_governor.LoopTicket
  case ticket {
    resource_governor.LoopTicket -> should.be_true(True)
  }
}

// === SlotType Types ===

pub fn slot_type_mutator_slot_exists_test() {
  let slot = resource_governor.MutatorSlot
  case slot {
    resource_governor.MutatorSlot -> should.be_true(True)
  }
}

pub fn slot_type_loop_slot_exists_test() {
  let slot = resource_governor.LoopSlot
  case slot {
    resource_governor.LoopSlot -> should.be_true(True)
  }
}

pub fn slot_type_workspace_slot_exists_test() {
  let slot = resource_governor.WorkspaceSlot
  case slot {
    resource_governor.WorkspaceSlot -> should.be_true(True)
  }
}

pub fn slot_type_gpu_slot_exists_test() {
  let slot = resource_governor.GpuSlot
  case slot {
    resource_governor.GpuSlot -> should.be_true(True)
  }
}

// === SlotId Type ===

pub fn slot_id_captures_id_string_test() {
  let slot_id = resource_governor.SlotId("mutator:1")
  case slot_id {
    resource_governor.SlotId(id: id) -> id |> should.equal("mutator:1")
  }
}

pub fn slot_id_can_have_loop_prefix_test() {
  let slot_id = resource_governor.SlotId("loop:42")
  case slot_id {
    resource_governor.SlotId(id: id) -> id |> should.equal("loop:42")
  }
}

// === ResourceError Types ===

pub fn resource_error_exhausted_exists_test() {
  let err = resource_governor.ResourceExhausted
  case err {
    resource_governor.ResourceExhausted -> should.be_true(True)
  }
}

pub fn resource_error_insufficient_ram_exists_test() {
  let err = resource_governor.InsufficientRam
  case err {
    resource_governor.InsufficientRam -> should.be_true(True)
  }
}

pub fn resource_error_timeout_exists_test() {
  let err = resource_governor.Timeout
  case err {
    resource_governor.Timeout -> should.be_true(True)
  }
}

// === RAM Sufficiency Check ===

pub fn is_sufficient_ram_returns_true_when_above_minimum_test() {
  resource_governor.is_sufficient_ram(2048, 1024)
  |> should.be_true
}

pub fn is_sufficient_ram_returns_true_when_equal_to_minimum_test() {
  resource_governor.is_sufficient_ram(1024, 1024)
  |> should.be_true
}

pub fn is_sufficient_ram_returns_false_when_below_minimum_test() {
  resource_governor.is_sufficient_ram(512, 1024)
  |> should.be_false
}

pub fn is_sufficient_ram_handles_zero_current_test() {
  resource_governor.is_sufficient_ram(0, 1024)
  |> should.be_false
}

pub fn is_sufficient_ram_handles_zero_required_test() {
  resource_governor.is_sufficient_ram(1024, 0)
  |> should.be_true
}

pub fn is_sufficient_ram_handles_large_values_test() {
  resource_governor.is_sufficient_ram(65536, 32768)
  |> should.be_true
}

// === OTP Actor Behavior (Integration Tests) ===

pub fn start_link_creates_actor_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      resource_governor.shutdown(gov)
      should.be_true(True)
    }
    Error(_) -> should.fail()
  }
}

pub fn acquire_mutator_succeeds_when_under_limit_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      case resource_governor.acquire_mutator(gov) {
        Ok(#(resource_governor.MutatorTicket, _slot_id)) -> {
          resource_governor.shutdown(gov)
          should.be_true(True)
        }
        _ -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn acquire_mutator_fails_when_at_limit_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 1,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      // Acquire first (should succeed)
      let _ = resource_governor.acquire_mutator(gov)
      // Acquire second (should fail - limit is 1)
      case resource_governor.acquire_mutator(gov) {
        Error("mutator limit") -> {
          resource_governor.shutdown(gov)
          should.be_true(True)
        }
        _ -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn acquire_loop_succeeds_when_under_limit_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      case resource_governor.acquire_loop(gov) {
        Ok(#(resource_governor.LoopTicket, _slot_id)) -> {
          resource_governor.shutdown(gov)
          should.be_true(True)
        }
        _ -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn release_allows_new_acquisition_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 1,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      // Acquire first
      case resource_governor.acquire_mutator(gov) {
        Ok(#(ticket, _)) -> {
          // Release it
          resource_governor.release(gov, ticket)
          // Should be able to acquire again
          case resource_governor.acquire_mutator(gov) {
            Ok(_) -> {
              resource_governor.shutdown(gov)
              should.be_true(True)
            }
            Error(_) -> {
              resource_governor.shutdown(gov)
              should.fail()
            }
          }
        }
        Error(_) -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn release_slot_succeeds_for_valid_slot_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      case resource_governor.acquire_mutator(gov) {
        Ok(#(_, slot_id)) -> {
          case resource_governor.release_slot(gov, slot_id) {
            Ok(Nil) -> {
              resource_governor.shutdown(gov)
              should.be_true(True)
            }
            Error(_) -> {
              resource_governor.shutdown(gov)
              should.fail()
            }
          }
        }
        Error(_) -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}

pub fn release_slot_succeeds_for_unknown_slot_test() {
  let limits = resource_governor.ResourceLimits(
    max_mutators: 2,
    max_loops: 2,
    max_workspaces: 5,
    min_free_ram_mb: 100,
    gpu_tickets: 1,
  )
  case resource_governor.start_link(limits) {
    Ok(gov) -> {
      // Releasing unknown slot should succeed (idempotent)
      let unknown_slot = resource_governor.SlotId("unknown:999")
      case resource_governor.release_slot(gov, unknown_slot) {
        Ok(Nil) -> {
          resource_governor.shutdown(gov)
          should.be_true(True)
        }
        Error(_) -> {
          resource_governor.shutdown(gov)
          should.fail()
        }
      }
    }
    Error(_) -> should.fail()
  }
}
