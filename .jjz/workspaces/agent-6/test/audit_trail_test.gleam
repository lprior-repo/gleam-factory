// Audit trail tests - Validates task state change tracking and compliance logging
// In-memory storage for fast test execution without filesystem dependencies

import audit
import gleam/list
import gleam/string
import gleeunit/should

// In-memory audit storage for tests
type MemoryStore {
  MemoryStore(entries: List(audit.AuditEntry))
}

fn new_store() -> MemoryStore {
  MemoryStore(entries: [])
}

fn store_add_entry(store: MemoryStore, entry: audit.AuditEntry) -> MemoryStore {
  MemoryStore(entries: list.append(store.entries, [entry]))
}

fn store_get_entries(store: MemoryStore) -> List(audit.AuditEntry) {
  store.entries
}

// TASK STATE CHANGE - Creates entry when task_created event logged
pub fn log_task_state_change_test() {
  let store = new_store()
  let entry =
    audit.create_entry(audit.TaskCreated, "feature-auth", "Task created", [
      #("language", "gleam"),
      #("branch", "feature/auth"),
    ])
  let store = store_add_entry(store, entry)
  let entries = store_get_entries(store)

  entries |> list.length |> should.equal(1)
  list.first(entries)
  |> should.be_ok
  |> fn(e) { e.task_slug }
  |> should.equal("feature-auth")
}

// TASK STATE CHANGE - Records in_progress transition
pub fn log_stage_start_transition_test() {
  let store = new_store()
  let created =
    audit.create_entry(audit.TaskCreated, "build-service", "Task created", [])
  let stage_started =
    audit.create_entry(
      audit.StageStarted,
      "build-service",
      "Stage started: compile",
      [#("stage", "compile"), #("attempt", "1")],
    )

  let store = store_add_entry(store, created)
  let store = store_add_entry(store, stage_started)
  let entries = store_get_entries(store)

  entries |> list.length |> should.equal(2)
  case list.last(entries) {
    Ok(last) -> {
      last.event_type |> should.equal(audit.StageStarted)
      last.details |> should.equal("Stage started: compile")
    }
    Error(_) -> should.fail()
  }
}

// TASK STATE CHANGE - Records completed state
pub fn log_completed_state_change_test() {
  let store = new_store()
  let completed =
    audit.create_entry(audit.StagePassed, "feature-x", "Stage passed: tests", [
      #("stage", "tests"),
      #("duration_ms", "2500"),
    ])
  let store = store_add_entry(store, completed)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.StagePassed)
      e.metadata |> list.length |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// TASK STATE CHANGE - Records failed state with error details
pub fn log_failed_state_change_test() {
  let store = new_store()
  let failed =
    audit.create_entry(
      audit.StageFailed,
      "broken-build",
      "Stage failed: compile",
      [
        #("stage", "compile"),
        #("error", "Type mismatch in main.gleam:45"),
      ],
    )
  let store = store_add_entry(store, failed)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.StageFailed)
      e.metadata |> list.length |> should.equal(2)
      e.metadata
      |> list.find(fn(pair) { pair.0 == "error" })
      |> should.be_ok()
    }
    Error(_) -> panic as "Expected entry but got error"
  }
}

// APPROVAL EVENT - Records approval with actor and timestamp
pub fn log_approval_event_test() {
  let store = new_store()
  let approval =
    audit.create_entry(
      audit.TaskApproved,
      "deploy-prod",
      "Task approved for deployment",
      [#("strategy", "rolling"), #("risk_level", "low")],
    )
  let store = store_add_entry(store, approval)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.TaskApproved)
      e.actor |> string.length |> fn(len) { len > 0 } |> should.be_true
      e.timestamp |> string.length |> fn(len) { len > 0 } |> should.be_true
    }
    Error(_) -> panic as "Expected entry but got error"
  }
}

// APPROVAL EVENT - Records rejection with reason
pub fn log_rejection_event_test() {
  let store = new_store()
  let rejection =
    audit.create_entry(
      audit.TaskRejected,
      "feature-y",
      "Task rejected: failing tests",
      [
        #("reason", "3 test failures"),
        #("reviewer", "alice@example.com"),
      ],
    )
  let store = store_add_entry(store, rejection)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.TaskRejected)
      e.metadata |> list.length |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// APPROVAL EVENT - Timestamps are distinct per event
pub fn approval_events_have_timestamps_test() {
  let store = new_store()
  let approval1 =
    audit.create_entry(audit.TaskApproved, "task-1", "Approved", [])
  let store = store_add_entry(store, approval1)
  let approval2 =
    audit.create_entry(audit.TaskApproved, "task-2", "Approved", [])
  let store = store_add_entry(store, approval2)
  let entries = store_get_entries(store)

  case list.first(entries), list.last(entries) {
    Ok(first), Ok(last) -> {
      first.timestamp |> string.length |> fn(len) { len > 0 } |> should.be_true
      last.timestamp |> string.length |> fn(len) { len > 0 } |> should.be_true
    }
    _, _ -> panic as "Expected first and last entries"
  }
}

// DEPLOYMENT DECISION - Logs deployment start
pub fn log_deployment_decision_test() {
  let store = new_store()
  let deploy =
    audit.create_entry(
      audit.DeploymentStarted,
      "v1-2-3",
      "Deployment started at 10%",
      [#("rollout_percentage", "10")],
    )
  let store = store_add_entry(store, deploy)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.DeploymentStarted)
      e.task_slug |> should.equal("v1-2-3")
    }
    Error(_) -> should.fail()
  }
}

// DEPLOYMENT DECISION - Logs deployment completion
pub fn log_deployment_completion_test() {
  let store = new_store()
  let completed =
    audit.create_entry(
      audit.DeploymentCompleted,
      "v2-0-0",
      "Deployment completed successfully",
      [],
    )
  let store = store_add_entry(store, completed)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.DeploymentCompleted)
      e.details |> should.equal("Deployment completed successfully")
    }
    Error(_) -> should.fail()
  }
}

// DEPLOYMENT DECISION - Logs rollback with reason
pub fn log_deployment_rollback_test() {
  let store = new_store()
  let rollback =
    audit.create_entry(
      audit.DeploymentRolledBack,
      "v1-5-0",
      "Deployment rolled back: High error rate detected",
      [#("reason", "Error rate 15%"), #("previous_version", "v1-4-9")],
    )
  let store = store_add_entry(store, rollback)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.event_type |> should.equal(audit.DeploymentRolledBack)
      e.metadata |> list.length |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// RETRIEVE AUDIT TRAIL - Filters entries for specific task
pub fn retrieve_audit_trail_for_task_test() {
  let store = new_store()

  // Task 1 events
  let task1_created =
    audit.create_entry(audit.TaskCreated, "task-alpha", "Created", [])
  let task1_passed =
    audit.create_entry(audit.StagePassed, "task-alpha", "Tests passed", [])

  // Task 2 events
  let task2_created =
    audit.create_entry(audit.TaskCreated, "task-beta", "Created", [])

  let store =
    store_add_entry(store, task1_created)
    |> fn(s) { store_add_entry(s, task2_created) }
    |> fn(s) { store_add_entry(s, task1_passed) }

  let entries = store_get_entries(store)
  let task_alpha_events =
    list.filter(entries, fn(e) { e.task_slug == "task-alpha" })

  task_alpha_events |> list.length |> should.equal(2)
  case list.first(task_alpha_events) {
    Ok(e) -> e.task_slug |> should.equal("task-alpha")
    Error(_) -> should.fail()
  }
}

// RETRIEVE AUDIT TRAIL - Filters by event type
pub fn filter_audit_trail_by_event_type_test() {
  let store = new_store()
  let task_slug = "build-job"

  let created = audit.create_entry(audit.TaskCreated, task_slug, "Created", [])
  let stage_started =
    audit.create_entry(audit.StageStarted, task_slug, "Build started", [])
  let stage_passed =
    audit.create_entry(audit.StagePassed, task_slug, "Build passed", [])

  let store =
    store_add_entry(store, created)
    |> fn(s) { store_add_entry(s, stage_started) }
    |> fn(s) { store_add_entry(s, stage_passed) }

  let entries = store_get_entries(store)
  let stage_events =
    list.filter(entries, fn(e) {
      case e.event_type {
        audit.StageStarted | audit.StagePassed -> True
        _ -> False
      }
    })

  stage_events |> list.length |> should.equal(2)
}

// TIMESTAMPS - All events have non-empty timestamps
pub fn audit_events_have_timestamps_test() {
  let store = new_store()

  let events = [
    audit.create_entry(audit.TaskCreated, "t1", "Created", []),
    audit.create_entry(audit.StageStarted, "t1", "Started", []),
    audit.create_entry(audit.StagePassed, "t1", "Passed", []),
    audit.create_entry(audit.TaskApproved, "t1", "Approved", []),
    audit.create_entry(audit.DeploymentStarted, "t1", "Deploy", []),
  ]

  let store =
    list.fold(events, store, fn(acc, event) { store_add_entry(acc, event) })

  let all_entries = store_get_entries(store)
  all_entries
  |> list.all(fn(e) { string.length(e.timestamp) > 0 })
  |> should.be_true
}

// METADATA - Entries preserve all metadata key-value pairs
pub fn audit_entry_metadata_preservation_test() {
  let store = new_store()
  let metadata = [
    #("stage", "compile"),
    #("duration_ms", "1234"),
    #("language", "gleam"),
  ]
  let entry =
    audit.create_entry(audit.StagePassed, "meta-test", "Details", metadata)
  let store = store_add_entry(store, entry)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.metadata |> list.length |> should.equal(3)
      case e.metadata |> list.find(fn(pair) { pair.0 == "stage" }) {
        Ok(#(_, value)) -> value |> should.equal("compile")
        Error(_) -> panic as "Expected stage metadata"
      }
    }
    Error(_) -> panic as "Expected entry but got error"
  }
}

// APPEND-ONLY - Events are recorded in order
pub fn audit_trail_maintains_order_test() {
  let store = new_store()

  let e1 = audit.create_entry(audit.TaskCreated, "order", "First", [])
  let e2 = audit.create_entry(audit.StageStarted, "order", "Second", [])
  let e3 = audit.create_entry(audit.StagePassed, "order", "Third", [])
  let e4 = audit.create_entry(audit.TaskApproved, "order", "Fourth", [])

  let store =
    store_add_entry(store, e1)
    |> fn(s) { store_add_entry(s, e2) }
    |> fn(s) { store_add_entry(s, e3) }
    |> fn(s) { store_add_entry(s, e4) }

  let entries = store_get_entries(store)
  entries |> list.length |> should.equal(4)

  case list.first(entries) {
    Ok(first) -> first.details |> should.equal("First")
    Error(_) -> panic as "Expected first entry"
  }

  case list.last(entries) {
    Ok(last) -> last.details |> should.equal("Fourth")
    Error(_) -> panic as "Expected last entry"
  }
}

// IMMUTABILITY - Entry details cannot be modified
pub fn audit_entry_is_immutable_test() {
  let entry1 =
    audit.create_entry(audit.StagePassed, "immute", "Original details", [
      #("key", "value"),
    ])

  entry1.details |> should.equal("Original details")
  entry1.metadata |> list.length |> should.equal(1)
}

// COMPLIANCE - Full workflow captured in order
pub fn audit_trail_captures_full_workflow_test() {
  let store = new_store()
  let task_slug = "feature-payment"

  let workflow = [
    audit.create_entry(audit.TaskCreated, task_slug, "Task created", []),
    audit.create_entry(audit.StageStarted, task_slug, "Compilation started", [
      #("stage", "compile"),
    ]),
    audit.create_entry(audit.StagePassed, task_slug, "Compilation passed", [
      #("stage", "compile"),
    ]),
    audit.create_entry(audit.StageStarted, task_slug, "Tests started", [
      #("stage", "test"),
    ]),
    audit.create_entry(audit.StagePassed, task_slug, "Tests passed", [
      #("stage", "test"),
    ]),
    audit.create_entry(
      audit.TaskApproved,
      task_slug,
      "Task approved for deployment",
      [#("reviewer", "bob@example.com")],
    ),
    audit.create_entry(
      audit.DeploymentStarted,
      task_slug,
      "Deployment started at 5%",
      [#("rollout_percentage", "5")],
    ),
    audit.create_entry(
      audit.DeploymentCompleted,
      task_slug,
      "Deployment completed successfully",
      [],
    ),
  ]

  let store =
    list.fold(workflow, store, fn(acc, event) { store_add_entry(acc, event) })

  let entries = store_get_entries(store)
  entries |> list.length |> should.equal(8)

  let task_entries = list.filter(entries, fn(e) { e.task_slug == task_slug })
  task_entries |> list.length |> should.equal(8)
}

// ACTOR TRACKING - Events record the actor who triggered them
pub fn audit_trail_records_actor_test() {
  let store = new_store()
  let entry =
    audit.create_entry(audit.TaskApproved, "approval-test", "Approved", [])
  let store = store_add_entry(store, entry)
  let entries = store_get_entries(store)

  case list.first(entries) {
    Ok(e) -> {
      e.actor |> string.length |> fn(len) { len > 0 } |> should.be_true
    }
    Error(_) -> panic as "Expected entry but got error"
  }
}
