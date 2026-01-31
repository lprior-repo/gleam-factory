package specs

// Inline schema definition for validation
#ValidBead: {
    id: string
    title: string
    type: "feature" | "bug" | "task" | "epic" | "chore"
    priority: 0 | 1 | 2 | 3 | 4
    effort_estimate: "15min" | "30min" | "1hr" | "2hr" | "4hr"
    labels: [...string]
    ears_requirements: _
    contracts: _
    inversions: _
    acceptance_tests: _
    e2e_tests: _
    implementation_tasks: _
    failure_modes: _
    completion_checklist: _
    context: _
    ai_hints: _
}

"factory-ilq": #ValidBead & {
    // ============================================================================
    // BEAD: factory-ilq - Document Architecture
    // ============================================================================

    id:              "factory-ilq"
    title:           "Document architecture, FP patterns, and JJ integration in code"
    type:            "task"
    priority:        3
    effort_estimate: "2hr"
    labels:          ["documentation", "architecture", "rustdoc", "P3"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL have module-level documentation (//!) for every public module",
            "THE SYSTEM SHALL document all Railway-Oriented Programming patterns with examples",
            "THE SYSTEM SHALL include architecture decision records in doc comments",
            "THE SYSTEM SHALL provide runnable examples in rustdoc (doctest) format",
        ]

        event_driven: [
            {
                trigger: "WHEN a developer opens any source file"
                shall:   "THE SYSTEM SHALL present a module-level doc explaining the module's purpose"
            },
            {
                trigger: "WHEN a developer runs cargo doc"
                shall:   "THE SYSTEM SHALL generate comprehensive HTML documentation with no warnings"
            },
            {
                trigger: "WHEN a developer looks up a public function"
                shall:   "THE SYSTEM SHALL show usage examples in the doc comment"
            },
            {
                trigger: "WHEN a developer wants to add a new language stage"
                shall:   "THE SYSTEM SHALL have a HOWTO guide in the stages module documentation"
            },
        ]

        state_driven: [
            {
                state: "WHILE the crate is being documented"
                shall: "THE SYSTEM SHALL compile all doc examples successfully"
            },
            {
                state: "WHILE a developer reads the architecture docs"
                shall: "THE SYSTEM SHALL explain design decisions with rationale"
            },
        ]

        unwanted: [
            {
                condition: "IF documentation contains outdated examples"
                shall_not: "THE SYSTEM SHALL NOT have doc examples that fail to compile"
                because:   "Broken examples mislead developers and erode trust in documentation"
            },
            {
                condition: "IF a public API lacks documentation"
                shall_not: "THE SYSTEM SHALL NOT have undocumented public items (warn(missing_docs))"
                because:   "Undocumented APIs create knowledge silos and increase onboarding time"
            },
            {
                condition: "IF architecture docs are only in markdown files"
                shall_not: "THE SYSTEM SHALL NOT separate architecture docs from code"
                because:   "External docs drift from code; inline docs stay synchronized"
            },
        ]

        complex: [
            {
                state:   "WHILE the project evolves"
                trigger: "WHEN a new pattern is introduced"
                shall:   "THE SYSTEM SHALL require pattern documentation before merge"
            },
        ]
    }

    // ============================================================================
    // SECTION 2: KIRK CONTRACTS
    // ============================================================================

    contracts: {
        preconditions: {
            auth_required: false
            required_inputs: [
                {
                    field:           "Existing codebase"
                    type:            "Directory"
                    constraints:     "Must have factory-core and factory crates"
                    example_valid:   "crates/factory-core/src/lib.rs exists"
                    example_invalid: "Empty crates/ directory"
                },
                {
                    field:           "Implemented features"
                    type:            "Code"
                    constraints:     "Dependencies factory-khq, factory-8cb, factory-2ar complete"
                    example_valid:   "Actor system, JJ integration, stage runners implemented"
                    example_invalid: "Stub implementations only"
                },
            ]
            system_state: [
                "Rust toolchain installed (rustc, cargo, rustdoc)",
                "Moon build system configured",
                "factory-core and factory crates buildable",
                "All dependent beads (factory-khq, factory-8cb, factory-2ar) complete",
            ]
        }

        postconditions: {
            state_changes: [
                "crates/factory-core/src/lib.rs has comprehensive module-level docs",
                "crates/factory-core/src/actor.rs has architecture documentation",
                "crates/factory-core/src/domain.rs has FP pattern documentation",
                "crates/factory/src/lib.rs has JJ integration documentation",
                "All public items have doc comments",
            ]
            return_guarantees: [
                {
                    field:     "cargo doc"
                    guarantee: "Generates HTML docs with zero warnings"
                },
                {
                    field:     "cargo test --doc"
                    guarantee: "All doctest examples pass"
                },
                {
                    field:     "rustdoc coverage"
                    guarantee: "100% of public items documented"
                },
            ]
            side_effects: [
                "Generated docs in target/doc/",
            ]
        }

        invariants: [
            "All public modules have //! module-level documentation",
            "All public functions have /// doc comments with examples",
            "All doc examples compile and pass as doctests",
            "Architecture decisions are documented where implemented",
            "Pattern documentation includes rationale and alternatives",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Doc examples use outdated API after refactor"
                prevention:  "Run cargo test --doc in CI pipeline"
                test_for_it: "test_all_doctests_pass"
            },
            {
                failure:     "Missing docs warning breaks CI"
                prevention:  "Add #![warn(missing_docs)] incrementally, fix as you go"
                test_for_it: "test_no_missing_docs_warnings"
            },
            {
                failure:     "Doc comments reference non-existent types"
                prevention:  "Use intra-doc links [`Type`] that cargo doc validates"
                test_for_it: "test_intra_doc_links_resolve"
            },
        ]

        usability_failures: [
            {
                failure:     "Architecture docs too abstract, no concrete examples"
                prevention:  "Every pattern doc must include real code from the crate"
                test_for_it: "review_docs_have_examples"
            },
            {
                failure:     "New contributor can't find how to add a language"
                prevention:  "Add HOWTO section in stages module with step-by-step guide"
                test_for_it: "review_howto_completeness"
            },
            {
                failure:     "JJ integration docs assume JJ knowledge"
                prevention:  "Link to JJ docs, explain jj-specific concepts inline"
                test_for_it: "review_jj_docs_self_contained"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Doc examples show patterns we don't actually use"
                prevention:  "Doc examples must be extracted from actual code"
                test_for_it: "review_examples_match_implementation"
            },
            {
                failure:     "Architecture diagrams become stale"
                prevention:  "Use text-based diagrams (ASCII art) in doc comments"
                test_for_it: "review_diagrams_current"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_lib_rs_has_crate_level_docs"
                given: "The factory-core crate"
                when:  "Opening crates/factory-core/src/lib.rs"
                then: [
                    "File starts with //! crate-level documentation",
                    "Docs explain the crate's purpose",
                    "Docs list main modules with brief descriptions",
                    "Docs include architecture overview",
                ]
                real_input: """
                    cat crates/factory-core/src/lib.rs | head -50
                    """
                expected_output: """
                    //! # Factory Core
                    //!
                    //! Core library for the Factory task automation system.
                    //!
                    //! ## Architecture Overview
                    //!
                    //! Factory uses a Railway-Oriented Programming (ROP) approach...
                    """
            },
            {
                name:  "test_actor_module_has_architecture_docs"
                given: "The actor module"
                when:  "Opening crates/factory-core/src/actor.rs"
                then: [
                    "Module has //! documentation",
                    "Docs explain actor model design",
                    "Docs reference BEAM/OTP inspiration",
                    "Docs include supervision tree diagram",
                ]
                real_input: """
                    cargo doc --open
                    # Navigate to factory_core::actor
                    """
                expected_output: """
                    //! # Actor System
                    //!
                    //! Factory implements an Erlang/OTP-inspired actor system using ractor.
                    //!
                    //! ## Supervision Tree
                    //!
                    //! ```text
                    //!            FactorySupervisor
                    //!           /        |        \
                    //!    TaskActor  StageActor  JjActor
                    //! ```
                    """
            },
            {
                name:  "test_domain_module_has_fp_pattern_docs"
                given: "The domain module"
                when:  "Opening crates/factory-core/src/domain.rs"
                then: [
                    "Docs explain Railway-Oriented Programming",
                    "Docs show Result chaining patterns",
                    "Docs explain newtype validation pattern",
                    "Docs include before/after comparisons",
                ]
                real_input: """
                    // In domain.rs
                    """
                expected_output: """
                    //! # Domain Types with Railway-Oriented Programming
                    //!
                    //! This module demonstrates functional programming patterns used throughout Factory.
                    //!
                    //! ## Railway-Oriented Programming (ROP)
                    //!
                    //! Instead of imperative error checking:
                    //!
                    //! ```rust,ignore
                    //! // BAD: Imperative style
                    //! let slug = match Slug::new(input) {
                    //!     Ok(s) => s,
                    //!     Err(e) => return Err(e),
                    //! };
                    //! ```
                    //!
                    //! We use functional composition:
                    //!
                    //! ```rust
                    //! use factory_core::domain::Slug;
                    //! use factory_core::error::Result;
                    //!
                    //! fn process(input: &str) -> Result<String> {
                    //!     Slug::new(input)
                    //!         .map(|s| s.as_str().to_uppercase())
                    //! }
                    //! ```
                    """
            },
            {
                name:  "test_jj_module_has_integration_docs"
                given: "The JJ integration module"
                when:  "Opening the jj module"
                then: [
                    "Docs explain JJ workspace strategy",
                    "Docs show workspace lifecycle",
                    "Docs link to JJ documentation",
                    "Docs explain why JJ over git branches",
                ]
                real_input: """
                    // In jj.rs or workspace.rs
                    """
                expected_output: """
                    //! # JJ (Jujutsu) Integration
                    //!
                    //! Factory uses JJ workspaces for task isolation instead of git branches.
                    //!
                    //! ## Why JJ Workspaces?
                    //!
                    //! - **Instant switching**: No stashing, no checkout conflicts
                    //! - **Parallel work**: Multiple tasks in separate directories
                    //! - **First-class conflicts**: JJ shows conflicts in working copy
                    //!
                    //! ## Workspace Lifecycle
                    //!
                    //! ```text
                    //! factory new -s my-task
                    //!     |
                    //!     v
                    //! jj workspace add .factory-workspaces/my-task
                    //!     |
                    //!     v
                    //! (work in isolated workspace)
                    //!     |
                    //!     v
                    //! factory approve -s my-task
                    //!     |
                    //!     v
                    //! jj squash + workspace forget
                    //! ```
                    """
            },
            {
                name:  "test_stages_module_has_howto_docs"
                given: "The stages module"
                when:  "Opening the stages module"
                then: [
                    "Docs include HOWTO: Adding a new language",
                    "HOWTO has numbered steps",
                    "HOWTO shows trait implementation",
                    "HOWTO references existing implementations",
                ]
                real_input: """
                    // In stages.rs or stages/mod.rs
                    """
                expected_output: """
                    //! # Pipeline Stages
                    //!
                    //! ## HOWTO: Adding a New Language
                    //!
                    //! To add support for a new language (e.g., Python):
                    //!
                    //! 1. Create `stages/python.rs`
                    //! 2. Implement the `StageRunner` trait:
                    //!
                    //! ```rust,ignore
                    //! pub struct PythonRunner;
                    //!
                    //! impl StageRunner for PythonRunner {
                    //!     fn run(&self, stage: Stage, ctx: &Context) -> Result<StageResult> {
                    //!         match stage {
                    //!             Stage::Lint => self.run_ruff(ctx),
                    //!             Stage::Test => self.run_pytest(ctx),
                    //!             // ...
                    //!         }
                    //!     }
                    //! }
                    //! ```
                    //!
                    //! 3. Register in `stages/mod.rs`:
                    //!
                    //! ```rust,ignore
                    //! pub fn runner_for(lang: Language) -> Box<dyn StageRunner> {
                    //!     match lang {
                    //!         Language::Rust => Box::new(RustRunner),
                    //!         Language::Python => Box::new(PythonRunner),
                    //!     }
                    //! }
                    //! ```
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_missing_docs_warning_fires"
                given: "#![warn(missing_docs)] is set"
                when:  "A public item lacks documentation"
                then: [
                    "Compiler emits warning",
                    "Warning identifies the undocumented item",
                    "CI can be configured to fail on warnings",
                ]
                real_input: """
                    // In lib.rs:
                    #![warn(missing_docs)]

                    pub fn undocumented_function() {}
                    """
                expected_output: null
                expected_error: """
                    warning: missing documentation for a function
                      --> src/lib.rs:3:1
                       |
                     3 | pub fn undocumented_function() {}
                       | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    """
            },
            {
                name:  "test_broken_doctest_fails"
                given: "A doc example with invalid code"
                when:  "Running cargo test --doc"
                then: [
                    "Doctest compilation fails",
                    "Error message points to the doc comment",
                    "CI fails, blocking merge",
                ]
                real_input: """
                    /// Example that won't compile:
                    /// ```
                    /// let x: i32 = "not a number";
                    /// ```
                    pub fn example() {}
                    """
                expected_output: null
                expected_error: """
                    error[E0308]: mismatched types
                      --> src/lib.rs:3:15
                       |
                     3 | let x: i32 = "not a number";
                       |        ---   ^^^^^^^^^^^^^^ expected `i32`, found `&str`
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_private_items_can_lack_docs"
                scenario: "Private functions don't require docs"
                input:    "fn private_helper() {} // no warning"
                expected: "No missing_docs warning for private items"
            },
            {
                name:     "test_doc_hidden_items_excluded"
                scenario: "#[doc(hidden)] items don't need visible docs"
                input:    "#[doc(hidden)] pub fn internal() {}"
                expected: "No missing_docs warning for hidden items"
            },
            {
                name:     "test_reexports_need_docs"
                scenario: "pub use re-exports should have docs"
                input:    "pub use inner::ImportantType;"
                expected: "Warning unless /// doc comment present"
            },
        ]

        contract_tests: [
            {
                name:     "test_all_public_modules_documented"
                verifies: "Every pub mod has //! docs"
                test:     "grep -r '^pub mod' crates/ | verify each module file has //!"
            },
            {
                name:     "test_cargo_doc_succeeds"
                verifies: "cargo doc runs without errors"
                test:     "moon run :doc (or cargo doc) exits 0"
            },
            {
                name:     "test_doctests_pass"
                verifies: "All doc examples compile and run"
                test:     "cargo test --doc exits 0"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_documentation_coverage"
            description: "Verify all modules have comprehensive documentation"

            setup: {
                files_to_create: []
                precondition_commands: [
                    "moon run factory-core:build",
                    "moon run factory:build",
                ]
            }

            execute: {
                command:    "cargo doc --no-deps --document-private-items 2>&1"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "Documenting factory-core",
                    "Documenting factory",
                    "Finished",
                ]
                files_created: [
                    {
                        path:     "target/doc/factory_core/index.html"
                        contains: "Factory Core"
                    },
                    {
                        path:     "target/doc/factory/index.html"
                        contains: "Factory CLI"
                    },
                ]
            }

            cleanup: {
                commands: []
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_new_contributor_journey"
                description: "Verify docs guide a new contributor to add a language"
                steps: [
                    {action: "Open crates/factory-core/src/lib.rs", verify: "Architecture overview visible"},
                    {action: "Navigate to stages module docs", verify: "HOWTO section present"},
                    {action: "Follow HOWTO steps", verify: "Steps are complete and accurate"},
                    {action: "Implement stub runner", verify: "Pattern matches docs"},
                ]
            },
            {
                name:        "e2e_api_discovery"
                description: "Verify developer can discover API from docs alone"
                steps: [
                    {action: "Run cargo doc --open", verify: "HTML docs render"},
                    {action: "Navigate to factory_core::domain", verify: "Types documented with examples"},
                    {action: "Click on Slug type", verify: "Constructor and methods documented"},
                    {action: "Copy example code", verify: "Example compiles when pasted"},
                ]
            },
        ]
    }

    // ============================================================================
    // SECTION 6: IMPLEMENTATION TASKS
    // ============================================================================

    implementation_tasks: {
        phase_1_tests_first: [
            {
                task:      "Verify current doc coverage"
                file:      "crates/factory-core/src/lib.rs"
                what:      "Run cargo doc and identify missing docs"
                done_when: "List of undocumented items compiled"
            },
            {
                task:      "Add #![warn(missing_docs)] to factory-core"
                file:      "crates/factory-core/src/lib.rs"
                what:      "Enable missing_docs warning"
                done_when: "Warnings list all undocumented public items"
            },
            {
                task:      "Add #![warn(missing_docs)] to factory"
                file:      "crates/factory/src/lib.rs"
                what:      "Enable missing_docs warning"
                done_when: "Warnings list all undocumented public items"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add crate-level docs to factory-core"
                file: "crates/factory-core/src/lib.rs"
                what: """
                    //! # Factory Core
                    //!
                    //! Core library for the Factory task automation system.
                    //!
                    //! ## Architecture Overview
                    //!
                    //! Factory follows these design principles:
                    //!
                    //! 1. **Railway-Oriented Programming**: All fallible operations return
                    //!    `Result<T, Error>`. Errors flow through the "error track" via `?`.
                    //!
                    //! 2. **Actor Model**: Long-running operations use actors for isolation
                    //!    and supervision (inspired by Erlang/OTP).
                    //!
                    //! 3. **Workspace Isolation**: Each task runs in a JJ workspace for
                    //!    parallel development without branch switching.
                    //!
                    //! ## Module Overview
                    //!
                    //! - [`actor`]: Actor system with supervision trees
                    //! - [`domain`]: Core domain types with validation
                    //! - [`error`]: Unified error handling
                    //! - [`stages`]: Pipeline stage runners
                    """
                done_when:     "cargo doc shows crate overview"
                patterns_to_use: ["Intra-doc links for module references"]
            },
            {
                task: "Document Railway-Oriented Programming in domain.rs"
                file: "crates/factory-core/src/domain.rs"
                what: """
                    //! # Domain Types
                    //!
                    //! ## Railway-Oriented Programming (ROP)
                    //!
                    //! Factory uses ROP for error handling. Think of your code as a railway:
                    //!
                    //! ```text
                    //! Input --> [Validate] --> [Transform] --> [Execute] --> Output
                    //!              |               |              |
                    //!              v               v              v
                    //!           Error Track -----> Error ------> Error
                    //! ```
                    //!
                    //! ### Pattern: Chained Validation
                    //!
                    //! ```rust
                    //! use factory_core::domain::{Slug, Stage};
                    //! use factory_core::error::Result;
                    //!
                    //! fn validate_input(slug: &str, stage: &str) -> Result<(Slug, Stage)> {
                    //!     let slug = Slug::new(slug)?;        // Early return on error
                    //!     let stage = Stage::parse(stage)?;   // Continues if Ok
                    //!     Ok((slug, stage))
                    //! }
                    //! ```
                    """
                done_when:     "ROP pattern documented with examples"
                patterns_to_use: [
                    "ASCII art diagrams for visual learners",
                    "Runnable doctest examples",
                ]
            },
            {
                task: "Document actor architecture in actor.rs"
                file: "crates/factory-core/src/actor.rs"
                what: """
                    //! # Actor System
                    //!
                    //! ## BEAM/OTP Inspiration
                    //!
                    //! Factory's actor system is inspired by Erlang/OTP:
                    //!
                    //! | OTP Concept     | Factory Equivalent |
                    //! |-----------------|-------------------|
                    //! | GenServer       | `Actor` trait     |
                    //! | Supervisor      | `Supervisor`      |
                    //! | one_for_one     | `RestartStrategy` |
                    //! | process mailbox | ractor message queue |
                    //!
                    //! ## Supervision Tree
                    //!
                    //! ```text
                    //!            RootSupervisor
                    //!           /       |       \
                    //!    TaskSup    StageSup    JjSup
                    //!       |          |          |
                    //!   [Tasks]    [Stages]   [JjOps]
                    //! ```
                    //!
                    //! ## Why Actors?
                    //!
                    //! - **Isolation**: Actor crashes don't bring down the system
                    //! - **Concurrency**: Message passing, no shared mutable state
                    //! - **Supervision**: Automatic restart on failure
                    """
                done_when:     "Actor architecture fully documented"
                patterns_to_use: [
                    "Comparison tables for familiar concepts",
                    "Supervision tree diagrams",
                ]
            },
            {
                task: "Document JJ integration strategy"
                file: "crates/factory/src/jj.rs"
                what: """
                    //! # JJ (Jujutsu) Integration
                    //!
                    //! ## Why JJ Over Git Branches?
                    //!
                    //! | Concern          | Git Branches      | JJ Workspaces     |
                    //! |------------------|-------------------|-------------------|
                    //! | Switching cost   | Stash + checkout  | cd to directory   |
                    //! | Parallel work    | Multiple clones   | Single repo       |
                    //! | Conflict handling| Merge or rebase   | First-class       |
                    //! | History editing  | Interactive rebase| Immutable + rebase|
                    //!
                    //! ## Workspace Lifecycle
                    //!
                    //! ```rust
                    //! // Create isolated workspace
                    //! jj.workspace_add(&slug).await?;
                    //!
                    //! // Work happens in .factory-workspaces/{slug}/
                    //!
                    //! // Integrate back to main
                    //! jj.squash_and_forget(&slug).await?;
                    //! ```
                    //!
                    //! ## Commands Used
                    //!
                    //! - `jj workspace add`: Create isolated working copy
                    //! - `jj squash`: Combine commits
                    //! - `jj workspace forget`: Clean up workspace
                    """
                done_when:     "JJ integration fully documented"
                patterns_to_use: [
                    "Comparison tables",
                    "Command reference",
                ]
            },
            {
                task: "Document how to add new languages in stages"
                file: "crates/factory-core/src/stages.rs"
                what: """
                    //! # Pipeline Stages
                    //!
                    //! ## HOWTO: Adding a New Language
                    //!
                    //! ### Step 1: Define Language Variant
                    //!
                    //! In `domain.rs`:
                    //! ```rust,ignore
                    //! pub enum Language {
                    //!     Rust,
                    //!     Python,  // Add new variant
                    //! }
                    //! ```
                    //!
                    //! ### Step 2: Create Stage Runner
                    //!
                    //! Create `stages/python.rs`:
                    //! ```rust,ignore
                    //! use crate::{Stage, StageResult, Context, Result};
                    //!
                    //! pub struct PythonRunner;
                    //!
                    //! impl PythonRunner {
                    //!     pub fn run(&self, stage: Stage, ctx: &Context) -> Result<StageResult> {
                    //!         match stage {
                    //!             Stage::Lint => self.run_ruff(ctx),
                    //!             Stage::UnitTest => self.run_pytest(ctx),
                    //!             Stage::Coverage => self.run_coverage(ctx),
                    //!             _ => Ok(StageResult::skipped("Not applicable")),
                    //!         }
                    //!     }
                    //!
                    //!     fn run_ruff(&self, ctx: &Context) -> Result<StageResult> {
                    //!         // Execute: ruff check .
                    //!         todo!()
                    //!     }
                    //! }
                    //! ```
                    //!
                    //! ### Step 3: Register Runner
                    //!
                    //! In `stages/mod.rs`:
                    //! ```rust,ignore
                    //! pub fn runner_for(lang: Language) -> Box<dyn StageRunner> {
                    //!     match lang {
                    //!         Language::Rust => Box::new(RustRunner),
                    //!         Language::Python => Box::new(PythonRunner),
                    //!     }
                    //! }
                    //! ```
                    //!
                    //! ### Step 4: Add Tests
                    //!
                    //! See `stages/rust.rs` for test patterns to follow.
                    """
                done_when:     "HOWTO guide complete with steps"
                patterns_to_use: [
                    "Numbered steps",
                    "Code examples with ignore for incomplete snippets",
                    "References to existing implementations",
                ]
            },
            {
                task: "Document testing strategy in lib.rs"
                file: "crates/factory-core/src/lib.rs"
                what: """
                    //! ## Testing Strategy
                    //!
                    //! Factory uses a multi-layer testing approach:
                    //!
                    //! ### Unit Tests (in-module)
                    //!
                    //! ```rust,ignore
                    //! #[cfg(test)]
                    //! mod tests {
                    //!     use super::*;
                    //!
                    //!     #[test]
                    //!     fn test_slug_validation() {
                    //!         assert!(Slug::new("valid-slug").is_ok());
                    //!         assert!(Slug::new("INVALID").is_err());
                    //!     }
                    //! }
                    //! ```
                    //!
                    //! ### Integration Tests (tests/ directory)
                    //!
                    //! Test actor interactions, JJ operations, stage runners.
                    //!
                    //! ### Doctests (in doc comments)
                    //!
                    //! Every public function should have a runnable example.
                    //!
                    //! ### Property Tests (proptest)
                    //!
                    //! For domain types like `Slug`, test invariants hold for all inputs.
                    """
                done_when:     "Testing strategy documented"
                patterns_to_use: [
                    "Categorized test types",
                    "Examples for each category",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Document all public functions with examples"
                file:      "All files in crates/"
                what:      "Add /// doc comments to every pub fn"
                done_when: "cargo doc --warn missing_docs shows no warnings"
            },
            {
                task:      "Add intra-doc links between modules"
                file:      "All doc comments"
                what:      "Use [`module::Type`] syntax for cross-references"
                done_when: "cargo doc links navigate correctly"
            },
            {
                task:      "Verify all doctests compile"
                file:      "All doc comments"
                what:      "Run cargo test --doc"
                done_when: "All doctests pass"
            },
        ]

        phase_4_verification: [
            {
                task:     "Run cargo doc"
                done_when: "No warnings, HTML generated"
                commands: ["cargo doc --no-deps"]
                expected: "exit code 0, no warnings"
            },
            {
                task:     "Run doctests"
                done_when: "All doc examples pass"
                commands: ["cargo test --doc"]
                expected: "exit code 0"
            },
            {
                task:     "Verify moon run :ci passes"
                done_when: "Full CI pipeline green"
                commands: ["moon run :ci"]
                expected: "exit code 0"
            },
            {
                task:      "Manual review of generated docs"
                done_when: "Docs are readable and complete"
                commands: [
                    "cargo doc --open",
                ]
                expected: "Reviewer confirms docs are helpful"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "warning: missing documentation for..."
                likely_cause: "#![warn(missing_docs)] enabled but items undocumented"
                where_to_look: [
                    {
                        file:          "The file mentioned in warning"
                        what_to_check: "pub items without /// or //! docs"
                    },
                ]
                fix_pattern: "Add /// doc comment above the item"
            },
            {
                symptom:      "error: unresolved link to `Foo`"
                likely_cause: "Intra-doc link references non-existent type"
                where_to_look: [
                    {
                        file:          "File with the doc comment"
                        what_to_check: "Is [`Foo`] spelled correctly? Is it in scope?"
                    },
                ]
                fix_pattern: "Use full path [`crate::module::Foo`] or import the type"
            },
            {
                symptom:      "doctest failed: cannot find type"
                likely_cause: "Doc example missing use statements"
                where_to_look: [
                    {
                        file:          "File containing the doc comment"
                        function:      "The documented function"
                        what_to_check: "Does example have all necessary imports?"
                    },
                ]
                fix_pattern: "Add use statements at top of example code block"
            },
            {
                symptom:      "Doc example runs but fails assertion"
                likely_cause: "Example code doesn't match actual behavior"
                where_to_look: [
                    {
                        file:          "The source file"
                        what_to_check: "Did API change? Update the example"
                    },
                ]
                fix_pattern: "Update example to match current implementation"
            },
        ]

        debugging_commands: [
            {
                scenario: "Find all undocumented public items"
                run:      "RUSTDOCFLAGS='-D warnings' cargo doc 2>&1 | grep 'missing documentation'"
                look_for: "List of items needing docs"
            },
            {
                scenario: "Test only doctests"
                run:      "cargo test --doc -- --nocapture"
                look_for: "Which examples fail and why"
            },
            {
                scenario: "Check intra-doc link resolution"
                run:      "cargo doc 2>&1 | grep 'unresolved link'"
                look_for: "Broken cross-references"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] cargo doc generates without warnings",
            "[ ] cargo test --doc passes (all doctests)",
            "[ ] All public modules have //! module docs",
            "[ ] All public functions have /// doc comments",
            "[ ] All public types have /// doc comments",
            "[ ] Doc examples are runnable (not just ignore)",
        ]

        code: [
            "[ ] #![warn(missing_docs)] enabled in factory-core",
            "[ ] #![warn(missing_docs)] enabled in factory",
            "[ ] Architecture overview in lib.rs",
            "[ ] ROP pattern documentation in domain.rs",
            "[ ] Actor architecture in actor.rs",
            "[ ] JJ integration docs in jj module",
            "[ ] HOWTO: Adding languages in stages module",
            "[ ] Testing strategy documented",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] Documentation builds in CI",
        ]

        documentation: [
            "[ ] Intra-doc links work (click through in HTML)",
            "[ ] ASCII diagrams render correctly",
            "[ ] Code examples compile when copied",
            "[ ] New contributor can follow HOWTO",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Main entry point for crate-level documentation"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Document ROP patterns and newtype validation"
            },
            {
                path:      "crates/factory-core/src/actor.rs"
                relevance: "Document actor architecture and supervision"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Document error handling strategy"
            },
            {
                path:      "crates/factory/src/lib.rs"
                relevance: "Document CLI architecture"
            },
            {
                path:      "crates/factory/src/jj.rs"
                relevance: "Document JJ integration"
            },
        ]

        external_references: [
            "https://doc.rust-lang.org/rustdoc/ - Rustdoc book",
            "https://doc.rust-lang.org/rustdoc/write-documentation/what-to-include.html - What to document",
            "https://fsharpforfunandprofit.com/rop/ - Railway-Oriented Programming",
            "https://github.com/martinsson/jj - JJ documentation",
            "https://www.erlang.org/doc/design_principles/des_princ.html - OTP design principles",
        ]

        codebase_patterns: [
            {
                pattern:          "Module-Level Documentation"
                example_location: "Any well-documented Rust crate"
                how_to_apply:     "Start file with //! docs explaining module purpose"
            },
            {
                pattern:          "Doctest Examples"
                example_location: "Standard library docs"
                how_to_apply:     "Add ```rust code blocks in /// comments"
            },
            {
                pattern:          "Intra-Doc Links"
                example_location: "https://doc.rust-lang.org/rustdoc/write-documentation/linking-to-items-by-name.html"
                how_to_apply:     "Use [`TypeName`] to link to types within docs"
            },
        ]

        dependencies: [
            {
                bead_id:    "factory-khq"
                title:      "Actor system implementation"
                relevance:  "Must be complete to document actor architecture"
            },
            {
                bead_id:    "factory-8cb"
                title:      "JJ integration"
                relevance:  "Must be complete to document JJ workspace strategy"
            },
            {
                bead_id:    "factory-2ar"
                title:      "Stage runners"
                relevance:  "Must be complete to document how to add languages"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Start with #![warn(missing_docs)] to find gaps",
            "Use intra-doc links [`Type`] for cross-references",
            "Include runnable examples in every public function",
            "Use ASCII art for diagrams (renders in terminal and docs)",
            "Reference the actual implementation, not hypothetical code",
            "Add HOWTO sections for common extension points",
            "Test all doc examples compile: cargo test --doc",
            "Use tables to compare concepts (JJ vs git, OTP vs Factory)",
        ]

        do_not: [
            "Do NOT write docs that drift from implementation",
            "Do NOT use ignore unless code is truly a snippet",
            "Do NOT skip error handling in examples",
            "Do NOT assume reader knows JJ or OTP",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT leave dead intra-doc links",
        ]

        code_patterns: [
            {
                name:     "Module-Level Documentation"
                use_when: "Starting any .rs file"
                example:  """
                    //! # Module Name
                    //!
                    //! Brief description of what this module provides.
                    //!
                    //! ## Overview
                    //!
                    //! More detailed explanation...
                    //!
                    //! ## Examples
                    //!
                    //! ```rust
                    //! use my_crate::my_module::MyType;
                    //!
                    //! let x = MyType::new();
                    //! ```
                    """
            },
            {
                name:     "Function Documentation"
                use_when: "Documenting any pub fn"
                example:  """
                    /// Creates a new [`Slug`] from the given string.
                    ///
                    /// # Errors
                    ///
                    /// Returns [`Error::InvalidSlug`] if:
                    /// - Input is empty
                    /// - Input contains uppercase letters
                    /// - Input contains invalid characters
                    ///
                    /// # Examples
                    ///
                    /// ```rust
                    /// use factory_core::domain::Slug;
                    ///
                    /// let slug = Slug::new("my-task")?;
                    /// assert_eq!(slug.as_str(), "my-task");
                    /// # Ok::<(), factory_core::error::Error>(())
                    /// ```
                    pub fn new(s: &str) -> Result<Self> {
                        // ...
                    }
                    """
            },
            {
                name:     "Architecture Decision Record in Docs"
                use_when: "Explaining why a design choice was made"
                example:  """
                    //! ## Design Decision: Actors over Async Tasks
                    //!
                    //! We chose actors over plain async tasks because:
                    //!
                    //! 1. **Supervision**: Actors can be restarted on failure
                    //! 2. **Backpressure**: Bounded mailboxes prevent memory exhaustion
                    //! 3. **Isolation**: Actor state is encapsulated, no shared mutability
                    //!
                    //! Alternatives considered:
                    //! - `tokio::spawn` with channels: No supervision, manual restart logic
                    //! - `async-std`: Doesn't have actor framework as mature as ractor
                    """
            },
        ]
    }
}
