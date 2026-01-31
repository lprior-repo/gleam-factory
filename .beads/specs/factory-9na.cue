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

"factory-9na": #ValidBead & {
    // ============================================================================
    // BEAD: factory-9na - Replace grep-based TODO detection with AST-aware analysis
    // ============================================================================

    id:              "factory-9na"
    title:           "Tree-sitter TODO Detection: Replace grep with AST-aware comment parsing"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["tree-sitter", "ast", "review-stage", "multi-language", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use tree-sitter for all comment extraction from source files",
            "THE SYSTEM SHALL support comment detection for Rust, Go, Python, JavaScript/TypeScript, and Gleam",
            "THE SYSTEM SHALL detect TODO, FIXME, XXX, and HACK markers within comments only",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL NOT match TODO/FIXME in string literals, variable names, or non-comment code",
        ]

        event_driven: [
            {
                trigger: "WHEN the review stage is invoked for a language"
                shall:   "THE SYSTEM SHALL parse all source files using language-appropriate tree-sitter grammar"
            },
            {
                trigger: "WHEN a source file is parsed"
                shall:   "THE SYSTEM SHALL extract all comment nodes from the AST"
            },
            {
                trigger: "WHEN a comment node contains TODO|FIXME|XXX|HACK"
                shall:   "THE SYSTEM SHALL record the file path, line number, and comment text"
            },
            {
                trigger: "WHEN tree-sitter grammar is not available for a language"
                shall:   "THE SYSTEM SHALL return Error::GrammarNotFound with language name"
            },
            {
                trigger: "WHEN a source file fails to parse"
                shall:   "THE SYSTEM SHALL continue with remaining files and report partial results"
            },
        ]

        state_driven: [
            {
                state: "WHILE scanning a directory for source files"
                shall: "THE SYSTEM SHALL skip hidden directories, target/, node_modules/, __pycache__/, build/"
            },
            {
                state: "WHILE processing comments"
                shall: "THE SYSTEM SHALL support single-line (//, #), multi-line (/* */), and doc comments (///, /** */)"
            },
        ]

        unwanted: [
            {
                condition: "IF TODO appears in a string literal"
                shall_not: "THE SYSTEM SHALL NOT flag it as a TODO marker"
                because:   "String literals are not comments - false positives confuse users"
            },
            {
                condition: "IF TODO appears in a variable or function name"
                shall_not: "THE SYSTEM SHALL NOT flag it as a TODO marker"
                because:   "Identifiers like 'todoList' are valid code, not work items"
            },
            {
                condition: "IF grep is available on the system"
                shall_not: "THE SYSTEM SHALL NOT use grep for comment detection"
                because:   "grep is non-portable, ignores AST structure, and has fragile exit code semantics"
            },
            {
                condition: "IF a binary file is encountered"
                shall_not: "THE SYSTEM SHALL NOT attempt to parse binary files"
                because:   "Tree-sitter expects UTF-8 source code, binary files cause undefined behavior"
            },
        ]

        complex: [
            {
                state:   "WHILE parsing nested comments (Rust /* /* */ */)"
                trigger: "WHEN inner comment contains TODO marker"
                shall:   "THE SYSTEM SHALL correctly identify the TODO within the nested structure"
            },
            {
                state:   "WHILE processing TypeScript"
                trigger: "WHEN JSDoc comment contains @TODO annotation"
                shall:   "THE SYSTEM SHALL treat @TODO and TODO equivalently"
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
                    field:           "cwd: &Path"
                    type:            "Path reference"
                    constraints:     "Must be a valid directory containing source files"
                    example_valid:   "/home/user/project/src"
                    example_invalid: "/nonexistent/path"
                },
                {
                    field:           "language: Language"
                    type:            "enum Language"
                    constraints:     "Must be one of: Rust, Go, Python, JavaScript, Gleam"
                    example_valid:   "Language::Rust"
                    example_invalid: "Language::Cobol"
                },
            ]
            system_state: [
                "factory-khq (StageTable refactor) is completed - provides StageHandler trait",
                "Rust toolchain installed (rustc, cargo)",
                "Moon build system configured",
                "tree-sitter and language grammars available as cargo dependencies",
            ]
        }

        postconditions: {
            state_changes: [
                "New module: crates/factory-core/src/todo_detector.rs",
                "New module: crates/factory-core/src/tree_sitter_utils.rs",
                "Updated: All 5 language stage handlers use TodoDetector instead of grep",
                "Cargo.toml updated with tree-sitter dependencies",
            ]
            return_guarantees: [
                {
                    field:     "TodoDetector::scan()"
                    guarantee: "Returns Result<TodoReport, Error>"
                },
                {
                    field:     "TodoReport"
                    guarantee: "Contains Vec<TodoItem> with file, line, column, text, marker_type"
                },
                {
                    field:     "review stage"
                    guarantee: "Returns Ok(()) when no TODOs found, Err(StageFailed) with list when found"
                },
                {
                    field:     "Grammar loading"
                    guarantee: "Returns Result<tree_sitter::Language, Error>"
                },
            ]
            side_effects: [
                "None - pure read-only operation on source files",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Comment detection is language-aware via tree-sitter grammar",
            "Binary files are never read or parsed",
            "Hidden directories and build artifacts are always skipped",
            "Tree-sitter parsing errors do not halt the entire scan",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "tree-sitter crate version conflicts with existing dependencies"
                prevention:  "Use workspace dependencies, check cargo tree before adding"
                test_for_it: "test_cargo_build_succeeds"
            },
            {
                failure:     "tree-sitter-* language grammars have incompatible versions"
                prevention:  "Pin all tree-sitter-* crates to same major version"
                test_for_it: "test_all_grammars_load"
            },
            {
                failure:     "StageHandler trait from factory-khq not compatible with TodoDetector"
                prevention:  "Design TodoDetector to match StageHandler signature"
                test_for_it: "test_todo_detector_as_stage_handler"
            },
        ]

        usability_failures: [
            {
                failure:     "TODO detection too slow on large codebases"
                prevention:  "Use parallel file scanning with rayon, lazy grammar loading"
                test_for_it: "test_scan_1000_files_under_5_seconds"
            },
            {
                failure:     "Error messages don't show which file failed"
                prevention:  "Include file path in all Error variants"
                test_for_it: "test_error_includes_file_path"
            },
            {
                failure:     "Can't distinguish TODO types (TODO vs FIXME priority)"
                prevention:  "TodoItem includes marker_type: TodoMarker enum"
                test_for_it: "test_marker_types_distinguished"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "TODO in string literal incorrectly flagged"
                prevention:  "Only extract from comment nodes, never string_literal nodes"
                test_for_it: "test_string_literal_not_flagged"
            },
            {
                failure:     "Multi-line comment split across TODO marker"
                prevention:  "Use tree-sitter node.utf8_text() for complete comment text"
                test_for_it: "test_multiline_comment_detected"
            },
            {
                failure:     "Unicode in comments breaks detection"
                prevention:  "Use UTF-8 aware text extraction from tree-sitter"
                test_for_it: "test_unicode_comment_handling"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_detect_todo_in_rust_comment"
                given: "A Rust file with // TODO: fix this"
                when:  "TodoDetector::scan() is called for Rust"
                then: [
                    "Returns Ok(TodoReport) with one TodoItem",
                    "TodoItem.file matches the source file",
                    "TodoItem.line is correct",
                    "TodoItem.marker_type is TodoMarker::Todo",
                ]
                real_input: """
                    // temp.rs
                    fn main() {
                        // TODO: implement this properly
                        println!("placeholder");
                    }
                    """
                expected_output: """
                    TodoReport {
                        items: vec![TodoItem {
                            file: PathBuf::from("temp.rs"),
                            line: 3,
                            column: 5,
                            text: "TODO: implement this properly".to_string(),
                            marker_type: TodoMarker::Todo,
                        }],
                    }
                    """
            },
            {
                name:  "test_detect_fixme_in_python_comment"
                given: "A Python file with # FIXME: broken logic"
                when:  "TodoDetector::scan() is called for Python"
                then: [
                    "Returns Ok(TodoReport) with one TodoItem",
                    "TodoItem.marker_type is TodoMarker::Fixme",
                ]
                real_input: """
                    # app.py
                    def calculate():
                        # FIXME: this math is wrong
                        return 2 + 2
                    """
                expected_output: """
                    TodoReport {
                        items: vec![TodoItem {
                            file: PathBuf::from("app.py"),
                            line: 3,
                            marker_type: TodoMarker::Fixme,
                            ..
                        }],
                    }
                    """
            },
            {
                name:  "test_multiline_block_comment_go"
                given: "A Go file with /* TODO: multi-line comment */"
                when:  "TodoDetector::scan() is called for Go"
                then: [
                    "Detects TODO in block comment",
                    "Returns correct line number for start of comment",
                ]
                real_input: """
                    // main.go
                    package main

                    /*
                     * TODO: refactor this entire section
                     * to be more idiomatic Go
                     */
                    func main() {}
                    """
                expected_output: """
                    TodoReport with one TodoItem at line 4
                    """
            },
            {
                name:  "test_no_todos_returns_empty_report"
                given: "A clean source file with no TODO markers"
                when:  "TodoDetector::scan() is called"
                then: [
                    "Returns Ok(TodoReport) with empty items vec",
                    "Review stage passes",
                ]
                real_input: """
                    fn clean_code() {
                        // This is a regular comment
                        println!("All good");
                    }
                    """
                expected_output: """
                    TodoReport { items: vec![] }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_directory_not_found"
                given: "A non-existent directory path"
                when:  "TodoDetector::scan() is called"
                then: [
                    "Returns Err(Error::DirectoryNotFound)",
                    "Error includes the path",
                ]
                real_input: """
                    let detector = TodoDetector::new(Language::Rust);
                    let result = detector.scan(Path::new("/does/not/exist"));
                    """
                expected_output: null
                expected_error: """
                    Err(Error::DirectoryNotFound { path: PathBuf::from("/does/not/exist") })
                    """
            },
            {
                name:  "test_parse_error_continues_with_other_files"
                given: "A directory with one valid file and one with syntax errors"
                when:  "TodoDetector::scan() is called"
                then: [
                    "Parses the valid file successfully",
                    "Continues scanning despite parse errors",
                    "Returns partial results",
                ]
                real_input: """
                    // valid.rs - has TODO
                    fn ok() { /* TODO: something */ }

                    // broken.rs - syntax error
                    fn broken { {{ not valid rust
                    """
                expected_output: """
                    TodoReport with TodoItem from valid.rs
                    """
                expected_error: null
            },
        ]

        edge_cases: [
            {
                name:     "test_todo_in_string_literal_ignored"
                scenario: "TODO marker appears inside a string, not a comment"
                input: """
                    let msg = "TODO: this is not a comment";
                    """
                expected: "Empty TodoReport - string literals are not comments"
            },
            {
                name:     "test_todo_in_variable_name_ignored"
                scenario: "Variable named todoList or similar"
                input: """
                    let todo_items: Vec<Todo> = vec![];
                    let fixme_later = true;
                    """
                expected: "Empty TodoReport - identifiers are not comments"
            },
            {
                name:     "test_nested_rust_comment"
                scenario: "Rust nested block comment /* /* TODO */ */"
                input: """
                    /* outer
                       /* TODO: inner comment */
                    */
                    """
                expected: "Detects TODO in nested comment structure"
            },
            {
                name:     "test_jsdoc_at_todo"
                scenario: "JSDoc @TODO annotation in TypeScript"
                input: """
                    /**
                     * @TODO Implement this function
                     * @param x The input
                     */
                    """
                expected: "Detects @TODO as equivalent to TODO"
            },
            {
                name:     "test_skip_node_modules"
                scenario: "TODO in node_modules should be ignored"
                input:    "node_modules/some-lib/index.js with TODO comment"
                expected: "Empty TodoReport - node_modules is always skipped"
            },
            {
                name:     "test_skip_target_directory"
                scenario: "TODO in Rust target/ directory"
                input:    "target/debug/deps/somefile.rs with TODO"
                expected: "Empty TodoReport - target/ is always skipped"
            },
            {
                name:     "test_case_sensitivity"
                scenario: "todo, Todo, TODO, tOdO variations"
                input: """
                    // todo: lowercase
                    // Todo: mixed case
                    // TODO: uppercase
                    """
                expected: "All case variations detected (case-insensitive matching)"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in codebase"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/todo_detector.rs returns empty"
            },
            {
                name:     "test_invariant_no_panic"
                verifies: "No panic!, todo!, unimplemented! in codebase"
                test:     "rg 'panic!|todo!|unimplemented!' crates/factory-core/src/todo_detector.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public functions return Result"
                test:     "Every pub fn has -> Result<...> return type"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_review_stage_with_tree_sitter"
            description: "Full review stage using tree-sitter instead of grep"

            setup: {
                files_to_create: [
                    {
                        path: "test_project/src/lib.rs"
                        content: """
                            //! Library with TODO markers

                            /// TODO: document this function properly
                            pub fn needs_work() {
                                // FIXME: this is broken
                                let x = 42;
                            }

                            // This is fine - no markers
                            pub fn complete() -> i32 {
                                42
                            }

                            // XXX: known issue here
                            pub fn problematic() {}
                            """
                    },
                    {
                        path: "test_project/src/clean.rs"
                        content: """
                            //! Clean module with no TODO markers

                            pub fn perfect() -> &'static str {
                                "no issues here"
                            }
                            """
                    },
                    {
                        path: "test_project/Cargo.toml"
                        content: """
                            [package]
                            name = "test_project"
                            version = "0.1.0"
                            edition = "2021"
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "factory stage -s test-task --stage review --language rust"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 1  // Should fail because TODOs exist
                stdout_contains: [
                    "TODO: document this function properly",
                    "FIXME: this is broken",
                    "XXX: known issue here",
                ]
                stdout_not_contains: [
                    "grep",  // Should not mention grep at all
                ]
                files_created: []
            }

            cleanup: {
                commands: [
                    "rm -rf test_project",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_multilang_project"
                description: "Scan project with multiple languages"
                steps: [
                    {action: "Create Rust, Python, and TypeScript files with TODOs", verify: "Files exist"},
                    {action: "Run TodoDetector for each language", verify: "All TODOs found"},
                    {action: "Verify no false positives from string literals", verify: "Only comment TODOs reported"},
                    {action: "Check performance with 100 files", verify: "Completes in under 2 seconds"},
                ]
            },
            {
                name:        "e2e_clean_project_passes"
                description: "Project with no TODOs passes review stage"
                steps: [
                    {action: "Create source files with no TODO markers", verify: "Files created"},
                    {action: "Run review stage", verify: "Exits with code 0"},
                    {action: "Verify empty TodoReport", verify: "No false positives"},
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
                task:      "Write test: test_detect_todo_in_rust_comment"
                file:      "crates/factory-core/src/todo_detector.rs"
                what:      "Test that Rust // TODO comment is detected"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_string_literal_not_flagged"
                file:      "crates/factory-core/src/todo_detector.rs"
                what:      "Test that TODO in string is ignored"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_multiline_comment_detected"
                file:      "crates/factory-core/src/todo_detector.rs"
                what:      "Test that block comment TODO is found"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_all_grammars_load"
                file:      "crates/factory-core/src/tree_sitter_utils.rs"
                what:      "Test that all 5 language grammars load successfully"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add tree-sitter dependencies to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    tree-sitter = "0.24"
                    tree-sitter-rust = "0.24"
                    tree-sitter-go = "0.24"
                    tree-sitter-python = "0.24"
                    tree-sitter-javascript = "0.24"
                    tree-sitter-typescript = "0.24"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Create tree_sitter_utils.rs module"
                file: "crates/factory-core/src/tree_sitter_utils.rs"
                what: """
                    /// Load tree-sitter grammar for a language.
                    pub fn load_grammar(lang: Language) -> Result<tree_sitter::Language> {
                        match lang {
                            Language::Rust => Ok(tree_sitter_rust::LANGUAGE.into()),
                            Language::Go => Ok(tree_sitter_go::LANGUAGE.into()),
                            Language::Python => Ok(tree_sitter_python::LANGUAGE.into()),
                            Language::JavaScript => Ok(tree_sitter_javascript::LANGUAGE.into()),
                            Language::Gleam => Err(Error::GrammarNotFound { lang: "Gleam".into() }),
                        }
                    }

                    /// Get comment node kinds for a language.
                    pub fn comment_kinds(lang: Language) -> &'static [&'static str] {
                        match lang {
                            Language::Rust => &["line_comment", "block_comment"],
                            Language::Go => &["comment"],
                            Language::Python => &["comment"],
                            Language::JavaScript => &["comment", "multiline_comment"],
                            Language::Gleam => &["comment"],
                        }
                    }
                    """
                done_when:     "Module compiles"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "Match on Language enum exhaustively",
                ]
            },
            {
                task: "Create todo_detector.rs module"
                file: "crates/factory-core/src/todo_detector.rs"
                what: """
                    pub struct TodoDetector { language: Language }
                    pub struct TodoReport { items: Vec<TodoItem> }
                    pub struct TodoItem { file, line, column, text, marker_type }
                    pub enum TodoMarker { Todo, Fixme, Xxx, Hack }

                    impl TodoDetector {
                        pub fn new(language: Language) -> Self;
                        pub fn scan(&self, dir: &Path) -> Result<TodoReport>;
                        fn parse_file(&self, path: &Path) -> Result<Vec<TodoItem>>;
                        fn extract_comments(&self, tree: &Tree, source: &str) -> Vec<CommentNode>;
                        fn find_markers(&self, comment: &str) -> Vec<(TodoMarker, String)>;
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Railway-oriented error handling with ?",
                    "Iterator chains with filter_map",
                    "WalkDir for directory traversal",
                ]
            },
            {
                task: "Create file walker with exclusions"
                file: "crates/factory-core/src/todo_detector.rs"
                what: """
                    /// Directories to always skip during scanning.
                    const SKIP_DIRS: &[&str] = &[
                        "target", "node_modules", "__pycache__", "build",
                        ".git", ".hg", ".svn", "vendor", "dist",
                    ];

                    fn should_skip(entry: &DirEntry) -> bool {
                        entry.file_name()
                            .to_str()
                            .map(|s| s.starts_with('.') || SKIP_DIRS.contains(&s))
                            .unwrap_or(false)
                    }
                    """
                done_when:     "test_skip_node_modules passes"
                patterns_to_use: ["WalkDir with filter_entry"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export modules from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod todo_detector; pub mod tree_sitter_utils;"
                done_when: "External crates can import factory_core::todo_detector"
            },
            {
                task:      "Add GrammarNotFound error variant"
                file:      "crates/factory-core/src/error.rs"
                what: """
                    #[error("tree-sitter grammar not found for language: {lang}")]
                    GrammarNotFound { lang: String },

                    #[error("failed to parse source file: {path}")]
                    ParseFailed { path: PathBuf },
                    """
                done_when: "Error variants compile and are documented"
            },
            {
                task: "Update all 5 language handlers to use TodoDetector"
                file: "crates/factory-core/src/stages/rust.rs"
                what: """
                    fn rust_review(cwd: &Path) -> Result<()> {
                        let detector = TodoDetector::new(Language::Rust);
                        let report = detector.scan(cwd)?;

                        if report.items.is_empty() {
                            Ok(())
                        } else {
                            Err(Error::stage_failed(
                                "Rust",
                                "review",
                                format!("Found {} TODO markers:\\n{}",
                                    report.items.len(),
                                    report.format_list()),
                            ))
                        }
                    }
                    """
                done_when:     "All review stages use TodoDetector"
                patterns_to_use: ["Railway-oriented: use ? for error propagation"]
            },
            {
                task:      "Remove all grep calls from stage handlers"
                file:      "crates/factory-core/src/stages/*.rs"
                what:      "Delete run_command(\"grep\", ...) from all *_review functions"
                done_when: "rg 'run_command.*grep' crates/ returns empty"
            },
        ]

        phase_4_verification: [
            {
                task:     "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
                commands: ["moon run :ci"]
                expected: "exit code 0"
            },
            {
                task:     "Verify no unwraps"
                done_when: "grep finds no unwrap/expect"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/todo_detector.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no grep usage"
                done_when: "No grep calls remain in stage handlers"
                commands: ["rg 'grep' crates/factory-core/src/stages/"]
                expected: "no output (empty)"
            },
            {
                task:      "Performance test"
                done_when: "Scan 100 files in under 2 seconds"
                commands: [
                    "cargo test -- test_scan_performance --nocapture",
                ]
                expected: "Test passes"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'tree_sitter' crate not found"
                likely_cause: "tree-sitter dependency not added to Cargo.toml"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "tree-sitter = \"0.24\" exists in dependencies"
                    },
                ]
                fix_pattern: "Add tree-sitter and all language grammar crates to Cargo.toml"
            },
            {
                symptom:      "Grammar load fails: 'incompatible ABI version'"
                likely_cause: "tree-sitter and language grammar versions don't match"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "All tree-sitter-* crates have same version as tree-sitter"
                    },
                ]
                fix_pattern: "Pin all tree-sitter related crates to same version (e.g., 0.24)"
            },
            {
                symptom:      "False positives: TODO in strings flagged"
                likely_cause: "Extracting from wrong AST node types"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/todo_detector.rs"
                        function:      "extract_comments()"
                        what_to_check: "Only comment node kinds are matched"
                    },
                ]
                fix_pattern: "Verify comment_kinds() returns correct node types for language"
            },
            {
                symptom:      "Gleam files not scanned"
                likely_cause: "No tree-sitter-gleam crate available"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/tree_sitter_utils.rs"
                        function:      "load_grammar()"
                        what_to_check: "Gleam case returns appropriate error or fallback"
                    },
                ]
                fix_pattern: "Use regex-based fallback for Gleam until grammar is available"
            },
            {
                symptom:      "Scan is very slow (>10s)"
                likely_cause: "Not skipping large directories like node_modules"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/todo_detector.rs"
                        function:      "should_skip()"
                        what_to_check: "SKIP_DIRS includes node_modules, target, etc."
                    },
                ]
                fix_pattern: "Ensure directory filter is applied in WalkDir::new().filter_entry()"
            },
        ]

        debugging_commands: [
            {
                scenario: "When tree-sitter fails to parse a file"
                run:      "RUST_LOG=debug cargo test test_parse_file -- --nocapture"
                look_for: "Parse error output, tree structure"
            },
            {
                scenario: "When wrong node types are matched"
                run:      "tree-sitter parse file.rs --output-type json | jq '.children[] | select(.type | contains(\"comment\"))'"
                look_for: "Actual comment node kinds in the language grammar"
            },
            {
                scenario: "When performance is slow"
                run:      "time cargo test test_scan_performance 2>&1 | grep real"
                look_for: "Total wall clock time for scan"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_detect_todo_in_rust_comment passes",
            "[ ] test_detect_fixme_in_python_comment passes",
            "[ ] test_multiline_block_comment_go passes",
            "[ ] test_no_todos_returns_empty_report passes",
            "[ ] test_string_literal_not_flagged passes",
            "[ ] test_variable_name_not_flagged passes",
            "[ ] test_skip_node_modules passes",
            "[ ] test_skip_target_directory passes",
            "[ ] test_all_grammars_load passes",
            "[ ] test_case_insensitive_matching passes",
            "[ ] E2E review stage test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Zero panic!, todo!, unimplemented! calls",
            "[ ] tree-sitter dependencies added",
            "[ ] todo_detector.rs module created",
            "[ ] tree_sitter_utils.rs module created",
            "[ ] All 5 language handlers updated",
            "[ ] All grep calls removed from stage handlers",
            "[ ] GrammarNotFound error variant added",
            "[ ] ParseFailed error variant added",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in todo_detector.rs",
            "[ ] Module-level docs in tree_sitter_utils.rs",
            "[ ] Doc comments on TodoDetector struct and methods",
            "[ ] Doc comments on TodoReport and TodoItem",
            "[ ] Example usage in doc comments",
        ]

        performance: [
            "[ ] Scan 100 files in under 2 seconds",
            "[ ] Lazy grammar loading implemented",
            "[ ] Parallel file processing with rayon",
            "[ ] Directory skip list working",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/stages/rust.rs"
                relevance: "Current grep-based rust_review() to replace"
            },
            {
                path:      "crates/factory-core/src/stages/python.rs"
                relevance: "Current grep-based python_review() to replace"
            },
            {
                path:      "crates/factory-core/src/stages/go.rs"
                relevance: "Current grep-based go_review() to replace"
            },
            {
                path:      "crates/factory-core/src/stages/javascript.rs"
                relevance: "Current grep-based javascript_review() to replace"
            },
            {
                path:      "crates/factory-core/src/stages/gleam.rs"
                relevance: "Current grep-based gleam_review() to replace"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add GrammarNotFound, ParseFailed"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add tree-sitter and grammar crates"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Language enum definition used for grammar selection"
            },
        ]

        external_references: [
            "https://docs.rs/tree-sitter - tree-sitter Rust bindings",
            "https://tree-sitter.github.io/tree-sitter/ - Official documentation",
            "https://github.com/tree-sitter/tree-sitter-rust - Rust grammar",
            "https://github.com/tree-sitter/tree-sitter-go - Go grammar",
            "https://github.com/tree-sitter/tree-sitter-python - Python grammar",
            "https://github.com/tree-sitter/tree-sitter-javascript - JavaScript grammar",
            "https://github.com/tree-sitter/tree-sitter-typescript - TypeScript grammar",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/stages/rust.rs:rust_lint()"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Language Enum Pattern"
                example_location: "crates/factory-core/src/domain.rs:Language"
                how_to_apply:     "Match exhaustively on Language enum for grammar selection"
            },
            {
                pattern:          "Error Factory Methods"
                example_location: "crates/factory-core/src/error.rs:Error::stage_failed()"
                how_to_apply:     "Add helper methods for common error construction"
            },
        ]

        dependencies: [
            {
                bead:   "factory-khq"
                type:   "blocks"
                reason: "StageTable refactor provides StageHandler trait that TodoDetector must integrate with"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use tree-sitter 0.24 - matches current Rust ecosystem conventions",
            "Lazy-load grammars only when needed (OnceCell or lazy_static)",
            "Use WalkDir with filter_entry for efficient directory traversal",
            "Cache parsed trees if same file scanned multiple times",
            "Use rayon for parallel file processing on large codebases",
            "Extract complete comment text with node.utf8_text(source.as_bytes())",
            "Use regex for TODO/FIXME/XXX/HACK pattern matching within comments",
            "Return partial results even if some files fail to parse",
            "Add #[must_use] to TodoReport",
            "Document with examples in doc comments",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT use grep or shell out to external commands",
            "Do NOT parse binary files",
            "Do NOT include node_modules, target, or hidden directories",
            "Do NOT flag TODO in string literals or identifiers",
            "Do NOT fail entire scan if one file has parse errors",
        ]

        code_patterns: [
            {
                name:     "TodoDetector with Language"
                use_when: "Creating the main detector struct"
                example:  """
                    pub struct TodoDetector {
                        language: Language,
                        parser: RefCell<tree_sitter::Parser>,
                    }

                    impl TodoDetector {
                        pub fn new(language: Language) -> Result<Self> {
                            let mut parser = tree_sitter::Parser::new();
                            let grammar = load_grammar(language)?;
                            parser.set_language(&grammar)
                                .map_err(|e| Error::grammar_init_failed(language, e))?;

                            Ok(Self {
                                language,
                                parser: RefCell::new(parser),
                            })
                        }
                    }
                    """
            },
            {
                name:     "Comment Extraction with tree-sitter"
                use_when: "Walking the AST to find comments"
                example:  """
                    fn extract_comments(&self, tree: &Tree, source: &[u8]) -> Vec<CommentNode> {
                        let mut cursor = tree.walk();
                        let mut comments = Vec::new();
                        let comment_kinds = comment_kinds(self.language);

                        loop {
                            let node = cursor.node();
                            if comment_kinds.contains(&node.kind()) {
                                if let Ok(text) = node.utf8_text(source) {
                                    comments.push(CommentNode {
                                        text: text.to_string(),
                                        line: node.start_position().row + 1,
                                        column: node.start_position().column + 1,
                                    });
                                }
                            }

                            // Depth-first traversal
                            if cursor.goto_first_child() { continue; }
                            while !cursor.goto_next_sibling() {
                                if !cursor.goto_parent() { return comments; }
                            }
                        }
                    }
                    """
            },
            {
                name:     "TODO Marker Detection"
                use_when: "Finding markers within comment text"
                example:  """
                    use regex::Regex;
                    use std::sync::LazyLock;

                    static TODO_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
                        Regex::new(r"(?i)\\b(TODO|FIXME|XXX|HACK)\\b:?\\s*(.*)").unwrap()
                    });

                    fn find_markers(comment: &str) -> impl Iterator<Item = (TodoMarker, String)> + '_ {
                        TODO_PATTERN.captures_iter(comment).filter_map(|cap| {
                            let marker = match cap.get(1)?.as_str().to_uppercase().as_str() {
                                "TODO" => TodoMarker::Todo,
                                "FIXME" => TodoMarker::Fixme,
                                "XXX" => TodoMarker::Xxx,
                                "HACK" => TodoMarker::Hack,
                                _ => return None,
                            };
                            let text = cap.get(2).map(|m| m.as_str().to_string()).unwrap_or_default();
                            Some((marker, text))
                        })
                    }
                    """
            },
            {
                name:     "Directory Walker with Exclusions"
                use_when: "Scanning files in a directory tree"
                example:  """
                    use walkdir::{DirEntry, WalkDir};

                    const SKIP_DIRS: &[&str] = &[
                        "target", "node_modules", "__pycache__", "build",
                        ".git", ".hg", ".svn", "vendor", "dist",
                    ];

                    fn should_skip(entry: &DirEntry) -> bool {
                        entry.file_type().is_dir() && entry.file_name()
                            .to_str()
                            .map(|s| s.starts_with('.') || SKIP_DIRS.contains(&s))
                            .unwrap_or(false)
                    }

                    fn walk_source_files(dir: &Path, ext: &str) -> impl Iterator<Item = PathBuf> {
                        WalkDir::new(dir)
                            .into_iter()
                            .filter_entry(|e| !should_skip(e))
                            .filter_map(|e| e.ok())
                            .filter(|e| e.file_type().is_file())
                            .filter(move |e| e.path().extension().is_some_and(|e| e == ext))
                            .map(|e| e.into_path())
                    }
                    """
            },
        ]
    }
}
