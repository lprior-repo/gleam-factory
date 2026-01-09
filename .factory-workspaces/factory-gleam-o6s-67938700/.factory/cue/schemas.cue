// Runtime validation schemas for AI outputs - COMPACT FORMAT
// Validate: echo '{"s":"tw",...}' | cue vet schemas.cue -d '#AuditorOutput' -
package tcr

// Auditor output - compact keys
#AuditorOutput: {
	s: "tw" | "rc" | "e"  // status: test_written, requirements_complete, error

	if s == "tw" {
		t: {  // test
			n: =~"^[a-z][a-z0-9_]*_test$"  // name
			f: =~"^test/.*\\.gleam$"        // file
			b: string & !=""               // behavior/target
		}
	}

	r?: string  // reason (optional)
}

// Implementer output
#ImplementerOutput: {
	s: "ok" | "bl" | "e"  // status: ok, blocked, error

	if s == "ok" {
		c: [...{  // changes
			f: =~"^src/.*\\.gleam$"  // file
			a: "+" | "~"            // action: add, modify
			l: int & >0             // lines
		}]
	}

	if s == "bl" {
		b: string & !=""  // blocker
	}

	r?: string
}

// Architect output
#ArchitectOutput: {
	s: "rf" | "nc" | "e"  // status: refactored, no_changes, error

	if s == "rf" {
		lb: int & >=0  // loc_before
		la: int & >=0  // loc_after

		c: [...{  // changes
			t: "d" | "s" | "x" | "r" | "i"  // type: delete, simplify, extract, rename, inline
			w: string & !=""                // what/target
		}]
	}

	// code smells
	sm?: [...{
		t: string & !=""  // title
		d: string & !=""  // description
		p: 0 | 1 | 2 | 3  // priority
	}]
}

// Reviewer output
#ReviewerOutput: {
	s: "p" | "f"  // status: passed, failed
	m: bool       // acceptance_criteria_met

	v: [...{  // verification
		c: string & !=""  // criterion
		m: bool           // met
		e: string         // evidence
	}]

	l?: string  // learnings
}
