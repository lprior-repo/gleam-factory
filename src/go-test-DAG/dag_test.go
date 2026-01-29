package dag

import (
	"testing"
)

func TestHappyPath_SimpleLinearDAG(t *testing.T) {
	var order []string

	d := NewDAG()
	d.AddTask("a", nil, func() error { order = append(order, "a"); return nil })
	d.AddTask("b", []string{"a"}, func() error { order = append(order, "b"); return nil })
	d.AddTask("c", []string{"b"}, func() error { order = append(order, "c"); return nil })

	results, err := d.Execute()
	if err != nil {
		t.Fatalf("expected nil error, got %v", err)
	}

	if len(results) != 3 {
		t.Fatalf("expected 3 results, got %d", len(results))
	}

	expected := []string{"a", "b", "c"}
	if !equalSlices(order, expected) {
		t.Fatalf("expected order %v, got %v", expected, order)
	}
}

func equalSlices(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
