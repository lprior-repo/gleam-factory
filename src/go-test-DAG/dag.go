package dag

import "errors"

var ErrCycle = errors.New("cycle detected in DAG")

type Task struct {
	ID   string
	Deps []string
	Fn   func() error
}

type Result struct {
	TaskID string
	Error  error
}

type DAG struct {
	Tasks map[string]*Task
}

func NewDAG() *DAG {
	return &DAG{Tasks: make(map[string]*Task)}
}

func (d *DAG) AddTask(id string, deps []string, fn func() error) {
	d.Tasks[id] = &Task{ID: id, Deps: deps, Fn: fn}
}

func (d *DAG) Execute() ([]Result, error) {
	order, err := d.topologicalSort()
	if err != nil {
		return nil, err
	}

	var results []Result
	for _, id := range order {
		task := d.Tasks[id]
		err := task.Fn()
		results = append(results, Result{TaskID: id, Error: err})
		if err != nil {
			return results, nil
		}
	}
	return results, nil
}

func (d *DAG) topologicalSort() ([]string, error) {
	inDegree := make(map[string]int)
	adj := make(map[string][]string)

	for id := range d.Tasks {
		inDegree[id] = 0
	}

	for id, task := range d.Tasks {
		for _, dep := range task.Deps {
			adj[dep] = append(adj[dep], id)
			inDegree[id]++
		}
	}

	var queue []string
	for id, deg := range inDegree {
		if deg == 0 {
			queue = append(queue, id)
		}
	}

	var result []string
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		result = append(result, current)

		for _, neighbor := range adj[current] {
			inDegree[neighbor]--
			if inDegree[neighbor] == 0 {
				queue = append(queue, neighbor)
			}
		}
	}

	if len(result) != len(d.Tasks) {
		return nil, ErrCycle
	}

	return result, nil
}
