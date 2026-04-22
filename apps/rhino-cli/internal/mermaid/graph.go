package mermaid

// rankAssign runs Kahn's BFS longest-path rank assignment.
// Returns map[nodeID]rank. Nodes in cycles get rank 0 (fallback).
func rankAssign(nodes []Node, edges []Edge) map[string]int {
	if len(nodes) == 0 {
		return map[string]int{}
	}

	// Build index of node IDs to detect which IDs are valid.
	nodeSet := make(map[string]bool, len(nodes))
	for _, n := range nodes {
		nodeSet[n.ID] = true
	}

	// Build adjacency list (out-edges) and in-degree for valid nodes only.
	adj := make(map[string][]string, len(nodes))
	inDegree := make(map[string]int, len(nodes))
	for _, n := range nodes {
		adj[n.ID] = nil
		inDegree[n.ID] = 0
	}
	for _, e := range edges {
		if nodeSet[e.From] && nodeSet[e.To] {
			adj[e.From] = append(adj[e.From], e.To)
			inDegree[e.To]++
		}
	}

	rank := make(map[string]int, len(nodes))
	visited := make(map[string]bool, len(nodes))

	// Kahn's BFS: start with all nodes that have in-degree 0.
	queue := make([]string, 0, len(nodes))
	for _, n := range nodes {
		if inDegree[n.ID] == 0 {
			queue = append(queue, n.ID)
			rank[n.ID] = 0
		}
	}

	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		visited[cur] = true

		for _, next := range adj[cur] {
			// Longest-path rank: update if going through cur gives a longer path.
			if rank[cur]+1 > rank[next] {
				rank[next] = rank[cur] + 1
			}
			inDegree[next]--
			if inDegree[next] == 0 {
				queue = append(queue, next)
			}
		}
	}

	// Cycle fallback: assign rank 0 to all unvisited nodes.
	for _, n := range nodes {
		if !visited[n.ID] {
			rank[n.ID] = 0
		}
	}

	return rank
}

// MaxWidth returns the maximum number of nodes sharing the same rank.
// Returns 0 for empty graphs.
func MaxWidth(nodes []Node, edges []Edge) int {
	if len(nodes) == 0 {
		return 0
	}

	ranks := rankAssign(nodes, edges)

	// Count nodes per rank.
	rankCount := make(map[int]int)
	for _, r := range ranks {
		rankCount[r]++
	}

	maxW := 0
	for _, cnt := range rankCount {
		if cnt > maxW {
			maxW = cnt
		}
	}
	return maxW
}

// Depth returns the number of distinct rank values (longest path length + 1).
// Returns 0 for empty graphs.
func Depth(nodes []Node, edges []Edge) int {
	if len(nodes) == 0 {
		return 0
	}

	ranks := rankAssign(nodes, edges)

	distinct := make(map[int]bool)
	for _, r := range ranks {
		distinct[r] = true
	}
	return len(distinct)
}
