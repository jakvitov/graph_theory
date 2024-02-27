library(igraph)

#Plot given graph using the given matrix
plot_graphs = function(g1, g2) {
  grph1 <- graph_from_adjacency_matrix(!(g1 == 0))
  grph2 <- graph_from_adjacency_matrix(!(g2 == 0))
  layout <- layout_in_circle(grph1)
  layout <- layout_in_circle(grph2)
  
  par(mfrow = c(1, 2))
  plot(grph1, main = paste("Weights: ", sum(g1)/2))
  plot(grph2, main = paste("Weights: ", sum(g2)/2))

}

#Return minimal coordinates vector in submatrix or rows and cols
min_in_submatrix = function(mat, rows, cols) {
  min = mat[rows[1], cols[1]]
  coord = c(rows[1],cols[1])
  for (i in rows) {
    for (j in cols) {
      current = mat[i, j]
      if (min > current && (i != j)) {
        min = current
        coord = c(i, j)
      }
    }
  }
  return(coord)
}

#Return matrix representing graph, that is a minimal spanning tree of
#the graph represented by input matrix
minimal_spanning_tree = function(graph) {
  #Empty result
  res = matrix(rep(0, nrow(graph) * ncol(graph)), nrow=nrow(graph), ncol=ncol(graph))
  unselected = seq(2,ncol(graph), by=1)
  selected = c(1)
  #While not all vertices are added we continue
  while (!length(unselected) == 0) {
    #Choose minimal value edge, that has one node in selected and other in uselected
      next_node = min_in_submatrix(graph, selected, unselected)
      selected = append(selected, next_node[2])
      print(selected)
      #Remove selected column from unselected
      unselected = unselected[!unselected == next_node[2]]
      res[next_node[1], next_node[2]] = graph[next_node[1], next_node[2]]
      res[next_node[2], next_node[1]] = res[next_node[1], next_node[2]]
  }
  plot_graphs(graph, res)
  return(res)
}


full_graph=matrix(c(0,7,5,4,8,  7,0,1,3,8, 5,1,0,2,7,  4,3,2,0,5,  8,8,7,5,0), nrow=5, ncol=5, byrow=TRUE)
spt = minimal_spanning_tree(full_graph)
