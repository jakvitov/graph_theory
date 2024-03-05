#This script contains implementation of shortest path in oriented graph finding algorithms

#Given adjacency matrix of a graph, return shortest path from start to end
ford_fulkerson = function(grph, start, end) {
  if (!is.numeric(start) || !is.numeric(end)){
    stop("Start and end must be numeric")
  }
  #Vector of all shortest path to our target 
  shortest_paths=rep(NaN, nrow(grph))
  #All predecessors of end
  end_pred = grph[,end]
  min = -Inf
  for (i in end_pred) {
    #Connection does not exist
    if (i == 0) {
      next
    }
    #If current shortest path is not yet known -> we recursively call to construct it
    if (is.nan(shortest_paths[i])) {
      shortest_paths[i] = ford_fulkerson(grph, start, i)
    }
    #If we found a shorter path - we update the value
    if ((shortest_paths[i] + end_pred[i]) < min) {
      min = end_pred[i] + shortest_paths[i] 
    }
  }
  shortest_paths[end] = min
  return(min)
}

test_adjacency = matrix(c(0,2,1,0, 0,0,0,5, 0,0,3,0, 0,0,0,0), nrow=4, ncol=4, byrow=TRUE)
print(ford_fulkerson(test_adjacency, 1, 4))