#This script contains implementation of shortest path in oriented graph finding algorithms
library("collections")
#One recursive iteration of ford fulkerson algorithm
ford_fulkerson_iteration =function(grph, start, end, shortest_paths) {
  #All predecessors of end
  end_pred = grph[,end]
  min = Inf
  for (i in 1:length(end_pred)) {
    #Connection does not exist
    if (end_pred[i] == 0) {
      next
    }
    #If current shortest path is not yet known -> we recursively call to construct it
    if (is.nan(shortest_paths[i])) {
      #Find shortest path from start to i
      #Dynamic updating of shortest paths helps us to dynamically update previously calculated paths, thus speeding us up
      shortest_paths = ford_fulkerson_iteration(grph, start, i, shortest_paths);
    }
    #If we found a shorter path - we update the value
    if ((shortest_paths[i] + end_pred[i]) < min) {
      min = end_pred[i] + shortest_paths[i] 
    }
  }
  shortest_paths[end] = min
  return(shortest_paths)
}

#Given adjacency matrix of a graph, return shortest path from start to end
ford_fulkerson = function(grph, start, end) {
  print(paste("calculating path from ", start, " ", end))
  #Vector of all shortest path to our target 
  shortest_paths=rep(NaN, nrow(grph))
  shortest_paths[1] = 0
  return(ford_fulkerson_iteration(grph, start, end, shortest_paths)[end])
}


#Given adjacency matrix of a graph, return shortest path from start to end
#Faster than Ford Fulkerson
dijkstra = function(grph, start, end) {
  shortest_paths=rep(Inf, nrow(grph))
  shortest_paths[start] = 0
  to_visit = priority_queue
  current_node = NaN
  
  #Empty the queue until it is empty or we reached the end
  while (to_visit$size() != 0 || current_node == end) {
    current_node = to_visit$pop()
    #Neighbours are in the column of the matrix 
    neighbours = grph[,current_node]
    for (i in range(1:length(neighbours))) {
      #The connnection does not exist - skip neighbour
      if (neighbours[i] == 0) {
        next
      }
    }
  }
}

test_adjacency = matrix(c(0,2,1,0, 0,0,0,5, 0,0,0,3, 0,0,0,0), nrow=4, ncol=4, byrow=TRUE)
print(ford_fulkerson(test_adjacency, 1, 4))
print(dijkstra(test_adjacency, 1, 4))