#' @title Dijkstra Function
#'
#' @description Provides the most efficient path along all the edges of a graph
#'
#' @param graph \code{data.frame} with 3 columns v1, v2 & w
#' @param y integer value that should be a valid vertex in the graph
#'
#' @return A resultant \code{data.frame} with the lowest computed cost for each vertex from the starting node
#'
#' @examples
#' dijkstra(wiki_graph, 2)
#'
#' @importFrom dplyr '%>%'
#'
#' @export dijkstra
#'
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
dijkstra <- function (graph, init_node) {

  #All the points
  v1 <- unique(graph$v1)

  working_graph <- data.frame(v1)

  # Set the cost of all points to infinity
  working_graph$cost <- Inf

  # Set the state of all nodes to Temp
  working_graph$state <- "temp"

  #Set the cost of the current node we are at  == 0
  working_graph$cost[working_graph$v1 == init_node] <- 0


  # Set the state of the node we are currently at to Permanent
  working_graph$state[working_graph$v1 == init_node] <- "permanent"


  # Set the current location to i to loop stuff
  i <- init_node
  current_min_distance = 0

  #initiate loop
  repeat {

    # Working_Graph_loop are all the Points except the one we started at
    working_graph_loop <- subset(working_graph, state == "temp")
    colnames(working_graph_loop)[colnames(working_graph_loop) == "v1"] <- "v2"

    # all the vertices & costs connected to the active vertex
    connected_nodes <- subset(graph, v1 == i)

    # drop the first column of the data frame because it will mess up merging
    connected_nodes <- connected_nodes[,-1]

    # appends the cost of the Nodes which are connected to the current location

    working_graph_loop <- merge(working_graph_loop, connected_nodes, by = "v2", all.x = TRUE)

    # replace NA with Inf
    working_graph_loop[is.na(working_graph_loop)] <- Inf

    #working_graph_loop1 <- data.frame(working_graph_loop)

    #update minimum cost to each node
    working_graph_loop$cost <- with(working_graph_loop, pmin(cost,w+current_min_distance))

    #update state of the min node to permanent
    working_graph_loop$state[which.min(working_graph_loop$cost)] <- "permanent"

    #append result of this loop to the resultant DF
    working_graph <- data.frame(working_graph)
    working_graph <- merge(working_graph, working_graph_loop, by.x = "v1", by.y = "v2", all.x = TRUE)
    working_graph <- working_graph %>% mutate(state = ifelse(is.na(state.y) == FALSE, state.y, state.x))
    working_graph <- working_graph %>% mutate(cost = ifelse(is.na(state.y) == FALSE, cost.y, cost.x))
    working_graph <- working_graph[,-2:-6]

    # Set the Current node to the node corresponding to the lowest distance from the previous current node
    current_node <- working_graph_loop$v2[working_graph_loop$state == "permanent"]

    #update current node location in i
    i <- current_node

    #store min distance for next loop
    current_min_distance = min(working_graph_loop$cost)

    if(length(unique(working_graph$state)) == 1) break

  }
  final_result <- working_graph[, c("v1", "cost")]
  return(final_result)
}
