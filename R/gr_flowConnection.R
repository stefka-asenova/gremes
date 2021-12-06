

#' Creates a matrix of flow connectedness for a tree
#'
#' Designed for use on river network and its representation as a tree. It is used on object of class
#' \code{FlowConnectionGraph}.
#'
#' As input it takes a directed tree according to the flow connection and the output is a matrix of 1's and 0's
#' 1 meaning flow connectedness between node i and node j.
#' @export
#' @rdname flowConnection
#' @param obj object of class \code{FlowConnectionGraph}
#' @param ... additional arguments
#' @examples
#' g<- graph(c(1,2,3,2, 2,4,4,5), directed=TRUE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d", "e"))
#' fcg<- FlowConnectionGraph(g)
#' flowConnection(fcg)
flowConnection<- function(obj, ...)
{
  UseMethod("flowConnection")
}




#' @export
flowConnection.FlowConnectionMatrix<- function(obj, ...)
{
  return(obj)
}



#' @rdname flowConnection
#' @export
flowConnection.FlowConnectionGraph<- function(obj, ...)
{
  # a matrix of 1 and 0 indicating whether node i is flow connected with node j
  # if matrixForm=TRUE it returns a matrix if not it returns a list
  # vertices - for each of the elements in "vertices" the set of Flow connected vertices  is obtained
  # g must be a DIRECTED graph corresponding to the direction of the flow


  U<- get.vertex.attribute(obj, "name", V(obj))

  outputObj<- matrix(0, vcount(obj), vcount(obj))
  colnames(outputObj)<- rownames(outputObj)<- U
  for (v in U)
  {
    paths<- all_shortest_paths(obj, from=v, mode = "out")
    set<- unique(unlist(paths$res))
    set<- get.vertex.attribute(obj, "name", set)
    outputObj[v, set]<- 1
    outputObj[set, v]<- 1
  }

  return(outputObj)
}





