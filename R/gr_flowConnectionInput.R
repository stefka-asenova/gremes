

#' Object of class \code{FlowConnectionGraph}
#'
#' It is used as an input to create subsets based on the criterion of flow connectedness.
#' Within a subset all nodes will be flow connected.
#' @export
#' @param g should be an object of class "igraph"
#' @examples
#' seg_dir<- graph(c("2","paris", "meaux", "2", "melun", "2","5", "melun","nemours", "5", "sens", "5"),
#'  directed = TRUE)
#' fcg<- FlowConnectionGraph(seg_dir)
FlowConnectionGraph<- function(g)
{
  class(g)<- append(class(g), "FlowConnectionGraph")
  g<- validate(g)
  return(g)
}






#' Object of class \code{FlowConnectionMatrix}
#'
#' It is used as an input to create subsets based on the criterion of flow connectedness.
#' Within a subset all nodes will be flow connected.
#' @param x should be a matrix of ones and zeros, 1 representing flow connected ness between node i and node j.
#' Note that x can not be symmetric.
#' @param g is an igraph object representing a directed graph corresponding to the flow connectedness.
#' @export
FlowConnectionMatrix<- function(x, g)
{
  class(x)<- append(class(x), "FlowConnectionMatrix")
  x<- validate(x, g)
  return(x)
}





