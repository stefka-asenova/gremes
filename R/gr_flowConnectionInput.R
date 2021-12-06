

#' Object of class \code{FlowConnectionGraph}
#'
#' It creates an object from class \code{FlowConnectionGraph}. The function verifies whether the graph is
#' directed and the vertices are named. The object can be used further to create subsets based on the criterion of
#' flow connectedness. See Vignette "Subsets and Coordiantes".
#' @export
#' @param g should be an object of class "igraph". It should have named vertices.
#' @examples
#' seg_dir<- graph(c("2","paris", "meaux", "2", "melun", "2","5", "melun","nemours", "5", "sens", "5"),
#'  directed = TRUE)
#' fcg<- FlowConnectionGraph(seg_dir)
#' rds<- FlowConnect()
#'
#' # call method subset to create subsets
#' subset(rds, fcg, seg_dir)
FlowConnectionGraph<- function(g)
{
  class(g)<- append(class(g), "FlowConnectionGraph")
  g<- validate(g)
  return(g)
}





#' Object of class \code{FlowConnectionMatrix}
#'
#' It creates an object of class \code{FlowConnectionMatrix}. Such an object is used as an input to
#' create subsets based on the criterion of flow connectedness. See Vignette "Subsets and Coordiantes".
#' Within a subset all nodes will be flow connected.
#' @param x should be a matrix of ones and zeros, 1 representing flow connectedness between node i and node j.
#' The matrix should have named rows and columns.
#' Note that x is symmetric.
#' @param g is an igraph object representing the graph. It should have named vertices.
#' @export
#' @examples
#' # create a graph and name the vertices
#' g<- graph(c(1,2,3,2, 2,4,4,5), directed=TRUE)
#' g<- set.vertex.attribute(g, "name", V(g), letters[1:5])
#'
#' # create the flow connection matrix if not available
#' x<- matrix(rep(1,25), 5, 5)
#' x[1,3]<- x[3,1]<- 0
#' colnames(x)<- rownames(x)<- letters[1:5] # note that the columns and rows should be named according to the nodes
#'
#' # create the object of class 'FlowConnectionMatrix'
#' fcmat<- FlowConnectionMatrix(x,g)
#' fcmat
#' rds<- FlowConnect()
#' sets<- subset(rds, from = fcmat, g)
FlowConnectionMatrix<- function(x, g)
{
  class(x)<- append(class(x), "FlowConnectionMatrix")
  x<- validate(x, g)
  return(x)
}





