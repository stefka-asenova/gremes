#' \code{Network} class
#'
#' Class definition
#' \code{Network} is a superclass
#'
#' @param x An object of class \code{igraph}
#' @param data Data set associated to the graph
#' @export Network
#' @return An object of class \code{Network}
Network<- function(x, data)
{
  obj<- list(graph=x,
             data=data,
             nodesWithData=colnames(data),
             noDataNodes= setdiff(get.vertex.attribute(x, "name", V(x)), colnames(data)))
  class(obj)<-  "Network"
  return(obj)
}




#' \code{Tree} class
#'
#' Class definition
#' @param x An object of class \code{igraph}
#' @param data Data set associated to the graph
#' @export Tree
#' @return An object of class \code{Tree}
Tree<- function(x, data)
{

  obj<- Network(x, data)
  class(obj)<- append(class(obj), "Tree")
  obj<- validate(obj)
  return(obj)
}





#' \code{GTree} class
#'
#' Class definition
#' It is used to differentiate from the CovSelectionTree class ???
#' Add more comment
#' @export GTree
GTree<- function(g, Data)
{
  obj<- Tree(x = g, data = Data)
  class(obj)<- append(class(obj), "GTree")
  return(obj)

}






#' @export CovSelectTree
CovSelectTree<- function(g, Data)
{
  obj<- Tree(g, Data)
  class(obj)<- append(class(obj), "CovSelectTree")
  return(obj)
}






BlockGraph<- function(x, data)
{
  # debug
  #x<- g
  #data<- X
  #-------------

  obj<- Network(x, data)
  class(obj)<- append(class(obj), "BlockGraph")
  obj<- validate(obj)
  return(obj)
}


