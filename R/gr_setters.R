

setGraph<- function(obj, ...)
{
  UseMethod("setGraph")
}






#' @export
setGraph.default<- function(obj,...)
{
  warning("Default method is called on unrecognized object")
  return(obj)
}







#' @export
setGraph.Network<- function(obj, x, ...)
{
  obj$graph<- x
  return(obj)
}





#' Setter for objects of class \code{RootDepSet}
#'
#' It is used to set the slots \code{$value} and \code{$root} of an object of class \code{RootDepSet} and
#' its subclasses, such as \code{Neighborhood} and \code{FlowConnect}. Every node in the graph with observable
#' variable
#' must be taken as a root. For a fixed root, a subset of nodes must be chosen.
#' @param obj Object of class \code{RootDepSet}
#' @param subset list containing the subsets on the set of vertices, referred by their names.
#' An element of the list should
#' be named with the name of the corresponding root to that subset.
#' @param root One dimensional array of the roots, corresponding to the subsets above
#' referred by name.
#' @param ... additional arguments
#' @rdname setRootDepSet
#' @export
#' @examples
#' rds<- RootDepSet()
#' rds_values<- list(a = c("a", "b"), b = c("b", "c", "d"))
#' rds_roots<- c("a", "b")
#' rds<- setRootDepSet(rds, rds_values, rds_roots)
setRootDepSet<- function(obj, subset, root, ...)
{
  UseMethod("setRootDepSet", obj)
}





#' @export
setRootDepSet.default<- function(obj, subset, root, ...)
{
  return("Default method called on unrecognized object")
}



#' @rdname setRootDepSet
#' @export
setRootDepSet.RootDepSet<- function(obj, subset, root, ...)
{
  if (is.atomic(subset)&& is.null(dim(subset)))
  {
    if (!is.character(subset))
      stop(" 'subset' must be a character vector")
    if(!is.character(root))
      stop(" 'root' must be a character")
    if (! (root %in% subset))
      stop("the root must be part of the subset")
  } else if (is.list(subset))  {
    vld<- sapply(subset, function(x) is.character(x))
    if(sum(!vld)!=0)
      stop(" 'subset' must be a list of character vectors, with characters corresponding to the names of the nodes ")
    if(!is.character(root))
      stop(" 'root' must be a character")
    for (i in 1:length(subset))
    {
      if (! (root[i] %in% subset[[i]]))
        stop("the root must be part of the subset")
    }
  } else stop("'subset' must be either one dimensional array or a list")

  obj$value<- subset
  names(obj$value)<- root
  obj$root<- root
  message("From setRootDepSet.RootDepSet: The order of the subset must correspond to the
            order of its corresponding root")
  return(obj)
}





#' Sets edge parameters
#'
#'
#' It assigns values to the vector of edge weights in the slot \code{$depParams} of object of class
#' \code{HRMnetwork} and its subclasses.
#' @param obj An object of class \code{HRMnetwork} or its subclasses \code{HRMtree, MME, MLE, MLE1, MLE2, EKS,
#' EKS_part, EngHitz, HRMBG, MMEave, MLEave}
#' @param value A named vector with names corresponding to the names of the edges.
#' The vector represents the edge weights: thetas in case of models on trees and deltas in case of
#' models on block graphs. See Vignette "Introduction" also. If an unnamed vector is passed the first element in
#' the vector is assigned to the first edge, the second element to the second edge and so on.
#' @param ... additional arguments
#' @export
#' @rdname setParams
#' @import igraph
setParams<- function(obj, value, ...)
{
  UseMethod("setParams")
}






#' @export
setParams.default<- function(obj, value, ...)
{
  return("Default method called on unrecognized object")
}




#' @rdname setParams
#' @export
#' @examples
#' # model on a block graph with three edge parameters
#' g<- graph(c(1,2,2,3, 3,1), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes
#' obj<- HRMBG(g)
#' obj<- setParams(obj, c(0.2,0.3, 0.1))
#' obj
setParams.HRMnetwork<- function(obj, value,...)
{

  g<- obj$graph
  ne<- get.edge.attribute(g, "name", E(g))
  if (length(value)<length(E(g)))
    stop("The number of parameters is smaller than the number of edges")
  if (is.null(names(value)))
  {
    names(value) <- get.edge.attribute(g, "name", E(g))
    message("From setParams.HRMtree: Names have been attributed to the vector 'value' in the order corresponding to the order of the edges: The fist element has the name of the first edge, the second element the name of the second edge, etc.")
  }
  obj$depParams[ne]<- value[ne]

  message("From setParams.HRMtree: The parameters have been attached to the edges according to their names")

  # if the names of the vector 'value' don't match the names of the parameters ???

  return(obj)
}





#' @rdname setParams
#' @export
#' @examples
#' # model on a tree with two edge parameters
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a" ,"b" ,"c")) # name the nodes
#' obj<- HRMtree(g)
#' obj<- setParams(obj, c(0.2,0.3))
#' obj
#' x<- c(0.2, 0.3)
#' names(x)<- names(obj$depParams)
#' obj<- setParams(obj, x)
#' obj
setParams.HRMtree<- function(obj, value,...)
{
  g<- obj$graph
  ne<- get.edge.attribute(g, "name", E(g))
  if (length(value)<length(E(g)))
    stop("The number of parameters is smaller than the number of edges")
  if (is.null(names(value)))
  {
    names(value) <- get.edge.attribute(g, "name", E(g))
    message("From setParams.HRMtree: Names have been attributed to the vector 'value' in the order corresponding to the order of the edges: The fist element has the name of the first edge, the second element the name of the second edge, etc.")
  }
  obj$depParams[ne]<- value[ne]

  message("From setParams.HRMtree: The parameters have been attached to the edges according to their names")

  # if the names of the vector 'value' don't match the names of the parameters ???

  return(obj)
}




# don't export
resetParams<- function(obj, ...)
{
  UseMethod("resetParams")
}



# don't document
#' @export
resetParams.MLE1params<- function(obj, cnames)
{
  x<- rep(1,length(cnames))
  names(x)<- cnames
  class(x)<- class(obj)
  return(x)
}

