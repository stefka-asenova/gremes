#' Generic for \code{setGraph}
setGraph<- function(obj, ...)
{
  UseMethod("setGraph", obj)
}







#' Default method for \code{setGraph}
#'
#' @return Returns warning if an unrecognised object is passed as an argument
#' @return It returns the argument that was called on
setGraph.default<- function(obj,...)
{
  warning("Default method is called on unrecognised object")
  return(obj)
}








#' Setter for class \code{Network}
#'
#' @param obj Object of class \code{Network}
#' @return The object
#'
setGraph.Network<- function(obj, x, ...)
{
  obj$graph<- x
  return(obj)
}





#' @export
#' @param obj Object of class \code{RootDepSet}
#' @param subset One dimensional array or list with subsets on the set of vertices, referred by their names.
#' @param root One dimensional array or a scalar of the root(s), corresponding to the subset(s) above
#' referred by name.
#' @rdname setRootDepSet
#' @title Setter for \code{RootDepSet}
#' @description It sets the values of the two slots in the object.
#' @examples
#' rds<- RootDepSet()
#' rds_values<- list(a=c("a", "b"), b=c("b", "c", "d"), c=c("c", "d"), d=c("c", "d", "e"), e=c("d", "e"))
#' rds_roots<- c("a", "b","c","d","e")
#' rds<- setRootDepSet(rds,rds_values, rds_roots )
setRootDepSet<- function(obj, subset, root, ...)
{
  UseMethod("setRootDepSet", obj)
}





#' @export
setRootDepSet.default<- function(obj, subset, root, ...)
{
  return("Default method called on unrecognized object")
}




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
  obj$root<- root
  message("From setRootDepSet.RootDepSet: The order of the subset must correspond to the order of its corresponding root")
  return(obj)
}





#' Sets the parameters attached to the edges of the tree
#'
#' @param value A named vector with names corresponding to the names of the edges
#' @export
#' @import igraph
setParams<- function(obj, value, ...)
{
  UseMethod("setParams", obj)
}

#' @export
setParams.default<- function(obj, value, ...)
{
  return("Default method called on unrecognized object")
}



#' @export
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




#' @export
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





resetParams<- function(obj, ...)
{
  UseMethod("resetParams", obj)
}


resetParams.MLE1params<- function(obj, cnames)
{
  x<- rep(1,length(cnames))
  names(x)<- cnames
  class(x)<- class(obj)
  return(x)
}

