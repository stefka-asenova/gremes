

#' Generic for \code{flowConnection}
#' @rdname flowConnection
flowConnection<- function(obj, ...)
{
  UseMethod("flowConnection")
}




#' @rdname flowConnection
#' @export
flowConnection.FlowConnectionMatrix<- function(obj, g, vertices = rownames(obj),...)
{
  return(obj)
}



#' @rdname flowConnection
#' @export
flowConnection.FlowConnectionGraph<- function(obj,
                                              vertices = get.vertex.attribute(obj, "name", V(obj)),
                                              ...)
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





