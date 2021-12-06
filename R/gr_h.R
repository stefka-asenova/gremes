
# don't export the generic


#' @title Number of PoT variables in a subset
h<- function(obj,...)
{
  UseMethod("h")
}



#' @export
h.default<- function(obj,...)
{
  return("Default method called on unrecognized object")
}







#' @description Computes the h_u, which is
#' the number of the elements in the subset after substracting the root and the nodes for which data
#' are missing
#' @rdname h
#' @param obj Object of class \code{RootDepSet}
#' @param U_bar The set of nodes for which data are missing, as character vector
#' @param ... additional arguments
#' @export
h.RootDepSet<- function(obj, U_bar=NULL, ...)
{
  # the purpose of this function is to facilitate the preallocation of
  # the sigma matrix and the data matrix. So instead of growing matrices
  # in a loop I am using this function to obtain h_u for all u's
  roots<- getRoot(obj)
  values<- getValue(obj)


  if (is.list(values))
  {
    h<- numeric(length(roots))
    for (i in 1:length(values))
    {
      h[i]<- length(base::setdiff(values[[i]], base::union(roots[i], U_bar)))
    }

  } else  {h<- length(base::setdiff(values, union(roots, U_bar)))}
  return(h)
}







h_edges<- function(obj, ...)
{
  UseMethod("h_edges")
}




#' @export
h_edges.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}





#' @export
h_edges.RootDepSet<- function(obj, g)
{
  # the purpose of this function is to facilitate the preallocation of
  # the sigma matrix and the data matrix. So instead of growing matrices
  # in a loop I am using this function to obtain h_u for all u's
  roots<- getRoot(obj)
  values<- getValue(obj)
  if (is.list(values))
  {
    h<- numeric(length(roots))
    for (i in 1:length(values))
    {
      g_set<- spanned_subgraph(g, roots[i], values[[i]])
      h[i]<- ecount(g_set)
    }

  } else  {h<- ecount(spanned_subgraph(g, roots, values))}
  return(h)
}
