#' Generic function for method \code{validate}
#'
#' Creates a generic function that validates an object
#' @param obj The object that needs to be validated
#' @param ... Additional arguments
#' @import igraph
validate<- function(obj,...)
{
  UseMethod("validate")
}





#' @export
validate.default<- function(obj, ...)
{
 # warning("Default method is called on unrecognized object")
  return(obj)
}






#' @export
validate.Network<- function(obj, ...)
{
  g<- getGraph(obj)
  Data<- getData(obj)
  if (is.null(colnames(Data)))
    stop("'Data' must have named columns")
  # if (is.null(rownames(Data)))
  # {
  #  rownames(Data)<- 1:nrow(Data)
  # }
  if ( length(unique(colnames(Data))) != length(colnames(Data)) )
    stop("'Data' contains certain variable(s) more than once")
  if ( !is.igraph(g) )
    stop("'g' must be an igraph object")
  if (is.directed(g))
    message("From validate.Network: 'g' is directed graph")
  if (!is.named(g))
    stop("Names should be attributed to the vertices of 'g'
         Use the same names as the names of the variables in 'Data'")
  if ( is.null(names(E(g))) )
  {
    g<- set.edge.attribute(g, "name", E(g), paste0("e",E(g)))
    obj<- setGraph(obj,g)
    message("From validate.Network: Edges have been assigned names")
  }
  if ( length(setdiff(colnames(Data),names(V(g))))!=0 )
    stop("The names of the variables in 'Data' do not correspond to the names of the vertices in 'g'
         or there are more variables than vertices")
  if ( setequal(names(V(g)), colnames(Data)) ) {
    message("From validate.Network: No latent variables")
  }
  if ( (length(setdiff(colnames(Data), names(V(g))))==0) && (length(V(g))>length(colnames(Data))) ){

    message("From validate.Network: There are nodes with latent variables")
  }

  NextMethod(gr=g, data=Data, ...)
}




#' @export
validate.BlockGraph<- function(obj, gr, data, ...)
{
  # the validation checks if the set of separators are of length 1
  SmS<- min_separators(gr)
  checkSmS<- sapply(SmS, function(x) {return(length(x))})
  if (length(which(checkSmS != 1))!=0)
    stop("The graph is not block graph")
  return(obj)

}


#' @export
validate.Tree<- function(obj, gr, data, ...)
{
   if ( (!is.connected(gr)) || ((length(E(gr))+1)!=length(V(gr))) )
     stop("'g' is not a tree")
  return(obj)
  #return(list(obj, gr, data))

}





#' @export validate
validate.HRMnetwork<- function(obj, ...)
{
  g<- getGraph(obj)
  if ( !is.igraph(g) )
    stop("'g' must be an igraph object")
  if (is.directed(g))
    message("From validate.HRMnetwork: 'g' is directed graph")
  if (!is.named(g))
    stop("Names should be attributed to the vertices of 'g' ")
  NextMethod(gr = g, ...)
}




#' @export
validate.HRMBG<- function(obj, gr, ...)
{
  # the validation checks if the set of separators are of length 1
  SmS<- min_separators(gr)
  checkSmS<- sapply(SmS, function(x) {return(length(x))})
  if (length(which(checkSmS != 1))!=0)
  stop("The graph is not block graph")
  return(obj)
}




#' @export
validate.HRMtree<- function(obj, gr, ...)
{
  if ( (!is.connected(gr)) || ((length(E(gr))+1)!=length(V(gr))) )
    stop("'g' is not a tree")
  return(obj)
}




#' @export
validate.FlowConnectionGraph<- function(obj, ...)
{
  #obj<- g

  if (!is.directed(obj))
    stop("The graph must be directed, corresponding to the flow direction")
  if (!is.named(obj))
    stop("The graph must have named vertices")
  if (!is.character(get.vertex.attribute(obj, "name", V(obj))) )
    stop("The names of the vertices should be type character")
  #if (sum(!vertices %in% get.vertex.attribute(obj, "name", V(obj)))!=0 )
  #  stop(" 'vertices' must belong to the set of vertices")


  return(obj)
}






#' @export
validate.FlowConnectionMatrix<- function(obj, g, ...)
{

  # #debug
  # obj<- FlowCon
  # class(obj)<- append(class(obj),"FlowConnectionMatrix")
  # g<- gnm
  #
  # #-----------

  if (is.null(dim(obj)))
  {
     if (length(obj) != vcount(g))
        stop("The FlowConnectionMatrix of a graph with d vertices should be either a matrix with d columns or a vector of length d")
    # if in fact the FlowConnection Matrix is a vector? but this is not a flow connection matrix
    # no immagine an extract of this matrix, just for specific set of vertices, then it is not a
    # symmetric matrix, but only the
  } else {
      if (dim(obj)[2]!=vcount(g))
         stop("The number of columns of 'obj' should equal the number of vertices")
  }
  if ( is.null(colnames(obj))|| is.null(rownames(obj)) )
    stop("Names should be assigned to the columns/rows, use the names of the graph")
  if ( length(setdiff(colnames(obj), get.vertex.attribute(g, "name", V(g)))) )
    stop("The names of the vertices in the matrix do not correspond to the names of the vertices in the graph")
  if ( !sum((obj==1)+(obj==0))==(ncol(obj)*nrow(obj)) ) # checks whether all entries are 1 or 0
    stop("'FlowConnection' is not a matrix of flow connected vertices")
  #if (sum(!vertices %in% get.vertex.attribute(g, "name", V(g)))!=0 )
   # stop(" 'vertices' must belong to the set of vertices")
  return(obj)
}








#' @export
validate.Simulation<- function(obj, ...)
{
  # debug
 # obj<- simobj


  #----------------------

  dm<- dim(obj$estimates)
  if(dm[1] != length(obj$trueParameters))
    stop("Incorrect number of parameters or parameter estimates")
  if (dm[2]!= length(obj$kValues))
    stop("Incorrect number of k values ")
 # if (dm[3] != length(obj$estimators))
 #  stop("Incorrect number of estimators")
  if (!is.matrix(obj$endsToParameters))
    stop("The ends of the parameters should be passed as a matrix of size #param*2" )
  if(dm[1]!= nrow(obj$endsToParameters))
    stop("Incorrect number of estimates or parameter list")
}





