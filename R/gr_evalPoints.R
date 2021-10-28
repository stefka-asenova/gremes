#' Generates a matrix of evaluation points, used by the EKS estimator.
#' @export
#' @rdname evalPoints
#' @param obj Object of a suitable class
#' @param obj2 Object of class \code{Tree}
#' @param x A named vector of coordinates of length |U|, where U is the set of available data nodes.
evalPoints<- function(obj, ...)
{
  UseMethod("evalPoints", obj)
}



#' @export
evalPoints.default<- function(obj, ...)
{
  return("Default method")
}




#' @export
#' @rdname evalPoints
#' @examples
#' # create an object of class Adjacent
#' adjobj<- Adjacent()
#' # create a graph with named vertices
#' g<- graph(c("a", "b", "b", "c", "b", "d"), directed = FALSE)
#' # create a dataset with naled columns
#' mydata<- matrix(rnorm(10*4), ncol=4, nrow=10)
#' colnames(mydata)<- get.vertex.attribute(g, "name", V(g))
#' # create object of class Network
#' mytree<- Tree(g, mydata)
#' # create the vector of coordinates and name it
#' x<- c(1:vcount(g))
#' names(x)<- c("a", "b", "c", "d")
#' # create the matrix of adjacent coordinates
#' mycoord<- evalPoints(adjobj, mytree, x)
#'
#' # create a matrix of pair coordinates
#' mytup<- Tuples()
#' mytup<- evalPoints(mytup, mytree, x)
#'
#' # create matrix of coordinates when there are missing data nodes
#' mydata<- matrix(rnorm(10*3), ncol=3, nrow=10)
#' colnames(mydata)<- c("b", "c", "d")
#' mytree<- Tree(g, mydata)
#' x<- c(1:vcount(g))
#' names(x)<- c("a", "b", "c", "d")
#' mytrip<- Triples()
#' mytrip<- evalPoints(mytrip, mytree, x)
#' mytrip
evalPoints.Coordinates<- function(obj, obj2, x, ...)
{
  #debug
  #obj2<- mytree

  #-----------

  g<- getGraph(obj2)
  U<- getNodesWithData(obj2)
  Vnames<- igraph::get.vertex.attribute(g, "name", V(g))

  if(length(names(x)) == 0)
    stop("'x' should be a named vector")

  if (length(Vnames) != 0)
  {
    if(length(base::setdiff(U, names(x)))!=0)
    stop("'x' should have the same names as the nodes with avalable data")
  }
  if (length(Vnames) == 0)
  {
    if (length(base::setdiff(Vnames, names(x))) !=0 )
    stop("'x' should have the same names as the nodes with avalable data")
  }
  NextMethod(nodes = Vnames, U = U, gr = g, ...)

}


#' @export
evalPoints.Adjacent<- function(obj, obj2, x, nodes, U, gr, ...)
{
  # if the graph passed in gr is not a tree this method is not going to work.


  if(length(setdiff(nodes, U))!=0)
    stop("Adjacent coordinates are defined only for graphs without missing data nodes")

  comb2<- walking(gr, U[1])
  comb3<- c()
  for (u in U)
  {
    nn<- get.vertex.attribute(gr, "name", igraph::neighbors(gr, u))
    if (length(nn)>1)
    {
      comb3<- cbind(comb3, rbind(utils::combn(nn,2), u))
    }
  }
  cm2<- apply(comb2, 1,place_x, x, U)
  cm3<- apply(comb3, 2, place_x, x, U)
  obj<- t(cbind(cm2, cm3))
  return(obj)

  #return(list(obj, obj2, x, nodes, U, gr))
}






#' @export
evalPoints.Tuples<- function(obj, obj2, x, nodes, U, ...)
{
  comb<- utils::combn(U,2)
  cm<- apply(comb, 2, place_x, x = x, U = U)
  cm<- t(cm)
  colnames(cm)<- U
  return(cm)
}









#' @export
evalPoints.Triples<- function(obj, obj2, x, nodes, U, ...)
{
  comb<- utils::combn(U,3)
  cm<- apply(comb, 2, place_x, x = x, U = U)
  cm<- t(cm)
  colnames(cm)<- U
  return(cm)
}


#' @export
evalPoints.Quadruples<- function(obj, obj2, x, nodes, U, ...)
{
  comb<- utils::combn(U,4)
  cm<- apply(comb, 2, place_x, x = x, U = U)
  cm<- t(cm)
  colnames(cm)<- U
  return(cm)
}




#' @export
#' @rdname evalPoints
#' @param U The set of nodes with observable data
#' @examples
#' # create a matrix of coordinates for an arbitrary subsets
#' rdsobj<- RootDepSet()
#' rds_values<- list(b=c("c", "d"), c=c("b", "d"), d=c("b", "c"))
#' rds_roots<- c("c", "d", "b")
#' rdsobj<- setRootDepSet(rdsobj, rds_values, rds_roots)
#' x<- c(2,3,4)
#' names(x)<- c("b", "c", "d")
#' evalPoints(rdsobj, x, getNodesWithData(mytree))
evalPoints.RootDepSet<- function(obj, x, U, ...)
{

  # # debug
  # obj<- subs
  # U<- getNodesWithData()
  # x<- x
  # names(x)<- U
  # #--------

  # this needs debugging


  subsets<- getValue(obj)
  roots<- getRoot(obj)

  if (is.list(subsets))
  {
    cmm<- matrix(0, nrow= length(roots), ncol = length(U) )
    colnames(cmm)<- U
    for (i in 1:length(roots))
    {
      cmm[i, ]<- place_x(subsets[[i]], x, U)
    }
  } else {
    cmm<- place_x(subsets, x, U)
    names(cmm)<- U
  }
  return(cmm)
}




