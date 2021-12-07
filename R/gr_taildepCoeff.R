#' Tail dependence coefficients
#'
#' It computes parametric (Huesler-Reiss) or non-parametric Tail dependence coefficients -
#' See Vignette "Application - Danube" for usage of TDCs.
#' @param obj should be of class \code{Network} or \code{HRMtree} or a subclass of these, such as
#' \code{Tree, BlochGraph} as subclasses of \code{Network} or subclasses \code{MME, MLE, MLE1, MLE2, EKS, EKS_part,
#' EngHitz, MLEave, MMEave} of class \code{HRMtree}.
#' @export
#' @rdname taildepCoeff
taildepCoeff<- function(obj, ...)
{
  UseMethod("taildepCoeff")
}

#' @title
#' @name TDC
#' @export
#' @rdname taildepCoeff
#' @param k_ratio the number of upper order statistics divided by the total number of observations
#' @param v a named vector of coordinates, the names should correspond to the names of the nodes
#' in the graph in \code{obj}
#' @param correction adds a correction of one half to n-k in computing the event {x > n - k + 1/2}.
#' Default is FALSE, hence n-k.
#' @param ... additional arguments
#' @return If the \code{obj} is of class \code{Network} the TDC are non-parametric. If the \code{obj} is of class
#' \code{HRMtree} or its subclasses respectively the TDC are parametric.
#' @examples
#' # non-parametric tdc
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes
#' data<- matrix(rnorm(1000*3), 1000,3)
#' colnames(data)<- c("a", "b", "c")  # name the columns
#' net<- Network(x = g, data = data)
#' v<- c(1,0,1)
#' names(v)<- c("a", "b", "c")
#' taildepCoeff(net, 0.2, v = v, correction = TRUE)
taildepCoeff.Network<- function(obj, k_ratio, v, correction = FALSE, ...)
{
  # v should be a named vector of coordinates
  # k_ratio should be a scalar
  # the taildepCoeff calculates the empirical estimate of
  # tP(X_v>t) when X_v is a vector of Pareto variables indexed on the set v.
  # Another way of v={W \union u}

  # debug
#   #X<- matrix(rnorm(5*3), 5,3)
# obj<- tobj
# v<- c(1,2,0,1)
# names(v)<- c("Meaux"  , "Melun","Sens"  , "Paris")
# k_ratio<- 0.3
# correction = FALSE
#   #---------


  X<- getData(obj)
  U<- getNodesWithData(obj)
  namesx<- names(which(v>0))
  n<- nrow(X)
  k<- round(k_ratio*n)
  crc<- ifelse(correction, 0.5, 0)

  if (is.null(namesx))
    stop("The vector of coordinates should be a named vector.")
  if (!all(namesx %in% U) )
    stop("The vector of coordinates should contains elements only for observable variables")

  X_rank<- apply(X[,namesx], 2, rank)
  tdc<- (1/k)*sum(apply(X_rank, 1, function(x) { y<- 1*all(x>(n-k+crc)); return(y) }))

  return(tdc)

}





#' @export
#' @importFrom utils combn
#' @rdname taildepCoeff
#' @examples
#' # parametric tdc
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes
#' obj<- HRMtree(g)
#' obj<- setParams(obj, c(0.5,0.6))
#' taildepCoeff(obj, v)
taildepCoeff.HRMtree<- function(obj, v, ... )
{

  # debug

#  obj<- hrm
#  Ubar<- Uc
#  x<- rep(1,8)
#  names(x)<- letters[1:8]
#  v<- x
  #-------

  Ubar=NULL
  g<- getGraph(obj)
  nv<- get.vertex.attribute(g, "name", V(g))
  namesx<- names(which(v>0))

  if (is.null(namesx))
    stop("The vector of coordinates should be a named vector.")

  if (!all(namesx %in% nv) )
    stop("Incorrect coordinates")


  tdc<- 0
  for (i in 1:length(namesx))
  {
    cx<- utils::combn(namesx, i)
    for (j in 1:ncol(cx))
    {

      x<- rep(1, i)
      names(x)<- cx[,j]
      tdc<- tdc + (-1)^(i-1)*stdf(obj, x, Ubar)
    }

  }

  return(tdc)
}
