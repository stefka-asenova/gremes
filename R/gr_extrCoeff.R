#' Calculates extremal coefficients
#'
#' Computes parametric and non-parametric extremal coefficients. For explanation of extremal coefficients
#' see Vignette "Additional functionalities".
#' @param obj If it is an object of class \code{Network} non-parametric estimates are computed.
#' If it is an object of class \code{HRMtree}, parametric EC are computed.
#' If object of class \code{HRMBG} is passed, parametric EC are computed.
#' @param k_ratio is the number of upper order statistics divided by the total number of observations.
#' @param v a vector of length the number of nodes and named according to the names of the nodes.
#'  NULL by default. See Details.
#' @param ... additional arguments
#' @export
#' @rdname extrCoeff
#' @details If the vector \code{v} is  non NULL then an extremal
#' coefficient is computed based on the vector \code{v}. This means for instance that if \code{v} is
#' (0, 1, 0, 2.5, 1.8) with names \eqn{(a,b,c,d, e)} then a trivariate extremal coefficient is computed taking
#' coordinates \eqn{(b,d,e)} to be equal to 1. If the vector \code{v} is NULL, then bivariate extremal coefficients are
#' computed and ordered in a matrix of pairwise extremal coefficients.
#'
#' @examples
#' # bivariate extremal coefficients of a tree model
#' g<- graph(c(1, 2, 2, 3, 2, 4), directed = FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d"))
#' obj<- HRMtree(g)
#' obj<- setParams(obj, c(0.2, 0.3, 0.4))
#' extrCoeff(obj)
#' # arbitrary vector of coordinates
#' v<- c(1,2,0,6); names(v)<- c("a", "b", "c", "d")
#' extrCoeff(obj, v)
#'
#' # non-parametric extremal coefficients
#' data<- matrix(rnorm(4 * 500), 500, 4)
#' colnames(data)<- c("a" , "b", "c", "d")
#' tobj<- Tree(g, data)
#' extrCoeff(tobj, 0.2)
#' # arbitrary vector of coordinates
#' v<- c(1, 2, 0, 6); names(v)<- c("a", "b", "c", "d")
#' extrCoeff(tobj, k_ratio = 0.2, v = v)

#'
#' #bivariate extremal coefficients of a block graph model
#' g<- graph(c(1, 2, 2, 3, 1, 3, 3, 4, 4, 5, 3, 5), directed = FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d", "e"))
#' obj<- HRMBG(g)
#' obj<- setParams(obj, c(0.2, 0.3, 0.4, 0.5, 0.6, 0.1))
#' extrCoeff(obj)
#' # arbitrary vector of coordinates
#' v<- c(1, 1, 0, 1, 1); names(v)<- c("a", "b", "c", "d", "e")
#' extrCoeff(obj, v)
extrCoeff<- function(obj, ...)
{
  UseMethod("extrCoeff", obj)
}

#' @rdname extrCoeff
#' @export
extrCoeff.Network<- function(obj, k_ratio, v = NULL,  ...)
{

  # this computes the empirical extremal coefficients
  # may be v should be passed as a named vector of 1s

  # debug
  #obj<- tobj
  #k_ratio=0.05
  #k=60
  #v<- tri[1,]
  #obj<- bgobj
  #--------------

  vv<- names(which(v != 0))
  U<- obj$nodesWithData
  nU<- length(U)
  n<- nrow(obj$data)
  k<- round(k_ratio*n)

  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    tr<- t(as.matrix(n + 0.5 - k*rep(1,2)))
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        R<- apply(obj$data[,c(u,i)], 2, rank)
        l[u,i]<- n*(1 - copula::F.n(tr, R))/k
      }
    }

  } else {
    if (length(vv)<2)
      stop("The number of variables for which the extremal coefficient is computed must be at least 2")

    if (sum(!vv%in% U)!=0)
      stop("The set of variables for which the extremal coefficient is computed
           must be in the set of variables with available data")

    tr<- t(as.matrix(n + 0.5 - k*rep(1,length(vv))))
    R<- apply(obj$data[,vv], 2, rank)
    l<- n*(1 - copula::F.n(tr, R))/k
  }

  return(l)
  # the output is either a scalar or a diagonal matrix
}








#' @rdname extrCoeff
#' @export
extrCoeff.HRMtree<- function(obj, v = NULL, ...)
{

  # v should be a named vector of coordinates, equal to 1, where the stdf is to be evaluated



  # debug
  #obj<- hrmobj_eks
  #u<- "X1"
  #i<- "X2"
  #-------

  Ubar = NULL
  # for now this is valid for pairwise extremal coefficients
  nU<- vcount(obj$graph)
  U<- get.vertex.attribute(obj$graph, "name", V(obj$graph))
  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        x<- edge_names_along_path(obj, u, i)
        y<- sum((obj$depParams[x])^2)
        l[u,i]<- 2*pnorm(1/2*sqrt(y))
      }
    }
  } else {

    vv<- names(which(v > 0))
    if (length(vv)<2)
      stop("The number of variables for which the extremal coefficient is computed must be at least 2")

    v[vv]<- 1
    class(obj)<- append(class(obj), "EKS")
    l<- stdf(obj, v, Ubar)
  }
  return(l)
}











#' @rdname extrCoeff
#' @export
extrCoeff.HRMBG<- function(obj, v = NULL,  ...)
{
  # v should be a named vector of coordinates, equal to 1, where the stdf is to be evaluated


  # debug
  #obj<- hrmobj_eks
  #u<- "X1"
  #i<- "X2"
  #-------

  Ubar = NULL
  # for now this is valid for pairwise extremal coefficients
  nU<- vcount(obj$graph)
  U<- get.vertex.attribute(obj$graph, "name", V(obj$graph))
  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        x<- edge_names_along_path(obj, u, i)
        y<- sum((obj$depParams[x]))
        l[u,i]<- 2*pnorm(sqrt(y))
      }
    }
  } else {

    vv<- names(which(v != 0))
    if (length(vv)<2)
      stop("The number of variables for which the extremal coefficient is computed must be at least 2")
    v[vv]<- 1
    l<- stdf(obj, x=v, Ubar=Ubar)
  }
  return(l)
}






