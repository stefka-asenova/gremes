#' Stable tail dependence function (stdf)
#'
#' Computes the stable tail dependence function for given coordinates. If object of class \code{Network} or
#'  its subclasses is passed
#' non-parametric estimates of the stdf are computed. If an object of class \code{HRMtree} or \code{HRMBG} is passed
#' parametric estimates are computed with parameters that constitute the slot \code{$depParams} in the object.
#' @export stdf
#' @rdname stdf
#' @param obj Object of class \code{Network}, or \code{HRMtree} or \code{HRMBG}
#' @param ... additional arguments
#' @param Y A matrix of evaluation points with columns named according to the names of the nodes with
#' observed variables,
#' and of dimensions qx|U| where U is the set of observed variables, and q is the number of vectors at which the
#' stdf is evaluated.
#' @param k_ratio the ration of the upper order statistics as a share of the size of the sample
#' @param x Vector of evaluation points with named elements according to the nodes of the graph.
#' @param Ubar The set of nodes for which data are missing. It should stay at its default value NULL.
#' @examples
#' #See Vignette "Additional functionalities".
stdf<- function(obj,...)
{
  UseMethod("stdf")
}


#' @rdname stdf
#' @importFrom copula F.n
#' @export
stdf.Network<- function(obj, Y, k_ratio, ...)
{

#  Y<- x
 # obj<- tobj

 evalPoints<- Y
  if (is.null(dim(evalPoints)))
  {
    evalPoints<- t(as.matrix(evalPoints))
  }
  if( length(colnames(evalPoints))==0)
    stop("The matrix of evaluation points should have named columns")

  n<- nrow(obj$data)
  k<- round(k_ratio*n)
  stopifnot(is.numeric(k),
            is.numeric(d <- ncol(obj$data)),
            is.matrix(evalPoints),
            d == ncol(evalPoints))


  tr<- n+0.5-k*evalPoints
  R<- apply(obj$data, 2, rank)
  l<- n*(1-F.n(tr, R))/k
  return(l)
}



#' @export
stdf.default<- stdf.Network


#' @export
#' @importFrom mvtnorm pmvnorm
stdf.EKS<- function(obj, x, Ubar = NULL, ...)
{

  # # debug
 # library(mvtnorm)
#  obj<- hrm
 #  Ubar=c("a", "b")
 #  par<- c(1:30)
 #   x<- tri[4,]
 #  # obj<- setParams(obj, depParams)
  # names(depParams)<- c("e1", "e2", "e3", "e4")
  # #---------

  # x is a named vector of evaluation points, has to be of dimension not greater than the
  # number of nodes in the graph


  J<- names(which(x != 0))

  if(length(J)==1)
  {
    return(x[J])
  }


  root<- J[1]
  Ju<- base::setdiff(J, root)
  nJu<- length(Ju)
  set<- RootDepSet()
  set<- setRootDepSet(set, J, root)

  A<- sigma(obj, set, U_bar = Ubar)
  par<- (obj$depParams)^2
  s<- A %*% par
  sig<- matrix(0, ncol = nJu, nrow = nJu)
  sig[lower.tri(sig, diag=TRUE)]<- s       #the code below creates a symmetric matrix
  #sig<- t(sig)                             #by first filling the rows !
  #sig[lower.tri(sig)]<- t(sig)[lower.tri(sig)]

  sig[upper.tri(sig)]<- t(sig)[upper.tri(sig)]


  mu<- -diag(sig)/2


  # initialize the tail function  with the first term in the summation
  l<- x[root]*pmvnorm(lower = -Inf,
                      upper = -log(x[Ju]/x[root]),
                      mean = mu,
                      sigma = sig)

  for (j in Ju)
  {

    mvRj<- tilde_Rj(Ju, j, mu, sig)
    mu_j<- as.vector(mvRj$mu) # as.vector is necessary, because otherwise it is a matrix and it gives error in the pmvnorm function, sth like downgrading to a vector
    sigma_j<- mvRj$sigma

    # determine the upper bound
    ub<- rep(0, length(Ju))
    names(ub)<- Ju
    ub[j]<- -log(x[root]/x[j])
    Juj<- base::setdiff(Ju,j)
    ub[as.character(Juj)]<- -log(x[Juj]/x[j])

    l<- l + x[j]*pmvnorm(lower = -Inf,
                         upper = ub,
                         mean = mu_j,
                         sigma = sigma_j)
  }

  return(l)
}


#' @rdname stdf
#' @export
stdf.HRMtree<- stdf.EKS




#' @rdname stdf
#' @export
stdf.HRMBG<- function(obj, x, Ubar=NULL, ...)
{

  # debug
 # x<- c(1,0,2,0,2)
#  names(x)<- get.vertex.attribute(g, "name", V(g))
 # obj<- hrmbgobj

  #-------------



  J<- names(which(x != 0))

  if(length(J)==1)
  {
    return(x[J])
  }

  l<- 0
  for (j in J)
  {

    Ju<- base::setdiff(J, j)
    nJu<- length(Ju)
    set<- RootDepSet()
    set<- setRootDepSet(set, J, j)

    A<- sigma(obj, set, U_bar = Ubar)
    par<- (obj$depParams)
    s<- A %*% par
    sig<- matrix(0, ncol = nJu, nrow = nJu)
    sig[lower.tri(sig, diag=TRUE)]<- s       #the code below creates a symmetric matrix
    #sig<- t(sig)                             #by first filling the rows !
    #sig[lower.tri(sig)]<- t(sig)[lower.tri(sig)]

    sig[upper.tri(sig)]<- t(sig)[upper.tri(sig)]


    mu<- -diag(sig)/2

    # initialize the tail function  with the first term in the summation


    l<- l+x[j]*pmvnorm(lower = -Inf,
                       upper = -log(x[Ju]/x[j]),
                       mean = mu,
                       sigma = sig)
  }
  return(l)
}






