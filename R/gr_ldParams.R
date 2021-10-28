

ldParams<- function(obj, ...)
{
  UseMethod("ldParams", obj)
}


#' Vector of derivatives of the stdf
#' Computes the vector of partial derivatives of the stdf with respect to the vector of parameters theta in the HRM tree.
#'
#'
#' @rdname ldParams
#' @param obj Object of a suitable class
#' @param x vector of coordinates, the function is made when x contains only two positive coordinates.
#' @return A vector of length |E|. Each element of it represents the derivative of \eqn{l(e_J; \theta)} with respect
#' to the parameter \eqn{\theta} associated to a particular edge.
#' @export
#' @importFrom stats dnorm
ldParams.HRMtree<- function(obj, x, ...)
{

  # debug
 # obj<- hrmobj
  #J<- c("a", "g")
  #x<-c(0,0,0,1,1)
  #names(x)<- nodes
  #--------

  g<- getGraph(obj)
  params<- getParams(obj)
  J<- names(which(x>0))
  x1<- x[J[1]]
  x2<- x[J[2]]

  if (length(J)!=2)
    stop("The set J must contain only two elements")
  if (sum(J %in% get.vertex.attribute(g, "name", V(g)))<2)
    stop("Incorrect vertex set J")

  ldot<- rep(0, length(params))
  names(ldot)<- names(params)
      #j=1
  for (i in names(params))
  {
    ep<- edge_names_along_path(obj, rt=J[1], id=J[2], edge_names = TRUE)
    s<- sum(params[ep]^2)
    #ldot[i]<- dnorm(sqrt(s)/2)/sqrt(s)*params[i]*(i %in% ep)

    ldot[i]<- x1*stats::dnorm((log(x1/x2)+s/2)/sqrt(s))*(1/2-log(x1/x2)/s)/sqrt(s)*params[i]*(i %in% ep)+
             x2*stats::dnorm((log(x2/x1)+s/2)/sqrt(s))*(1/2-log(x2/x1)/s)/sqrt(s)*params[i]*(i %in% ep)
# j<- j+1

  }
  return(ldot)
}
