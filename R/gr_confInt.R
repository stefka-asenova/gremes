#' @export
#' @rdname confInt
confInt<- function(obj, ...)
{
  UseMethod("confInt")
}


#' Confidence intervals for pairwise EC estimates
#'
#' Provides confidence intervals for the estimates using the ECE. It is suitable when only pairs are used -
#' pairwise extremal coefficients.
#' @export
#' @rdname confInt
#' @importFrom stats qnorm
#' @param obj is an object of class EKS which contains the estimates of the edge weights
#' @param evpo is the matrix of evaluation points used in the EC estimator. It should contain only pairs of ones.
#' @param k is the number of upper order statistics. It should be the same as in the estimates of the edge weights.
#' @param level is the level of confidence. default is 0.05.
#' @param ... additional arguments
#' @examples
#' # See Vignette "Application Seine"
confInt.EKS<- function(obj, evpo, k, level = 0.05, ...)
{


  # ## debug
  # obj<- eceobj
  # evpo<- tup
  # U_bar<- c("2", "5")
  # ##-----------------------

  U_bar = NULL

  alpha<- level
  g<- getGraph(obj)
  parms<- getParams(obj)
  sigma<- sigma_L(obj, evpo, U_bar)
  Ldot<- matrix(0, nrow(evpo), length(parms))
  colnames(Ldot)<- names(parms)
  for (j in 1:nrow(evpo))
  {
    Ldot[j,]<- ldParams(obj, evpo[j,])
  }

  M<- solve(t(Ldot)%*% Ldot)%*% (t(Ldot)%*% sigma %*% Ldot)%*% solve(t(Ldot)%*% Ldot)
  #e_names<- get.edge.attribute(g, "name", E(g))
  conf_int<- matrix(0,ecount(g), 2)
  rownames(conf_int)<- names(parms)
  for (e in names(parms))
  {
    conf_int[e,]<- c(parms[e] - stats::qnorm(1-alpha/2)*sqrt(M[e,e]/k),
                     parms[e] + stats::qnorm(1-alpha/2)*sqrt(M[e,e]/k))
  }
  return(conf_int)

}
