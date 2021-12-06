# don't export any of these functions


#' @importFrom mvtnorm dmvnorm
ll<- function(params, data_u, A_u)
{


  # note that once you have named the params vector as the columns of A_u this guarantees that
  # there should not be mismatching at this point

  # similarly if the column names of the data are the same as those of the matrix it should not be a
  # problem
data_u<- as.matrix(data_u)
   hv_sig<- A_u %*% (params^2)

  #  if (dim(data_u)==0)
   sig<- matrix(0, ncol(data_u), ncol(data_u))
   sig[lower.tri(sig, diag=TRUE)]<- hv_sig
   sig[upper.tri(sig)]<- t(sig)[upper.tri(sig)]


  # obtain the mu
  mu<- -diag(sig)/2
  ll<- -sum(mvtnorm::dmvnorm(data_u, mean = mu, sigma = sig, log = TRUE))

  return(ll)
}








# params The parameters to estimate, It should be a named vector of initial values
#  named according to the names of the edges in the
#  whole graph (!!! Named vector of values)
# data The dataset. It should be a matrix with dimensions k by sum(h).
# obj Object of class HRMtree. This one is necessary for construction
#  of the covariance matrix, see details
# The covariance matrix constructed in this function and passed to the
#  multivariate normal density is a block diagonal matrix. Each matrix on the diagonal
#  corresponds to the covariance matrix of the variables \eqn{R_{W_u}}. The mean vector is equal
#  to \emph{-diag(sig)/2}, where \emph{sig}  is the covariance matrix.
# subsets Object of class \code{RootDepSet}
# k_ratio The share of the upper order statistics
# h1 The number of the variables \eqn{R_{W_u}} for every set in subsets, after subtracting
#  the root and the variables with missing data.
# Ubar the set of missing data nodes
#' @importFrom mvtnorm dmvnorm
llmle2<- function(params, data, obj, subsets, k_ratio, h1, Ubar)
{
  sig<- ArgumentD(c(sum(h1),sum(h1)))
  sig<- for_u_in_U(obj=sig, obj2=obj, subsets=subsets, k_ratio=k_ratio,
                   h1=h1, depParams=params, Ubar=Ubar)
  mu<- -diag(sig)/2

  ll<- -sum(mvtnorm::dmvnorm(data, mean = mu, sigma = sig, log = TRUE))
  return(ll)
}



llmleave<- function(params, data, Lambda )
{

  # # debug
  # params<- obj$depParams+1
  # data<- deltaExcess
  # Lambda<- lam*4
  # #-----------------------

  d<- ncol(data)
  m<- crossprod(t(Lambda), params^2)
  lambd<- matrix(m, ncol = d, nrow = d, byrow=TRUE)/4
  M<- diag(d) - tcrossprod(rep(1,d), rep(1,d))/d
  sig<- -crossprod(t(crossprod(t(M), lambd)), M)


  ed<- rep(1,d)/d
  gmm<- as.vector(crossprod(t(crossprod(ed, lambd)), ed))
  mu<- -as.vector(crossprod(t(lambd), ed)) + gmm*ed*d
  ll<- -sum(mvtnorm::dmvnorm(data, mean = mu, sigma = sig+10^(-7)*diag(d), log = TRUE))
  return(ll)

}





ll_EngHitz<- function(params, data, MA, Uc, obj)
{
  # obj should be HRMtree obj

  # # debug
  #
  # obj<- hrmtobj
  # data<- eot
  #
  # #----------------


  obj<- setParams(obj, params)
  nvars<- colnames(data)
  u<- nvars[1]
  nvars_u<- base::setdiff(nvars, u)
  vv<- rep(1, ncol(data))
  names(vv)<- nvars
  data1<- log(data[,nvars_u]/data[,u])

  # a minus because the funct returns '-'
  ff<- - (-2*sum(log(data[,u])) - sum(log(data[,nvars_u])) -
            ll(params, data1, MA) - nrow(data)*log(stdf(obj, vv, Ubar=Uc)))

  return(ff)

}






