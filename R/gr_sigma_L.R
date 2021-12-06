

sigma_L<- function(obj, ...)
{
  UseMethod("sigma_L")
}



sigma_L.HRMtree<- function(obj, evalPts, Ubar = NULL)
{

    # the code is not for general sets J and K but only for such that involve pairs and whose
  # entries are units ?? is that correct ??- put verification conditions on evalPoints


  sum_ep<- apply(evalPts, 1, sum)
  if(sum((sum_ep>2))>0)
    stop("Invalid matrix of evaluation points. The evaluation points should be based on tuples only.
         Also the coordinates should be 1 or 0.")
    # the sets J and K must not contain nodes with unobservable variables - this is respected in the construction
  # Tuples, evaiPoints(), the methods developped to generate the coordinates for the EKS estimator

  # debug
  # obj<- hrmobj
  # Ubar<- getNoDataNodes(tobj)

  #----------

  class(obj)<- append(class(obj), "EKS") # the stdf is written for class EKS

  W_<- 1:nrow(evalPts)
  nW_<- length(W_)
  q<- c(1:nW_)
  colIndex<- unlist(sapply(q, function(x) base::setdiff(W_, W_[1:x])))
  pairsOfSigma<- base::rbind(rep(W_, c(nW_:1)), c(W_, colIndex))

  sigma_L<- rep(0, ncol(pairsOfSigma))

  for (i in 1:ncol(pairsOfSigma))
  {
    eJ<- evalPts[pairsOfSigma[1,i], ]
    eK<- evalPts[pairsOfSigma[2,i], ]
    eJ1<- eJ2<- eJ
    eJ1[names(which(eJ!=0))[2]]<- 0
    eJ2[names(which(eJ!=0))[1]]<- 0
    eK1<- eK2<- eK
    eK1[names(which(eK!=0))[2]]<- 0
    eK2[names(which(eK!=0))[1]]<- 0

    # the stdf in J K
    lJ<- stdf(obj, eJ, Ubar=Ubar)
    lK<- stdf(obj, eK, Ubar=Ubar)
    lJK<- stdf(obj, pmax(eJ, eK ), Ubar=Ubar)

    # the derivatives
    ldj<- ldArgument(obj, names(which(eJ!=0))) # this is a scalar derivative
    ldk<- ldArgument(obj, names(which(eK!=0)))



    s1<- ldj*(1 + lK - stdf(obj, pmax(eK, eJ1), Ubar=Ubar)) +
      ldj*(1 + lK - stdf(obj, pmax(eK, eJ2), Ubar=Ubar))



    s2<- ldk*(1 + lJ - stdf(obj, pmax(eJ, eK1), Ubar=Ubar)) +
      ldk*(1 + lJ - stdf(obj, pmax(eJ, eK2), Ubar=Ubar))

    s3<- ldj*ldk*(2 - stdf(obj, pmax(eJ1, eK1), Ubar=Ubar)) +
      ldj*ldk*(2 - stdf(obj, pmax(eJ1, eK2), Ubar=Ubar)) +
      ldj*ldk*(2 - stdf(obj, pmax(eJ2, eK1), Ubar=Ubar)) +
      ldj*ldk*(2 - stdf(obj, pmax(eJ2, eK2), Ubar=Ubar))

    sigma_L[i]<- lJ + lK - lJK - s1 - s2 + s3


  }
  # transform the vector in a upper triangular matrix by filling the rows first
  sigma<- matrix(0,nW_, nW_)
  sigma[lower.tri(sigma, diag=TRUE)] <- sigma_L
  sigma <- t(sigma)

  # transform the upper triangular matrix in symmetric matrix
  sigma[lower.tri(sigma)]  <- t(sigma)[lower.tri(sigma)]

  return(sigma)

}









#
# sigma_L.Tree<- function(obj, evalPts, k_ratio, Ubar = NULL)
# {
#
#
#   # the sets J and K must not contain nodes with unobservable variables
#   # ideally, if the evalPts has r rows, the function should produce rxr matrix of covariances. However
#   # in the case of the bivariate Pickands dep. function, if evalPts contains always at the same place
#   # non-zero elements, but the values of these non-zero elements vary,
#   # it is interesting to produce only the diagonal elements, not the whole matrix. For the
#
#   # debug
#   obj<- tobj
#  # Ubar<- getNoDataNodes(tobj)
#   evalPts<- x
#     #----------
#
#   s<- rep(0, nrow(evalPts))
#   for (i in 1:nrow(evalPts))
#   {
#
#     lJ<- stdf(obj, evalPts[i,], k_ratio[15])
#     ld<- ldArgument(obj, evalPts[i,], k_ratio[15])
#     x1<- evalPts[i,]
#     x1[2]<-0
#     x2<- evalPts[i,]
#     x2[1]<-0
#
#
#     s[i]<- lJ - 2*sum(ld) + ld[1]^2*(2-stdf(obj, x1, k_ratio[15])) +
#       ld[2]^2*(2-stdf(obj, x2, k_ratio[15]))+
#       2*ld[1]*ld[2]*(2-lJ)
#
#   }
#
#
#
# }
#

























