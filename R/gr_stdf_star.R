

stdf_star<- function(obj,...)
{
  UseMethod("stdf_star", obj)
}







stdf_star<- function(obj, x, k_ratio, ...)
{

  # debug
  # k<- k_ind
  # obj<- tobjR_star

  #-----------------


  if(is.matrix(x))
  {
    rr<- nrow(x)
    nx<- colnames(x)
  }  else {
    rr<- 1
    nx<- names(x)
  }

  U<- getNodesWithData(obj)
  if (length(nx %in% U)!= length(U))
    stop("The names of the coordinates should correspond to the names of the nodes
         with available data")


  l_star<- matrix(0, nrow=rr, ncol = length(k_ratio))

  for(i in 1:rr)
  {
    for (j in 1:length(k_ratio))
    {
      l_star[i,j]<- stdf(obj, x[i,], k_ratio[j])
    }
  }
  l_star<- apply(l_star, 1, mean)
  return(l_star)

}




#
# stdf_bounds<- function(obj,...)
# {
#   UseMethod("stdf_bounds", obj)
# }
#
#
#
# stdf_bounds.Tree<- function(obj, x, k, N)
# {
#
#   # add a check on the coordinates x, they
#   U<- getNodesWithData(obj)
#   if (length(names(x) %in% U)!= length(U))
#     stop("The names of the coordinates should correspond to the names of the nodes
#          with available data")
#
#
#   X<- getData(obj)
#   RX<- copula::pobs(X)*(n+1)
#   n<- nrow(X)
#   d<- ncol(X)
#   u<- 1-k/n*x
#
#   l_beta_star<- rep(0, N)
#   for (nn in 1:N)
#   {
#
#     U_star<- matrix(0,n,d)
#     for (i in 1:n)
#     {
#       I<- sample(1:n, d)
#       r<- sapply(1:d, function(x) RX[I[x],x])
#       V<- sapply(r, function(x) rbeta(1, x, n - x + 1))
#       U_star[i, ]<- V
#     }
#
#     R_star<- copula::pobs(U_star)*(n+1)
#     colnames(R_star)<- colnames(X)
#
#     # F_star<- matrix(0,n,d)
#     # colnames(F_star)<- U
#     # for (i in 1:n)
#     # {
#     #   for (j in U)
#     #   {
#     #     ss<- R_star[i,j]:n
#     #     F_star[i,j]<- sum(choose(n,ss)*u[j]^ss*(1-u[j])^(n-ss))
#     #
#     #   }
#     # }
#
#     C_beta_star<- mean(apply(F_star, 1, prod))
#     l_beta_star[nn]<- n/k*(1 - C_beta_star)
#
#   }
#
#   bounds<- quantile(l_beta_star, c(0.025, 0.975))
#   return(bounds)
# }
#
#






