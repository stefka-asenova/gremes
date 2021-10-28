


stdf_beta<- function(obj, ...)
{
  UseMethod("stdf_beta", obj)
}




stdf_beta.Tree<- function(obj, x, k_ratio)
{
  X<- getData(obj)
  n<- nrow(X)
  RX<- copula::pobs(X)*(n+1)
  U<- getNodesWithData(obj)
  k<- round(k_ratio*n)
  d<- ncol(X)
  Fn<- matrix(0,n,d)
  colnames(Fn)<- U
  u<- 1-k/n*x
  for (i in 1:n)
  {
    for (j in U)
    {
      ss<- RX[i,j]:n
      Fn[i,j]<- sum(choose(n,ss)*u[j]^ss*(1-u[j])^(n-ss))

    }
  }

  C_beta<- mean(apply(Fn, 1, prod))
  l_beta<- n/k*(1 - C_beta)
  return(l_beta)

}


