


R_star<- function(obj, ...)
{
  UseMethod("R_star")
}


#' @importFrom stats rbeta
R_star.Tree<- function(obj, ...)
{
  # it generates


  X<- getData(obj)
  n<- nrow(X)
  RX<- copula::pobs(X)*(n+1)

  d<- ncol(X)

  U_star<- matrix(0,n,d)
  for (i in 1:n)
  {
    I<- sample(1:n,1)
    r<- sapply(1:d, function(x) RX[I,x])
    V<- sapply(r, function(x) stats::rbeta(1, x, n - x + 1))
    U_star[i, ]<- V
  }

  r_star<- copula::pobs(U_star)*(n+1)
  colnames(r_star)<- colnames(X)
  return(r_star)

}

