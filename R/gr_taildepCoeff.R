

taildepCoeff<- function(obj, ...)
{
  UseMethod("taildepCoeff", obj)
}




taildepCoeff.Network<- function(obj, k_ratio, v, correction = FALSE, ...)
{
  # v shoud be a named vector of coordinates
  # k_ratio should be a scalar
  # the taildepCoeff calculates the empirical estimate of
  # tP(X_v>t) when X_v is a vector of Pareto varibles indexed on the set v.
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


#' @importFrom utils combn
taildepCoeff.HRMtree<- function(obj, v, Ubar=NULL, ... )
{

  # debug

  #obj<- hrmobj
  #Ubar<- Uc

  #-------


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
