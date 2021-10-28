

Lambda<- function(obj, ...)
{
  UseMethod("Lambda", obj)
}


Lambda.default<- function(obj, ...)
{
  return("Default method")
}


Lambda.MMEave<- function(obj, obj2, U_bar=NULL, ... )
{
  # #debug
  # obj<- aveobj
  # obj2<- rdsobj
  # U_bar<- Uc
  # #-------------


  g<- obj$graph
  W<- getValue(obj2)
  if(is.list(W))
  {
    W<- W[[1]]
    message("Multiple input sets. Only the first is considered")
  }

  W_<- base::setdiff(W, U_bar)
  nW_<- length(W_)

  # gives all the indices of the matrix Lambda
  pairsOfSigma<- base::rbind(rep(W_, rep(nW_, nW_)), rep(W_, nW_))
  A_u<- matrix(0, nrow = ncol(pairsOfSigma), ncol = ecount(g))
  colnames(A_u)<- get.edge.attribute(g, "name", E(g))
  for (i in 1:ncol(pairsOfSigma))
  {
    if (pairsOfSigma[1,i]==pairsOfSigma[2,i]) edges_ij<- NULL
    else { edges_ij<- edge_names_along_path(HRMtree(g), # locally transforming it to a new class to be able to use the method
                                           pairsOfSigma[1,i], pairsOfSigma[2,i])}

    A_u[i, edges_ij]<- 1
  }
  return(A_u)
}



Lambda.MLEave<- Lambda.MMEave
