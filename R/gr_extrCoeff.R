

extrCoeff<- function(obj, ...)
{
  UseMethod("extrCoeff", obj)
}


extrCoeff.Network<- function(obj, k_ratio, v=NULL,  ...)
{

  # this computes the empirical extremal coefficients
  # may be v should be passed as a named vector of 1s

  # debug
  #obj<- tobj
  #k_ratio=0.05
  #k=60
  #v<- tri[1,]
  #obj<- bgobj
  #--------------

  vv<- names(which(v != 0))
  U<- obj$nodesWithData
  nU<- length(U)
  n<- nrow(obj$data)
  k<- round(k_ratio*n)

  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    tr<- t(as.matrix(n + 0.5 - k*rep(1,2)))
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        R<- apply(obj$data[,c(u,i)], 2, rank)
        l[u,i]<- n*(1 - copula::F.n(tr, R))/k
      }
    }

  } else {
    if (length(vv)<2)
      stop("The number of variables for which the extremal coefficient is computed must be at least 2")

    if (sum(!vv%in% U)!=0)
      stop("The set of variables for which the extremal coefficient is computed
           must be in the set of variables with available data")

    tr<- t(as.matrix(n + 0.5 - k*rep(1,length(vv))))
    R<- apply(obj$data[,vv], 2, rank)
    l<- n*(1 - copula::F.n(tr, R))/k
  }

  return(l)
  # the output is either a scalar or a diagonal matrix
}










extrCoeff.HRMtree<- function(obj, v=NULL, Ubar=NULL, ...)
{

  # v should be a named vector of coordinates, equal to 1, where the stdf is to be evaluated



  # debug
  #obj<- hrmobj_eks
  #u<- "X1"
  #i<- "X2"
  #-------


  # for now this is valid for pairwise extremal coefficients
  nU<- vcount(obj$graph)
  U<- get.vertex.attribute(obj$graph, "name", V(obj$graph))
  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        x<- edge_names_along_path(obj, u, i)
        y<- sum((obj$depParams[x])^2)
        l[u,i]<- 2*pnorm(1/2*sqrt(y))
      }
    }
  } else {

    #"should be adapted for extremal coefficients of any order"
    class(obj)<- append(class(obj), "EKS")
    l<- stdf(obj, v, Ubar)
  }
  return(l)
}












extrCoeff.HRMBG<- function(obj, v=NULL, Ubar=NULL, ...)
{
  # v should be a named vector of coordinates, equal to 1, where the stdf is to be evaluated


  # debug
  #obj<- hrmobj_eks
  #u<- "X1"
  #i<- "X2"
  #-------


  # for now this is valid for pairwise extremal coefficients
  nU<- vcount(obj$graph)
  U<- get.vertex.attribute(obj$graph, "name", V(obj$graph))
  if (is.null(v))
  {
    l<- matrix(0, nrow=nU, ncol = nU )
    colnames(l)<- rownames(l)<- U
    b<- character(0)
    for (u in U)
    {
      b<- base::union(b,u)
      for (i in base::setdiff(U,b))
      {
        x<- edge_names_along_path(obj, u, i)
        y<- sum((obj$depParams[x]))
        l[u,i]<- 2*pnorm(sqrt(y))
      }
    }
  } else {
    #"should be adapted for extremal coefficients of any order"
    l<- stdf(obj, x=v, Ubar=Ubar)
  }
  return(l)
}






