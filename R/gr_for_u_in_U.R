
# don't export generics ; export other methods



# Generic for method \code{for_u_in_U}
# It generates arguments that are used in the estimation via MM and ML methods. For every node \eqn{u\in U} produces the empirical or the parameric covariance matrices and
# depending on the class of the first argument it combines them in (so far) three different ways:
# - substacking of vectors
# - substacking of matrices
# - creating a block diagonal matrix
for_u_in_U<- function(obj,...)
{
  UseMethod("for_u_in_U")
}






#' @export
for_u_in_U.default<- function(obj,...)
{
  return("Default method called on unrecognized object")
}






#' @export
# obj Object of class \code{Argument}
# obj2 Object of class \code{HRMtree, GTree, CovSelectTree}
# subsets Object of class \code{RootDepSet}
# k_ratio The fraction of the upper order statistics in the sample size
# h1 A numeric vector with the length of the subsets for each root, after removing the corresponding root and the nodes with missing data.
# depParams Named vector with the values with the values of the parameters
# Ubar Vector with the names of the nodes with missing data
# ... additional arguments
for_u_in_U.Argument<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar=NULL, ...)
{
  # # #debug
  #  obj<- s
  # obj2<- mmeobj
  # subsets=rds
  #  h1<- h1
  #  k_ratio=0.05
  #  i=4
  #  nvalues=1
  # # # #-------------
  #
  values<- getValue(subsets)
  roots<- getRoot(subsets)
  nvalues<- length(values)
  this_graph<- getGraph(obj2)
  if(!is.list(values))
    stop(" 'subsets' must contain at least two subsets")

  j=1
  set_i<- RootDepSet()
  for (i in 1:nvalues)
  {
    set_i<- setRootDepSet(set_i, values[[i]], roots[i])
    yy<- pot(obj=obj2, set_i, k_ratio=k_ratio)
    thisArg<- NextMethod(y = yy, set = set_i, which_graph = this_graph, ...)
    obj<- combine(obj, thisArg, h1, j, depParams)
    j<- j+1
  }
  return(obj)
  # return(thisArg)
 #return(list(yy))
}






#' @export
for_u_in_U.ArgumentSS<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, ...)
{


  x<- sigma(y, set, U_bar=Ubar)
  return(x)

  #mylist<- list(a=y, b=set, c=Ubar, d=ind, e=h1)
  #return(mylist)
}




#' @export
#' @importFrom stats optim
for_u_in_U.ArgumentSSvec<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, obj_mle1, ...)
{
  # obj must be ArgumentSSvec
  # y must be GTree on the subset set
  # set must be RootDepSetobj but only one set and one root
  # mle1obj on the whole graph

  # #debug
  # set=set_i
  # y<- yy
 # obj_mle1<- mle1obj
  # Ubar<-
  # #-------

  eps<- 10^(-5)
  #up<- 10^2
  pot_set<- pot(obj_mle1, set)

  # initial values set at 1's
  params<- rep(1, ecount(pot_set$graph))

  A_u<- sigma(pot_set, set, U_bar=Ubar)
  theta_u<- stats::optim(params, ll,
                  gr=NULL,
                  method = c("L-BFGS-B"),
                  lower = rep(eps, length(params) ),
                  upper = rep(Inf, length(params) ),
                  A_u = A_u,
                  data_u = y$data)$par


  return(theta_u)

  # mylist<- list(a=y, b=set, c=Ubar, d=mle1obj, e=h1)
  #return(list(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, obj_mle1))
}








#' @export
for_u_in_U.ArgumentEKS_part<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, Data, xx, ...)
{
  # 1) include submodel identifiability conditions here !!!!!

  # 2) then verify if the number of tuples is enough if not add the triples and check this too
  # if still not enough add the adjacent ev.points


  # # debug
  # y<- yy
  # Data<- data_red
  # set<- set_i
  # Ubar<- UBAR
  # #---------

  # check condition
  set_cast<- paste(set$value, collapse = ",")
  UC<- base::intersect(get.vertex.attribute(y$graph, "name", V(y$graph)), Ubar)
    if (sum(degree(y$graph, UC) < 3)!=0)
      stop(paste("The model on subset ", set_cast, " is not identifiable"))
  #------------------------------

  eksobj<- EKS(y$graph)
  en<- length(eksobj$depParams)
  ep<- Tuples()
  tobj<- Tree(eksobj$graph,
              Data[ , base::setdiff(get.vertex.attribute(y$graph, "name", V(y$graph)), Ubar)]) # the
  # dataset here is taken to be on the nodes of the graph, not the one of the set, because for some
  # reason this is still unclear to me.

  ep<- evalPoints(ep, tobj, xx)

  # check condition
  if (nrow(ep) < en)
  {
    trip<- Triples()
    ep1<- evalPoints(trip, tobj, xx)
    ep<- rbind(ep, ep1)
  }

  if (nrow(ep) < en)
    stop("The model is not identifiable. All possible, tuples and triples coordinates, are added, but more coordinates are required. Add more coordinates.")
  #--------------------------------

  th_hat<- estimate(obj = eksobj, getData(tobj), ep, k_ratio)
  return(th_hat$depParams)

  # return(list(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, Data, xx))
  # return(list(eksobj, getData(tobj), ep, k_ratio))
}





#' @export
for_u_in_U.ArgumentHvec<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, ...)
{
  x<- sigma(y, set, U_bar=Ubar)
  return(x)

  #return(list(y,set, Ubar))
}






#' @export
for_u_in_U.ArgumentD<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, ... ) # the inputs should be modified, it is somehow
  #unclear why the arguments are matched
{
  a<- sigma(y, set, U_bar=Ubar)
  return(a)
  # return(list(obj, y, set))
}






#' @export
for_u_in_U.ArgumentMLE1<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, ...)
{
  g<- getGraph(y)
  x<- diag(ecount(g))
  colnames(x)<- get.edge.attribute(g, "name", E(g))
  # if (ind)

  #x<- sigma(y, set, U_bar=Ubar)
  return(x)


  #mylist<- list(a=y, b=set, c=Ubar, d=ind, e=h1)
  #return(mylist)
}







#' @export
for_u_in_U.ArgumentCC<- function(obj, obj2, subsets, k_ratio, h1, depParams, Ubar, y, set, which_graph, ...)
{
  return(y$data)
}



