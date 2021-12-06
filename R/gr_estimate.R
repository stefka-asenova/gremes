#' Provides estimation of parameters
#'
#' Estimates the edge weights or edge parameters of a model parameterized cliquewise by Huesler-Reiss
#' distributions. For description of the model please consult the Vignette "Introduction".
#' @export
#' @param obj object of on of the following classes \code{MME, MLE, MLE1, MLE2, EKS, EKS_part, EngHitz, MME_ave, MLE_ave, HRMBG}.
#' The classes \code{MME, MLE, MLE1, MLE2, EKS, EKS_part, EngHitz} are subclasses of class \code{HRMtree},
#' hence if no other method is available, the method for \code{HRMtree} applies.
#' @param Data the dataset, it should have named columns. For \code{estimate.MLE} all variables on the tree should
#' be observed, i.e., \code{Data} should have nonempty column for every node in the tree.
#' @param subsets object of class \code{RootDepSet} containing the roots and the respective subsets.
#' When \code{obj} is of class \code{EngHitz} the argument \code{subsets} should be a list of subsets,
#' not a \code{RootDepSet} object. When \code{obj} is of class \code{EngHitz} the union of all subsets should cover
#' the whole vertex set and also
#' every pair of subsets can have at most one node in common. See also Vignettes
#' "Code - Note 4" and "Estimation - Note 4".
#' @param k_ratio a scalar the ratio of the number of upper order statistics to the number of observations
#' @param coord A matrix of evaluation points with number of columns equal to the number of nodes in the tree and
#' column names according to the
#' names of the nodes.
#' The matrix of evaluation points should not contain a positive coordinate for a variable which is unobserved.
#' For creation of coordinates based on tuples, triples, and adjacent nodes see classes
#' \code{Coordinates}, \code{Tuples}, \code{Triples}, \code{Adjacent} as well as method \code{evalPoints}
#' @param ... additional arguments
#' @rdname  estimate
#' @return The estimates of edge weights. For objects \code{MME} and \code{MLE} it returns the squared
#' values of the parameters.
#' @details For an object of class \code{EKS_part} for a fixed subset all coordinates based on both tuples and triples
#' are taken.
#' @note For detailed examples please consult Vignettes "Code - Note" 1-6.
estimate<- function(obj, ...)
{
  UseMethod("estimate")
}




#' @export
estimate.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}




#' @export
estimate.HRMnetwork<- function(obj, ...)
{
  NextMethod()
}








#' @rdname estimate
# #' @aliases estimate
# #' @export
# #' @param obj Object of class \code{MME}, \code{MLE}
# #' @param Data Dataset with the variables placed on the columns, object of classes \code{matrix} or \code{data.frame}.
# #' Each variable should have the same names as the node to which it belongs. For instance
# #' if the name of the variable in the dataset is 'X2' the name of the node to which it belongs should be
# #' 'X2'.
# #' @param subsets Object of class \code{RootDepSet}. The sets for EngHitz are different!!!! EXPLAIN
# #' @param k_ratio The fraction of the upper order statistics of the sample size
 #' @param xx A vector of length the number of observed variables and named with the names of the nodes with
 #' observed variables.
 #' Default is NULL. For \code{estimate.EKS_part} it should be filled in. For the other methods
 #' the value at default should be kept.
# #' The vector should be of length at least the number of vertices for which there is data available.
# #' It should be named with the names of the vertices. The values given can be arbitrary, but values of
# #' 1 correspond to extremal coefficints.
#' @import igraph
#' @importFrom quadprog solve.QP
#' @export
estimate.HRMtree<- function(obj, Data, subsets, k_ratio, xx=NULL, ...)
{

 #  # # # # #debug
 #    obj<- mme
 #    Data=DataEvents
 #     subsets<- comb_set
 # k_ratio=0.5
 #  # # # # #-------


  x<- createObj(obj, Data) # by default it creates a GTree (CovSelectTree is in case .MLE,
  # and BlockGraph in case of HRMBG)
  g1<- getGraph(x)
  pn<- get.edge.attribute(g1, "name", E(g1))
  e<- ecount(g1)
  Uc<- getNoDataNodes(x)
  U<- getNodesWithData(x)
  roots<- getRoot(subsets)
  if(length(setdiff(U, roots))!=0)
    stop("A subset must be defined for every node in the graph for which there is data available")


   args<- NextMethod(obj2 = x, Ubar = Uc, par_names = pn, gr = g1, ... )



  sol<- quadprog::solve.QP(Dmat = 2*crossprod(args$A),
                                dvec = 2*crossprod(args$A,args$s),
                                Amat = diag(e),
                                bvec = rep(0,e))$solution
  obj<- setParams(obj, sol)
  return(obj)
  #return(list(obj, x, g1, pn, e, Uc, U, roots))
  # return(args)
}








#' @export
estimate.MME<- function(obj, Data, subsets, k_ratio, obj2, Ubar, par_names,  gr, ...)
{
  #obj MME, MLE, ..
  #obj2 GTree, CovSelectTree, BlockGraph

  h1<- h(subsets, U_bar = Ubar)
  s<- ArgumentHvec(sum(h1*(h1+1)/2))
  s<- for_u_in_U(obj=s, obj2=obj2, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar)


   A<- ArgumentSS( c(sum(h1*(h1+1)/2),ecount(gr)), par_names)
  A<- for_u_in_U(obj=A, obj2=obj, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar )
  return(list(A=A, s=s))

  #return(list(obj, obj2, subsets, k_ratio, par_names, Ubar, gr, exm))
  # return(A)
  # return(A)
}




#' @rdname estimate
#' @export
estimate.HRMBG<- estimate.HRMtree



# #' Estimates the parameters of a HRM tree using the MLE version 1
# #'
# #' The function is designed to be used as inherited method
# #' @references Asenova, S and Segers, J. Husler-Reiss Markov tree
# #' @param mle1obj Object of class \code{MLE1}
# #' @param obj2 Object of class \code{Gtree}
# #' @param Ubar The set of nodes for which there are no data available
# #' @param par_names A character vector with the names of the parameters associated to the edges of the tree
# #' @param gr The graph associated to the mle1obj
#' @export
estimate.MLE1<- function(obj,  Data, subsets, k_ratio, obj2, Ubar, par_names, gr, ...)
{
  # # # debug
  #subsets<- rds
  #  par_names<- get.edge.attribute(g, "name", E(g))
  #  obj2<- x
  #gr=g_mis
  # # Uc<- getNoDataNodes(obj2)
  # # #-------------


  #obj_mle1<- obj # this preassignment is necessary, when you pass it to for_u_in_U
  h1<- h_edges(subsets, gr)
  s<- ArgumentSSvec(sum(h1))
  s<- for_u_in_U(obj=s, obj2=obj2, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar, obj_mle1=obj, depParams=NULL)


  A<- ArgumentMLE1( c(sum(h1),ecount(gr)), par_names)
  A<- for_u_in_U(obj=A, obj2=obj, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar, depParams=NULL)

  return(list(A=A, s=s))


  # return(list(obj, Data, subsets, k_ratio, obj2, Ubar, par_names, gr))
}










#' @export
estimate.EKS_part<- function(obj, Data, subsets, k_ratio, obj2 , Ubar, par_names, gr, xx, ...)
{
  # debug
  # gr=grr
  # subsets<- rds
  #------

   h1<- h_edges(subsets, gr)
   s<- ArgumentEKS_part(sum(h1))
   s<- for_u_in_U(obj=s, obj2=obj, subsets, k_ratio, h1, depParams=NULL, Ubar, Data, xx)
  #
  #
  A<- ArgumentMLE1( c(sum(h1),ecount(gr)), par_names)
  A<- for_u_in_U(obj=A, obj2=obj, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar)
  return(list(A = A, s = s))

  # return(list(obj, Data, subsets, k_ratio, obj2, Ubar, par_names, gr, xx))
  # return(s)
}



















#' @export
#' @importFrom stats optim
estimate.MLE2<- function(obj, Data, subsets, k_ratio, ...)
{
  # # # debug
  # obj<- mle2obj
  #  Data<- data_red
  # subsets<- rdsobj
  #  k_ratio=0.05
  # # #-------

  g<- obj$graph
  en<- ecount(g)
  gtobj<- GTree(g, Data)
  Uc<- getNoDataNodes(gtobj)
  h1<- h(subsets, U_bar = Uc)
  k<- round(k_ratio*nrow(Data))

  # create the dataset
  xobj<- ArgumentCC( c(k, sum(h1)) )
  X<- suppressMessages(for_u_in_U(obj=xobj, obj2=gtobj,
                                  subsets= subsets, k_ratio=k_ratio,
                                  h1=h1, depParams=NULL, Ubar=Uc))

  params<- obj$depParams+1 # the initial values
  eps<- 10^(-5)
  #up<- 10^2

  obj$depParams<- suppressMessages(stats::optim(params, llmle2, gr=NULL,
                                         method = "L-BFGS-B",
                                         lower = rep(eps, en),
                                         upper = rep(Inf, en),
                                         data= X,
                                         obj = obj, subsets = subsets, k_ratio= k_ratio, h1=h1, Ubar=Uc)$par)

  return(obj)

 # return(list(g, en, gtobj, Uc, h1, k, xobj, X, params, eps))

}













# #' Estimation based on extremal coefficients
#'
# #' @param coord A matrix of evaluation points with column names according to the
# #' names of the edges. For creation of coordinates based on tuples, triples, and adjacent nodes see classes
# #' \code{Coordinates}, \code{Tuples}, \code{Triples}, \code{Adjacent} as well as method \code{evalPoints}
#' @rdname estimate
#' @importFrom stats optim
#' @export
estimate.EKS<- function(obj, Data, coord, k_ratio,... )
{
  #debug
  # Data<- mydata
  # obj<- objprob
  # Data = dataprob
  # coord<- mypairs
  # k_ratio = 0.05
  #---------

  en<- ecount(obj$graph)
  tobj<- Tree(obj$graph, Data)
  Uc<- getNoDataNodes(tobj)
  if (length(Uc) != 0)
  {
    if (sum(degree(obj$graph, Uc) < 3)!=0) # I think you should place this in the validation part of the class Tree
      stop("The model is not identifiable")
    if (nrow(coord) < en)
      stop("The model is not identifiable. Add more coordinates at which the stdf is evaluated")
  }

  stdf_emp<- suppressMessages(stdf(tobj, Y = coord, k_ratio))
  params<- rep(1, en)         # the initial values
  res<- suppressMessages(stats::optim(params, fnk,
                               gr = NULL,
                               method = "L-BFGS-B",
                               lower = rep(0, en),
                               upper = rep(Inf, en),
                               hessian = FALSE,
                               eksobj = obj, evalPoints = coord, stdf_emp = stdf_emp, Ubar = Uc)$par)
  obj<- setParams(obj, res)
  return(obj)
}














#' @export
estimate.MLE<- function(obj, Data, subsets, k_ratio, obj2, Ubar, par_names, gr, ...)
{
  h1<- h(subsets, U_bar = Ubar)
  s<- ArgumentHvec(sum(h1*(h1+1)/2))
  s<- for_u_in_U(obj=s, obj2=obj2, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar)


  A<- ArgumentSS( c(sum(h1*(h1+1)/2),ecount(gr)), par_names)
  A<- for_u_in_U(obj=A, obj2=obj, subsets, k_ratio=k_ratio, h1=h1, Ubar=Ubar )
  return(list(A=A, s=s))
}









#' @rdname estimate
#' @export
estimate.MMEave<- function(obj, Data, k_ratio, ...)
{

  # obj is an object of class MMEave
  # Data is the data in matrix form with named columns
  # rdsobj should be the rootDepSet
  # ss_bar should be the matrix A such that we minimize(A*theta-vec(Sigma_hat))



  # # # # debug
  # obj<- mme_ave #aveobj
  # Data<- resid #mydata
  # k_ratio<- 0.2
  # rdsobj<- RootDepSet()
  # rdsobj<- setRootDepSet(rdsobj,  get.vertex.attribute(seg, "name", V(seg)), "Paris")
  # ################
  n<- nrow(Data)
  k<- round(n*k_ratio)

  Y<- -log(1-copula::pobs(Data))
  Y_bar<- apply(Y, 1, mean)
  ind_ex<- order(Y_bar)[(n-k+1):n]
  deltaExcess<- Y[ind_ex,]-Y_bar[ind_ex]


  seg<- getGraph(obj)
  gtobj<- GTree(seg, Data = deltaExcess)

  Uc<- getNoDataNodes(gtobj)
  U<- getNodesWithData(gtobj)

  rdsobj<- RootDepSet()
  rdsobj<- setRootDepSet(rdsobj,  get.vertex.attribute(seg, "name", V(seg)), U[1])


  ss_bar = NULL
  ss_hat<- as.vector(sigma(gtobj))

  if (is.null(ss_bar))
  {
      Lamb<- Lambda(obj, obj2 = rdsobj, U_bar = Uc)
      ss_bar<- sigma(obj, obj2= rdsobj, Lamb, U_bar= Uc)
  }
  sol<- quadprog::solve.QP(Dmat = 2*crossprod(ss_bar),
                           dvec = 2*crossprod(ss_bar, ss_hat),
                           Amat = diag(ncol(ss_bar)),
                           bvec = rep(0,ncol(ss_bar)))$solution
   obj<- setParams(obj, sqrt(sol))
   return(obj)

}


#' @rdname estimate
#' @importFrom stats optim
#' @export
estimate.MLEave<- function(obj, Data, k_ratio, ...)
{

  # # # debug
  #  obj<- mle_ave
  #  Data<- Seine
  #  k_ratio<- 0.2
  # # lmbd<- Lambda
  # # #------------------
  seg<- obj$graph
  tobj<- Tree(x= seg, data = Data)
  Uc<- getNoDataNodes(tobj)
  U<- getNodesWithData(tobj)

  rdsobj<- RootDepSet()
  rdsobj<- setRootDepSet(rdsobj,  get.vertex.attribute(seg, "name", V(seg)), U[1])

  n<- nrow(Data)
  k<- round(k_ratio*n)
  lmbd = NULL
  if (is.null(lmbd))
  {
    lmbd<- Lambda(obj, rdsobj, U_bar = Uc)
  }

  Y<- -log(1-copula::pobs(Data))
  Y_bar<- apply(Y, 1, mean)
  ind_ex<- order(Y_bar)[(n-k+1):n]
  deltaExcess<- Y[ind_ex,]-Y_bar[ind_ex]


  params<- obj$depParams+1 # the initial values
  eps<- 10^(-5)
  en<- length(params)
  # debug
  # go to the function llmleave to debug that one too and come back here
  #----------------

  obj$depParams<- suppressMessages(stats::optim(params, llmleave, gr=NULL,
                                         method = "L-BFGS-B",
                                         lower = rep(eps, en),
                                         upper = rep(Inf, en),
                                         data= deltaExcess,
                                         Lambda = lmbd)$par)

  return(obj)


}



#' @importFrom stats optim
#' @export
estimate.EngHitz<- function(obj, Data, subsets, k_ratio, ...)
{

  # #debug
  #  obj<- eh
  #  Data<- XU
  #  subsets<- list(c("c", "h"), letters[1:7])
  #  k_ratio<- 0.2
  # #-----------------------



  g<- getGraph(obj)
  n<- nrow(Data)
  k<- round(k_ratio*n)
  Xbar<- 1/(1-copula::pobs(Data))
  Xmax<- apply(Xbar, 1, max)
  excIndex<- order(Xmax)[(n-k+1):n]
  deltaExc<- Xbar[excIndex,]*k/n

  #create the tobj to obtain the Uc, the set of latent variables, else you don't need this object
  tobj<- Tree(g, Data)
  Uc<- getNoDataNodes(tobj)

  res<- getParams(obj)

  for (i in 1:length(subsets))
  {
    subtree<- induced_subgraph(g, subsets[[i]])

    good_for_roots<- base::setdiff(subsets[[i]], Uc)
    # create the sigma matrix
    hrmtobj<- HRMtree(subtree)
    ssobj<- RootDepSet()
    ssobj<- setRootDepSet(ssobj, subsets[[i]], good_for_roots[1])
    MA<- sigma(hrmtobj, ssobj, Uc)

    # prepare for optimization
    eps<- 10^(-6)
    en<- ecount(subtree)
    #params<- c(1,1,1)
    eot<- deltaExc[,base::setdiff(subsets[[i]], Uc)]

    res[get.edge.attribute(subtree, 'name', E(subtree))]<- stats::optim(rep(0.5, en), ll_EngHitz, gr=NULL,
                                                                 method = "L-BFGS-B",
                                                                 lower = rep(eps, en),
                                                                 upper = rep(Inf, en),
                                                                 data= eot,
                                                                 obj = hrmtobj, MA= MA, Uc=Uc)$par

  }

  obj<- setParams(obj, res)
  return(obj)


}







