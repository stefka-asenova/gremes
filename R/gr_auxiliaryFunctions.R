#'
#' Combine matrices along in a three dimensional matrix
#' @importFrom abind abind
#' @export
acomb<- function(...) abind::abind(..., along=3)


#' @importFrom abind abind
# #' @export
acomb<- function(...) abind::abind(..., along=3)


# #' @export walking
walking <-  function(g, v, a = NULL)
{

  pairs <- character(0)
  N <- setdiff(igraph::get.vertex.attribute(g, "name", igraph::neighbors(g, v)), a)
  for (w in N)
  {
    pairs <- rbind(pairs, c(v, w))
    pairs <- rbind(pairs, walking(g, w, v))
  }

  return(pairs)

}






augmentCols<- function(A, nameCols)
{
  A_aug<- matrix(0, ncol = length(nameCols), nrow=nrow(A))
  colnames(A_aug)<- nameCols
  for (e in colnames(A))
  {
    A_aug[, e]<- A[,e]
  }
  return(A_aug)
}


augmentMatrix1<- function(A, name_to_add)
{
  if (length(name_to_add)==0)
  {
    return(A)
  } else {
    vnames<- colnames(A)
    all_names<- base::union(vnames, name_to_add)
    rowvec<- rep(0, length(vnames))
    colvec<- rep(0, length(all_names))
    A_aug<- rbind(A, rowvec)
    A_aug<- cbind(A_aug, colvec)
    colnames(A_aug)<- all_names
    rownames(A_aug)<- all_names


    # A_aug<- matrix(0, ncol = length(nameCols), nrow=length(nameCols))
    # colnames(A_aug)<- nameCols
    # rownames(A_aug)<- nameCols
    # for (e in colnames(A))
    # {
    #   A_aug[, e]<- A[,e]
    # }
    return(A_aug)
  }
}

#' Creates a connected induced subgraph on a set of vertices
#'
#' On the basis of a root and a set of vertices it creates the tree spanned by the paths between
#' the root and the vertices.
#' @param g The original graph, an \code{igraph} object
#' @param u The root from which the paths to the other nodes will be computed
#' @param W The set of nodes to which the paths from the root will be computed
#' @return A subgraph that is induced from all nodes on the paths between the root and the given set
#' of vertices. If the original graph is connected, then the spanned subgraph will be connected too.
# #' @export spanned_subgraph
spanned_subgraph<- function(g, u, W)
{

  P<- shortest_paths(graph=g, from=u, to=W, output="vpath")
  ab<- c()
  for (i in 1:length(P$vpath))
  {
    a<- get.vertex.attribute(g, "name", P$vpath[[i]])
    ab<- base::union(ab, a)

  }
  g_u<- induced.subgraph(g, ab)
  return(g_u)
}


#' Mean and covariance matrix
#'
#' Computes the mean and the covariance matrix for the vector \eqn{R_j}. It is designed to
#' be used by the method \code{stdf.EKS()}
tilde_Rj<- function(Ju, j, mu, sig)
{

  #debug


  #---------


  nJu<- length(Ju)

  # determine the matrix M^j
  Mj<- matrix(0, ncol = nJu, nrow = nJu)
  rownames(Mj)<- Ju
  colnames(Mj)<- Ju
  Mj[,j]<- -1
  Juj<- base::setdiff(Ju,j)

  for (u in Juj)
  {
    Mj[u,u]<-1
  }

  # determine the mean and the variance of \tilde(R^j)
  aj<- rep(0, nJu)
  names(aj)<- Ju
  aj[as.character(j)]<- 1
  tilde_muj<- Mj %*% (sig %*% aj + mu)
  sigma_muj<- Mj %*% sig %*% t(Mj)

  results<- list(tilde_muj, sigma_muj)
  names(results)<- c("mu", "sigma")
  return(results)
}



place_x<- function(y, x, U)
{
  # it is necessary for evalPoints()
  # n_miss is U
  # x needs to be a named vector
  cm<- rep(0,length(U))
  names(cm)<- U
  cm[y]<- x[y]
  return(cm)
}



looknan<- function(x)
{
  a<- which(is.nan(x))
}


#' Computes means and standard deviations for a matrix of estimates
#'
#' It cleans up the matrix of estimates from NaNs and after that it computes the means
#' and the variances of the estimators per parameter and per k.
#'
#' @param obj A matrix of dimensions (#parameters)X(#k)X(#estimators)X(#simulations)
#' @return A list with two sub-lists: one containing a matrix of means (m), the other containing
#' a matrix of variances (v). The matrix of means is of dimensions (#estimators)X(#k)X(#parameters).
#' To access the estimates of the first parameter use [,,1] and so on. Similarly for the matrix
#' of variances.
#' @export
#' @importFrom stats var
means_vars<- function(obj)
{
  dims<- dim(obj)
  mean_<- list()
  var_<- list()
  for (i in 1:dims[1])
  {

    for (j in 1:dims[2])
    {
      vv<- apply(obj[i,j,,], 1, looknan) # it doesn't recognize the function looknan

      myl<- unique(unlist(vv))
      mean_[[paste0("m",i,j)]]<- apply(obj[i,j,,base::setdiff(1:dims[4], myl)], 1, mean)
      var_[[paste0("v",i,j)]]<- apply(obj[i,j,,base::setdiff(1:dims[4], myl)], 1, stats::var)

    }

  }
  mym<- array(unlist(mean_), dim=c(dims[3], dims[2], dims[1]))
  myv<- array(unlist(var_), dim=c(dims[3], dims[2], dims[1]))
  mv<- list(m=mym, v=myv)
  return(mv)
}




# the function serves to clean up from zeros the estimates
#??
clean_up<- function(estim, endval)
{

  #estim must have named columns



  #debug

  #estim<- mme_all
  #endval<- 0

  #-----------------


  b<- c()
  for (i in 1:nrow(estim))
  {
    b<- union(b, names(which(estim[i,]==endval)))
  }
  ind<- base::setdiff(colnames(estim), b)
  estim<- estim[,ind]

  return(estim)
}


#??
clean_up_nan<- function(estim)
{

  #estim must have named columns



  #debug

  #estim<- mme_all
  #endval<- 0

  #-----------------


  b<- c()
  for (i in 1:nrow(estim))
  {
    b<- union(b, names(which(is.nan(estim[i,]))))
  }
  ind<- base::setdiff(colnames(estim), b)
  estim<- estim[,ind]
  #colnames(estim)<- ind

  return(estim)
}




# this function is equivalent to 'edge_names_along_path(obj, u,v, edge_names=FALSE)'
# hence it needs to be suppressed

# shortPath2vec<- function(g, fc, sc)
# {
#   # # debug
#   # fc<- "a"
#   # sc<- "d"
#   # #-------------
#
#
#   sp<- unlist(get.shortest.paths(g, fc, sc)$vpath)
#   sp1<- rep(sp, rep(2, length(sp)))
#   sp1<- sp1[2:(length(sp1)-1)]
#   geids<- get.edge.ids(g, sp1)
#   enames<- get.edge.attribute(g, "name", geids)
#   aa<- rep(0, ecount(g))
#   names(aa)<- get.edge.attribute(g, "name", E(g))
#   aa[enames]<- 1
#   return(aa)
# }
