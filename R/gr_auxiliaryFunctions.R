

#' Combine matrices along in a three dimensional matrix
#' @importFrom abind abind
#' @param ... arrays of dimension 3
acomb<- function(...) abind::abind(..., along=3)






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



# Computes the mean and the covariance matrix for the vector \eqn{R_j}. It is designed to
# be used by the method \code{stdf.EKS()}
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



# this one isn't working properly - DON't export
# the function serves to clean up from zeros the estimates
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


#' Plots Extremal coefficients
#'
#' It generates a scatterplot of bivariate parametric versus non-parametric extremal coefficients.
#' It marks which ones extremal coefficients are between flow connected nodes. Used for application on rivers.
#'
#' @param matT Matrix of parametric estimates of biavariate extremal coefficients
#' @param matE Matrix of non-parametric estimates of bivariate extremal coefficients
#' @param flowConnect Matrix of ones and zeros: 1 at row i and column j means that node i and node j are
#' flow connected
#' @param ... additional arguments
#' @export
#' @importFrom graphics plot
#' @importFrom graphics points
#' @note See Vignettes "Application Danube" and "Application Seine" for examples on post-estimation analysis.
plotEC<- function(matT, matE, flowConnect,...)
{
  W_<- colnames(matT)
  nW_<- length(W_)
  q<- c(1:nW_)
  colIndex<- unlist(sapply(q, function(x) base::setdiff(W_, W_[1:x])))
  pairsOfSigma<- base::rbind(rep(W_, c((nW_-1):0)), colIndex)

  #nV<- ncol(ecMatrix)
  graphics::plot(1, type="n",
       ylab="non-parametric",
       xlim=c(1, 2),
       ylim=c(1, 2),
       lwd=1.5,
       ...)
  abline(a=0, b=1)
  legend(x = "topleft",c("flow unconnected", "flow connected"),
         pch = c(1, 3), col = c("black", "tomato"), pt.lwd = 1.5, bty = "n")

  for (i in 1:(nW_*(nW_+1)/2-nW_))
  {
    ch<- ifelse(flowConnect[pairsOfSigma[1,i],pairsOfSigma[2,i]]==1, 3, 1 )
    colrs<- ifelse(flowConnect[pairsOfSigma[1,i],pairsOfSigma[2,i]]==1, "tomato", "black")
    graphics::points(matT[pairsOfSigma[1,i],pairsOfSigma[2,i]], matE[pairsOfSigma[1,i], pairsOfSigma[2,i]],
           col=colrs, type="p", pch=ch, lwd=1.5)
  }

}


#' Identifiability in case of latent variables
#'
#' It verifies if the idenifiability criterion in case of latent variables is satisfied on the level of subsets.
#' Basically it verifies for each subset whether the identifiability criterion is satisfied for the subgraph
#' induced by this subset: verifies if every node with latent variables within the subgraph has degree at least
#'  three. It is applicable only for tree models.
#' @param obj should be an object of class \code{RootDepSet}
#' @param tobj should be an object of class \code{Tree}
#' @export
#' @examples
#'  seg<- graph(c(1,2,
#' 2,3,
#' 2,4,
#' 4,5,
#' 5,6,
#' 5,7), directed = FALSE)
#' name_stat<- c("Paris", "2", "Meaux", "Melun", "5", "Nemours", "Sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' # we need some data to create the object of class "Tree"
#' seg_data<- matrix(rnorm(10*7), 10, 7)
#' colnames(seg_data)<- name_stat
#' tobj<- Tree(seg, seg_data[,c("Paris", "Meaux", "Melun", "Nemours", "Sens")])
#' # create the neighborhood of order one and call the function "is_identifiable"
#' nobj<- Neighborhood()
#' nobj<- subset(nobj, 1, seg, U_bar=getNoDataNodes(tobj))
#' is_identifiable(nobj, tobj)
#' nobj<- subset(nobj, 2, seg, U_bar=getNoDataNodes(tobj))
#' is_identifiable(nobj, tobj)
#'  # See also Vignette "Subsets and Coordianates"
is_identifiable<- function(obj, tobj)
{

  # debug
  #obj<- nobj
  #tobj
  #-----------


  nvalue<- length(obj$value)
  for (i in 1:nvalue)
  {
    set<- obj$value[[i]]
    set_Uc<- base::intersect(set, tobj$noDataNodes)
    g_set<- induced_subgraph(tobj$graph, set)
    non_confUset<- set_Uc[degree(g_set, set_Uc)<3]
    if (length(non_confUset)>0)
    {
      message(cat("The nodes with latent variables { ", non_confUset,  " } in set { ", set, " } have degree less than three.
                  The subgraph contains edge parameters that are non-identifiable.\n", sep=" "))
    }
  }
}




