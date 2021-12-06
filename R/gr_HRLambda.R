#' Parameter matrix of a Huesler-Reiss distribution
#'
#' It creates the parameter matrix Lambda of the limiting max-stable Huesler-Reiss distribution which is an attractor of
#' a graphical model with respect to some block graph and whose distribution is composed cliquewise from
#' Huesler-Reiss distributions. See Vignette "Introduction" too.
#' The entry lambda_{ij} is the sum of the edge weights on the shortest
#'  path between node i and node j in the block graph.
#' The matrix Lambda can be used to generate observations from that max-stable Huesler-Reiss distribution.
#' @rdname HRLambda
#' @export
HRLambda<- function(obj, ...)
{
  UseMethod("HRLambda")
}




#' @rdname HRLambda
#' @param obj is an object of class \code{HRMBG} with non-zero edge weights.
#' @param ... additional arguments
#' @return A symmetric matrix whose entry lambda_{ij} is the sum of the edge weights on the shortest
#' path between node i and node j.
#' @export
#' @examples
#' g<- graph(c(1,3,1,2,2,3,
#'             3,4,4,5,5,3,
#'             3,7,3,6,6,7), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c", "d", "e", "f", "g"))
#' # all deltas are squares already
#' C1<- c(0.2, 0.8, 0.6)   # d_13^2, d_12^2, d_23^2
#' C2<- c(0.3, 0.5, 0.1)   # d_34^2, d_45^2, d_35^2
#' C3<- c(0.4, 0.05, 0.25) # d_37^2, d_36^2, d_67^2
#' hrmbgobj<- HRMBG(g)
#' hrmbgobj<- setParams(hrmbgobj, c(C1, C2, C3))
#' hrmlam<- HRLambda(hrmbgobj)
HRLambda.HRMBG<- function(obj,...)
 {
  #the scope of this method is to build the matrix Lambda of the HR distribution
  #based on the edge-wise parameters; The matrix is as in Prop 3.1

  # debug
  #obj<- hrmbgobj
  #------------------------

  g<- obj$graph

  vs<- get.vertex.attribute(g, "name", V(g))
  nvs<- length(vs)
  q<- c(1:nvs)
  colIndex<- unlist(sapply(q, function(x) base::setdiff(vs, vs[1:x])))
  pairsOfLambda<- base::rbind(rep(vs, c(nvs:1)), c(vs, colIndex))

  Lamb<- matrix(0, nrow = nvs, ncol = nvs)
  colnames(Lamb)<- rownames(Lamb)<- vs
  for (i in 1:ncol(pairsOfLambda))
  {
    fc<- pairsOfLambda[1,i]
    sc<- pairsOfLambda[2,i]
    if ( fc == sc)
    {
      Lamb[fc, sc]<- 0
    }
    else {
      # sp<- unlist(get.shortest.paths(g, fc, sc)$vpath)
      # sp1<- rep(sp, rep(2, length(sp)))
      # sp1<- sp1[2:(length(sp1)-1)]
      # geids<- get.edge.ids(g, sp1)
      # enames<- get.edge.attribute(g, "name", geids)

      # potentially replace this by !!!!!!!!!!!!
      enames<- edge_names_along_path(obj, fc, sc)



      Lamb[fc, sc]<- sum(obj$depParams[enames])
    }
  }

  Lamb[lower.tri(Lamb)]<- t(Lamb)[lower.tri(Lamb)]
  return(Lamb)
}
