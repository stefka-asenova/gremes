#' Object of class \code{HRMnetwork}
#'
#' @param g must be an \code{igraph} object
#' @return Object of class \code{HRMnetwork} with two slots: \code{$graph} containing the graph and
#' \code{$depParams} the edge weights corresponding to the dependence parameters.
#' The dependence parameters are initialized at zero.
#' @export HRMnetwork
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' hrmnet<- HRMnetwork(g)
HRMnetwork<- function(g)
{

  if ( is.null(names(igraph::E(g))) )
  {
    g<- igraph::set.edge.attribute(g, "name", E(g), paste0("e",E(g)))
    message("From HRMnetwork: Edges have been assigned names")
  }
  edgeNames<- get.edge.attribute(g, "name", E(g))
  x<- numeric(length(edgeNames))
  names(x)<- edgeNames
  hr<- list(graph =g, depParams = x)
  class(hr)<- "HRMnetwork"
  return(hr)

}




#' Object of class \code{HRMtree}
#'
#' @param g must be an \code{igraph} object
#' @return Object of class \code{HRMtree} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
#' @export HRMtree
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' hrmtree<- HRMtree(g)
HRMtree<- function(g)
{
  # g must have named edges - this is important, else the code doesn't seem to work

  obj<- HRMnetwork(g)
  class(obj)<- append(class(obj), "HRMtree")
  obj<- validate(obj)
  return(obj)

}






#' Object of class \code{MME}
#'
#' It creates an object with two slots: \code{$graph} which contains the graph and \code{$depParams}
#' which contains the edge weights, initialized with zero values. It is a subclass of \code{HRMtree}.
#' It is intended to be used for models on trees to estimate edge weights using method of moment estimator.
#' Please consult Vignettes "Code - Note 1" and "Estimation - Note 1".
#' @param g must be an \code{igraph} object, a tree.
#' @return Object of class \code{MME} with two slots: \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights corresponding to the dependence parameters.
#' The edge weights are initialized with zero values.
#' @export
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' MME(g)
MME<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MME")
  return(obj)
}




#' Object of class \code{MLE}
#'
#' Creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for edge weights estimation using the Covariance selection model.
#' Consult also Vignettes "Code - Note 2" and "Estimation - Note 2".
#' @param g must be an \code{igraph} object, a tree.
#' @return Object of class \code{MLE} with two slots:  \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights corresponding to the dependence parameters.
#' The edge weights are initialized with zero values.
#' @export
#' @examples
#' g<- make_tree(8,3, mode="undirected")
#' g<- set.vertex.attribute(g, "name", V(g), letters[1:8])
#' mle<- MLE(g)
MLE<- function(g)
{

  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MLE")
  return(obj)
}



#' Object of class \code{MLE1}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult also Vignettes "Code - Note 2" and "Estimation - Note 2".
#' @param g must be an \code{igraph} object, a tree.
#' @return Object of class \code{MLE1} with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#'  the edge weights corresponding to the dependence parameters. The edge weights are initialized with zero values.
#' @export
#' @examples
#' g<- make_tree(8,3, mode="undirected")
#' g<- set.vertex.attribute(g, "name", V(g), letters[1:8])
#' mle1<- MLE1(g)
MLE1<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MLE1")
  return(obj)
}



#' Object of class \code{MLE2}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 2" and "Estimation - Note 2".
#' @param g must be an \code{igraph} object, a tree.
#' @return Object of class \code{MLE2} with two slots: \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights corresponding to the dependence parameters.
#' The edge weights are initialized with zero values.
#' @export
#' @examples
#' g<- make_tree(8,3, mode="undirected")
#' g<- set.vertex.attribute(g, "name", V(g), letters[1:8])
#' mle2<- MLE2(g)
MLE2<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append("MLE2", class(obj))
  return(obj)
}




#' Object of class \code{EKS}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 3" and "Estimation - Note 3".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{EKS} with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights corresponding to the dependence parameters.
#' @export EKS
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' eks<- EKS(g)
EKS<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append("EKS", class(obj))
  return(obj)
}




#' Object of class \code{EKS_part}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 3" and "Estimation - Note 3".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{EKS_part} with two slots: \code{$graph} containing the graph and \code{$depParams} containing the edge weights
#' corresponding to the dependence parameters.
#' @export EKS_part
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' ekspart<- EKS_part(g)
EKS_part<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append( class(obj), "EKS_part")
  return(obj)
}


#' Object of class \code{MMEave}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 6" and "Estimation - Note 6".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MMEave} with two slots: \code{$graph} containing the graph and \code{$depParams} containing the edge weights
#' corresponding to the dependence parameters.
#' @export MMEave
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mmeave<- MMEave(g)
MMEave<- function(g)
{
  obj<- HRMnetwork(g)
  class(obj)<- append(class(obj), "MMEave")
  return(obj)
}





#' Object of class \code{MLEave}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 6" and "Estimation - Note 6".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MLEave} with two slots: \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights
#' corresponding to the dependence parameters.
#' @export MLEave
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mleave<- MLEave(g)
MLEave<- function(g)
{
  obj<- HRMnetwork(g)
  class(obj)<- append(class(obj), "MLEave")
  return(obj)
}




#' Object of class \code{HRMBG}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on block graphs.
#' Consult Vignettes "Code - Note 5" and "Estimation - Note 5".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{HRMBG} with two slots: \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights corresponding to the dependence parameters. Ig the graph which is passed
#' in the argument has named edges these names are kept, if the edges are not named,
#' they are given names "e1", "e2", etc.
#' if the graph
#' @export HRMBG
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' hrmbg<- HRMBG(g)
HRMBG<- function(g)
{
  obj<- HRMnetwork(g)
  class(obj)<- append(class(obj), c("HRMBG", "MME"))
  return(obj)

}




#' Object of class \code{EngHitz}
#'
#' It creates an object with two slots: \code{$graph} containing the graph and \code{$depParams} containing
#' the edge weights which are initialized with zero values.
#' It is intended to be used for estimation of edge weights of models on trees.
#' Consult Vignettes "Code - Note 4" and "Estimation - Note 4".
#' @param g must be an \code{igraph} object
#' @return Object of class \code{EngHitz} with two slots: \code{$graph} containing the graph and \code{$depParams}
#' containing the edge weights corresponding to the dependence parameters.
#' @export EngHitz
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' enghitz<- EngHitz(g)
EngHitz<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append("EngHitz", class(obj))
  return(obj)

}






