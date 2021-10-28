#' \code{HRMnetwork} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{HRMnetwork} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
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




#' \code{HRMtree} class
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






#' \code{MME} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MME} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
#' @export MME
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mme<- MME(g)
MME<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MME")
  return(obj)
}




#' \code{MLE} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MLE} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
#' @export MLE
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mle<- MLE(g)
MLE<- function(g)
{

  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MLE")
  return(obj)
}



#' \code{MLE1} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MLE1} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
#' @export MLE1
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mle1<- MLE1(g)
MLE1<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append(class(obj), "MLE1")
  return(obj)
}



#' \code{MLE2} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MLE2} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
#' @export MLE2
#' @examples
#' g<- graph(c("a","b", "b","c", "b", "d"), directed=FALSE)
#' mle2<- MLE2(g)
MLE2<- function(g)
{
  obj<- HRMtree(g)
  class(obj)<- append("MLE2", class(obj))
  return(obj)
}




#' \code{EKS} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{EKS} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
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




#' \code{EKS_part} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{EKS_part} with two slots: one containing the graph and the other the edge weights
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


#' \code{MMEave} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MMEave} with two slots: one containing the graph and the other the edge weights
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

#' \code{MLEave} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{MLEave} with two slots: one containing the graph and the other the edge weights
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

#' \code{HRMBG} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{HRMBG} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
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





#' \code{EngHitz} class
#' @param g must be an \code{igraph} object
#' @return Object of class \code{Enghitz} with two slots: one containing the graph and the other the edge weights
#' corresponding to the dependence parameters.
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






