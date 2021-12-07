
#' Generic for \code{subset}
#' @section  See \code{subset.FlowConnect} :
#' For description and examples of the method applied on objects of class \code{FlowConnect}.
#' @section See \code{subset.Neighborhood} :
#' For description and examples of the method applied on objectobjects of class \code{Neighborhood}.
#' @param obj object of appropriate class
#' @param ... additional arguments
#' @export
subset<- function(obj, ...)
{
  UseMethod("subset")
}






#' @export
subset.default<- function(obj, set, root, ...)
{
  return(set)
}





#' @export
subset.RootDepSet<- function(obj,...)
{
  NextMethod()
}





#' Subsets using flow connection information
#'
#' It derives subsbset(s) based on flow connection information. For uses see Vignettes "Subsets and Coordinates"
#' or "Application Danube".
#' @param obj Object of class \code{FlowConnect}
#' @param from The object from which the subsets should be extracted. For now two input types
#' are supported: \code{FlowConnectionMatrix} and \code{FlowConnectionGraph}.
#' The matrix in an object of class \code{FlowConnectionMatrix} is such that: entry ij is equal to 1 if node i
#' is flow connected to node j.
#' The graph in object of class \code{FlowConnectionGraph} should be directed according to the direction
#' of the water flow.
#' @param g \code{igraph} object, it may be directed (according to the water flow) or undirected graph
#' @param U_bar The set of nodes for which data are missing. Default is NULL.
#' @param vertices The vertices for which the set(s) of flow connected nodes are computed. The default
#' is the vertices with observed variables
#' @param matrixForm a matrix of ones and zeros: one if the node is in the subset and zero otherwise.
#' @param ... additional arguments
#' @return Object of the same class as \code{obj} but with non empty slots.
#' Slot \code{$value} contains a matrix or a list.
#' If it is a matrix, the number of columns is the same as the number of vertices
#' in the graph. If it is a list, the list contains for each of the roots an array of the nodes with
#' which that given root is flow connected.
#' The slot \code{$root} represents the roots - one for every subset.
#' @export
#' @examples
#' rdsobj<- FlowConnect()
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' seg_dir<- graph(c("2","paris", "meaux", "2", "melun", "2","5", "melun","nemours", "5", "sens", "5"),
#' directed = TRUE)
#' fcg<- FlowConnectionGraph(seg_dir)
#' subset(rdsobj, from = fcg, g = seg, U_bar = c("2", "5"), matrixForm = TRUE)
#' subset(rdsobj, from = fcg, g = seg, vertices=c("paris",  "meaux", "melun", "nemours", "sens"))
#' subset(rdsobj, from = fcg, g = seg)
subset.FlowConnect<- function(obj, from,
                              g,
                              U_bar = NULL,
                              vertices = base::setdiff(get.vertex.attribute(g, "name", V(g)), U_bar),
                              matrixForm = FALSE,  ...)
{

  #debug
  #obj<- rdsobj
  #from<- fcg
 # g<- seg
#  vertices=c("paris",  "meaux", "melun", "nemours", "sens")
  #---------------------------------------


  obj$value<- flowConnection(from)
   if (matrixForm)
   {
     obj$value<-obj$value[vertices,]
   } else {

             lobj<- list()
             for (v in vertices)
             {
                lobj[[v]]<- names(which(obj$value[v,]==1))
             }

             obj$value<- lobj
   }

  obj$root<- vertices
  return(obj)
}







#' Subsets based on neighborhood
#'
#' Derives subsets based on neighborhood of a certain order. For detailed uses see Vignettes.
#'
#' @param obj Object of class \code{Neighborhood}
#' @param eta Order of the neighborhood
#' @param g The graph, an \code{igraph} object
#' @param U_bar The set of nodes for which data are missing. Default is NULL. If there are nodes for which the
#' corresponding variables are not observed the set of these nodes should be provided here. This will avoid taking
#' a variable with unobserved variable to be taken as a root.
#' @param vertices Vertex(vertices) for which the neighboring set(s) are derived. Default is all vertices with observed variable.
#' @param matrixForm a matrix of ones and zeros: one if the node is in the subset and zero otherwise.
#' @param ... additional arguments
#' @return An object of class \code{Neighborhood} containing two slots: slot \code{$value} can be a matrix or list. If it
#' is a matrix, the number of columns is the same as the number of vertices
#' in the graph. The number of rows is the length of argument \code{vertices}.
#' If it is a list, the list contains for each of the nodes in \code{vertices}
#' the neighborhood of order \code{eta}. The slot \code{$root} contains the vertices
#' for which the neighborhood sets are obtained. If there are nodes with missing data subsets will be created for
#' roots with available data only. A subset however can contain a node with missing data.
#' @export
#' @examples
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' rdsobj<- Neighborhood()
#' subset(rdsobj, 2, seg)
#' subset(rdsobj, 2, seg, U_bar = c("melun", "5"), matrixForm = TRUE)
subset.Neighborhood<- function(obj,
                               eta,
                               g,
                               U_bar = NULL,
                               vertices = base::setdiff(get.vertex.attribute(g, "name", V(g)), U_bar),
                               matrixForm = FALSE, ...)
{
  if (matrixForm)
  {
    # debug
    # g<-t
    # U_bar<- letters[c(1,2,3)]
     # eta=2
     #vertices = base::setdiff(get.vertex.attribute(g, "name", V(g)), U_bar)
    # ------------

    set<- matrix(0, length(vertices), vcount(g))
    #  U<- get.vertex.attribute(g, "name", V(g))
    rownames(set)<- vertices
    colnames(set)<- get.vertex.attribute(g, "name", V(g))
    for (u in vertices)
    {
      set[u, neighborhood(g, order = eta, u, mode = "all")[[1]]]<- 1

      # #### IMPORTANT MODIFICATION ######
      # # in case of missing nodes this ensures that all the neighbors of the nodes with missing
      # # data are included such that each subtree is an identifiable model.
      # neib<- get.vertex.attribute(g, "name", igraph::neighborhood(g, order = eta, u, mode = "all")[[1]])
      # neib_mis<- base::intersect(neib, U_bar)
      # if (length(neib_mis) != 0)
      # {
      #   neib_mis<- unique(names(unlist(igraph::neighborhood(g, 1,neib_mis))))
      # }
      # neib<- base::union(neib, neib_mis)
      #
      # set[u, neib]<- 1
      # ##################################
    }

  } else {
    vv<- igraph::neighborhood(g, order = eta, nodes = vertices, mode="all")
    set<- lapply(vv, function(x) get.vertex.attribute(g, "name", x))
    names(set)<- vertices
  }

  obj$value<- set
  obj$root<- vertices
  #obj$order<- eta

  return(obj)

}


# !!!!!!!!!!!!!!!!!!!! Note : an important part of the subset function is that if the list output
# is chosen it needs to assign names to the components of the list corresponding to the
# value of the vertices






#' @export
subset.CovSelectTree<- function(obj, set, root, ...)
{
  g_u<- spanned_subgraph(obj$graph, root, set)
  Vg_u<- get.vertex.attribute(g_u, 'name', V(g_u))
  return(Vg_u)
}


