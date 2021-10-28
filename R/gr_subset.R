
#' Generic for \code{subset}
#' @section  See \code{subset.FlowConnect} :
#' For description and examples of the method applied on objectobjects of class \code{FlowConnect}.
#' @section See \code{subset.Neighborhood} :
#' For description and examples of the method applied on objectobjects of class \code{Neighborhood}.
#' @section See \code{subset.CovSelectTree} :
#' For description and examples of the method applied on objectobjects of class \code{CovSelectTree}.
#' @export
subset<- function(obj, ...)
{
  UseMethod("subset")
}

#' @export
subset.RootDepSet<- function(obj,...)
{
  NextMethod()
}


#' Subset from flow connection
#'
#' It derives the sbset(s) based on flow connection information.
#' @param obj Object of class \code{FlowConnect}
#' @param from The object from which the subset should be extracted. For now two input types
#' are supported: \code{FlowConnectionMatrix} and \code{FlowConnectionGraph}.
#' In case of a matrix: entry ij is equal to 1 if node i is flow connected to node j.
#' In case of a graph: the graph should be directed according to the flow direction.
#' @param g \code{igraph} object, it may be directed (according to the flow) or undirected graph
#' @param vertices The vertices for which the set(s) of flow connected nodes are computed. The default
#' is all vertices in the graph
#' @param matrixForm Display results as matrix; default form
#' @return Object of the same class as \code{obj} but with non empty slots
#' @return value A matrix or a list. If matrix the number of columns should be the same as the number of vertices
#' in the graph. If list the list should contain for each of the roots an array of the nodes with
#' which it is (a given root) flow connected.
#' @return root The vertices for which the flow connection sets are obtained
#' @export
#' @examples
#' rdsobj<- FlowConnect()
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' seg_dir<- graph(c("2","paris", "meaux", "2", "melun", "2","5", "melun","nemours", "5", "sens", "5"), directed = TRUE)
#' fcg<- FlowConnectionGraph(seg_dir)
#' rdsobj<- subset(rdsobj, from=fcg, g=seg)
subset.FlowConnect<- function(obj,
                              from,
                              g,
                              vertices = get.vertex.attribute(g, "name", V(g)),
                              matrixForm = FALSE, ...)
{
  # a matrix of 1 and 0 indicating whether node i is flow connected with node j
  # if matrixForm=TRUE it returns a matrix if not it returns a list
  # vertices - for each of the elements in "vertices" the set of Flow connected vertices  is obtained
  obj$value<- flowConnection(from, g, vertices = get.vertex.attribute(g, "name", V(g)), ...)
  if (!matrixForm)
  {
    lobj<- list()
    for (v in vertices)
    {
      lobj[[v]]<- names(which(obj$value[v,]==1))
    }
    obj$value<-lobj
  }
  obj$root<- vertices
  #obj$order<- NULL
  return(obj)
}





#' Subset from neighborhood
#'
#' Derives the subset based on neighborhood of a certain order
#'
#' @param obj Object of class \code{Neighborhood}
#' @param eta Order of the neighborhood
#' @param g The graph, an \code{igraph} object
#' @param U_bar The set of nodes for which data are missing.
#' @param vertices Vertex(vertices) for which the neighbouring set(s) are derived. Default is all vertices.
#' @return An object of class \code{Neighborhood} containing two slots: slot 'value' can be a matrix or list. If it
#' is a matrix, the number of columns should be the same as the number of vertices
#' in the graph. The number of columns should be the number of the parameter 'vertices'.
#' If it is a list, the list should contain for each of the nodes in the parameter 'vertices'
#' the neighborhood of order eta. The slot 'root' contains the vertices
#' for which the neighborhood sets are obtained.
#' @export
#' @examples
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' rdsobj<- Neighborhood()
#' rdsobj<- subset(rdsobj, 2, seg)
subset.Neighborhood<- function(obj,
                               eta,
                               g,
                               U_bar=NULL,
                               vertices = base::setdiff(get.vertex.attribute(g, "name", V(g)), U_bar),
                               matrixForm = FALSE, ...)
{
  if (matrixForm)
  {
    set<- matrix(0, length(vertices), vcount(g))
    #  U<- get.vertex.attribute(g, "name", V(g))
    rownames(set)<- vertices
    colnames(set)<- get.vertex.attribute(g, "name", V(g))
    for (u in vertices)
    {
      #set[u, neighborhood(g, order = eta, u, mode = "all")[[1]]]<- 1

      #### IMPORTANT MODIFICATION ######
      # in case of missing nodes this ensures that all the neighbours of the nodes with missing
      # data are included such that each subtree is an identifiable model.
      neib<- igraph::neighborhood(g, order = eta, u, mode = "all")[[1]]
      neib_mis<- base::intersect(neib, U_bar)
      if (length(neib_mis) != 0)
      {
        neib_mis<- unique(names(unlist(igraph::neighborhood(g, 1,neib_mis))))
      }
      neib<- base::union(neib, neib_mis)

      set[u, neib]<- 1
      ##################################
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
subset.default<- function(obj, set, root, ...)
{
  return(set)
}



#' @export
subset.CovSelectTree<- function(obj, set, root, ...)
{
  g_u<- spanned_subgraph(obj$graph, root, set)
  Vg_u<- get.vertex.attribute(g_u, 'name', V(g_u))
  return(Vg_u)
}


