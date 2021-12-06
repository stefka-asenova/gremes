
# RootDepSet<- function()
# {
#   class(obj)<- append(class(obj), "RootDepSet")
#   return(obj)
# }



#' An empty object of class \code{RootDepSet}
#'
#' Creates an object of class \code{RootDepSet} with two empty slots \code{$value} and \code{$root}.
#' The slots are supposed to be assigned values. The simplest way is using method \code{setRootDepSet}.
#' Every node with observable variable should be taken as a root.
#' A subset of nodes is created for every root.
#' For every root a subset of nodes must be created.
#' For other examples see Vignette "Subsets and Coordinates".
#' @export
#' @examples
#' rdsobj<- RootDepSet()
#' rds_values<- list(a=c("a", "b"), b=c("b", "c", "d"))
#' rds_roots<- c("a", "b")
#' rdsobj<- setRootDepSet(rdsobj, rds_values, rds_roots)
RootDepSet<- function()
{
  obj<- list(value = array(), root = character(0))
  class(obj)<- append(class(obj), "RootDepSet")
  return(obj)
}






#' An empty object of class \code{Neighborhood}
#'
#' It is a subclass of \code{RootDepSet}.
#' It creates an object with two empty slots \code{$value} for the collection of subsets and \code{$root} for the roots
#'  associated to each of the subsets in \code{$value}.
#' Every node with observable variable should be taken as a root.
#' A subset of nodes is created for every root.
#' For every root the subset of nodes is created on the principle of neighborhood of some order.
#' The slots are supposed to be assigned values.
#' The simplest one is using method \code{setRootDepSet}. For other examples see Vignette "Subsets and Coordinates".
#' @export
#' @examples
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' rdsobj<- Neighborhood()
#' rdsobj # the slots are empty
#' rdsobj<- subset(rdsobj, 2, seg)
#' rdsobj # the slots are filled under the criterion of neighborhood of order 2.
Neighborhood<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "Neighborhood")
  return(obj)
}





#' An empty object of class \code{FlowConnect}
#'
#' It is a subclass of \code{RootDepSet}.
#' It creates an object with two empty slots \code{$value} for the collection of subsets of nodes
#' and \code{$root} for the roots
#' associated to each of the subsets in \code{$value}.
#' Every node with observable variable should be taken as a root.
#' A subset of nodes is created for every root.
#' For every root the subset of nodes is created on the principle of flow connection -
#' a term from river networks applications.
#' The slots are supposed to be assigned to some values. The simplest ways is
#' using method \code{setRootDepSet}. For other examples see Vignette "Subsets and Coordinates".
#' @export
#' @examples
#' seg<- make_tree(7,3, mode = "undirected")
#' name_stat<- letters[1:7]
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#'
#' seg_dir<- make_tree(7,3, mode = "in")
#' seg_dir<- set.vertex.attribute(seg_dir, "name", V(seg_dir), name_stat)
#' fcg<- FlowConnectionGraph(seg_dir)
#'
#' rdsobj<- FlowConnect()
#' subset(rdsobj, from=fcg, g=seg)
FlowConnect<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "FlowConnect")
  return(obj)
}





#' An empty object of class \code{Set}
#'
#' It is a subclass of \code{RootDepSet}.
#' It creates an object with two empty slots \code{$value} for the collection of subsets
#' and \code{$root} for the roots
#' associated to each of the subsets in \code{$value}.
#' An object of class \code{Set} contains a collection of subsets on the
#'  vertex set and it is used in some of the estimation methods.
#' @export Set
#' @examples
#' myset<- Set()
#' # See Vignette "Application Danube"
Set<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "Set")
  return(obj)
}





