
# RootDepSet<- function()
# {
#   class(obj)<- append(class(obj), "RootDepSet")
#   return(obj)
# }



#' An empty object of class \code{RootDepSet}
#' @param value Matrix or list containing subsets of the vertex set
#' @param root A list of vertices.
#' @export
#' @examples
#' rdsobj<- RootDepSet()
#' rds_values<- list(a=c("a", "b"), b=c("b", "c", "d"), c=c("c", "d"), d=c("c", "d", "e"), e=c("d", "e"))
#' rds_roots<- c("a", "b","c","d","e")
#' rdsobj<- setRootDepSet(rdsobj, rds_values, rds_roots)
RootDepSet<- function()
{
  obj<- list(value = array(), root = character(0))
  class(obj)<- append(class(obj), "RootDepSet")
  return(obj)
}






#' An empty object of class \code{Neighborhood}
#' @param value Matrix or list containing subsets of the vertex set.
#' A subset represents a neighborhood of a given node.
#' @param root A list of vertices.
#' @export
#' @examples
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' rdsobj<- Neighborhood()
#' rdsobj<- subset(rdsobj, 2, seg)
Neighborhood<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "Neighborhood")
  return(obj)
}





#' An empty object of class \code{FlowConnect}
#' @export
#' @examples
#' rdsobj<- FlowConnect()
#' seg<- graph(c(1,2, 2,3, 2,4, 4,5, 5,6, 5,7), directed = FALSE)
#' name_stat<- c("paris", "2", "meaux", "melun", "5", "nemours", "sens")
#' seg<- set.vertex.attribute(seg, "name", V(seg), name_stat)
#' seg_dir<- graph(c("2","paris", "meaux", "2", "melun", "2","5", "melun","nemours", "5", "sens", "5"), directed = TRUE)
#' fcg<- FlowConnectionGraph(seg_dir)
#' rdsobj<- subset(rdsobj, from=fcg, g=seg)
FlowConnect<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "FlowConnect")
  return(obj)
}





#' \code{Set} class
#'
#' Class definition
#' @export Set
Set<- function()
{
  obj<- RootDepSet()
  class(obj)<- append(class(obj), "Set")
  return(obj)
}



