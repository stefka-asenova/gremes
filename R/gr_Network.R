#' Creates objects of calss \code{Network}
#' @param x The graph with named nodes, an object of class \code{igraph}
#' @param data Data set, with named columns
#' @export Network
#' @examples
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b" ,"c")) # name the nodes
#' data<- matrix(rnorm(10*3), 10,3)
#' colnames(data)<- c("a", "b", "c")  # name the columns
#' net<- Network(x = g, data = data)
Network<- function(x, data)
{

  obj<- list(graph=x,
            data=data)
  class(obj)<-  "Network"
  validate(obj)
  obj<- list(graph=x,
             data=data,
             nodesWithData=colnames(data),
              noDataNodes= setdiff(get.vertex.attribute(x, "name", V(x)), colnames(data)))
  class(obj)<-  "Network"
  return(obj)
}






#' Creates an object of class \code{Tree}
#'
#' An object which contains the tree and the dataset
#' @param x the graph with named nodes,  an object of class \code{igraph}
#' @param data Dataset with named columns
#' @export
#' @examples
#' g<- make_tree(7,3, mode="undirected")
#' g<- set.vertex.attribute(g, "name", V(g), letters[1:7])
#' data<- matrix(rnorm(10*7), 10,7)
#' colnames(data)<- get.vertex.attribute(g, "name", V(g))
#' # object without latent variables
#' Tree(x = g, data = data)
#' # object with latent variables
#' Tree(x = g, data = data[,-c(1,2)])
Tree<- function(x, data)
{

  obj<- Network(x, data)
  class(obj)<- append(class(obj), "Tree")
  obj<- validate(obj)
  return(obj)
}




#' Creates object of class \code{GTree}
#' @export GTree
#' @param g should be the graph, an object of class \code{igraph} with named nodes
#' @param Data is the dataset with named columns
#' @examples
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes of the graph
#' data<- matrix(rnorm(10*3), 10,3)
#' colnames(data)<- c("a", "b", "c") # name the columns of the data
#' gt<- GTree(g = g, Data = data)
GTree<- function(g, Data)
{
  obj<- Tree(x = g, data = Data)
  class(obj)<- append(class(obj), "GTree")
  return(obj)

}





#' Creates object of class \code{CovSelectTree}
#' @export CovSelectTree
#' @param g should be the graph, an object of class \code{igraph} with named nodes
#' @param Data is the dataset with named columns
#' @examples
#' g<- graph(c(1,2,2,3), directed=FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes of the graph
#' data<- matrix(rnorm(10*3), 10,3)
#' colnames(data)<- c("a", "b", "c") # name the columns of the data
#' cst<- CovSelectTree(g = g, Data = data)
CovSelectTree<- function(g, Data)
{
  obj<- Tree(g, Data)
  class(obj)<- append(class(obj), "CovSelectTree")
  return(obj)
}




#' Creates object of class \code{BlockGraph}
#' @export
#' @param x the graph, an object of class \code{igraph} with named nodes
#' @param data dataset with named columns
#' @examples
#' g<- graph(c(1,2,2,3), directed = FALSE)
#' g<- set.vertex.attribute(g, "name", V(g), c("a", "b", "c")) # name the nodes of the graph
#' data<- matrix(rnorm(10*3), 10, 3)
#' colnames(data)<- c("a", "b", "c") # name the columns of the data
#' bg<- BlockGraph(x = g, data = data)
BlockGraph<- function(x, data)
{
  # debug
  #x<- g
  #data<- X
  #-------------

  obj<- Network(x, data)
  class(obj)<- append(class(obj), "BlockGraph")
  obj<- validate(obj)
  return(obj)
}


