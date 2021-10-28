
#' It retrives the graph of an object of appropriate class
#'
#' The function is applied on objects of appropriate class with slot 'graph'. It is used instead of the command 'obj_name$graph'.
#' @param obj Object of appropriate class ('Network', 'HRMnetwork')
#' @rdname getGraph
#' @export
getGraph<- function(obj,...)
{
  UseMethod("getGraph")
}









#' @rdname getGraph
#' @export
getGraph.default<- function(obj,...)
{
  warning("Default method is called on unrecognised object")
  return(obj)
}



#' @rdname getGraph
#' @export
getGraph.Network<- function(obj,...)
{
  return(obj$graph)
}








#' @rdname getGraph
#' @export
getGraph.HRMnetwork<- function(obj,...)
{
  return(obj$graph)
}













#' It retrieves the dataset from an appropriate object
#'
#' The function is applied on objects of appropriate class with slot 'data'.
#' It is used instead of the command 'obj_name$data'.
#'
#' @rdname getData
#' @param obj Object with slot 'data'
#' @param ... Additional arguments
#' @export
getData<- function(obj,...)
{
  UseMethod("getData")
}







#' @rdname getData
#' @export
getData.default<- function(obj,...)
{
  warning("Default method is called on unrecognised object")
  return(obj)
}







#' @rdname getData
#' @export
getData.Network<- function(obj,...)
{
  return(obj$data)
}





#' It retrives the nodes without data
#'
#' It retrieves the value of the slot 'noDataNodes', the set of nodes for which there are no data available
#'  for an object 'Network'.
#' @rdname getNoDataNodes
#' @param obj Object of class 'Network'
#' @export
getNoDataNodes<- function(object, ...)
{
  UseMethod("getNoDataNodes")
}


#' @rdname getNoDataNodes
#' @export
getNoDataNodes.default<- function(object, ...)
{
  return("NA")
}



#' @rdname getNoDataNodes
#' @export
getNoDataNodes.Network<- function(object, ...)
{
  U_bar<- object$noDataNodes
  return(U_bar)
}




#' It retrieves the nodes for which data are missing
#'
#' It retrieves the slot 'nodesWithData' of an object 'Network'. The slot represents the set of nodes for
#' which there is no data available.
#' @rdname getNodesWithData
#' @param obj Object of class 'Network'
#' @export
getNodesWithData<- function(obj, ...)
{
  UseMethod("getNodesWithData")
}


#' @rdname getNodesWithData
#' @export
getNodesWithData.default<- function(obj, ...)
{
  return(character(0))
}



#' @rdname getNodesWithData
#' @export
getNodesWithData.Network<- function(obj, ...)
{
  U<- obj$nodesWithData

  return(U)
}

#' It retrives the parameters associated to an HRMnetwork
#'
#' It retrieves the value of the slot 'depParams' of a HRMnetwork.
#' @param obj An object of class 'HRMnetwork'
#' @rdname getParams
#' @export
getParams<- function(obj, ...)
{
  UseMethod("getParams")
}



#' @rdname getParams
#' @export
getParams.default<- function(obj, ...)
{
  return("Default method is called on object of unknown class")
}


#' @rdname getParams
#' @export
getParams.HRMnetwork<- function(obj, ...)
{
  x<- obj$depParams
  return(x)
}



#' It retrieves the subsets of an object 'RootDepSet'
#'
#' It retrieves the slot 'value' of an object of class 'RootDepSet'.
#' @rdname getValue
#' @param obj An object of class 'RootDepSet'
#' @export
getValue<- function(obj,...)
{
  UseMethod("getValue")
}







#' @rdname getValue
#' @export
getValue.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}








#' @rdname getValue
#' @export
getValue.RootDepSet<- function(obj, ...)
{
  return(obj$value)
}



getValue.RootIndSet<- function(obj, ...)
{
  return(obj$value)
}



#' It retrieves the roots associated to a subset
#'
#' It retrieves the slot 'root'
#' @rdname getRoot
#' @param obj An object of class 'RootDepSet'
#' @export
getRoot<- function(obj,...)
{
  UseMethod("getRoot")
}





#' @rdname getRoot
#' @export
getRoot.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}



#' @rdname getRoot
#' @export
getRoot.RootDepSet<- function(obj, ...)
{
  return(obj$root)
}


getRoot.RootIndSet<- function(obj, ...)
{
  return(obj$root)
}


