createObj<- function(obj,...)
{
  UseMethod("createObj", obj)
}

createObj.default<- function(obj, data,...)
{
  #creating a GTree object as default method
  myobj<- GTree(obj$graph, Data = data)
  return(myobj)
}


createObj.MLE<- function(obj, data)
{
  myobj<- CovSelectTree(obj$graph, Data = data)
  Ubar<- getNoDataNodes(myobj)
  if (length(Ubar)!=0)
    stop("Covariance Selection Model estimation is impossible with missing variables in the data.")

  return(myobj)
}




createObj.HRMBG<- function(obj, data)
{
  myobj<- BlockGraph(obj$graph, data)
  return(myobj)
}
