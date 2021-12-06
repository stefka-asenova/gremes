#' Object with zero entries and of fixed dimensions
#'
#' Creates a numeric object with given dimensions and zero entries
#' @rdname Argument
#' @description Definition of the class. \code{Argument} is an abstract class. The objects of this class
#' are used usually in the final estimation steps.
Argument<- function(dims)
{
  args<- array(0, dim=dims)
  class(args)<-  append(class(args), "Argument")
  return(args)
}





#' Class definition
#' It should be used when vectors or matrices are to be substacked
#' @param dims Dimensions of the argument
#' @param x Vector of names that can be used to be assigned to the columns/rows
#' @rdname Argument
ArgumentSS<- function(dims, x)
{
  #suppose dims is a scalar or two-el vector
  if (length(dims)!=2)
    stop("The dimension of this argument should be 2")

  obj<- Argument(dims)
  colnames(obj)<- x
  class(obj)<- append(class(obj), "ArgumentSS")
  return(obj)
}






#' @rdname Argument
ArgumentMLE1<- function(dims, x)
{
  #suppose dims is a scalar or two-el vector
  if (length(dims)!=2)
    stop("The dimension of this argument should be 2")

  obj<- Argument(dims)
  colnames(obj)<- x
  class(obj)<- append(class(obj), "ArgumentMLE1")
  return(obj)
}



#' \code{ArgumentHvec} class
#'
#' Class definition
#' It should be used when two dimensional matrices are to be halfvectorized
#' @rdname Argument
ArgumentHvec<- function(dims)
{
  hvec<- Argument(dims)
  class(hvec)<- append(class(hvec), "ArgumentHvec")
  return(hvec)
}





#' @rdname Argument
ArgumentD<- function(dims)
{
  if (length(dims)!=2)
    stop(" 'dims' should be a numeric of length 2")
  if (dims[1]!=dims[2])
    stop("This argument should have equal number of rows and columns")

  ad<- Argument(dims)
  class(ad)<- append(class(ad), "ArgumentD")
  return(ad)
}






#' @rdname Argument
ArgumentCC<- function(dims)
{
  if (length(dims)!=2)
    stop(" 'dims' should be a numeric of length 2 ")
  acc<- Argument(dims)
  class(acc)<- append(class(acc), "ArgumentCC")
  return(acc)
}






#' @rdname Argument
ArgumentSSvec<- function(dims)
{
  x<- Argument(dims)
  class(x)<- append(class(x), "ArgumentSSvec")
  return(x)
}






#' @rdname Argument
ArgumentEKS_part<- function(dims)
{
  x<- Argument(dims)
  class(x)<- append(class(x), "ArgumentEKS_part")
  return(x)
}







