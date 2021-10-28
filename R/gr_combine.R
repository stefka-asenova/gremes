
#' @title Generic for method \code{combine}
#' @description Defines different ways of combining objects according to the class of its first argument.
#' The use of the method with objects of class \code{Argument} is internal method and it does not have
#' independent use.
#'
#' @rdname combine
#' @param obj Object of class \code{Set} or \code{Argument}
#' @param ... If the first argument is of class \code{Set} this argument can be as many objects of class
#' \code{RootDepSet}, for instance \code{Neighborhood} or \code{FlowConnect}
#' @details If the first argument is of class \code{Set} followed by several objects of
#' class \code{RootDepSet} the method combines the root dependent sets into one set
#' which is the intersection of all the sets per root. See examples.
combine<- function(obj, ...)
{
  UseMethod("combine")
}


combine.default<- function(obj,...)
{
  return(paste("Method combine undefined for class ", class(obj)))
}

combine.RootDepSet<- function(obj,...)
{
  NextMethod()
}


#' @rdname combine
combine.Set<- function(obj, ...)
{
  # ----!---- NB----!----
  # !!!!!!!!!!!! the arguments passed should be matrices in the correct form of size (.)xd
  #  IMPORTANT CASE:  in this respect be careful with vectors, which can happen when as input to 'vertices' you pass
  # is only one node. Hence in RootDepSet$value if the value is in a matrix form:
  # - it should be a matrix, in R sense
  # - it should be of size (.)xd, where d is the number of vertices in the graph
  # - it should have named rows and named columns
  # ----!---- NB----!----


  x<- list(...)

  z<- list()
  i=1
  for (y in x){
    z[[i]]<- y$value
    i=i+1
  }

  checkIfMatrices<- sapply(z, function(x) is.matrix(x))
  checkIfLists<- sapply(z, function(x) is.list(x))
  if (sum(checkIfMatrices)==length(z))
  {

    newz<- c()
    for(y in z)
    {
      newz<- rbind(newz, y)
    }
    root_set<- unique(rownames(newz))
    vertex_set<- unique(colnames(newz))
    mat<- matrix(0, nrow=length(root_set), ncol=length(vertex_set))
    rownames(mat)<- root_set
    colnames(mat)<- vertex_set
    for (root in root_set)
    {
      if (sum(rownames(newz)==root)==1)
      {
         mat[root,]<- newz[root,]
      } else {
        mat[root,]<- apply(newz[which(rownames(newz)==root),], 2, min)
      }
    }

  } else if (sum(checkIfLists)==length(z)) {

    newz<- c()
    for(y in z)
    {
      newz<- c(newz, y)
    }
    root_set<- unique(names(newz))
    mat<- vector("list", length(root_set))
    names(mat)<- root_set
    for (root in root_set)
    {
      l<- newz[which(names(newz)==root)]
      mat[[root]]<- intersect(l)
    }


  } else { stop("The sets must be either all matrices or all lists")
  }

  obj$value<- mat
  obj$root<- root_set
  return(obj)

}





combine.Argument<- function(obj,...)
{
  NextMethod()
}


#' @rdname combine
#' @param x the element which is to be combined with other elements passed before
#' @param h Argument necessary for efficiency reasons, namely to avoid growing objects within a loop
#' @param j step of the combining process
#' @details If the argument passed is of class \code{ArgumentSS} and the second argument is a matrix
#' then the matrices are substacked step by step as j increases. The method is similar to rbind(), but
#' it avoids growing up matrices at each step j.
combine.ArgumentSS<- function(obj, x, h1, j, ...)
{
  h1<- c(0,cumsum(h1*(h1+1)/2))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- augmentCols(x, colnames(obj))
  obj[dim1,]<- x
  return(obj)
}


combine.ArgumentMLE1<- function(obj, x, h1, j, ...)
{
  h1<- c(0,cumsum(h1))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- augmentCols(x, colnames(obj))
  obj[dim1,]<- x
  return(obj)
}



#' @rdname combine
#' @param depPars ???
#' @details If the argument passed is of class \code{ArgumentD} and the second argument is a matrix
#' then the matrices are placed on the diagonal of a block matrix.
combine.ArgumentD<- function(obj, x, h1, j, depParams)
{

  x<- augmentCols(x, names(depParams))
  m<- x %*% (depParams^2)
  sig<- matrix(0, h1[j], h1[j])
  sig[lower.tri(sig, diag = TRUE)]<- m
  sig[upper.tri(sig)]<- t(sig)[upper.tri(sig)]

  #sig[lower.tri(sig, diag=TRUE)]<- s       #the code below creates a symmetric matrix
  #sig<- t(sig)                             #by first filling the rows !
  #sig[lower.tri(sig)]<- t(sig)[lower.tri(sig)]

  #sig[upper.tri(sig)]<- t(sig)[upper.tri(sig)]


  h2<- c(0, cumsum(h1))
  dim1<- c((h2[j]+1):h2[j+1])
  obj[dim1, dim1]<- sig
  return(obj)
}




#' @rdname combine
#' @details If the argument passed is of class \code{ArgumentHvec} and the second argument is a matrix
#' then the matrices half-vectorized and the vectors are substacked step by step as j increases.
combine.ArgumentHvec<- function(obj, x, h1, j,...)
{
  h1<- c(0,cumsum(h1*(h1+1)/2))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- t(x)[lower.tri(x, diag=TRUE)]
  obj[dim1]<- x
  return(obj)
}


#' @rdname combine
#' @details If the argument passed is of class \code{ArgumentCC} then second arguments are
#' right stacked step by step as j increases. The result is the one of \code{cbind}, but it
#' avoids pre-allocation at every step.
combine.ArgumentCC<- function(obj, x, h1, j, ...)
{
  m<- c(0,cumsum(h1))
  obj[,(m[j]+1):(m[j+1])]<- x
  return(obj)
}


combine.ArgumentSSvec<- function(obj, thisArg, h1, j,...)
{
  b<- c(0, cumsum(h1))
  coord<- c((b[j]+1):b[j+1])
  obj[coord ]<- thisArg
  return(obj)
}



combine.ArgumentEKS_part<- function(obj, thisArg, h1, j,...)
{
  b<- c(0, cumsum(h1))
  coord<- c((b[j]+1):b[j+1])
  obj[coord ]<- thisArg
  return(obj)
}


