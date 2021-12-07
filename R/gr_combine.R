
#' Combines objects
#'
#' It is designed to combine objects from classes \code{RootDepSet} for instance an object of class
#' \code{Neighborhood} and an object of class \code{FlowConnect}. For instance if for in one of the objects
#' we have root \eqn{a} with subset \eqn{(a,b,c)} and in the second object for root \eqn{a} we have subset
#' \eqn{(a,b,d,e)} then
#' applying \code{combine} will create for root \eqn{a} a subset \eqn{(a,b)}. See Vignette "Application Danube" for
#' examples.
#'
#' @rdname combine
#' @param obj Object of class \code{Set}
#' @param ... lists that are going to be combined
#' @export
#' @examples
#' # using two sets of class \code{RootDepSet}
#' rds1<- RootDepSet()
#' rds1<- setRootDepSet(rds1, list(a=letters[1:3], b=letters[1:4]), root=c("a", "b"))
#' rds2<- RootDepSet()
#' rds2<- setRootDepSet(rds2, list(a=letters[1:7], c=letters[2:5]), root=c("a", "c"))
#' myset<- Set()
#' combine(myset, rds1, rds2)
#'
#' # using lists with structure that imitates the one of class RootDepSet
#' list1<- list(value=list(a=letters[1:5], b=letters[1:4]), root=NULL)
#' list2<- list(value=list(a=letters[1:7], b=letters[2:5]), root=NULL)
#' myset<- Set()
#' combine(myset, list1, list2)
#' # See Vignette "Application Danube" for more examples.
combine<- function(obj, ...)
{
  UseMethod("combine")
}



#' @export
combine.default<- function(obj,...)
{
  return(paste("Method combine undefined for class ", class(obj)))
}


#' @export
combine.RootDepSet<- function(obj,...)
{
  NextMethod()
}

#' @export
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

  #x<- list(list1, list2)

  x<- list(...)

  z<- list()
  i=1
  for (y in x){
    z[[i]]<- y$value # takes only the slot value
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




#' @export
combine.Argument<- function(obj,...)
{
  NextMethod()
}


#' @export
combine.ArgumentSS<- function(obj, x, h1, j, ...)
{

  #  x the element which is to be combined with other elements passed before
  # h1 Argument necessary for efficiency reasons, namely to avoid growing objects within a loop
  # j step of the combining process
  #  If the argument passed is of class \code{ArgumentSS} and the second argument is a matrix
  #  then the matrices are substacked step by step as j increases. The method is similar to rbind(), but
  #  it avoids growing up matrices at each step j.

  h1<- c(0,cumsum(h1*(h1+1)/2))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- augmentCols(x, colnames(obj))
  obj[dim1,]<- x
  return(obj)
}



#' @export
combine.ArgumentMLE1<- function(obj, x, h1, j, ...)
{
  h1<- c(0,cumsum(h1))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- augmentCols(x, colnames(obj))
  obj[dim1,]<- x
  return(obj)
}




#' @export
combine.ArgumentD<- function(obj, x, h1, j, depParams, ...)
{

  # If the argument passed is of class \code{ArgumentD} and the second argument is a matrix
  #  then the matrices are placed on the diagonal of a block matrix.
  #  depParams a named vector of edge weights


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





#' @export
combine.ArgumentHvec<- function(obj, x, h1, j,...)
{

  #  If the argument passed is of class \code{ArgumentHvec} and the second argument is a matrix
  #  then the matrices half-vectorized and the vectors are substacked step by step as j increases.

  h1<- c(0,cumsum(h1*(h1+1)/2))
  dim1<- c((h1[j]+1):h1[j+1])
  x<- t(x)[lower.tri(x, diag=TRUE)]
  obj[dim1]<- x
  return(obj)
}


#' @export
combine.ArgumentCC<- function(obj, x, h1, j, ...)
{

#  If the argument passed is of class \code{ArgumentCC} then second arguments are
#  right stacked step by step as j increases. The result is the one of \code{cbind}, but it
#  avoids pre-allocation at every step.

  m<- c(0,cumsum(h1))
  obj[,(m[j]+1):(m[j+1])]<- x
  return(obj)
}




#' @export
combine.ArgumentSSvec<- function(obj, thisArg, h1, j,...)
{
  # thisArg a vector which is substacked


  b<- c(0, cumsum(h1))
  coord<- c((b[j]+1):b[j+1])
  obj[coord ]<- thisArg
  return(obj)
}



#' @export
combine.ArgumentEKS_part<- function(obj, thisArg, h1, j,...)
{
  b<- c(0, cumsum(h1))
  coord<- c((b[j]+1):b[j+1])
  obj[coord ]<- thisArg
  return(obj)
}


