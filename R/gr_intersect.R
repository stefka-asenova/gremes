#' Intersects elements in a list
#'
#' It finds the intersection of all objects in a list.
#' @param x a list
#' @export
#' @rdname intersect
intersect<- function(x)
{
  UseMethod("intersect", x)
}

#' @export
intersect.default<- function(x)
{
  return(base::intersect(x))
}




#' @export
intersect.list<- function(x)
{  if (length(x)==1)
   {
     x<- unlist(x)
     names(x)<- c()
     return(x)
   } else {
            g<- x[[1]]
            for (i in 2:length(x))
            {
               g<- base::intersect(g, x[[i]])
            }
            return(g)
          }
}


