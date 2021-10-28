#' generic for method 'intersect'
intersect<- function(x)
{
  UseMethod("intersect", x)
}

#' default method for 'intersect'
intersect.default<- function(x)
{
  return(base::intersect(x))
}


#'
#' Intersects the elements in a list; the list can have more than 2 elements
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


