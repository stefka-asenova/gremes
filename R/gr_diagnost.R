

#' Diagnostic tool for generated sample
#'
#' Performs diagnosis on the generated data according to a graphical model on a tree
#' with cliquewise Huesler-Reiss distributions. As one diagnostic tool, the function produces a graph comparing
#' the real marginal distribution
#' with the empirical distribution of the variable in \code{id}. As a second diagnostic tool
#' for every adjacent node to \code{id} it computes the real and the empirical copulas at the coordinates
#' passed to \code{y}.
#'
#' @param obj Object of class \code{HRMtree}
#' @param ... additional arguments
#' @param y bivariate vector with elements between 0 and 1. The coordiantes of the bivariate copula. The default is
#' (0.5, 0.5).
#' @export
diagnost<- function(obj, ...)
{
  UseMethod("diagnost", obj)
}






#' @export
diagnost.default<- function(obj, ...)
{
  return("Default method called on unrecognized object")
}






#' @export
#' @rdname diagnost
#' @param X A matrix of data to be diagnosted
#' @param id The name of the variable for which the diagnostics is done.
#' @examples
#' # create a graph with named vertices
#' g<- graph(c("a", "b", "b","c", "b", "d"), directed = FALSE)
#' # create a HRMtree object
#' myobj<- HRMtree(g)
#' x<- c(0.1,0.2,0.3)
#' myobj<- setParams(myobj, x)
#' # create a dataset
#' mydata<- rHRM(myobj, 1000)
#' # do diagnostic on the node "b"
#' diagnost(myobj, mydata, "b")
#' # include noise in the data
#' mydata<- rHRM(myobj, 1000, noise = TRUE)
#' diagnost(myobj, mydata, "c")
diagnost.HRMnetwork<- function(obj, X, id, y = c(0.5, 0.5),  ...)
{
  if(!is.character(id))
    stop("The 'id' should be a character, corresponding to the name of a vertex in the tree")
  gr<- getGraph(obj)
  params<- getParams(obj)
  NextMethod(gr = gr , theta = params, ...)
}





#' @export
#' @importFrom stats plot.ecdf
#' @importFrom graphics curve
diagnost.HRMtree<- function(obj, X, id, y, gr, theta, ...)
{

  x<- NULL
  # diagnostic of Marginal distribution
  stats::plot.ecdf(log(X[,id]))
  title(paste("variable id: ", id), adj=1)
  graphics::curve(exp(-exp(-x)), add = TRUE, col=3)
  legend(x= "bottomright", c("Empirical", "True"), col=c(1,3), lty=c(1,1)  )


  #iTau(huslerReissCopula(),cor(X[,4],X[,5], method="kendall"))

  # to verify the dependencies look at the true copula given the parameters and the empirical
  # copula given the sample.

  nb<- names(neighborhood(gr, order=1, nodes=id, mode="all")[[1]])

  for (i in 2:length(nb))
  {
    edge_id<- get.edge.ids(gr, vp=c(nb[1],nb[i]))
    edge_name<- get.edge.attribute(gr, "name", edge_id)

    HR_cop<- copula::huslerReissCopula(2/theta[edge_name])
    u<- matrix(y,nrow = 1)
    U<- cbind( copula::pobs(X[,id]), copula::pobs(X[,nb[i]]) )
    print(paste("variable ", id,
                "adj. variable ", nb[i],
                "; true copula ", copula::pCopula( u, HR_cop ),
                "; empirical copula ", copula::C.n(u, U)))
  }

  #return(list(obj, X, id, gr, theta))

}




