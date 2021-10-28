





ldArgument1<- function(obj, ...)
{
  UseMethod("ldArgument1", obj)
}



#' @importFrom stats dnorm
ldArgument1.HRMtree<- function(obj, x, ...)
{
  g<- getGraph(obj)
  params<- getParams(obj)
  J<- names(which(x>0))
  x1<- x[J[1]]
  x2<- x[J[2]]

  if (length(J)!=2)
    stop("The set J must contain only two elements")
  if (sum(J %in% get.vertex.attribute(g, "name", V(g)))<2)
    stop("Incorrect vertex set J")

  ep<- edge_names_along_path(obj, rt=J[1], id=J[2], edge_names = TRUE)
  s<- sum(params[ep]^2)
  ld1<- pnorm((log(x1/x2)-s/2)/sqrt(s))+
    1/sqrt(s)*dnorm((log(x1/x2)-s/2)/sqrt(s))-
    x2/(sqrt(s)*x1)*dnorm((log(x2/x1)-s/2)/sqrt(s))

  ld2<- pnorm((log(x2/x1)-s/2)/sqrt(s))+
    1/sqrt(s)*dnorm((log(x2/x1)-s/2)/sqrt(s))-
    x1/(sqrt(s)*x2)*dnorm((log(x1/x2)-s/2)/sqrt(s))
  ld<- c(ld1,ld2)

  return(ld)

}
