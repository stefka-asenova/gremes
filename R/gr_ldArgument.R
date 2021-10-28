
ldArgument<- function(obj, ...)
{
  UseMethod("ldArgument")
}




ldArgument.HRMtree<- function(obj, J, ...)
{
  g<- getGraph(obj)
  params<- getParams(obj)

  if (length(J)!=2)
    stop("The set J must contain only two elements")
  if (sum(J %in% get.vertex.attribute(g, "name", V(g)))<2)
    stop("Incorrect vertex set J")

  ep<- edge_names_along_path(obj, rt=J[1], id=J[2], edge_names = TRUE)
  s<- sum(params[ep]^2)
  ld<- pnorm(sqrt(s)/2)
  return(ld)

}


ldArgument.Tree<- function(obj, x, k_ratio, ...)
{
  # it returns a vector of the derivatives  via finite differencing method

  # debug
#  x<- c(0,0.1,0,0.9,0)
 # x<- evalPts[20,]
 # names(x)<- nodes
#  obj<- tobj
#  j=J[2]
  #----------

  J<- names(which(x>0))
  ld<- rep(0, length(J))
  names(ld)<- J
#  i<- 1
  for(j in J)
  {

    x_up<- x

    x_down<- x

    dt<- 10^{-5}
    dtt<- dt
    repeat
    {

      x_up[j]<- x[j]+dt
      x_down[j]<- x[j]-dt
      ld[j]<- (stdf(obj, x_up, k_ratio ) - stdf(obj, x_down, k_ratio ))/(2*dt)
     # print(ld[j])
      if (ld[j]>0)
      {
        break
      }
      dt<- dt+dtt
    }

  }

  return(ld)
}





