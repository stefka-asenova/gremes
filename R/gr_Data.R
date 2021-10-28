

GData<- function(x)
{
  class(x)<- append(class(x), "GData")
  return(x)
}



CSData<- function(x)
{
  class(x)<- append(class(x), "CSData")
  return(x)
}
