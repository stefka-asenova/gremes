#' @export Coordinates
Coordinates<- function()
{
  x<- numeric(0)
  class(x)<- "Coordinates"
  return(x)
}




#' @export Tuples
Tuples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Tuples")
  return(obj)
}




#' @export Triples
Triples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Triples")
  return(obj)
}





#' @export Quadruples
Quadruples<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Quadruples")
  return(obj)
}





#' @export Adjacent
Adjacent<- function()
{
  obj<- Coordinates()
  class(obj)<- append(class(obj),"Adjacent")
  return(obj)
}



