# metody dla funkcji sferycznej


# funkcja oceny - na razie liczy po prostu wartość funkcji
evaluation<-function(coordinates)
{
  x1 = coordinates[1]#x
  x2 = coordinates[2]#y
  return(x1^2 + x2^2)
}

genPointGP<-function()
{
  return (point(rnorm(2)))
}




# return for values in range, false for NULL and another values
inRange<-function(point)
{
  if (is.null(point))
    return (FALSE)
  return (TRUE)
}