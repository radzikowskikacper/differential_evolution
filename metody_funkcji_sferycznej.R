# metody dla funkcji sferycznej


# funkcja oceny - na razie liczy po prostu wartość funkcji
evaluation<-function(coordinates)
{
  x1 = coordinates[1]#x
  x2 = coordinates[2]#y
  return(x1^2 + x2^2)
}


shift_p<-function(point)
{
  point[[1]][1] = point[[1]][1] + shift_v[1]
  point[[1]][2] = point[[1]][2] + shift_v[2]
  return (point)
}

genPointGP<-function()
{
  return (shift_p(point(rnorm(2))))
}




# return for values in range, false for NULL and another values
inRange<-function(point)
{
  if (is.null(point))
    return (FALSE)
  return (TRUE)
}
