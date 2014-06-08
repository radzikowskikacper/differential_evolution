

# funkcja oceny - na razie liczy po prostu wartość funkcji
evaluation<-function(coordinates)
{
  x1 = coordinates[1]#x
  x2 = coordinates[2]#y
  return((1+(x1+x2+1)^2*(19-14*x1+3*x1^2-14*x2+6*x1*x2+3*x2^2))*(30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2)))
}


genPointGP<-function()
{
  return (point(runif(2, -2, 2)))
}

# return for values in range, false for NULL and another values
inRange<-function(point)
{
  if (is.null(point))
    return (FALSE)
  if (point$coordinates[1] <= 2 && point$coordinates[1] >= -2 && point$coordinates[2] >= -2 && point$coordinates[2] <= 2)
    return (TRUE)
  return (FALSE)
} 
