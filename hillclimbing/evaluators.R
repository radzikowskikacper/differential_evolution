evaluationGP<-function(point)
{
  x1 = point[1]
  x2 = point[2]
  
  return((1 + (x1 + x2 + 1) ^ 2 * (19 - 14 * x1 + 3 * x1 ^ 2 - 14 * x2 + 6 * x1 * x2 + 3 * x2 ^ 2)) * (30 + (2 * x1 - 3 * x2) ^ 2 * (18 - 32 * x1 + 12 * x1 ^ 2 + 48 * x2 - 36 * x1 * x2 + 27 * x2 ^ 2)))
}

evaluationSpherical<-function(point)
{
  return(point[1] ^ 2 + point[2] ^ 2)
} 

evaluationEggHolder<-function(point)
{
  x = point[1]
  y = point[2]
  
  return (-y - 47) * sin(sqrt(abs(y + x / 2 + 47))) - x * sin(sqrt(abs(x - y + 47)))
}