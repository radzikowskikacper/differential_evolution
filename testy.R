# 1 

# metoda wyswietlająca wynik w przyjazny sposób
show_output2D<-function(out)
{
  a = NULL
  b = NULL
  c = NULL
  h = NULL
  for (i in 1:length(out))
  {
    h[i] = i
    a[i] = out[[i]]$coordinates[1]
    b[i] = out[[i]]$coordinates[2]
    c[i] = out[[i]]$quality
  }
  plot(h,c,cex=0.75,xlab="x (cm)",ylab="y (cm)",xlim=c(-2,2),ylim=c(-2,2))
}


# metoda wyswietlająca wynik w przyjazny sposób
show_outputIter<-function(out)
{
  a = NULL
  b = NULL
  c = NULL
  for (i in 1:length(out))
  {
    a[i] = out[[i]]$coordinates[1]
    b[i] = out[[i]]$coordinates[2]
    c[i] = out[[i]]$quality
  }
  plot(a,b,pch=20,xlab="x (cm)",ylab="y (cm)",cex=0.75,,xlim=c(-2,2),ylim=c(-2,2))
}

# wyjscie ściezki

show_plot_lines<-function(out)
{
  a = NULL
  b = NULL
  for (i in 1:length(out))
  {
    a[i] = out[[i]][[1]]
    b[i] = out[[i]][[2]]
  }
  

  plot(a,b, type="l",xlab="x (cm)",ylab="y (cm)",xlim=c(-2,2),ylim=c(-2,2))
}


# fast_run1
fast_run1<-function()
{
  h = metaheuristicRun(initialization,list(),terminationIter,evaluation)
  show_output(h);
}

hh = list()
midle_midle = list()
test1<-function()
{
  hh <<-  list()
  midle_midle <<- list()
 
  for (ii in 1:20)
  {
    h = metaheuristicRun(initialization,list(),terminationQualityIter,evaluation)
    hh <<- append(hh,list(h))
    midle_midle <<- append(midle_midle,list(middle_points[[length(middle_points)]]))
  }
  
  a = NULL
  b = NULL
  sum = 0
  for (q in 1: length(hh))
  {
    for (i in 1:length(hh[[q]]))
    {
      sum = sum + 1
      a[sum] = hh[[q]][[i]][[1]][[1]]
      b[sum] = hh[[q]][[i]][[1]][[2]]
    }
  } 
    plot(a,b,pch=20,xlab="x (cm)",ylab="y (cm)",cex=0.75,,xlim=c(-2,2),ylim=c(-2,2),main="20 uruchomień z losową populacją startową")
  
}
