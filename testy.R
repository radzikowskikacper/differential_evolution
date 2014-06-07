# 1 

# metoda wyswietlająca wynik w przyjazny sposób
show_outputIter<-function(out)
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
  plot(h,c,cex=0.75,xlab="iteracja",ylab="wartość",ylim=c(0,10^3))
}


# metoda wyswietlająca wynik w przyjazny sposób
show_output2D<-function(out)
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
  plot(a,b,pch=20,xlab="x (cm)",ylab="y (cm)",cex=0.75,xlim=c(-2,2),ylim=c(-2,2))
}


# metoda wyswietlająca wykres składający się z kilka dla wektorół ruchu dla powtóþrze” iteracji 200 razy
show_output_plots<-function(out)
{
  a = NULL
  b = NULL
  c = NULL
  for (i in 1:length(out))
  {
    
    a[i] = out[[i]][[1]]
    b[i] = out[[i]][[2]]

  }
  dat <- data.frame (x=a, y=b,
                     var=factor (rep (c("A", "B", "C","D","E"), each=200))
                     )
  xyplot (y ~ x , groups=var, data=dat, type="l",
           xlab="x (cm)", ylab="y (cm)" )
  #plot(a,b,pch=20,xlab="x (cm)",ylab="y (cm)",cex=0.75,xlim=c(-2,2),ylim=c(-2,2))
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
  

  plot(a,b,xlab="x (cm)",ylab="y (cm)",xlim=c(-2,2),ylim=c(-2,2) ,type="l")
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


my_out <- list()
print_midle<-function(midle_midle_)
{
  my_out <<- list()
  for (ff in 1:length(midle_midle_))
  {
    my_out <<- append(my_out, c(sprintf("%f",midle_midle_[[ff]][1]),sprintf("%f",midle_midle_[[ff]][2])))
    
  }
  for (q in 1:length(my_out))
  {
    cat (q," x: ",my_out[[q]][[1]]," y: ", my_out[[q]][[2]])
  }
}


# test 2
test2<-function()
{
  hh = list();
  for(i in 1:5)
  {
    metaheuristicRun(initialization,list(),terminationIter,evaluation)
    hh <- append(hh ,  middle_points  )
  }
  show_output_plots(hh)
}

