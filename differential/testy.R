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
           xlab="x", ylab="y",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5) )
  #plot(a,b,pch=20,xlab="x (cm)",ylab="y gen(cm)",cex=0.75,xlim=c(-2,2),ylim=c(-2,2))
}

# metoda wyswietlająca wykres składający się z kilka dla wektorół ruchu dla powtóþrze” iteracji 200 razy
show_plots_test3<-function(out)
{
  a = NULL
  b = NULL
  c = NULL
  for (i in 1:10)
  {
    
    a[i] = out[[3*(i-1)+1]] # iteracje
    b[i] = out[[3*(i-1)+2]] # jakość
    c[i] = out[[3*(i-1)+3]] # param
    
  }
  dat <- data.frame (x=append(c,c), y=append(a,b),
                     var=factor (rep (c("A", "B", "C","D","E"), each=20)),
                     graph = (rep (c("Liczba iteracji","Wartość funkcji"), each=10)) )
  
  xyplot (y ~ x | graph , groups=var, data=dat, type="l",layout=c(1, 2),
          xlab="x", ylab="y" ,main="Zależność jakości rozwiązania i liczby iteracji od współczynnika CR",scales=list(
            y=list(tick.number = 15)))
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

start_point <- initialization(list())
sec <-initialization(start_point)

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


test3_out = list() # 1 - jakość; 2 - ilość iteracji
numb_iter = c()

my_quality = c()

out_list = list()

# test 3
test3<-function()
{
  out_list <<- list()
  hh = list()
  out = list()
  
  #parametersDE$S <<- 7
  #parametersDE$F <<- 0.0
  parametersDE$CR <<- 0.0
  for (j in 1:11)
  {
    numb_iter <<- c()
    my_quality <<- c()
    # wykonuj 20 symulacji
    for(i in 1:20)
    {
      
      h = metaheuristicRun(initialization,list(),terminationQualityIter,evaluation)
      my_quality[i] <<- evaluation(middle_points[[length(middle_points)]])
      numb_iter[i] <<-  parametersTerm$currValue
      
    }
   
   
   out_list <<- append(out_list, list(mean(numb_iter),mean(my_quality),parametersDE$CR))
   # parametersDE$S <<- parametersDE$S + 1
   #parametersDE$F <<- parametersDE$F + 0.1
   parametersDE$CR <<- parametersDE$CR + 0.1
  }
  #show_output_plots(hh)
}



