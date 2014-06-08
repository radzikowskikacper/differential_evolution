#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

source('evaluators.R')

start = FALSE

setParams<-function(S, I, size, R, m)
{
  R <<- R
  neighb_step <<- as.integer(S / R)

  parameters <<- (list(R = R, S = S, max = m))
  loop_counter <<- (list(max = I, curr = 0))
  dimensions <<- (list(lx = -size, rx = size, ty = size, by = -size));
}

initialization<-function(startPoints)
{
  if(neighb_step == 0)
    stop('Promień sąsiedztwa mniejszy od odleglosci miedzy punktami')
  
  loop_counter$curr <<- 0
  
  last_neighbors <<- 1
  found_new <<- TRUE
  first_loop <<- TRUE
  
  if(start)
    return (list(list(coordinates = c(start[1], start[2]))))
  
  steps = sample(0:(dimensions$rx * 2 / parameters$R), 2);
  point = list(coordinates = c(dimensions$lx + parameters$R * steps[1], dimensions$by + parameters$R * steps[2]))
  return (list(point))
}

setStartPoint<-function(startPoint)
{
  start <<- startPoint
}

distance<-function(point1, point2)
{
  return(sqrt((point1[1] - point2[1]) ^ 2 + (point1[2] - point2[2]) ^ 2));
}

termination<-function(history,model)
{
  if(!found_new)
  {
    return (TRUE)
  }
    
  loop_counter$curr <<- loop_counter$curr + 1
  
  if(loop_counter$curr > loop_counter$max)
  {
    return (TRUE);
  }
  
  found_new <<- FALSE
  return (FALSE)
}

initModel<-function(history)
{
  return (history)
}



#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  #select a number of points from the history using the 
  #method's parameters and the current state of the model
  selectedPoints = tail(history, last_neighbors)#history[(length(history) - last_neighbors + 1):length(history)]
  
  return(selectedPoints)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
  #take a look at the list of selectedPoints and 
  #on the current state of the model, update it 
  #and then return
  newModel = oldModel
  
  if(first_loop)
  {
    found_new <<- TRUE;
    first_loop <<- FALSE;
  }
  else
  {
    if(parameters$max)
    {
      point = tail(selectedPoints[order(sapply(selectedPoints, "[[", "quality"))], 1)
      if(point[[1]]$quality > newModel[[1]]$quality)
      {
        newModel = point;
        found_new <<- TRUE;
      }
    }
    else
    {
      point = selectedPoints[order(sapply(selectedPoints, "[[", "quality"))][1]
      if(point[[1]]$quality < newModel[[1]]$quality)
      {
        newModel = point;
        found_new <<- TRUE;
      }
    }
  }
  
  return (newModel)
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
  #generate the list of newPoints and then  
  
  newPoints <- list()
  
  x = minx = max(dimensions$lx, model[[1]]$coordinates[1] - parameters$R * neighb_step)
  y = miny = max(dimensions$by, model[[1]]$coordinates[2] - parameters$R * neighb_step)
  
  maxx = min(dimensions$rx, model[[1]]$coordinates[1] + parameters$R * neighb_step)
  maxy = min(dimensions$ty, model[[1]]$coordinates[2] + parameters$R * neighb_step)
  
  while(x <= maxx)
  {
    y = miny
    while(y <= maxy)
    {      
      dist = distance(c(x, y), model[[1]]$coordinates)
      if(dist < parameters$S || isTRUE(all.equal(dist, parameters$S)))
      { 
        if(!(isTRUE(all.equal(x, model[[1]]$coordinates[1])) && isTRUE(all.equal(y, model[[1]]$coordinates[2]))))
        {
          newPoints[[length(newPoints)+1]] <- list(coordinates = c(x, y));
        }
      }
      y = y + parameters$R;
    }
    
    x = x + parameters$R;
  }
  
  last_neighbors <<- length(newPoints);
  
  return (newPoints)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{
  selectedPoints<-selection(history, oldModel)
  
  newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  
  return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  
  history<-evaluateList(history, evaluation);
  model<-initModel(history)
  
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  
  for(i in 1:length(history))
  {
    points(history[[i]]$coordinates[1], history[[i]]$coordinates[2], cex=0.5, col="black", pch=20)
  }
  
  return (list(history, model[[1]]))
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  if(length(points) != 0)
    for (i in 1:length(points))
      points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}

####  THAT'S ALL FOLKS

runTests<-function(run_num = 100, width = 743, height = 743, start = F, rate = 0.02, dimension = 2, alg_iter = 1000, neigh = 0.5, maxym = F)
{
  cat('\nRozpoczęcie testu z parametrami:')
  cat('\n[parametry przestrzeni]\nodległość między punktami:', rate, '\nwymiary: (-', dimension, ', -', dimension, ') x (', dimension, ', ', dimension, 
      ')\n\n[parametry algorytmu wspinaczkowego]\npromień sąsiedztwa:', neigh, '\nmaksymalna liczba iteracji:', alg_iter, '\npunkt startowy:', start, '\nminimalizacja:', !maxym, 
      '\n\n[parametry wykresu]\nszerokość:', width, '\nwysokość:', height, '\n\n[parametry testu]\nilość wywołań:', run_num)  
  
  functions_names = list('sferycznej', "Goldsteina - Price'a", 'EggHolder')
  functions = list(evaluationSpherical, evaluationGP, evaluationEggHolder)
  fname = paste(neigh, alg_iter, run_num, sep = "_")
  results = list(c(0, 0), c(0, -1), c(512, 404.2319))
  gfiles = list(paste(fname, '_spherical.jpg', sep = ""), paste(fname, '_gp.jpg', sep = ""), paste(fname, '_eh.jpg', sep = ""))
  dfiles = list(paste(fname, '_spherical.txt', sep = ""), paste(fname, '_gp.txt', sep = ""), paste(fname, '_eh.txt', sep = ""))
  for(i in 1:2)
    file.remove(dfiles[[i]])
  
  for(i in 1:2)
  {
    title = paste(run_num, "uruchomień optymalizacji funkcji", functions_names[i], 'z losowym punktem startowym')
    
    setParams(neigh, alg_iter, dimension, rate, maxym)
    start <<- start
    
    jpeg(filename = gfiles[[i]], width = width, height = height)
    plot(dimensions$lx *2, dimensions$by *2, xlab="X",ylab="Y", xlim=c(dimensions$lx, dimensions$rx), ylim=c(dimensions$by, dimensions$ty), main=title)
    par(new=T)
    
    cat('\n\nOptymalizacja funkcji', functions_names[[i]], '\n')
    
    tm = 0
    score = 0
    for(j in 1:run_num)
    {
      cat(j, 'wywołanie testu\n')
      temp = proc.time()
      his=metaheuristicRun(initialization, list(), termination, functions[[i]])
      tm = tm + (proc.time() - temp)[[3]]
      #cat('Punkt startowy:', his[[1]][[1]]$coordinates)
      
      if(found_new)
        cat('\nPrzekroczono liczbę iteracji algorytmu\n')
      
      if(isTRUE(all.equal(his[[2]]$coordinates[1], results[[i]][1])) && isTRUE(all.equal(his[[2]]$coordinates[2], results[[i]][2])))
        score = score + 1
      
      #cat('Punkt końcowy:', his[[2]]$coordinates, '\n')
    }
    write(tm / run_num, file=dfiles[[i]], append = T)
    write(score / run_num, file=dfiles[[i]], append = T)
    dev.off()
    
    cat('\nWykres: ', gfiles[[i]])
    cat('\nDane pomiarowe: ', dfiles[[i]], '\n')
  }
}

perform<-function(args)
{
  for(arg in args)
  {
    runTests(neigh = arg[1], alg_iter = arg[2], run_num = arg[3])
  }
}