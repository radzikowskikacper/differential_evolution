#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

setParams<-function(S, I)
{
  R = 0.01
  range = 0
  if(as.integer(S / R) == 0 && S >= R)
  {
    range = 1
  }
  else
  {
    range = as.integer(S / R)
  }
  
  
  params <<- (list(R = R, S = S, range = range))
  iteration_term <<- (list(value=I, curr = 0))
  
  
  print(params$range)
}

initialization<-function(startPoints)
{
  if(params$range == 0)
    stop('Promień sąsiedztwa mniejszy od odleglosci miedzy punktami')
  
  iteration_term <<- (list(value=iteration_term$value, curr = 0))
  last_neighbors <<- 1
  found_new <<- TRUE
  
  steps = sample(0:(4 / params$R), 2);
  point = list(coordinates = c(-2 + params$R * steps[1], -2 + params$R * steps[2]))
  return (list(point))
}

evaluationGP<-function(point)
{
  x1 = point[1]
  x2 = point[2]
  return((1+(x1+x2+1)^2*(19-14*x1+3*x1^2-14*x2+6*x1*x2+3*x2^2))*(30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2)))
}

evaluationSpherical<-function(point)
{
  return (point[1]^2 + point[2]^2)
}

distance<-function(point1, point2)
{
  return (sqrt((point1[1] - point2[1]) ^ 2 + (point1[2] - point2[2]) ^ 2));
}

termination<-function(history,model)
{
  if(!found_new)
  {
    print('Nie znaleziono lepszego sąsiada')
    return (TRUE)
  }
    
  found_new <<- FALSE
  
  iteration_term$curr <<- iteration_term$curr + 1
  if(iteration_term$curr > iteration_term$value)
  {
    print('Przekroczono maksymalną liczbę iteracji');
    return (TRUE);
  }
  
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
  selectedPoints = history[(length(history) - last_neighbors):length(history)]
  #print(length(selectedPoints))
  #print('her')
  #print(last_neighbors)
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
  
  if(length(selectedPoints) == 1)
  {
    newModel = selectedPoints;
    #print('here1');
    found_new <<- TRUE;
  }
  else
  {
    #print("MAYBE NEW");
    #print(length(selectedPoints));
    #print(length(last_neighbors));
    
    for(point in selectedPoints)
    {
      print(point);
      if(point$quality < newModel[[1]]$quality)
      {
        
        print('change')
        newModel = list(point);
        found_new <<- TRUE;
        
        #break;
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
  
  x = minx = max(-2, model[[1]]$coordinates[1] - params$R * params$range)
  y = miny = max(-2, model[[1]]$coordinates[2] - params$R * params$range)
  
  maxx = min(2, model[[1]]$coordinates[1] + params$R * params$range)
  maxy = min(2, model[[1]]$coordinates[2] + params$R * params$range)
  
  print(y)
  print(maxy)
#
  while(x <= maxx)
  {
    y = miny
    while(y <= maxy)
    {         print(c(x, y));

      if(distance(c(x, y), model[[1]]$coordinates) <= params$S)
      {#          print('---')
#                 print(c(x, y));
  #               print(model[[1]]$coordinates)
   #             print('--');
        if(!(isTRUE(all.equal(x, model[[1]]$coordinates[1])) && isTRUE(all.equal(y, model[[1]]$coordinates[2]))))
        {
          print('inserted')
          newPoints[[length(newPoints)+1]] <- list(coordinates = c(x, y));
        }
        else 
        {
print('no')
        }
      }
      y = y + params$R;
    }
    
    x = x + params$R;
  }
#  print('okks')
  
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
    #print('loop');
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  
  print(history[1])
  print(model[1])
  #print(history)
  
  print(length(history))
  
  jpeg(filename = "fileName.jpg", width = 1000, height = 800)
  plot(3, 3, xlab="X",ylab="Y", xlim=c(-2, 2), ylim=c(-2, 2), main="20 uruchomień z losowym punktem startowym")
  par(new=T)
  for(point in history)
    {
  #  plot(history[[1]]$coordinates, cex=0.5, col="red")
  points(point$coordinates[1], point$coordinates[2], cex=0.01, col="red")
  #par(new=T)
  #plot(history[[length(history) - i]]$coordinates[1], history[[length(history) - i]]$coordinates[2], axes=F)#, cex=0.1, col="red", xlim=c(-2, 2), ylim=c(-2, 2))
   # print(history[[i]]$coordinates
  #par(new=T)
  }
  dev.off()
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

test<-function()
{
  setParams(0.002, 1000);
  
  if(params$R != 0.0001)
    log('params$R');
  
  if(params$S != 0.002)
    log('params$S');
  
  if(params$range != 20)
  {
    print(params$range)
    log('params$range');
  }
  
  if(iteration_term$value != 1000)
  {
    log('iteration$value');
  }
  
  
  initialization(list());
  
    
}

log<-function(msg)
{
  print(msg)
  stop
}


####  THAT'S ALL FOLKS
