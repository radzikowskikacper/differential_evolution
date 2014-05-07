#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

# notka wyciagamy z listy poprzez _lista[[nr]] !!
# przypisania globalne poprzez <<- zamiast <- lub = !!
# parametry algorytmu ewolucyjnego i metoda ustawiająca
parametersDE = list(S = 10, F = 0.4, CR = 0.5)

# ustawia parametry metody
setParamDE<-function(S,F,CR)
{
  parametersDE <<- (list(S = S, F = F, CR = CR))
}


# tworzy punkt o współrzędnych x, y
# dla n wymiarów wymaga małego uproszczenia i by działało , do przedyskutowania
# wtedy zmiana metod odwołań (nie przez nazwe ale w pętlach przez numer)
point<-function(pointCoord)
{
  return  ( list(coordinates = c(pointCoord[1],pointCoord[2]),quality = 0) )
}

genPointGP<-function()
{
  return (point(runif(2, -2, 2)))
}

# funkcja oceny - na razie liczy po prostu wartość funkcji
evaluation<-function(coordinates)
{
  x1 = coordinates[1]#x
  x2 = coordinates[2]#y
  return((1+(x1+x2+1)^2*(19-14*x1+3*x1^2-14*x2+6*x1*x2+3*x2^2))*(30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2)))
}

# inicjalizacja wybiera n elementów z listy startowej punktów,
# jesli lista pusta lub brakuje elementow, to
# zostaną elemnety dogenerowane. W ten sposób pozwala
# uruchamiać deterministycznie i niedeterministycznie.
initialization<-function(startPoints)
{
  if (length(startPoints) > parametersDE$S)
    returnListPoints = startPoints[1:parametersDE$S]
  else
    returnListPoints = startPoints
  numbGenPts =  parametersDE$S - length(startPoints)
  if (numbGenPts > 0)
  {
    for (i in 1:numbGenPts)
    {
      #generuj kolejne elementy do dopełnienia
      returnListPoints = append(returnListPoints, list(genPointGP()))
    }
  }
  return (returnListPoints)
}

# termination with values
parametersTerm = list (type='repeats',value=100000, currValue = 0)

termination<-function(history,model)
{
  if (parametersTerm$type == "repeats")
  {
    parametersTerm$currValue <<- parametersTerm$currValue + 1
    if (parametersTerm$currValue >= parametersTerm$value)
      return (TRUE)
  }
  
  return (FALSE)
}

# zwraca wylosowany (w ust sposób) wektor współrzędnych danego punktu
selectDE<-function(selectedPoints, model)
{
  # na razie zwykłe sample ale może pożniej inaczej
  l = sample(1:length(selectedPoints),1)
  return (selectedPoints[[l]]$coordinates)
}
# wersja dla dwóch elementów, w przyp potrzeby można zrobić dla n wymiarół
crossoverDE<-function(old,y)
{
  order = sample(1:length(old),length(old))
  val = runif(1) # losuj wart z przedziału <0,1>
  new = NULL
  if (val > parametersDE$CR || val == 0 )
  {# parametr pierwszy wylosowany, z wektora rodzica (wcześ. populacji)
    new[order[1]] = old[order[1]]
  }
  else
  {
    new[order[1]] = y[order[1]]
  }
  new[order[2]] = y[order[2]]
  return (new)
}

tournamentDE<-function(a,b)
{
  q_a = evaluation(a)
  q_b = evaluation(b)
  if (q_a > q_b)
  {
    pt = point(a)
    pt$quality = q_a
    return (pt)
  }
  else
  {
    pt = point(b)
    pt$quality = q_b
    return (pt)
  }
}

initModel<-function(history)
{
  return (1) # mocking
}
#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model) # zrobione
{
  #select a number of points from the history using the 
  #method's parameters and the current state of the model
  l = length(history)
  return (history[max(l - parametersDE$S + 1,1):l])
  
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
  #take a look at the list of selectedPoints and 
  #on the current state of the model, update it 
  #and then return
  newModel = 1 # mocking
  return (newModel)
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
  newPoints = list()
  for (i in 1:parametersDE$S)
  {
    x_j <- selectDE(selectedPoints,model)
    x_nr = sample(1:length(selectedPoints),2) # def replace = false -> bez zwracania
    x_a = list()
    for (i in 1:2) 
      x_a = append(x_a,list( selectedPoints[[x_nr[i]]]$coordinates)) # wybiera wektory współrzednych punktow
    y = x_j + parametersDE$F*( x_a[[1]] - x_a[[2]] )
    z = crossoverDE(selectedPoints[[i]]$coordinates,y) 
    newPoints = append(newPoints, list(tournamentDE(selectedPoints[[i]]$coordinates,z)))# zwraca punkt!!!
  } 
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
  return (list(newPoints=newPoints,newModel=newModel)) # to "=" dodaje nam nazwy dla dostępu do poszczególnych elementów
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  history<-evaluateList(history, evaluation) # dodałem drugi parametr bo mnie to wkurzało że go nie było
  model<-initModel(history)
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation) # tu niepotrzebne bo wariancja liczy to
    history<-historyPush(history,aa$newPoints) 
    model<-aa$newModel
  }
  parametersTerm$currValue <<- 0
  return(history)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints) # utwórz wektor - konkatenacja
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
  for (i in 1:length(points))
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}


####  THAT'S ALL FOLKS