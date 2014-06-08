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

library(lattice)

# history - wszystkie wygenerowane punkty
# model - indeksy punktów aktualnego 
parametersDE = list(S = 15, F = 0.9, CR = 0.5)

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

#funkcja oceny - na razie liczy po prostu wartość funkcji
evaluation<-function(coordinates)
{
  x1 = coordinates[1]#x
  x2 = coordinates[2]#y
  return((1+(x1+x2+1)^2*(19-14*x1+3*x1^2-14*x2+6*x1*x2+3*x2^2))*(30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2)))
}


# metoda wyswietlająca wynik w przyjazny sposób
show_output<-function(out)
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
parametersTerm = list (type='repeats',value=400, currValue = 0)

terminationIter<-function(history,model)
{# wyłączenie spr. warunku
  #if (parametersTerm$type == "repeats")
  {
    parametersTerm$currValue <<- parametersTerm$currValue + 1
    if (parametersTerm$currValue > parametersTerm$value)
      return (TRUE)
  }
  
  return (FALSE)
}

# zwraca wylosowany pinkt z populacji (rozkład jednostajny)
selectDE<-function(selectedPoints)
{
  l = sample(1:length(selectedPoints),1)
  return (selectedPoints[[l]]$coordinates)
}
# wersja dla dwóch elementów, w przyp potrzeby można zrobić dla n wymiarów
# zasada działąnia:
# - wylosuj porządek w którym będzie dokonywane losowanie elementów
# - za ostatni element wektora podstaw element wektora mutanta (to zapewnia nam, że krzyżowanie nigdy)
# nie zwróci nam starego punktu.
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
  return (point(new))
}

tournamentDE<-function(a,b)
{

  if (a$quality < b$quality)
  {
    return (a)
  }
  return (b)
}

# sprawdza czy punkt jest na liście zwraca 0 gdy nie ma lub pozycje na liście
findPoint<-function(list,point)
{
  for (i in 1:length(list))
  {
    if (list[[i]]$coordinates[1] == point$coordinates[1] && list[[i]]$coordinates[2] == point$coordinates[2])
      return (i)
  }
  return (0)
}
# sprawdza równość dwóch punktów
equalPoint<-function(p_a,p_b)
{
  if (p_a$coordinates[1] == p_b$coordinates[1] && p_a$coordinates[2] == p_b$coordinates[2])
    return (TRUE)
  return (FALSE)
}

# model zawiera numery naktórych znajdują sie elementy populacji w historii
initModel<-function(history)
{
  return (c(1:length(history)))
}
#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model) # zrobione
{
  #select a number of points from the history using the 
  #method's parameters and the current state of the model
  #l = length(history)
  #return (history[max(l - parametersDE$S + 1,1):l])
  selectedPoints = list()
  for (i in 1:length(model))
  {
    selectedPoints = append(selectedPoints,history[model[i]])
  }
  return (selectedPoints)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel) # nieużywane!!
{
  #take a look at the list of selectedPoints and 
  #on the current state of the model, update it 
  #and then return
  newModel = 1 # mocking
  return (newModel)
}

#generation of a proposed LIST of new points
#to be defined
# w tej chwili tylko lepsze punkty trafiają do loga
variation<-function(selectedPoints,history)
{
  newPoints = list()
  for (i in 1:parametersDE$S)
  {
    x_j <- selectDE(selectedPoints) # wybierz jeden punkt
    z = NULL
    while (!inRange(z))
    {
      x_nr = sample(1:length(selectedPoints),2) # def sample.replace = false -> losowanie bez zwracania
      x_a = list()
      for (j in 1:2) 
      {
        x_a = append(x_a,list( selectedPoints[[x_nr[j]]]$coordinates)) # wybiera wektory współrzednych punktow
      }
      y = x_j + parametersDE$F*( x_a[[1]] - x_a[[2]] )
      z = crossoverDE(selectedPoints[[i]]$coordinates,y)
      z$quality = evaluation(z$coordinates)
    }
    # aktualnie pty y i z nie wrzucane do loga
    newPoints = append(newPoints,list(tournamentDE(selectedPoints[[i]],z)))# tournament zwraca punkt

  } 
  return (newPoints)
}

# funkcja pobiera starą populację i wybrane punkty przez wariancję oraz
# zwraca
getNewModelAndPoints<-function(history,oldModel,newPoints)
{
  #length(oldModel) == length(newPoints)
  returnedPoints = list()
  for (i in 1:length(newPoints))
  {
    if (equalPoint(history[[oldModel[i]]],newPoints[[i]]))
    { # takie same punkty
      
    }else
    { # różne, czyli nowo wygenerowane punkty są lepsze od starych na odpowiadających porównywanych pozycjach
      pos = findPoint(history,newPoints[[i]]) # jak != 0 to znalazlo punkt w historii
      if (pos != 0 )
      { # znaleziono pkt w historii - edytujemy model ale nie dodajemy punktu
        oldModel[i] = pos
      }
      else
      { # nie znaleziono pktu - edytujemy model i dopisujemy punkt do listy returned points
        returnedPoints = append(returnedPoints, newPoints[i])
        oldModel[i] = length(history) + length(returnedPoints)
      }
    }
  }
  return (list(newPoints=returnedPoints,newModel=oldModel))
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

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{
  
 selectedPoints<-selection(history, oldModel)
 
 newPoints<-variation(selectedPoints,history)
  # metoda otrzymuje listę proponowanych punktów
  #newModel<-modelUpdate(newPoints, oldModel) # zmiana kolejności! inaczej się nie da
  # funkcja zwracająca list(newPoints=newPoints,newModel=newModel)
  return (getNewModelAndPoints(history, oldModel, newPoints))
}

my_middle<-function(model,history)
{
  p = c(0,0)
  for (i in 1:length(model))
  {
    p = p + history[[model[i]]]$coordinates
  }
  return ( p / length(model) )
}

shift_v = c(runif(2,-2,2))

middle_points = list()
my_model = list()
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
  
  shift_v <<- c(runif(2,-2,2))
  middle_points <<- list()
  parametersTerm$currValue <<- 0
  parametersQualityTerm$actual_iter <<- 0
  parametersQualityTerm$pos_element <<- 1
  while (!termination(history,model))
    
  {
    middle_points <<- append(middle_points, list(my_middle(model,history)))
    aa<-aggregatedOperator(history, model)
    #aa$newPoints<-evaluateList(aa$newPoints, evaluation) # tu niepotrzebne bo wariancja liczy to
    history<-historyPush(history,aa$newPoints) 
    #cat("nr: ", parametersTerm$currValue , "  " , aa$newModel,"\n")
    
    model<-aa$newModel
  }
  cat("nr: ", parametersTerm$currValue,"\n")
  
  my_model <<- model
  
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