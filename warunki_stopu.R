
# termination with values
parametersTerm = list (type='repeats',value=1000, currValue = 0)


# zatrzymanie dla warunku związanego z wykonanie określonej ilości iteracji
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



#evaluation(history[model[parametersQualityTerm$pos_element]]$coordinations)
parametersQualityTerm = list( quality_step = 0.00000001 , max_iter = 100 , actual_iter = 0, saved_iter = 0 , pos_element = 1 )

terminationQualityIter<-function(history,model)
{
 

  min = model[1]
  for (i in 2:length(model))
  {
    if (evaluation(history[[min]]$coordinates) > evaluation(history[[model[i]]]$coordinates) )
        min <- model[i]
  }
  
  parametersTerm$currValue <<- parametersTerm$currValue + 1
  
  parametersQualityTerm$actual_iter <<- parametersQualityTerm$actual_iter + 1
  # jeśli nowy punkt jest lepszy niż zapisany powiększony o krok jakościowy
  #cat(evaluation(history[[min]]$coordinates),"\n")
  #cat(evaluation(history[[parametersQualityTerm$pos_element]]$coordinates ),"\n")
 if ( evaluation(history[[min]]$coordinates) < 
        - parametersQualityTerm$quality_step + evaluation(history[[parametersQualityTerm$pos_element]]$coordinates ) )
  {
   #cat(parametersQualityTerm$actual_iter,"\n")
   #cat(parametersQualityTerm$saved_iter,"\n")
  
   parametersQualityTerm$saved_iter <<- parametersQualityTerm$actual_iter
   parametersQualityTerm$pos_element <<- min
    
    return (FALSE)
  }
  if (parametersQualityTerm$actual_iter - parametersQualityTerm$saved_iter > parametersQualityTerm$max_iter)
  {
    #parametersQualityTerm$actual_iter <<- 0
    parametersQualityTerm$saved_iter  <<- 0
    parametersQualityTerm$pos_element <<- 1
    return (TRUE)
  }


return (FALSE)
}


