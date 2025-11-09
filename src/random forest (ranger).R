runForest = function(window,y_index){
  step = 1
# data restructured for ranger, data.frame format needs to be kept for all variables
  aux <- embed(as.matrix(window), 6 + step)
  y <- aux[, y_index]                                   
  X <- aux[, -c(1:ncol(window) ), drop = FALSE]  
  X.out <- aux[nrow(aux), 1:ncol(X), drop = FALSE]
  
  # Placeholder names, ranger will not run otherwise as it was how its constructed 
  # source: https://github.com/imbs-hl/ranger/issues/597
  colnames(X) <- paste0("X",1:ncol(X))
  colnames(X.out) <- paste0("X",1:ncol(X.out))
  
  # run function, time taken for both models is around 2-3 mins
  # setting these argumemnts below should replicate the randomForest
  model=ranger(y = y, x = X, importance = "impurity",scale.permutation.importance = TRUE)
  # placeholder
  pred  <- predict(model, X.out)$predictions[1]
  return(list("model"=model,"pred"=pred))
}



rf.rolling.window = function(data,window_count,y_index){
  # form the storage
  forecast.index = nrow(data)-window_count
  variable.importance <- list()
  prediction = matrix(NA,window_count,1)
  actual = matrix(NA,window_count,1)
  
  # run the rolling window
  for(i in 1:window_count){
    # a more readable window
    window = data[1:forecast.index,] 
    if(is.na(data[forecast.index,y_index]) ) next
    # run the function
    Forest = runForest(window,y_index) 
    # load results
    prediction[i] = Forest$pred
    actual[i] = data[forecast.index + 1, y_index]
    variable.importance[[i]] = importance(Forest$model)
    cat("forecast",i,"completed","\n") #display iteration number, nicer looking than profs
    forecast.index = forecast.index + 1 # made this familiar for those of yall who have done basic coding or 1010S 
  }
  cat("Done!","\n") # signal for done
  
  # i removed other help functions, if you want to include it back lmk
  return(list("pred"=prediction, "actual"=actual, "save.importance"=variable.importance)) 
}
