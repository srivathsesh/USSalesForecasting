getArimaForecasts <- function(model,data,newdata,currentYear = NULL){
  library(fpp3) # being lazy here, need to be specific
  
  # =======================================================================================
  #     Update level of the model
  #   This is done by taking the difference between current years average sales value &
  #   the last sales value from training data (the one the model was trained on) to get the   
  #   level update. This level update is added to the model forecast - basically correcting   
  #   the intercept value
  #========================================================================================
  
  getlastTrainingValue <- model %>% 
    augment() %>% 
    tail(1) %>% 
    select(Sales_in_trillions) %>% .$Sales_in_trillions
  #browser()
  # if(is.null(currentYear)){
  #   data %>% 
  #     as_tibble() %>% 
  #     filter(year(YearMonth) == year(YearMonth[nrow(.)])) %>% 
  #     summarise(MeanLevel = mean(log(Sales_in_trillions), na.rm = T)) %>% 
  #     .$MeanLevel -> CurrentYearLevel
  # } else {
  #   data %>% 
  #     as_tibble() %>% 
  #     filter(year(YearMonth) == currentYear) %>% 
  #     summarise(MeanLevel = mean(log(Sales_in_trillions), na.rm = T)) %>% 
  #     .$MeanLevel -> CurrentYearLevel
  # }
  
  if(is.null(currentYear)){
    data %>% 
      as_tibble() %>% 
      filter(year(YearMonth) == year(YearMonth[nrow(.)])) %>% 
      tail(1) %>% 
      .$Sales_in_trillions -> CurrentYearLevel
  } else {
    data %>% 
      as_tibble() %>% 
      filter(year(YearMonth) == currentYear) %>% 
      tail(1) %>% 
      .$Sales_in_trillions -> CurrentYearLevel
  }
  
  # levelUpdate <- exp(CurrentYearLevel) - getlastTrainingValue
  
  levelUpdate <- CurrentYearLevel - getlastTrainingValue

  
  model %>% 
    forecast(new_data = newdata) %>% 
    mutate(Sales_in_trillions = Sales_in_trillions + levelUpdate)
  
}