#' A function that applies a composite model of ETS and ARIMA model for US Sales. A pre-trained ETS model that was trained on data that ends in recession year is used as input for this function. The ARIMA model is fit for the data supplied. For forecasting sales if the future year is forecasted as recession year, ETS model is used. The forecast from ARIMA model is tagged along with ETS forecast. If the Recession year is later in the forecast horizon, the ARIMA forecast is generated for the horizon in which there is no recession and then the level adjusted ETS forecast is tagged along to the ARIMA forecast.
#'
#' @param data US Sales data as a tsibble
#' @param driftmodel  pre-trained ETS model (Trend = Ad) object that was trained on data ending with a recession year.
#' @param currentyear Year as numeric, that denotes the current year
#' @param RecessionYears numeric vector that lists the years that are forecasted to be recession year.
#' @param horizon Forecast horizon
#'
#' @return list containing forecasts, plots, accuracy metrics, 
#' @export
#'
#' @examples
twoStepForecast <- function(data,driftmodel,
                           # rampmodel,
                            currentyear = NULL, 
                            RecessionYears = NULL, 
                            horizon = "5 years"){
  
  library(fpp3) # should be calling out the individual packages here!
  # =======================================================================================
  #     Update level of the model
  #   This is done by taking the difference between current years average sales value &
  #   the last sales value from training data (the one the model was trained on) to get the   
  #   level update. This level update is added to the model forecast - basically correcting   
  #   the intercept value
  #========================================================================================
  #browser()
  lastTrainingValueDrift <- driftmodel %>% 
    augment() %>% 
    tail(1) %>% 
    select(Sales_in_trillions) %>% .$Sales_in_trillions
  
  # lastTrainingValueRamp <- rampmodel %>% 
  #   augment() %>% 
  #   tail(1) %>% 
  #   select(Sales_in_trillions) %>% .$Sales_in_trillions
  
  if(!is.null(currentyear)){
    lastline <- data %>% 
      filter(year(YearMonth) == currentyear) %>% 
      tail(1) 
    lastval <- lastline$Sales_in_trillions
    last.time <- lastline$YearMonth
  } else {
    lastval <- data %>% 
      tail(1) %>% 
      select(Sales_in_trillions) %>% .$Sales_in_trillions
    last.time <- max(data$YearMonth)
  }
  
  
  driftmdl_level_update <- lastval - lastTrainingValueDrift
  #rampmdl_level_update <-  lastval -  lastTrainingValueRamp
  
  #==================================================================================
  #             Figure out the forecast horizon for the intermediate forecast
  #==================================================================================
  
  # if no predicted recession in the forecast horizon then go with ARIMA model 
  if(is.null(RecessionYears)){
   data %>% 
      filter(year(YearMonth) <= year(last.time)) %>% 
      model(arima = ARIMA(log(Sales_in_trillions))) -> fit
    
    fit %>% 
      forecast(h = "5 years") -> finalforecast
    
    if(last.time == max(data$YearMonth)){
      metrics <- fit %>% 
        accuracy()
      Plot <- finalforecast %>% 
        autoplot(data)
    } else{
      metrics <- finalforecast %>% 
        accuracy(data)
      Plot <- finalforecast %>% 
        autoplot(data %>% 
                   filter(YearMonth <= last.time + 60))
    } # if/else(last.time == max(data$YearMonth))
    
    return(list(forecasts = finalforecast,
                metrics = metrics,
                Plot = Plot))
  
  } # if(is.null(RecessionYears))
  
  # ******************************************************************************************
  # if there is a predicted recession in the forecast horizon then make use of two models
  #*******************************************************************************************
  Years_to_recession <- RecessionYears - year(last.time)
  
  # Depending on when the recession is occuring, choose the appropriate model for 
  # intermediate forecast
  if(length(RecessionYears) > 1){
    driftmodel %>% 
      forecast(h = "2 years") %>% 
      mutate(Sales_in_trillions = Sales_in_trillions + driftmdl_level_update) %>% 
      as_tibble() %>% 
      mutate(YearMonth = last.time + 1:24) %>% 
      as_tsibble(index = YearMonth) -> intermediateForecast
    # convert it back to fable for easier handling 
    intermediateForecast %>% 
      mutate(.model = "ets") %>% # re-adding dependancy from simulation urggg!
      update_tsibble(key = .model) %>% 
      as_fable(response = Sales_in_trillions,distribution = .distribution) -> intermediateForecast
  }
  else if(Years_to_recession < 2){
    driftmodel %>% 
      forecast(h = "1 year") %>% 
      mutate(Sales_in_trillions = Sales_in_trillions + driftmdl_level_update) %>% 
      as_tibble() %>% 
      mutate(YearMonth = last.time + 1:12) %>% 
      as_tsibble(index = YearMonth) -> intermediateForecast
    # convert it back to fable for easier handling 
    intermediateForecast %>% 
      mutate(.model = "ets") %>% # re-adding dependancy from simulation urggg!
      update_tsibble(key = .model) %>% 
      as_fable(response = Sales_in_trillions,distribution = .distribution) -> intermediateForecast
  } else {
    inter.horizon <- RecessionYears - year(last.time)-1
    data %>% 
      filter(year(YearMonth) <= year(last.time)) %>% 
      model(arima = ARIMA(log(Sales_in_trillions))) -> fit
    
    fit %>% 
      forecast(h = paste(inter.horizon, "years")) -> intermediateForecast
    
  } #if/else(Years_to_recession < 2)
  
 # find which model was applied for the first iteration of intermediate forecast
 last.inter <- max(intermediateForecast$YearMonth)
 
 if((RecessionYears - year(last.inter)) == 0){
   # in this case the drift model was already apllied, whats left is the 
   # Arima model
   
   # fit ARIMA on the merged series of "data" and "intermediate forecast" 
   #browser()
   data %>% 
     filter(YearMonth <= last.time) %>% 
     rbind(as_tsibble(intermediateForecast, index = YearMonth) %>% 
             select(Sales_in_trillions)) %>% 
     model(arima = ARIMA(log(Sales_in_trillions))) -> fit
   
   
   
   # forecast for the next 4 years
   fit %>% 
     forecast(h = "4 years") -> finalforecast
   
   # get metrics and plot
   if(last.time == max(data$YearMonth)){
     metrics <- fit %>% 
       accuracy()
     #browser()
     Plot <- rbind(intermediateForecast,
                   finalforecast) %>% 
       autoplot(data,level = NULL)
     
   } else{
     metrics <- rbind(intermediateForecast,
                      finalforecast) %>% 
       mutate(.model = "composite") %>% 
       accuracy(data%>% 
                  filter(YearMonth <= last.time + 60 ))
     Plot <- rbind(intermediateForecast,
                   finalforecast) %>% 
       mutate(.model = "composite") %>% 
       autoplot(data %>% 
                  filter(YearMonth <= last.time + 60 ),level = NULL)
   } # if/else(last.time == max(data$YearMonth))
   
   return(list(forecasts = rbind(intermediateForecast,
                                finalforecast) %>% arrange(YearMonth),
               metrics = metrics,
               Plot = Plot))
   
 } else{
   # apply drift model to forecast for the next year and ARIMA for the rest
   
   # update level for the drift model
   
   data %>% 
     filter(YearMonth <= last.time) %>% 
     rbind(as_tsibble(intermediateForecast, index = YearMonth) %>% 
             select(Sales_in_trillions)) -> intermediatedata
   
   driftmdl_level_update <- intermediatedata$Sales_in_trillions[which(intermediatedata$YearMonth == last.inter)] - 
     lastTrainingValueDrift
   
   # Apply the drift model to the data that includes forecast from the previous step
   driftmodel %>% 
     forecast(h = "1 year") %>% 
     mutate(Sales_in_trillions = Sales_in_trillions + driftmdl_level_update) %>% 
     as_tibble() %>% 
     mutate(YearMonth = last.inter + 1:12) %>% 
     as_tsibble(index = YearMonth) -> intermediateforecast2
   
   intermediateforecast2 %>% 
     mutate(.model = "ets") %>% 
     update_tsibble(key = .model) %>% 
     as_fable(response = Sales_in_trillions,distribution = .distribution) -> intermediateforecast2
   
   intermediatedata %>% 
     rbind(as_tsibble(intermediateforecast2,index = YearMonth) %>% 
             select(Sales_in_trillions)
           ) -> intermediatedata2
   
   #*******************************************************************
   # finally forecast using ARIMA model for the remaining time period
   #*******************************************************************
   if((year(last.time + 60) - year(max(intermediatedata2$YearMonth)))==0){
     
     finalforecast <- rbind(intermediateForecast,intermediateforecast2)
     metric <- finalforecast %>% 
       accuracy(data %>% 
                  filter(YearMonth <= last.time + 60 ))
     
     Plot <- finalforecast %>% 
       autoplot(data,level = NULL)
     
     list(forecast = finalforecast,
          metric = metric,
          Plot = Plot)
   } else{
     intermediatedata2 %>% 
       model(arima = ARIMA(log(Sales_in_trillions))) %>% 
       forecast(h = paste((year(last.time + 60) - year(max(intermediatedata2$YearMonth))),"years")) -> finarimafct
    # browser()
     finalforecast <- rbind(intermediateForecast,
                            intermediateforecast2,
                            finarimafct) %>% 
       mutate(.model = "composite")
     
     metrics <- rbind(intermediateForecast,
                     intermediateforecast2,
                     finarimafct) %>% 
       mutate(.model = "composite") %>% 
       accuracy(data)
     
     Plot <- finalforecast %>% 
       autoplot(data,level = NULL)
     
     list(forecast = finalforecast,
          Plot = Plot,
          metrics = metrics)
     
     
   }
   
 } #if((RecessionYears - year(last.inter)) == 0)
  
  
  
}
