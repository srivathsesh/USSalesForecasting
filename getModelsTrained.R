#' This is a function to train and validate a set of models
#'
#' @param TrainStart Year as numeric, from which the training data begins
#' @param TrainEnd  Year as numeric, at which training data ends
#' @param ValidStart Year as numeric, at which validation data begins
#' @param ValidEnd Year as numeric, at which validation data ends.
#'
#' @return list containing forecast, accuracy, plots, models 
#' @export
#'
#' @examples getModelsTrained(2010,2015,2016,2019)
#' @author Sri Seshadri
#' 
getModelsTrained <- function(TrainStart,TrainEnd,ValidStart,ValidEnd){
  library(fpp3)
  Sales %>% 
    mutate(RecessionYears = RecessionCorrection) %>% 
    filter(year(YearMonth) >= !!TrainStart & year(YearMonth) <= !!TrainEnd) %>%
    model(stl = dcmp_spec,
          #stl2 = dcmp_spec2,
          snaive = SNAIVE(log(Sales_in_trillions)),
          #mean = MEAN(log(Sales_in_trillions)),
          hw_a = ETS(log(Sales_in_trillions)~ error("A") + trend("A") + season("A")),
          ets = ETS(log(Sales_in_trillions)),
          arima = ARIMA(log(Sales_in_trillions)),
          `K = 1` = ARIMA(log(Sales_in_trillions) ~ fourier(K = 1) + PDQ(0, 0, 0)),
          `K = 4` = ARIMA(log(Sales_in_trillions) ~ fourier(K = 4) + PDQ(0, 0, 0)),
          `K = 6` = ARIMA(log(Sales_in_trillions) ~ fourier(K = 6) + PDQ(0, 0, 0)),
          LMArima = ARIMA(log(Sales_in_trillions) ~ RecessionYears),
          tslm = TSLM(log(Sales_in_trillions) ~ trend() + season() + RecessionYears)
         
    ) -> fit
  
  newdata <- Sales %>% 
    filter(year(YearMonth) >= !!ValidStart & year(YearMonth) <= !!ValidEnd) %>% 
    select(-RecessionYears,-Sales,-Sales_in_trillions) %>% 
    rename(RecessionYears = RecessionCorrection)
  
  #browser()
  
  fit %>% 
    forecast(h = paste(ValidEnd - ValidStart + 1,"years"),
             new_data = newdata) -> fctsExp 
  
  fctsExp %>% 
    accuracy(Sales %>% 
               filter(year(YearMonth) <= !!ValidEnd)) %>% 
    arrange(MAPE)-> accuracySet
  

  fctsExp %>% 
    autoplot(Sales %>% 
               filter(year(YearMonth) <= !!ValidEnd),level = NULL) + geom_line(col = "grey") + theme_bw() -> P
  
  list(forecasts = fctsExp,
       accuracy = accuracySet,
       Plot = P,
       ModelStack = fit,
       new_data = newdata)
  
}