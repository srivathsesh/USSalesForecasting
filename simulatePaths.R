simulatePaths <- function(data){
  
  library(fpp3)
  library(magrittr)
  source('twoStepForecast.R')
  
  # step 1: generate shocks that resemble the input data
  simulateddata <- forecast::bld.mbb.bootstrap(data, n = 10) %>% 
    as.data.frame() %>% ts(start = 1992, frequency = 12)
  
  colnames(simulateddata) <- paste0("sim",1:10)
  
  simulateddata %<>% 
    as_tsibble(index = time(.))
  
  simulateddata %<>% 
    rename(Sales_in_trillions = value,
           YearMonth = index) 

  
  # step 2: learn the drift during the 2001 slump and apply to the future for each ofthe series - 
  #          being consistent in the time period we train the model to learn the drift when there is              a recession.
  simulateddata %>% 
    filter(year(YearMonth) < 2002) %>%
    group_by_key() %>% 
    model(ets = ETS(log(Sales_in_trillions)~trend("Ad"))) -> recessionDrifts
  
  # step 3: run the twoStepForecast method for forecasting 
  
  # twoStepForecast(simulateddata %>% 
  #                   filter(key=="sim2") %>% 
  #                   select(-key),
  #                 driftmodel = recessionDrifts$ets[[2]],currentyear = 2019,RecessionYears = 2021)
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                          filter(key == paste0("sim",x)) %>% 
                                                   select(-key),
                                                 driftmodel = recessionDrifts$ets[[x]],
                                        currentyear = 2019,
                                        RecessionYears = 2020)) -> simresult2020
  
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                                filter(key == paste0("sim",x)) %>% 
                                                select(-key),
                                              driftmodel = recessionDrifts$ets[[x]],
                                              currentyear = 2019,
                                              RecessionYears = 2021)) -> simresult2021
  
  
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                                filter(key == paste0("sim",x)) %>% 
                                                select(-key),
                                              driftmodel = recessionDrifts$ets[[x]],
                                              currentyear = 2019,
                                              RecessionYears = 2022)) -> simresult2022
  
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                                filter(key == paste0("sim",x)) %>% 
                                                select(-key),
                                              driftmodel = recessionDrifts$ets[[x]],
                                              currentyear = 2019,
                                              RecessionYears = 2023)) -> simresult2023
  
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                                filter(key == paste0("sim",x)) %>% 
                                                select(-key),
                                              driftmodel = recessionDrifts$ets[[x]],
                                              currentyear = 2019,
                                              RecessionYears = 2024)) -> simresult2024
  
  
  1:10 %>% lapply(function(x) twoStepForecast(simulateddata %>% 
                                                filter(key == paste0("sim",x)) %>% 
                                                select(-key),
                                              driftmodel = recessionDrifts$ets[[x]],
                                              currentyear = 2019,
                                              RecessionYears = NULL)) -> simresult
  
  list(simresult2020=simresult2020,
       simresult2021 = simresult2021,
         simresult2022 = simresult2022,
         simresult2023= simresult2023,
         simresult2024 = simresult2024,
         simresult = simresult
         )
  
  
}
