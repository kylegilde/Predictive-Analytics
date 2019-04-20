#Hyperparameter training for model

#Source
source('Source/project1_jg.R')
source('local_cv_test.R')

#Library
library(doMC)
library(forecast)

#load file if exists
destfile <- 'Models/project1_jg_stlf_2.RData'
if(file.exists(destfile)){
  load(destfile)
}
if(!file.exists(destfile)){
  #empty metric df
  model_metrics_stlf_2_df <- data.frame()
}

#forecasting function for tsCV
forecast_fun_no_reg <- function(y, h = h, xreg=NULL, method_sub=NULL, tw= NULL, sw = NULL) {
  x <- stlf(y, 
          method=method_sub, 
          t.window = tw, 
          s.window = sw, 
          robust = TRUE, 
          h = h)
  return(x)
}

#tmp_csv <- tsCV_jg(y = msts(s03_v05_full_ts, seasonal.periods = c(6,18)), 
#                   forecastfunction = forecast_fun_no_reg,
#                   h=600,
#                   initial=1000,
#                   tw = 3,
#                   sw = 7)

registerDoMC(3)
#stl hyperparameter training
for(p in seq(90, 210, 30)){
  tmp_ts <- ts(s03_v07_full_ts, frequency = p, start = 1)
  #tmp_train_ts <- subset(tmp_ts, start = 1, end = length(tmp_ts)-test_length)
  #tmp_test_ts <- subset(tmp_ts, start = length(tmp_ts)-test_length+1)
  for(m in c('arima', 'ets')){
    for(s1 in c(7, 101, 501)){
      tmp_df <- foreach(t1 = c(1, 5, 9, 11, 21, 51), .combine = rbind) %dopar% {
      tmp_csv <- tsCV_jg(y = tmp_ts, 
                         forecastfunction = forecast_fun_no_reg,
                         h=140,
                         initial=1000,
                         extra_args = TRUE,
                         method_sub = m,
                         tw = t1,
                         sw = s1)
      data.frame(
        ModelName=paste('stlf', 'p', p, 'm', m, 't1', t1, 's1', s1, sep='_'),
        RMSE_10=sqrt(mean(tmp_csv$e[,10]^2, na.rm=TRUE)),
        RMSE_100=sqrt(mean(tmp_csv$e[,100]^2, na.rm=TRUE)),
        RMSE_140=sqrt(mean(tmp_csv$e[,140]^2, na.rm=TRUE)),
        MAPE_10=mean(abs(tmp_csv$pe[,10]), na.rm = TRUE),
        MAPE_100=mean(abs(tmp_csv$pe[,100]), na.rm = TRUE),
        MAPE_140=mean(abs(tmp_csv$pe[,140]), na.rm = TRUE),
        RunTime=Sys.time())
      }
      model_metrics_stlf_2_df <- rbind(model_metrics_stlf_2_df, tmp_df)
      print('latest model')
      print(tmp_df %>% arrange(RMSE_140))
      print('all models')
      print(model_metrics_stlf_2_df %>% arrange(RMSE_140) %>% head(10))
      flush.console()
    }
  }
}

save(model_metrics_stlf_2_df, file = destfile)
