#Hyperparameter training for model

#Source
source('Source/project1_jg.R')
source('local_cv_test.R')

#Library
library(doMC)
library(forecast)

#load file if exists
destfile <- 'Models/project1_jg_holt.RData'
if(file.exists(destfile)){
  load(destfile)
}
if(!file.exists(destfile)){
  #empty metric df
  model_metrics_holt_df <- data.frame()
}

#forecasting function for tsCV
forecast_fun_no_reg <- function(y, h = h, lambda_sub=NULL) {
  if(lambda_sub=='NULL'){
    lambda_sub=NULL
  }
  tmp_model <- holt(y, h=h, lambda = lambda_sub)
  tmp_fit <- tmp_model %>% forecast(h=h)
  #Need to slice the output because using xreg returns prediction of xreg no matter what
  return(tmp_fit)
}

#tmp_csv <- tsCV_jg(y = ts(s03_v05_full_ts, frequency = 6), 
#                  forecastfunction = forecast_fun_no_reg,
#                  h=2,
#                  initial=1600,
#                  lambda_sub=NULL)

registerDoMC(3)
for(p in seq(1, 200, 199)){
  tmp_ts <- ts(s03_v05_full_ts, frequency = p, start = min(excel_df$SeriesInd))
  tmp_df <- foreach(l = c('NULL', 'auto', 0), .combine = rbind) %dopar% {
    tmp_csv <- tsCV_jg(y = tmp_ts, 
                       forecastfunction = forecast_fun_no_reg,
                       h=140,
                       initial=1000,
                       extra_args = TRUE,
                       lambda_sub=l)
    data.frame(
      ModelName=paste('holt', 'p', p, 'l', as.character(l), sep='_'),
      RMSE_10=sqrt(mean(tmp_csv$e[,10]^2, na.rm=TRUE)),
      RMSE_100=sqrt(mean(tmp_csv$e[,100]^2, na.rm=TRUE)),
      RMSE_140=sqrt(mean(tmp_csv$e[,140]^2, na.rm=TRUE)),
      MAPE_10=mean(abs(tmp_csv$pe[,10]), na.rm = TRUE),
      MAPE_100=mean(abs(tmp_csv$pe[,100]), na.rm = TRUE),
      MAPE_140=mean(abs(tmp_csv$pe[,140]), na.rm = TRUE),
      RunTime=Sys.time())
  }
  model_metrics_holt_df <- rbind(model_metrics_holt_df, tmp_df)
  print('latest model')
  print(tmp_df %>% arrange(RMSE_140))
  print('all models')
  print(model_metrics_holt_df %>% arrange(RMSE_140) %>% head(10))
  flush.console()
}

save(model_metrics_holt_df, file = destfile)