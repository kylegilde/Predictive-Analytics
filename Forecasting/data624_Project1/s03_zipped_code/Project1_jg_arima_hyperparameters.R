#Hyperparameter training for model

#Source
source('Source/project1_jg.R')
source('local_cv_test.R')

#Library
library(doMC)
library(forecast)

#load file if exists
destfile <- 'Models/project1_jg_arima.RData'
if(file.exists(destfile)){
  load(destfile)
}
if(!file.exists(destfile)){
  #empty metric df
 model_metrics_arima_df <- data.frame()
}

#forecasting function for tsCV
forecast_fun_no_reg <- function(y, h = h, xreg=NULL, lambda_sub=NULL, p_sub=NULL, d_sub=NULL, q_sub=NULL, drift_sub=drift) {
  if(lambda_sub=='NULL'){
    lambda_sub=NULL
  }
  tmp_model <- Arima(y, 
                    lambda = lambda_sub,
                    include.drift = drift_sub,
                    order=c(p_sub, d_sub, q_sub))
  tmp_fit <- tmp_model %>% 
    forecast(h=h)
  return(tmp_fit)
}

#tmp_csv <- tsCV_jg(y = msts(s03_v05_full_ts, seasonal.periods = c(6,6)), 
#                  forecastfunction = forecast_fun_no_reg,
#                  h=2,
#                  initial=1600,
#                  k1=2,
#                  k2=3)

#k2 = seq(1, min(floor(p/2), 10), max(floor(min(floor(p/2),10)/5),1))

registerDoMC(3)
#stl hyperparameter training
for(pd in seq(90, 210, 30)){
  tmp_ts <- ts(s03_v05_full_ts, frequency = pd, start = 1)
  for (dr in c(FALSE, TRUE)){
    for (l in c('NULL', 'auto', 0)){
      for(p in seq(0, 2, 1)){
        for(d in seq(0, 2, 1)){
          tmp_df <- foreach(q = seq(0,2,1), .combine = rbind) %dopar% {
            tmp_csv <- tsCV_jg(y = tmp_ts, 
                               forecastfunction = forecast_fun_no_reg,
                               h=140,
                               initial=1000,
                               extra_args = TRUE,
                               lambda_sub=l,
                               p_sub=p,
                               d_sub=d,
                               q_sub=q,
                               drift=dr)
          data.frame(
            ModelName=paste('arima', 'pd', pd, 'l', as.character(l), 'p', p, 'd', d, 'q', q, 'dr', dr, sep='_'),
            RMSE_10=sqrt(mean(tmp_csv$e[,10]^2, na.rm=TRUE)),
            RMSE_100=sqrt(mean(tmp_csv$e[,100]^2, na.rm=TRUE)),
            RMSE_140=sqrt(mean(tmp_csv$e[,140]^2, na.rm=TRUE)),
            MAPE_10=mean(abs(tmp_csv$pe[,10]), na.rm = TRUE),
            MAPE_100=mean(abs(tmp_csv$pe[,100]), na.rm = TRUE),
            MAPE_140=mean(abs(tmp_csv$pe[,140]), na.rm = TRUE),
            RunTime=Sys.time())
          }
          model_metrics_arima_df <- rbind(model_metrics_arima_df, tmp_df)
          print('latest model')
          print(tmp_df %>% arrange(RMSE_100))
          print('all models')
          print(model_metrics_arima_df %>% arrange(RMSE_100) %>% head(10))
          flush.console()
        }
      }
    }
  }
}
save(model_metrics_arima_df, file = destfile)
