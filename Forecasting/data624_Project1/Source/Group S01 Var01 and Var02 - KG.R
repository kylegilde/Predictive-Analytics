<<<<<<< HEAD
#### libraries #### 

install_and_load_pkgs <- function(pkg){
  # Load a vector of packages and install them if needed.
  # CODE SOURCE: https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T, quietly = T, warn.conflicts = T)
}

# list the packages here
pkgs <- c("dplyr", "tidyr", "forecast", "ggplot2", "xts",  "readxl", "gridExtra", "MLmetrics", 
          "Metrics", "knitr", "ztable", "checkmate", "htmlTable", "kableExtra")  
# trace(utils:::unpackPkgZip, edit=TRUE)
install_and_load_pkgs(pkgs)


#### read & filter data #### 

raw_df <- readxl::read_excel('Set for Class.xls', col_names = TRUE) 
# c:/Users/kyleg/data624_hw_group_2/Project1/Reference/

# select only S01, Var01 and Var02
group_s01_df <- 
  raw_df %>% 
  dplyr::filter(group == "S01") %>% 
  dplyr::select(SeriesInd, Var01, Var02) %>% 
  arrange(SeriesInd)


# filter to the training observations
train_s01_df <- 
  group_s01_df %>%  
  dplyr::filter(SeriesInd < 43022)


#### create ts #### 
s01_var01_ts <- ts(train_s01_df$Var01)
s01_var02_ts <- ts(train_s01_df$Var02)

# interpolate 2 missing values
s01_var01_ts_int <- na.interp(s01_var01_ts)
na_indices <- which(is.na(s01_var01_ts))
interpolated_points <- window(s01_var01_ts_int, start = na_indices[1], end = na_indices[2])


theme_update(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
             axis.text = element_text(size = 20))


################ user-defined functions ################

fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x, stepwise = F, approximation = F), h=h)
}


get_predictions <- function(matrix_errors, actuals){
  sweep(matrix_errors, 1, -actuals)
}

mape_no_nas <- function(pred, actuals){
  
  non_na_indices <- !(is.na(pred) | is.na(actuals))
  MLmetrics::MAPE(pred[non_na_indices], actuals[non_na_indices]) * 100
  
}

rmse_no_nas <- function(errors){
  
  sqrt(mean(errors^2, na.rm = T))
  
}


################ S01 Fit Models ################

starting_point <- 1000

################ Var01 ################

# Var01 models
arima_model_var01 <- auto.arima(s01_var01_ts_int, stepwise = F, approximation = F)
# arima_model_var01_parameters <- c(length(arima_model_var01$model$phi), length(arima_model_var01$model$theta), arima_model_var01$model$Delta)
ets_model_var01 <- ets(s01_var01_ts_int)


# Var01 cross-validation
arima_errors_var01 <- tsCV(s01_var01_ts_int, farima, h = 140, initial = starting_point)
ets_errors_var01 <- tsCV(s01_var01_ts_int, fets, h = 140, initial = starting_point)

# Var01 CV predictions
arima_predictions_var01 <- get_predictions(arima_errors_var01, s01_var01_ts_int)
ets_predictions_var01 <- get_predictions(ets_errors_var01, s01_var01_ts_int)


# Var01 CV accuracy
var01_arima_results_df <- data.frame(
  model = "ARIMA(0,1,2) with drift",
  mape = apply(arima_predictions_var01, 2, function(x) mape_no_nas(x, s01_var01_ts_int)),
  rmse = apply(arima_errors_var01, 2, rmse_no_nas)
)

var01_ets_results_df <- data.frame(
  model = "ETS(M,N,N)",
  mape = apply(ets_predictions_var01, 2, function(x) mape_no_nas(x, s01_var01_ts_int)),
  rmse = apply(ets_errors_var01, 2, rmse_no_nas)
)



var01_results_df <- rbind(var01_arima_results_df, var01_ets_results_df, make.row.names = F)

var01_results_summary <- 
  var01_results_df %>% 
  group_by(model) %>% 
  summarize(mape = mean(mape),
            rmse = mean(rmse)) %>% 
  as.data.frame()

# row.names(var01_results_summary) <- var01_results_summary$model
# var01_results_summary[1] <- NULL


################ Var02 ################

# Var02 models
arima_model_var02 <- auto.arima(s01_var02_ts, stepwise = F, approximation = F)
ets_model_var02 <- ets(s01_var02_ts)


# Var02 cross-validation
arima_errors_var02 <- tsCV(s01_var02_ts, farima, h = 140, initial = starting_point)
ets_errors_var02 <- tsCV(s01_var02_ts, fets, h = 140, initial = starting_point)

# Var02 CV predictions
arima_predictions_var02 <- get_predictions(arima_errors_var02, s01_var02_ts)
ets_predictions_var02 <- get_predictions(ets_errors_var02, s01_var02_ts)

# Var02 CV accuracy
var02_arima_results_df <- data.frame(
  model = "ARIMA(2,1,2) with drift",
  mape = apply(arima_predictions_var02, 2, function(x) mape_no_nas(x, s01_var02_ts)),
  rmse = apply(arima_errors_var02, 2, rmse_no_nas)
)

var02_ets_results_df <- data.frame(
  model = "ETS(M,A,N)",
  mape = apply(ets_predictions_var02, 2, function(x) mape_no_nas(x, s01_var02_ts)),
  rmse = apply(ets_errors_var02, 2, rmse_no_nas)
)

var02_results_df <- rbind(var02_arima_results_df, var02_ets_results_df, make.row.names = F)

var02_results_summary <- 
  var02_results_df %>% 
  group_by(model) %>% 
  summarize(mape = mean(mape),
            rmse = mean(rmse))

########################## results ##########################

# initial plots
gridExtra::grid.arrange(
  autoplot(s01_var01_ts) +  
    geom_point(data = interpolated_points, size = 3, color = "red") +
    ggtitle("S01 Var01") +
    labs(y = NULL),
  autoplot(s01_var02_ts) +
    ggtitle("S01 Var02") +
    labs(y = NULL),
  nrow=1
)

# summary stats for the 4 models
var01_results_summary %>% 
  kable(digits = 2, caption = "Var01") %>% 
  kable_styling(full_width = F, font_size = 16)

var02_results_summary %>% 
  kable(digits = 2, caption = "Var02", format.args = list(big.mark = ",")) %>% 
  kable_styling(full_width = F, font_size = 16)


# forecast plots for the 2 best models
gridExtra::grid.arrange(
  autoplot(arima_model_var01_forecast) + ggtitle("S01 Var01 Forecast") + labs(x = NULL, y = NULL),
  autoplot(arima_model_var02_forecast) + ggtitle("S01 Var02 Forecast") + labs(x = NULL, y = NULL),
  nrow = 2
)

autoplot(forecast(ets_model_var01, h = 140))
autoplot(forecast(ets_model_var02, h = 140))

# print model summaries
print(arima_model_var01)
print(arima_model_var02)

checkresiduals(arima_model_var01)
checkresiduals(arima_model_var02)


# plots for the less accurate forecasts
autoplot(ets_model_var01)
autoplot(ets_model_var02)

print(ets_model_var01)
print(ets_model_var02)


checkresiduals(ets_model_var01)
checkresiduals(ets_model_var02)

gridExtra::grid.arrange(
  autoplot(s01_var02_ts) + autolayer(arima_model_var02$fitted),
  autoplot(s01_var02_ts) + autolayer(ets_model_var02$fitted)
)

########################## forecast output ##########################
arima_model_var01_forecast <- forecast(arima_model_var01, h = 140)
arima_model_var02_forecast <- forecast(arima_model_var02, h = 140)

forecasted_s01_df <- data.frame(
  Var01 = arima_model_var01_forecast$mean,
  Var02 = arima_model_var02_forecast$mean
)

write.csv(forecasted_s01_df, "forecasted_s01_df.csv")

forecast::accuracy
=======
#### libraries #### 

install_and_load_pkgs <- function(pkg){
  # Load a vector of packages and install them if needed.
  # CODE SOURCE: https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T, quietly = T, warn.conflicts = T)
}

# list the packages here
pkgs <- c("dplyr", "tidyr", "forecast", "ggplot2", "xts",  "readxl", "gridExtra", "MLmetrics", 
          "Metrics", "knitr", "ztable", "checkmate", "htmlTable", "kableExtra")  
# trace(utils:::unpackPkgZip, edit=TRUE)
install_and_load_pkgs(pkgs)


#### read & filter data #### 

raw_df <- 
  readxl::read_excel('Set for Class.xls', col_names = TRUE) 
# c:/Users/kyleg/data624_hw_group_2/Project1/Reference/

# select only S01, Var01 and Var02
group_s01_df <- 
  raw_df %>% 
  dplyr::filter(group == "S01") %>% 
  dplyr::select(SeriesInd, Var01, Var02) %>% 
  arrange(SeriesInd)


# filter to the training observations
train_s01_df <- 
  group_s01_df %>%  
  dplyr::filter(SeriesInd < 43022)


#### create ts #### 
s01_var01_ts <- ts(train_s01_df$Var01)
s01_var02_ts <- ts(train_s01_df$Var02)

# interpolate 2 missing values
s01_var01_ts_int <- na.interp(s01_var01_ts)
na_indices <- which(is.na(s01_var01_ts))
interpolated_points <- window(s01_var01_ts_int, start = na_indices[1], end = na_indices[2])


theme_update(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
             axis.text = element_text(size = 20))

# initial plots
gridExtra::grid.arrange(
  autoplot(s01_var01_ts) +  
    geom_point(data = interpolated_points, size = 3, color = "red") +
    ggtitle("S01 Var01") +
    labs(y = NULL),
  autoplot(s01_var02_ts) +
    ggtitle("S01 Var02") +
    labs(y = NULL),
  nrow=1
)


################ user-defined functions ################

fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x, stepwise = F, approximation = F), h=h)
}


get_predictions <- function(matrix_errors, actuals){
  sweep(matrix_errors, 1, -actuals)
}

mape_no_nas <- function(pred, actuals){
  
  non_na_indices <- !(is.na(pred) | is.na(actuals))
  MLmetrics::MAPE(pred[non_na_indices], actuals[non_na_indices])
  
}

mase_no_nas <- function(pred, actuals){
  
  non_na_indices <- !(is.na(pred) | is.na(actuals))
  Metrics::mase(actuals[non_na_indices], pred[non_na_indices])
  
}

rmse_no_nas <- function(errors){
  
  sqrt(mean(errors^2, na.rm = T))
  
}


################ S01 Fit Models ################

starting_point <- 1000

################ Var01 ################

# Var01 models
arima_model_var01 <- auto.arima(s01_var01_ts_int, stepwise = F, approximation = F)
# arima_model_var01_parameters <- c(length(arima_model_var01$model$phi), length(arima_model_var01$model$theta), arima_model_var01$model$Delta)
ets_model_var01 <- ets(s01_var01_ts_int)


# Var01 cross-validation
arima_errors_var01 <- tsCV(s01_var01_ts_int, farima, h = 140, initial = starting_point)
ets_errors_var01 <- tsCV(s01_var01_ts_int, fets, h = 140, initial = starting_point)

# Var01 CV predictions
arima_predictions_var01 <- get_predictions(arima_errors_var01, s01_var01_ts_int)
ets_predictions_var01 <- get_predictions(ets_errors_var01, s01_var01_ts_int)


# Var01 CV accuracy
var01_arima_results_df <- data.frame(
  model = "ARIMA(0,1,2) with drift",
  mape = apply(arima_predictions_var01, 2, function(x) mape_no_nas(x, s01_var01_ts_int)),
  mase = apply(arima_predictions_var01, 2, function(x) mase_no_nas(x, s01_var01_ts_int)),
  rmse = apply(arima_errors_var01, 2, rmse_no_nas)
)

var01_ets_results_df <- data.frame(
  model = "ETS(M,N,N)",
  mape = apply(ets_predictions_var01, 2, function(x) mape_no_nas(x, s01_var01_ts_int)),
  mase = apply(ets_predictions_var01, 2, function(x) mase_no_nas(x, s01_var01_ts_int)),
  rmse = apply(ets_errors_var01, 2, rmse_no_nas)
)

var01_results_df <- rbind(var01_arima_results_df, var01_ets_results_df, make.row.names = F)

var01_results_summary <- 
  var01_results_df %>% 
  group_by(model) %>% 
  summarize(mape = mean(mape),
            mase = mean(mase),
            rmse = mean(rmse)) %>% 
  as.data.frame()

# row.names(var01_results_summary) <- var01_results_summary$model
# var01_results_summary[1] <- NULL


################ Var02 ################

# Var02 models
arima_model_var02 <- auto.arima(s01_var02_ts, stepwise = F, approximation = F)
ets_model_var02 <- ets(s01_var02_ts)


# Var02 cross-validation
arima_errors_var02 <- tsCV(s01_var02_ts, farima, h = 140, initial = starting_point)
ets_errors_var02 <- tsCV(s01_var02_ts, fets, h = 140, initial = starting_point)

# Var02 CV predictions
arima_predictions_var02 <- get_predictions(arima_errors_var02, s01_var02_ts)
ets_predictions_var02 <- get_predictions(ets_errors_var02, s01_var02_ts)

# Var02 CV accuracy
var02_arima_results_df <- data.frame(
  model = "ARIMA(2,1,2) with drift",
  mape = apply(arima_predictions_var02, 2, function(x) mape_no_nas(x, s01_var02_ts)),
  mase = apply(arima_predictions_var02, 2, function(x) mase_no_nas(x, s01_var02_ts)),
  rmse = apply(arima_errors_var02, 2, rmse_no_nas)
)

var02_ets_results_df <- data.frame(
  model = "ETS(M,A,N)",
  mape = apply(ets_predictions_var02, 2, function(x) mape_no_nas(x, s01_var02_ts)),
  mase = apply(ets_predictions_var02, 2, function(x) mase_no_nas(x, s01_var02_ts)),
  rmse = apply(ets_errors_var02, 2, rmse_no_nas)
)

# ggplot(var02_ets_results_df) + geom_bar(aes(x = row.names(var02_ets_results_df), y = mase))

var02_results_df <- rbind(var02_arima_results_df, var02_ets_results_df, make.row.names = F)

var02_results_summary <- 
  var02_results_df %>% 
  group_by(model) %>% 
  summarize(mape = mean(mape),
            mase = mean(mase),
            rmse = mean(rmse))

########################## results ##########################

var01_results_summary %>% 
  kable(digits = 4, caption = "Var01") %>% 
  kable_styling(full_width = F, font_size = 16)

var02_results_summary %>% 
  kable(digits = 2, caption = "Var02", format.args = list(big.mark = ",")) %>% 
  kable_styling(full_width = F, font_size = 16)

# forecast output
arima_model_var01_forecast <- forecast(arima_model_var01, h = 140)
arima_model_var02_forecast <- forecast(arima_model_var02, h = 140)

forecasted_s01_df <- data.frame(
  Var01 = arima_model_var01_forecast$mean,
  Var02 = arima_model_var02_forecast$mean
)

write.csv(forecasted_s01_df, "forecasted_s01_df.csv")

gridExtra::grid.arrange(
  autoplot(arima_model_var01_forecast) + ggtitle("S01 Var01 Forecast") + labs(x = NULL, y = NULL),
  autoplot(arima_model_var02_forecast) + ggtitle("S01 Var02 Forecast") + labs(x = NULL, y = NULL),
  nrow = 2
)

autoplot(forecast(ets_model_var01, h = 140))

autoplot(forecast(ets_model_var02, h = 140))

print(arima_model_var01)
print(ets_model_var01)
print(arima_model_var02)
print(ets_model_var02)

# Var02 ets is a 
gridExtra::grid.arrange(
  autoplot(s01_var02_ts) + autolayer(arima_model_var02$fitted),
  autoplot(s01_var02_ts) + autolayer(ets_model_var02$fitted)
)

autoplot(ets_model_var01)
autoplot(ets_model_var02)


checkresiduals(arima_model_var01)
checkresiduals(ets_model_var01)
checkresiduals(arima_model_var02)
checkresiduals(ets_model_var02)
>>>>>>> d64b1791109478ae7d743c070de0c0721820f80f
