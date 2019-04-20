install_and_load_pkgs <- function(pkg){
  # Load a vector of packages and install them if needed.
  # CODE SOURCE: https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T, quietly = T, warn.conflicts = T)
}

# list the packages here
pkgs <- c("dplyr", "tidyr", "forecast", "ggplot2", "xts",  "readxl", "gridExtra", "MLmetrics", "Metrics") #"doMC", "kableExtra", "openxlsx",
# trace(utils:::unpackPkgZip, edit=TRUE)
install_and_load_pkgs(pkgs)

# if you are using windows, use this command:
# install.packages("doMC", repos="http://R-Forge.R-project.org")






# read file
getwd()
raw_df <- 
  readxl::read_excel('Set for Class.xls', col_names = TRUE) 

# c:/Users/kyleg/data624_hw_group_2/Project1/Reference/

group_s01_df <- 
  raw_df %>% 
  dplyr::filter(group == "S01") %>% 
  dplyr::select(SeriesInd, Var01, Var02) %>% 
  arrange(SeriesInd)

# filter to S01 and vars 1 and 2
train_s01_df <- 
  group_s01_df %>%  
  dplyr::filter(SeriesInd < 43022)


nrow(train_s01_df)
s01_var01_ts <- ts(train_s01_df$Var01)



s01_var01_ts_int <- na.interp(s01_var01_ts)
s01_var02_ts <- ts(train_s01_df$Var02)

na_indices <- which(is.na(s01_var01_ts))
interpolated_points <- window(s01_var01_ts_int, start = na_indices[1], end = na_indices[2])


gridExtra::grid.arrange(
  autoplot(s01_var01_ts) + geom_point(data = interpolated_points, size = 2, color = "red") + ggtitle("S01 Var01"),
  autoplot(s01_var02_ts) + ggtitle("S01 Var02"),
  nrow=1 #, ncol=2
)

fets <- function(x, h) {
  forecast(ets(x), h = h)
}

farima <- function(x, h) {
  forecast(auto.arima(x, stepwise = F, approximation = F), h=h)
}


get_predictions <- function(matrix_errors, actuals){
  sweep(matrix_errors, 1, -actuals)
}

col_wise_mape <- function(prediction_matrix, actuals){
  
  mape_values <- rep(NA, ncol(prediction_matrix))
  names(mape_values) <- colnames(prediction_matrix)
  
  for (i_col in 1:ncol(prediction_matrix)){
    
    prediction_col <- prediction_matrix[, i_col]
    non_na_indices <- !(is.na(pred) | is.na(actuals))
    mape_values[i_col] <- MLmetrics::MAPE(prediction_col[non_na_indices], actuals[non_na_indices])
    
  }
  
  return(mape_values)
}

col_wise_mase <- function(prediction_matrix, actuals){
  
  mase_values <- rep(NA, ncol(prediction_matrix))
  names(mase_values) <- colnames(prediction_matrix)
  
  for (i_col in 1:ncol(prediction_matrix)){
    
    prediction_col <- prediction_matrix[, i_col]
    non_na_indices <- !(is.na(pred) | is.na(actuals))
    mase_values[i_col] <- Metrics::mase(actuals[non_na_indices], prediction_col[non_na_indices])
    
  }
  
  return(mase_values)
}

mape_no_nas <- function(pred, actuals){
  
  non_na_indices <- !(is.na(pred) | is.na(actuals))
  MLmetrics::MAPE(prediction_col[non_na_indices], actuals[non_na_indices])
  
}



starting_point <- 1000

# Var01 models
arima_model_var01 <- auto.arima(s01_var01_ts_int, stepwise = F, approximation = F)
checkresiduals(arima_model_var01)

ets_model_var01 <- ets(s01_var01_ts_int)
checkresiduals(ets_model_var01)


# Var02 models
arima_model_var02 <- auto.arima(s01_var02_ts, stepwise = F, approximation = F)
checkresiduals(arima_model_var02)

ets_model_var02 <- ets(s01_var02_ts)
checkresiduals(ets_model_var02)


# Var01 cross-validation
arima_errors_var01 <- tsCV(s01_var01_ts_int, farima, h = 140, initial = starting_point)
ets_errors_var01 <- tsCV(s01_var01_ts_int, fets, h = 140, initial = starting_point)

# Var02 cross-validation
arima_errors_var02 <- tsCV(s01_var02_ts, farima, h = 140, initial = starting_point)
ets_errors_var02 <- tsCV(s01_var02_ts, fets, h = 140, initial = starting_point)

# Var01 CV predictions
arima_predictions_var01 <- get_predictions(arima_errors_var01, s01_var01_ts_int)
ets_predictions_var01 <- get_predictions(ets_errors_var01, s01_var01_ts_int)

# Var02 CV predictions
arima_predictions_var02 <- get_predictions(arima_errors_var02, s01_var02_ts)
ets_predictions_var02 <- get_predictions(ets_errors_var02, s01_var02_ts)

# Var01 CV accuracy
arima_mape_var01 <- apply(arima_predictions_var01, 2, function(x) mape_no_nas(x, s01_var01_ts_int))
arima_mape_var01_b <- col_wise_mape(arima_predictions_var01, s01_var01_ts_int)
all(arima_mape_var01 == arima_mape_var01_b)

ets_mape_var01 <- col_wise_mape(arima_predictions_var01, s01_var01_ts_int)

arima_mase_var01 <- col_wise_mase(arima_predictions_var01, s01_var01_ts_int)
ets_mase_var01 <- col_wise_mase(arima_predictions_var01, s01_var01_ts_int)

# Var02 CV accuracy
arima_mape_var02 <- col_wise_mape(arima_predictions_var02, s01_var02_ts)
ets_mape_var02 <- col_wise_mape(arima_predictions_var02, s01_var02_ts)

arima_mase_var02 <- col_wise_mase(arima_predictions_var02, s01_var02_ts_int)
ets_mase_var02 <- col_wise_mase(arima_predictions_var02, s01_var02_ts_int)