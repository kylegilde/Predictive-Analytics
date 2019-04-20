install_and_load_pkgs <- function(pkg){
  # Load a vector of packages and install them if needed.
  # CODE SOURCE: https://gist.github.com/stevenworthington/3178163
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T, quietly = T, warn.conflicts = T)
}

# list the packages here
pkgs <- c("dplyr", "tidyr", "forecast", "ggplot2", "xts", "doMC", "kableExtra", "openxlsx")
install.packages("doMC", repos="http://R-Forge.R-project.org")

library(dplyr)
library(tidyr)
library(forecast)
library(xts)
library(ggplot2)

raw_excel_df <- readxl::read_excel('Reference/Set for Class.xls', col_names = TRUE)
excel_df <- raw_excel_df %>% 
  #Remove the rows intended to be filled by prediction
  filter(SeriesInd<43022) 

# Select Var01 and Var02
s4v1 <- excel_df %>% select(group,SeriesInd, Var01) %>% filter(group == 'S04')
s4v2 <- excel_df %>% select(group,SeriesInd,Var02) %>% filter(group == 'S04')
s4 <- s4v1 %>% inner_join(s4v2, by=c('SeriesInd','group'))

#check outliers
boxplot(s4v1$Var01,
        main = "Outliers for S4 Var01",
        at = c(1),
        names = c("Var01"),
        las = 1,
        col = c("orange"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
summary(s4v1$Var01)
#No apparent outliers


boxplot(s4v2$Var02,
        main = "Outliers for S4 Var02",
        at = c(1),
        names = c("Var02"),
        las = 1,
        col = c("red"),
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
# it appears the 5 highest are outliers
outliers_s4v2 <- tail(sort.int(s4v2$Var02, partial=length(s4v2$Var02) - 4), 5)
s4v2_no_outlier <- s4v2 %>% filter(!(Var02 %in% outliers_s4v2))

# No NAs in Var01, and only 2 NAs in Var02, so remove NAs, and arrange by SeriesID
colSums(is.na(s4))
s4v2_trimmed <- s4v2_no_outlier %>% na.omit() %>% arrange(SeriesInd)
s4 <- s4 %>% na.omit() %>% arrange(SeriesInd)

#convert to time series
s4v1_ts <- s4v1 %>% select(Var01) %>% ts(frequency = 1)

s4v2_ts <- s4v2_trimmed %>% select(Var02) %>% ts(frequency = 1)

#test/train split
test_length <- 600

ggplot_df <- excel_df %>% select(1:4) %>%  gather('Var', 'Value', 3:4) %>% filter(group == "S04")
ggplot_df <- excel_df %>%  gather('Var', 'Value', 3:4) %>% filter(group == "S04")

#check trends
ggplot(ggplot_df, aes(x=SeriesInd, y=Value)) + 
  geom_line() + 
  facet_wrap(~Var, scales = 'free', ncol = 2) +
  ggtitle('S04 Plots With Outliers') +
  theme(plot.title = element_text(hjust = 0.5))


#graph and examine possible frequencies
ggplot(s4v1, aes(x=SeriesInd, y=Var01)) + geom_line()
ggplot(s4v2_trimmed, aes(x=SeriesInd, y=Var02)) + geom_line() 

library(GGally)
ggpairs(excel_df %>% select(-SeriesInd, -group))

#The relationship is strongly positive, more so for lower values of lags.
# This suggests a strong seasonality in the data. 
gglagplot(s4v1_ts) + 
  ggtitle('S04 Var01 Lag Plot') + 
  theme(plot.title = element_text(hjust = 0.5))

# There is no noticeable relationship other than a weak positive relationship for lags 1 and 2.
gglagplot(s4v2_ts) + 
  ggtitle('S04 Var02 Lag Plot') + 
  theme(plot.title = element_text(hjust = 0.5))

#The autocorrelation plots show that all points are significantly different from zero, indicating substantial autocorrelation.
# There is a strong indication of trend since  the autocorrelations for small lags are large and positive that decrease slowly as lags increase. 
ggAcf(s4v1_ts, lag = 1600) + 
  ggtitle('S04 Var01 ACF Plot') + 
  theme(plot.title = element_text(hjust = 0.5))

# The "scalloped" shape in the Var02 chart suggests a strong indication of seasonality 
ggAcf(s4v2_ts, lag = 1600) + 
  ggtitle('S04 Var01 ACF Plot') + 
  theme(plot.title = element_text(hjust = 0.5))

#train/test set
test_ratio <- 0.95
cv_h <- 140
cv_start <- 1000
prediction_length <- 140

####################################################################
###################### Find Optimal Frequency ######################
####################################################################

window_finder <- function(freq){
  ts1 <- ts(s4v1_ts, frequency = freq, start = 1)
  ts_data <- na.interp(ts1)
  train_ts<- window(ts_data,start = 1,end = floor(test_ratio * 1622/freq))
  test_ts <- window(ts_data,start = floor(test_ratio * 1622/freq))
  return(list(train_ts, test_ts))
}

window_finder2 <- function(freq){
  ts1 <- ts(s4v2_ts, frequency = freq, start = 1)
  ts_data <- na.interp(ts1)
  train_ts<- window(ts_data,start = 1,end = floor(test_ratio * 1622/freq))
  test_ts <- window(ts_data,start = floor(test_ratio * 1622/freq))
  return(list(train_ts, test_ts))
}

train_datasets <- list()
test_datasets <- list()

train_datasets2 <- list()
test_datasets2 <- list()

for (i in 1:500){
  temp <- window_finder(i)
  train_datasets <- c(train_datasets, list(temp[[1]]))
  test_datasets <- c(test_datasets, list(temp[[2]]))
}

for (i in 1:500){
  temp <- window_finder2(i)
  train_datasets2 <- c(train_datasets2, list(temp[[1]]))
  test_datasets2 <- c(test_datasets2, list(temp[[2]]))
}


save(train_datasets, file = 'Models/train_datasets.RData')
save(test_datasets, file = 'Models/test_datasets.RData')
save(train_datasets2, file = 'Models/train_datasets2.RData')
save(test_datasets2, file = 'Models/test_datasets2.RData')


num_cases <- length(test_datasets)



####################################################################
########################## Baseline Model ##########################
####################################################################

############################### V01 ################################
mape <- list()
rmse <- list()

for (i in 1:num_cases){
  model1 <- naive(train_datasets[[i]], h = length(test_datasets[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets[[i]])[10]))
}

optimum_freq <- which.min(mape)

naive <- do.call(rbind, Map(data.frame, algorithm = 'naive',freq=seq(1,length(rmse)), 
                            rmse=rmse, mape=mape))
ggplot() + 
  geom_line(data = naive, aes(x=freq,y = rmse), color = "blue") +
  geom_line(data = naive, aes(x=freq,y = mape), color = "red") 

naive_frequency = optimum_freq

best <- do.call(rbind, Map(data.frame, var='Var01', algorithm = 'naive',h=naive_frequency, 
                rmse=min(unlist(rmse)), mape=min(unlist(mape))))

s4v1_naive_ts <- s4v1_ts 
s4v1_naive_train_ts <- train_datasets[[optimum_freq]]
s4v1_naive_test_ts <- test_datasets[[optimum_freq]]
s4v1_naive_test_fit <- naive(s4v1_naive_train_ts, h = length(s4v1_naive_test_ts))
s4v1_naive_fit <- naive(s4v1_naive_ts, h = prediction_length)



# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
res_s4v1 <- residuals(s4v1_naive_fit)
checkresiduals(s4v1_naive_fit)
autoplot(s4v1_naive_fit) + 
  autolayer(s4v1_naive_test_fit) +
  autolayer(s4v1_naive_ts) + 
  ggtitle('S04 Var01 Naive Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v1, lag=10, fitdf=0); Box.test(res_s4v1,lag=10, fitdf=0, type="Lj")


############################### V02 ################################
mape <- list()
rmse <- list()

for (i in 1:num_cases){
  model1 <- naive(train_datasets2[[i]], h = length(test_datasets2[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets2[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets2[[i]])[10]))
}

optimum_freq <- which.min(mape)

naive2 <- do.call(rbind, Map(data.frame, algorithm = 'naive',freq=seq(1,length(rmse)), 
                            rmse=rmse, mape=mape))

ggplot() + 
  geom_line(data = naive2, aes(x=freq,y = rmse), color = "blue")
ggplot() + 
  geom_line(data = naive2, aes(x=freq,y = mape), color = "red") 

naive_frequency2 = optimum_freq

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var02', algorithm = 'naive',
                                       h=naive_frequency2, 
                rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

s4v2_naive_ts <- s4v2_ts
s4v2_naive_train_ts <- train_datasets2[[optimum_freq]]
s4v2_naive_test_ts  <- test_datasets2[[optimum_freq]]
s4v2_naive_test_fit <- naive(s4v2_naive_train_ts, h = length(s4v2_naive_test_ts))
s4v2_naive_fit <- naive(s4v2_naive_ts, h = prediction_length)


# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
res_s4v2 <- residuals(s4v2_naive_fit)

autoplot(s4v2_naive_fit) + 
  autolayer(s4v2_naive_test_fit) +
  autolayer(s4v2_naive_ts) + 
  ggtitle('S04 Var02 Naive Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# The two tests for autucorrelation are both statistically significant, hinting at some autocorrelation effects.
# The lag graph shows that the autocorrelation is significant at low values of lags. 
checkresiduals(s4v2_naive_test_fit)

# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v2, lag=10, fitdf=0); Box.test(res_s4v2,lag=10, fitdf=0, type="Lj")


####################################################################
############################ STL Model #############################
####################################################################

############################### V01 ################################
mape <- list()
rmse <- list()

# loop starting from 2 since with i=1, it wouldn't be a seasonal ts object
for (i in 2:num_cases){
  model1 <- stlf(train_datasets[[i]], 
                 method='ets', 
                 s.window = "periodic", 
                 robust = TRUE,
                 h =  length(test_datasets[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets[[i]])[10]))
}

optimum_freq <- which.min(mape)
stl_frequency = optimum_freq

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var01', algorithm = 'stl',
                                       h=stl_frequency, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

stl <- do.call(rbind, Map(data.frame, algorithm = 'stl',freq=seq(2,length(rmse)+1), 
                            rmse=rmse, mape=mape))
ggplot() + geom_line(data = stl, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = stl, aes(x=freq,y = mape), color = "red") 

s4v1_stl_ts <- ts(s4v1_ts, frequency = stl_frequency, start = 1)
s4v1_stl_train_ts <- train_datasets[[optimum_freq]]
s4v1_stl_test_ts  <- test_datasets[[optimum_freq]]
s4v1_stl_test_fit <- stlf(s4v1_stl_train_ts, 
                          method='ets', 
                          s.window = "periodic", 
                          robust = TRUE,
                          h =  length(s4v1_stl_test_ts))
s4v1_stl_fit  <- stlf(s4v1_stl_ts, 
                      method='ets', 
                      s.window = "periodic",
                      robust = TRUE,
                      h = prediction_length)


# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(s4v1_stl_fit)
autoplot(s4v1_stl_fit) + 
  autolayer(s4v1_stl_test_fit, series = 'STL_Test (Red)') +
  autolayer(s4v1_stl_test_ts, series = 'Actual (Black)') +
  autolayer(naive(s4v1_stl_ts, h = prediction_length), 
            series="Naive (Green)", PI=FALSE) +
  autolayer(naive(s4v1_stl_train_ts, h = length(s4v1_stl_test_ts)), 
            series="Naive_Test (Blue)", PI=FALSE) +
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var01 STL Plot') + 
  theme(plot.title = element_text(hjust = 0.5))



# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v1, lag=10, fitdf=0); Box.test(res_s4v1,lag=10, fitdf=0, type="Lj")


############################### V02 ################################

mape <- list()
rmse <- list()

# loop starting from 2 since with i=1, it wouldn't be a seasonal ts object
for (i in 2:num_cases){
  model1 <- stlf(train_datasets2[[i]], 
                 method='ets', 
                 s.window = "periodic", 
                 robust = TRUE,
                 h =  length(test_datasets2[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets2[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets2[[i]])[10]))
}

optimum_freq <- which.min(mape)
stl_frequency2 = optimum_freq

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var02', algorithm = 'stl',
                                       h=stl_frequency2, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

stl2 <- do.call(rbind, Map(data.frame, algorithm = 'stl',freq=seq(2,length(rmse)+1), 
                          rmse=rmse, mape=mape))
ggplot() + geom_line(data = stl2, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = stl2, aes(x=freq,y = mape), color = "red") 

s4v2_stl_ts <- ts(s4v2_ts, frequency = stl_frequency2, start = 1)
s4v2_stl_train_ts <- train_datasets2[[optimum_freq]]
s4v2_stl_test_ts  <- test_datasets2[[optimum_freq]]
s4v2_stl_test_fit <- stlf(s4v2_stl_train_ts, 
                          method='ets', 
                          s.window = "periodic", 
                          robust = TRUE,
                          h =  length(s4v2_stl_test_ts))
s4v2_stl_fit  <- stlf(s4v2_stl_ts, 
                      method='ets', 
                      s.window = "periodic",
                      robust = TRUE,
                      h = prediction_length)


# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(s4v2_stl_fit)
autoplot(s4v2_stl_fit) + 
  autolayer(s4v2_stl_test_fit, series = 'STL_Test (Red)') +
  autolayer(s4v2_stl_test_ts, series = 'Actual (Black)') +
  autolayer(naive(s4v2_stl_ts, h = prediction_length), 
            series="Naive (Green)", PI=FALSE) +
  autolayer(naive(s4v2_stl_train_ts, h = length(s4v2_stl_test_ts)), 
            series="Naive_Test (Blue)", PI=FALSE) +
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var02 STL Plot') + 
  theme(plot.title = element_text(hjust = 0.5))



# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v2, lag=10, fitdf=0); Box.test(res_s4v2,lag=10, fitdf=0, type="Lj")



####################################################################
############################ Holt Model ############################
####################################################################

################################ V01  ##############################

mape <- list()
rmse <- list()

# loop starting from 2 since with i=1, it wouldn't be a seasonal ts object
for (i in 2:num_cases){
  model1 <- holt(train_datasets[[i]], 
                 lambda = NULL, 
                 level = c(85, 90),
                 h=length(test_datasets[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets[[i]])[10]))
}

optimum_freq <- which.min(mape)
holt_frequency = optimum_freq

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var01', algorithm = 'holt',
                                       h=holt_frequency, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

holt <- do.call(rbind, Map(data.frame, algorithm = 'holt',freq=seq(2,length(rmse)+1), 
                          rmse=rmse, mape=mape))
ggplot() + geom_line(data = holt, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = holt, aes(x=freq,y = mape), color = "red") 

s04_v01_holt_ts <- ts(s4v1_ts, frequency = holt_frequency, start = 1)
s04_v01_holt_train_ts <- train_datasets[[optimum_freq]]
s04_v01_holt_test_ts  <- test_datasets[[optimum_freq]]
s04_v01_holt_test <- holt(s04_v01_holt_train_ts, 
                          lambda = NULL, 
                          level = c(85, 90),
                          h=length(s04_v01_holt_test_ts))
s04_v01_holt_test_fit <- s04_v01_holt_test %>% 
  forecast(h=length(s04_v01_holt_test_ts), lambda=NULL, level = c(85, 90))
s04_v01_holt <- holt(s04_v01_holt_ts, 
                     lambda = NULL, 
                     level = c(85, 90),
                     h=prediction_length)
# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(s04_v01_holt)
s04_v01_holt_fit <- s04_v01_holt %>% forecast(h=prediction_length, level = c(85, 90))
autoplot(s04_v01_holt_fit) + 
  autolayer(s04_v01_holt_test_fit, series = 'Holt_Test (Green)') +
  autolayer(s04_v01_holt_test_ts, series = 'Actual (Black)') +
  autolayer(naive(s04_v01_holt_ts, h = prediction_length), 
            series="Naive Forecast (Blue)", PI=FALSE) +
  autolayer(naive(s04_v01_holt_train_ts, h = length(s04_v01_holt_test_ts)), 
            series="Naive_Test (Red)", PI=FALSE) + 
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var01 Holt Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v1, lag=10, fitdf=0); Box.test(res_s4v1,lag=10, fitdf=0, type="Lj")


################################ V02  ##############################

mape <- list()
rmse <- list()

# loop starting from 2 since with i=1, it wouldn't be a seasonal ts object
for (i in 2:num_cases){
  model1 <- holt(train_datasets2[[i]], 
                 lambda = NULL, 
                 level = c(85, 90),
                 h=length(test_datasets2[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets2[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets2[[i]])[10]))
}

optimum_freq <- which.min(mape)
holt_frequency2 = optimum_freq

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var02', algorithm = 'holt',
                                       h=holt_frequency2, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

holt2 <- do.call(rbind, Map(data.frame, algorithm = 'holt',freq=seq(2,length(rmse)+1), 
                           rmse=rmse, mape=mape))
ggplot() + geom_line(data = holt, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = holt, aes(x=freq,y = mape), color = "red") 

s04_v02_holt_ts <- ts(s4v2_ts, frequency = holt_frequency2, start = 1)
s04_v02_holt_train_ts <- train_datasets[[optimum_freq]]
s04_v02_holt_test_ts  <- test_datasets[[optimum_freq]]
s04_v02_holt_test <- holt(s04_v02_holt_train_ts, 
                          lambda = NULL, 
                          level = c(85, 90),
                          h=length(s04_v02_holt_test_ts))
s04_v02_holt_test_fit <- s04_v02_holt_test %>% 
  forecast(h=length(s04_v02_holt_test_ts), lambda=NULL, level = c(85, 90))
s04_v02_holt <- holt(s04_v02_holt_ts, 
                     lambda = NULL, 
                     level = c(85, 90),
                     h=prediction_length)
# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(s04_v02_holt)
s04_v02_holt_fit <- s04_v02_holt %>% forecast(h=prediction_length, level = c(85, 90))
autoplot(s04_v02_holt_fit) + 
  autolayer(s04_v02_holt_test_fit, series = 'Holt_Test (Green)') +
  autolayer(s04_v02_holt_test_ts, series = 'Actual (Black)') +
  autolayer(naive(s04_v02_holt_ts, h = prediction_length), 
            series="Naive Forecast (Blue)", PI=FALSE) +
  autolayer(naive(s04_v02_holt_train_ts, h = length(s04_v02_holt_test_ts)), 
            series="Naive_Test (Red)", PI=FALSE) + 
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var02 Holt Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals are not distinguishable from white noise.
Box.test(res_s4v2, lag=10, fitdf=0); Box.test(res_s4v2,lag=10, fitdf=0, type="Lj")


####################################################################
######################## Holt Winters Model ########################
####################################################################

################################ V01  ##############################
mape <- list()
rmse <- list()

# frequencies higher than 24 is "too high" for forecast(ets()) call
for (i in 2:24){
  model1 <- hw(train_datasets[[i]], 
                 lambda = NULL, 
                 damped=TRUE,
                 seasonal='additive',
                 level = c(85, 90),
                 h=length(test_datasets[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets[[i]])[10]))
}

optimum_freq <- which.min(mape)
best <- rbind(best, do.call(rbind, Map(data.frame, var='Var01', algorithm = 'hw',
                                       h=optimum_freq, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

df <- do.call(rbind, Map(data.frame, algorithm = 'hw',freq=seq(2,length(rmse)+1), 
                           rmse=rmse, mape=mape))
ggplot() + geom_line(data = df, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = df, aes(x=freq,y = mape), color = "red") 

s4v1_ts_NoNA <- na.remove(s4v1_ts)

full_data <- ts(s4v1_ts_NoNA, frequency = optimum_freq, start = 1)
train_data <- train_datasets[[optimum_freq]]
test_data  <- test_datasets[[optimum_freq]]
test_model <- hw(train_data, 
                lambda = NULL, 
                 damped=TRUE,
                 seasonal='additive',
                level = c(85, 90),
                h=length(test_data))
test_fit <- test_model %>% 
  forecast(h=length(test_data), lambda=NULL, level = c(85, 90))

full_model <- hw(full_data, 
                 lambda = NULL,
                 damped=TRUE,
                 seasonal='additive',
                 level = c(85, 90),
                 h=prediction_length)
# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(full_model)
full_fit <- full_model %>% forecast(h=prediction_length, level = c(85, 90))

# The forecast appears to be a constant in the prediction window. 
autoplot(full_fit) + 
  autolayer(test_fit, series = 'Holt_Test (Green)') +
  autolayer(test_data, series = 'Actual (Black)') +
  autolayer(naive(full_data, h = prediction_length), 
            series="Naive Forecast (Blue)", PI=FALSE) +
  autolayer(naive(train_data, h = length(test_data)), 
            series="Naive_Test (Red)", PI=FALSE) + 
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var01 Holt Winters Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals 
##  are not distinguishable from white noise.
Box.test(res_s4v1, lag=10, fitdf=0); Box.test(res_s4v1,lag=10, fitdf=0, type="Lj")

################################ V02  ##############################
mape <- list()
rmse <- list()

# frequencies higher than 24 is "too high" for forecast(ets()) call
for (i in 2:24){
  model1 <- hw(train_datasets2[[i]], 
               lambda = NULL, 
               damped=TRUE,
               seasonal='additive',
               level = c(85, 90),
               h=length(test_datasets2[[i]]))
  rmse <- c(rmse, list(accuracy(model1,test_datasets2[[i]])[4]))
  mape <- c(mape, list(accuracy(model1,test_datasets2[[i]])[10]))
}

optimum_freq <- which.min(mape)
best <- rbind(best, do.call(rbind, Map(data.frame, var='Var02', algorithm = 'hw',
                                       h=optimum_freq, 
                                       rmse=min(unlist(rmse)), mape=min(unlist(mape)))))

df <- do.call(rbind, Map(data.frame, algorithm = 'hw',freq=seq(2,length(rmse)+1), 
                         rmse=rmse, mape=mape))
ggplot() + geom_line(data = df, aes(x=freq,y = rmse), color = "blue")
ggplot() + geom_line(data = df, aes(x=freq,y = mape), color = "red") 

s4v2_ts_NoNA <- na.remove(s4v2_ts)
full_data <- ts(s4v2_ts_NoNA, frequency = optimum_freq, start = 1)
train_data <- train_datasets2[[optimum_freq]]
test_data  <- test_datasets2[[optimum_freq]]
test_model <- hw(train_data, 
                 lambda = NULL, 
                 damped=TRUE,
                 seasonal='additive',
                 level = c(85, 90),
                 h=length(test_data))
test_fit <- test_model %>% 
  forecast(h=length(test_data), lambda=NULL, level = c(85, 90))

full_model <- hw(full_data, 
                 lambda = NULL,
                 damped=TRUE,
                 seasonal='additive',
                 level = c(85, 90),
                 h=prediction_length)
# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  The residuals do not have a normal distribution, although the mean is approximately 0.
# The time plot of the residuals suggests a constant variance over time.
checkresiduals(full_model)
full_fit <- full_model %>% forecast(h=prediction_length, level = c(85, 90))

# The confidence interval is significantly larger for Variable02, which makes sense given 
#  the high variability in the original data. 
autoplot(full_fit) + 
  autolayer(test_fit, series = 'Holt_Test (Green)') +
  autolayer(test_data, series = 'Actual (Black)') +
  autolayer(naive(full_data, h = prediction_length), 
            series="Naive Forecast (Blue)", PI=FALSE) +
  autolayer(naive(train_data, h = length(test_data)), 
            series="Naive_Test (Red)", PI=FALSE) + 
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var02 Holt Winters Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals 
##  are not distinguishable from white noise.
Box.test(res_s4v2, lag=10, fitdf=0); Box.test(res_s4v2,lag=10, fitdf=0, type="Lj")


####################################################################
############################### ARIMA #############################
####################################################################

################################ V01  ##############################

# We saw earlier that there were no signs of heterskedasticity, while noting that the time series
#  were clearly not stationary since it wandered up and down for long periods. 
# Hence, We first check for stationarity of differenced time series. 
# As shown below, the series clearly appear to be stationary after the first differencing. 

# ACF is sinusoidal, suggesting a ARIMA(p,1,0). 
# PACF above shows a significant spike at lag 17, but none thereafter, suggesting ARIMA(17,1,0)
s4v1_ts_NoNA %>% diff() %>% ggtsdisplay(main="")

# frequencies higher than 24 is "too high" for forecast(ets()) call
for (i in 2:num_cases){
  model1 <- Arima(s4v1_ts_NoNA, 
               order = c(17,1,0),
               lambda = NULL,
               include.drift = TRUE)
  rmse <- c(rmse, list(accuracy(model1)[2]))
  mape <- c(mape, list(accuracy(model1)[5]))
}

optimum_freq <- which.min(mape)

full_data <- ts(s4v2_ts_NoNA, frequency = optimum_freq, start = 1)
train_data <- train_datasets[[optimum_freq]]
test_data  <- test_datasets[[optimum_freq]]

test_model <- Arima(test_data, 
                    order = c(17,1,0),
                    lambda = NULL,
                    include.drift = TRUE)

test_fit <- test_model %>% 
  forecast(h=length(test_data), lambda=NULL, level = c(85, 90))

# ARIMA appears to have the best accuracy metrics.
accuracy(test_model)

best <- rbind(best, do.call(rbind, Map(data.frame, var='Var01', algorithm = 'arima',
                                       h=optimum_freq, 
                                       rmse=accuracy(test_model)[2], 
                                       mape=accuracy(test_model)[5])))
full_model <- Arima(full_data, 
                       order = c(17,1,0),
                       lambda = NULL,
                       include.drift = TRUE)
# As far as the essential properties of the forecast, from the ACF chart, 
#  the forecast from the baseline model appears to account for most of the information and for the most part uncorrelated.
#  From the histogram of residuals, the residuals have a normal distribution with an approximate mean of 0 and hence appear unbiased.
# As for the other properties, the time plot of the residuals at the top suggests a slight increase in variance over time.
checkresiduals(full_model)
full_fit <- full_model %>% forecast(h=prediction_length, level = c(85, 90))

# The forecast appears to be a constant in the prediction window. 
autoplot(full_fit) + 
  autolayer(test_fit, series = 'Holt_Test (Green)') +
  autolayer(test_data, series = 'Actual (Black)') +
  autolayer(naive(full_data, h = prediction_length), 
            series="Naive Forecast (Blue)", PI=FALSE) +
  autolayer(naive(train_data, h = length(test_data)), 
            series="Naive_Test (Red)", PI=FALSE) + 
  scale_color_manual(values=c('black', 'darkgreen', 'dodgerblue4', 'darkred')) + 
  ggtitle('S04 Var01 Holt Winters Plot') + 
  theme(plot.title = element_text(hjust = 0.5))


# Tests of autocorrelation using Box-Pierce and Box-Ljung both reveal that residuals 
##  are not distinguishable from white noise.
Box.test(res_s4v1, lag=10, fitdf=0); Box.test(res_s4v1,lag=10, fitdf=0, type="Lj")

