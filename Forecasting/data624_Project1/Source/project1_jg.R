#Base setup for time series data

#library modules
library(dplyr)
library(tidyr)
library(forecast)
library(xts)

#Load data
raw_excel_df <- readxl::read_excel('Reference/Set for Class.xls', col_names = TRUE)
excel_df <- raw_excel_df %>% 
  #Remove the rows intended to be filled by prediction
  filter(SeriesInd<43022) #%>% 
  #convert the Series Index to dates
  #mutate(SeriesInd = as.Date(SeriesInd, 
  #                           origin = '1901-01-01'
  #                           )) #%>% 
  #mutate(Wd = weekdays(SeriesInd))

#Format data
excel_no_outlier_df <- excel_df %>%
  #remove outlier in S02
  mutate(Var01 = ifelse(group=='S02' & Var01 > 25, NA, Var01)) %>% 
  mutate(Var03 = ifelse(group=='S02' & Var03 > 25, NA, Var03)) %>% 
  mutate(Var05 = ifelse(group=='S02' & Var05 > 25, NA, Var05)) %>% 
  mutate(Var07 = ifelse(group=='S02' & Var07 > 25, NA, Var07)) %>% 
  #remove outlier in S06
  mutate(Var01 = ifelse(group=='S06' & Var01 > 150, NA, Var01)) %>% 
  mutate(Var03 = ifelse(group=='S06' & Var03 > 150, NA, Var03)) %>% 
  mutate(Var05 = ifelse(group=='S06' & Var05 > 150, NA, Var05)) %>% 
  mutate(Var07 = ifelse(group=='S06' & Var07 > 150, NA, Var07))

#split into separate dfs for study
base_df <- excel_no_outlier_df %>% 
  filter(group=='S03') %>% 
  arrange(SeriesInd)

s03_v01_df <- base_df %>% 
  select(SeriesInd, Var01)

s03_v02_df <- base_df %>% 
  select(SeriesInd, Var02)

s03_v03_df <- base_df %>% 
  select(SeriesInd, Var03)

s03_v05_df <- base_df %>% 
  select(SeriesInd, Var05)

s03_v07_df <- base_df %>% 
  select(SeriesInd, Var07)

#Make dataframe of consecutive indexes and left join to current dfs so nulls show
#s03_v01_full_df <- data.frame(SeriesInd = seq(min(s03_v01_df$SeriesInd), max(s03_v01_df$SeriesInd), 1)) %>% 
#  left_join(s03_v01_df, by = c('SeriesInd'='SeriesInd'))

#s03_v02_full_df <- data.frame(SeriesInd = seq(min(s03_v02_df$SeriesInd), max(s03_v02_df$SeriesInd), 1)) %>% 
#  left_join(s03_v02_df, by = c('SeriesInd'='SeriesInd'))

#s03_v03_full_df <- data.frame(SeriesInd = seq(min(s03_v03_df$SeriesInd), max(s03_v03_df$SeriesInd), 1)) %>% 
#  left_join(s03_v03_df, by = c('SeriesInd'='SeriesInd'))

#s03_v05_full_df <- data.frame(SeriesInd = seq(min(s03_v05_df$SeriesInd), max(s03_v05_df$SeriesInd), 1)) %>% 
#  left_join(s03_v05_df, by = c('SeriesInd'='SeriesInd'))

#s03_v07_full_df <- data.frame(SeriesInd = seq(min(s03_v07_df$SeriesInd), max(s03_v07_df$SeriesInd), 1)) %>% 
#  left_join(s03_v07_df, by = c('SeriesInd'='SeriesInd'))

#s03_v01_xts <- xts(s03_v01_df$Var01, order.by = s03_v01_df$SeriesInd)
#s03_v02_xts <- xts(s03_v02_df$Var02, order.by = s03_v01_df$SeriesInd)
#s03_v03_xts <- xts(s03_v03_df$Var03, order.by = s03_v01_df$SeriesInd)
#s03_v05_xts <- xts(s03_v05_df$Var05, order.by = s03_v01_df$SeriesInd)
#s03_v07_xts <- xts(s03_v07_df$Var07, order.by = s03_v01_df$SeriesInd)

#convert to time series
s03_v01_ts <- s03_v01_df %>% 
  select(Var01) %>% 
  ts(frequency = 1)
s03_v02_ts <- s03_v02_df %>% 
  select(Var02) %>% 
  ts(frequency = 1)
s03_v03_ts <- s03_v03_df %>% 
  select(Var03) %>% 
  ts(frequency = 1)
s03_v05_ts <- s03_v05_df %>% 
  select(Var05) %>% 
  ts(frequency = 1)
s03_v07_ts <- s03_v07_df %>% 
  select(Var07) %>% 
  ts(frequency = 1)

#Impute
s03_v01_full_ts <- na.interp(s03_v01_ts)
s03_v02_full_ts <- na.interp(s03_v02_ts)
s03_v03_full_ts <- na.interp(s03_v03_ts)
s03_v05_full_ts <- na.interp(s03_v05_ts)
s03_v07_full_ts <- na.interp(s03_v07_ts)

#test/train split
test_length <- 600
