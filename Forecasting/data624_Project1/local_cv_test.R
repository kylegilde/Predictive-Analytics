#try cv instead
#y = s03_v03_arima_ts
#forecastfunction = forecast_fun_no_reg
#forecastfunction = forecast_fun_reg
#xreg = NULL
#xreg = NULL
#h=1
#initial=1600
#window=NULL

#forecasting function for tsCV
#forecast_fun_no_reg <- function(y, h = h, xreg=NULL, t_window = NULL, s_window = NULL, ...) {
#  x <- stlf(y, 
#            method='arima', 
#            t.window = t_window, 
#            s.window = s_window, 
#            robust = TRUE, 
#            h = h)
#  return(x)
#}

#stlf test
#tst <- tsCV_jg(
#  y = msts(s03_v03_full_ts, seasonal.periods = c(6,6)),
#        forecastfunction = forecast_fun_no_reg,
#        h=2,
#        initial=1619,
#        tw = c(1, 1),
#        sw  = c(1, 1)
#)
        
#arima test
#forecasting function for tsCV
forecast_fun_no_reg <- function(y, h = h, xreg=NULL, k1= NULL, k2=2) {
  tmp_model <- auto.arima(y, 
                  lambda = 0,
                  xreg=fourier(y, 
                               K = c(k1, k2)))
  tmp_fit <- tmp_model %>% 
    forecast(xreg=fourier(y, 
                          K = c(k1, k2)), 
             h=h)
  #Need to slice the output because using xreg returns prediction of xreg no matter what
  return(tmp_fit)
}
#tst <- tsCV_jg(y = msts(s03_v03_full_ts, seasonal.periods = c(6,6)),
#        forecastfunction = forecast_fun_no_reg,
#        h=2,
#        initial=1600,
#        k1=2,
#        k2=3
#)
window = NULL
xreg = NULL

iter = 0

tsCV_jg <- function(y, forecastfunction, h=1, window=NULL, xreg=NULL, initial=0, extra_args=NULL, ...) {
  #Edited and debugged, requires custom forecastfunctions
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  pe <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if(initial >= n) stop("initial period too long")
  tsp(e) <- tsp(y)
  if (!is.null(xreg)) {
    # Make xreg a ts object to allow easy subsetting later
    xreg <- ts(as.matrix(xreg))
    tsp(xreg) <- tsp(y)
  }
  if (is.null(window)) {
    indx <- seq(1+initial, n - 1L)
  } else {
    indx <- seq(window+initial, n - 1L, by = 1L)
  }
  for (i in indx) {
    y_subset <- subset(
      y,
      start = ifelse(is.null(window), 1L,
                     ifelse(i - window >= 0L, i - window + 1L, stop("small window"))
      ),
      end = i
    )
    if (is.null(xreg)) {
      fc <- try(suppressWarnings(
        forecastfunction(y_subset, h = h, ...)
      ), silent = TRUE)
    } else {
      xreg_subset <- as.matrix(subset(
        xreg,
        start = ifelse(is.null(window), 1L,
                       ifelse(i - window >= 0L, i - window + 1L, stop("small window")))
      ))
      fc <- try(suppressWarnings(
        forecastfunction(y_subset, h = h, xreg = xreg_subset, ...)
      ), silent = TRUE)
    }
    if (!is.element("try-error", class(fc))) {
      #[1:h] added for when xreg is used because it returns length(xreg)
      e[i, ] <- y[i + (1:h)] - fc$mean[1:h]
      if(!is.null(extra_args)){
        pe[i, ] <- 100 * (y[i + (1:h)] - fc$mean[1:h]) / y[i + (1:h)] 
      }
      if (i %% 100 == 0) cat(paste(i,"\n", sep = ' '))
      cat(".")
    }
  }
  if (h == 1) {
    return(list(e = e[, 1L], pe = pe[, 1L]))
  } else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    colnames(pe) <- paste("h=", 1:h, sep = "")
    return(list(e = e, pe = pe))
  }
}

