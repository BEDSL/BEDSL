# Loading and installing relevent libraries
if(!require("readr")) {install.packages("readr"); library("readr")}
if(!require("readxl")) {install.packages("readxl"); library("readxl")}
if(!require("forecast")) {install.packages("forecast"); library("forecast")}
if(!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")}
if(!require("TSstudio")) {install.packages("TSstudio"); library("TSstudio")}
if(!require("lmtest")) {install.packages("lmtest"); library("lmtest")}
if(!require("Metrics")) {install.packages("Metrics"); library("Metrics")}
if(!require("uroot")) {install.packages("uroot"); library("uroot")}
if(!require("urca")) {install.packages("urca"); library("urca")}
if(!require("aTSA")) {install.packages("aTSA"); library("aTSA")}
if(!require("portes")) {install.packages("portes"); library("portes")}
if(!require("FinTS")) {install.packages("FinTS"); library("FinTS")}
if(!require("TSA")) {install.packages("TSA"); library("TSA")}
if(!require("rugarch")) {install.packages("rugarch"); library("rugarch")}
if(!require("fracdiff")) {install.packages("fracdiff"); library("fracdiff")}
if(!require("LongMemoryTS")) {install.packages("LongMemoryTS"); library("LongMemoryTS")}
if(!require("arfima")) {install.packages("arfima"); library("arfima")}
if(!require("tseries")) {install.packages("tseries"); library("tseries")}
if(!require("pracma")) {install.packages("pracma"); library("pracma")}
if(!require("EnvStats")) {install.packages("EnvStats"); library("EnvStats")}
if(!require("seasonal")) {install.packages("seasonal"); library("seasonal")}
if(!require("ArfimaMLM")) {install.packages("ArfimaMLM"); library("ArfimaMLM")} #!
if(!require("fractal")) {install.packages("fractal"); library("fractal")}       #!


#--------------Install deprecated fractal-----------------------------------------
#install.packages("C:/Users/RUXI/Downloads/ifultools_2.0-23.tar.gz", repos = NULL, type="source") #https://cran.r-project.org/src/contrib/Archive/ifultools/
#install.packages("C:/Users/RUXI/Downloads/sapa_2.0-3.tar.gz", repos = NULL, type="source")       #https://cran.r-project.org/src/contrib/Archive/sapa/
#install.packages("C:/Users/RUXI/Downloads/wmtsa_2.0-3.tar.gz", repos = NULL, type="source")      #https://cran.r-project.org/src/contrib/Archive/wmtsa/
#install.packages("C:/Users/RUXI/Downloads/fractal_2.0-4.tar.gz", repos = NULL, type="source")    #https://cran.r-project.org/src/contrib/Archive/fractal/
library(fractal) #(pt hurstSpec())

#--------------Install deprecated ArfimaMLM---------------------------------------
library('devtools')
#install_github("jyypma/nloptr")   #de pe cran nu mergea: error #include "nlopt.h"

#install.packages("C:/Users/RUXI/Downloads/minqa_1.2.3.tar.gz", repos = NULL, type="source")      #https://cran.r-project.org/src/contrib/Archive/minqa/
#install.packages("C:/Users/RUXI/Downloads/lme4_1.1-28.tar.gz", repos = NULL, type="source")       #https://cran.r-project.org/src/contrib/Archive/lme4/
#install.packages("C:/Users/RUXI/Downloads/ArfimaMLM_1.3.tar.gz", repos = NULL, type="source")   #https://cran.r-project.org/src/contrib/Archive/ArfimaMLM/

----------------------------------------------------------------------------------

# import the dataset
z <- read_csv("C:/Users/RUXI/Desktop/unemployment_rate.csv")
head(z)

# creating a ts object
y <- ts(z, start=2000, frequency = 12)


#training 2000M01-2018M06 test 2018M07 2022M12
# Spliting the data intro training and test sets
training <- window(y, start=2000, end=c(2018,06))
test <- tail(y, 54)


# Time series plots
autoplot(y) +
  ggtitle("The evolution of monthly Romanian unemployment rate") +
  xlab("Year") +
  ylab("%")

ggsubseriesplot(y) +
  ylab("%") +
  ggtitle("Seasonal subseries plot: monthly unemployment rate")

y  %>% ggtsdisplay()

# Seasonally adjustment 
y_seas_adj <- y %>% seas(x11="")
y_seas_adj_data<- as.data.frame(y_seas_adj$data)
y_seas_adj_data <- ts(y_seas_adj_data$seasonaladj,start=2000, frequency = 12)
y_seas_adj_data  %>% ggtsdisplay()

hurstexp(y_seas_adj_data)#0.8
fdSperio(y) #0.9  #din hurst si acf rezulta long memory


# Testarea radacinii unitare
#none
rw_none <- ur.df(y, type='none', selectlags = c("AIC"))
summary(rw_none) # nonstationary ts  
#drift
rw_t <- ur.df(y, type='drift', selectlags = c("AIC"))
summary(rw_t) # nonstationary ts
#trend
rw_ct <- ur.df(y, type='trend', selectlags = c("AIC"))
summary(rw_ct) # nonstationary ts

# KPSS
y %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts

# Philips-Perron
PP.test(y) # p = 0.03832 < 0.05 => stationary ts


#Training
training_seas_adj <- training %>% seas(x11="") 
training_seas_adj_data<- as.data.frame(training_seas_adj$data)
training_seasonal_adj_dt <- ts(training_seas_adj_data$seasonaladj,start=2000, frequency = 12)
training_seasonal_adj_dt %>% ggtsdisplay()  # nu e stationar prin x11


training4 <- training %>% diff(lag=12)    
ggtsdisplay(training4)

adf.test(training4)  #pvalue=0.01 < 0.05 => accept H1: stationary
kpss.test(training4) #pvalue = 0.11 > 0.05 => accept H0: stationary

hurstexp(training4)  #hurst exp din articol



# parametric estimators 

#A temporary d value is obtained by fitting ARFIMA (0, d, 0) model.
#The estimated d is 0.494.
fit <- arfima(training4, order = c(0, 0, 0),
              numeach = c(1, 1), back=FALSE, dmean = FALSE)
summary(fit)   #0.494

training4_fractal<-fdiff(training4, d=0.494)
ggtsdisplay(training4_fractal)

# According to the plot of ACF and PACF, the model candidates are ARFIMA (2, d, 0),
# ARFIMA (3, d, 0), ARFIMA ([9], d, 0), ARFIMA ([3,10], d, 0), ARFIMA (0, d, 2), ARFIMA
# (0, d, 3), ARFIMA (1, d, 1), and ARFIMA (2, d, 2).

#Modelare
fit_test <- arfima(training4, order = c(2, 0, 0)) 
summary(fit_test)
fit_test2 <- arfima(training4, order = c(3, 0, 0))
summary(fit_test2)
fit_test3 <- arfima(training4, order = c(9, 0, 0))
summary(fit_test3)
fit_test4 <- arfima(training4, order = c(10, 0, 0))
summary(fit_test4)
fit_test5 <- arfima(training4, order = c(0, 0, 2))
summary(fit_test5)
fit_test6 <- arfima(training4, order = c(0, 0, 3))
summary(fit_test6)
fit_test7 <- arfima(training4, order = c(0, 0, 9))
summary(fit_test7)
fit_test8 <- arfima(training4, order = c(0, 0, 10))
summary(fit_test8)
fit_test9 <- arfima(training4, order = c(1, 0, 1))
summary(fit_test9)
fit_test10 <- arfima(training4, order = c(2, 0, 1))
summary(fit_test10)
fit_test11 <- arfima(training4, order = c(2, 0, 2))
summary(fit_test11)

arfima_res_aic <- rep(0,11)
arfima_res_aic[1] <-AIC(fit_test)
arfima_res_aic[2] <- AIC(fit_test2)
arfima_res_aic[3] <- AIC(fit_test3) 
arfima_res_aic[4] <- AIC(fit_test4)
arfima_res_aic[5] <- AIC(fit_test5)
arfima_res_aic[6] <- AIC(fit_test6) 
arfima_res_aic[7] <- AIC(fit_test7) 
arfima_res_aic[8] <- AIC(fit_test8) 
arfima_res_aic[9] <- AIC(fit_test9)
arfima_res_aic[10] <- AIC(fit_test10)
arfima_res_aic[11] <- AIC(fit_test11)
which(arfima_res_aic == min(arfima_res_aic)) #=> 4  Arfima([3,10], d, 0)


jarque.bera.test(fit_test4[["modes"]][[1]][["residuals"]])   #p-value = 0.000223 < 0.05 reziduuri nenormal distribuite 
LjungBox(fit_test4[["modes"]][[1]][["residuals"]],lags = 10) #p-value = 0.8786454 >0.1 nu prezinta autocorelare la lag10
summary(fit_test4) #nu toti semnificativi, dar destui
ArchTest(fit_test4[["modes"]][[1]][["residuals"]],lags = 10) #p-value = 0.09 > 0.05 no arch effects la lag 10

prediction_arfima_parametric <-predict(fit_test4, n.ahead = 54)
prediction_arfima_parametric
plot(prediction_arfima_parametric)


# semiparametric estimators 
#-------------------------------------------------------------------------------------------------
options(scipen = 999)

fracdif_sperio<-fd(training4,dval="Sperio")
fracdif_sperio #0.2632827
fracdif_sperio <- fracdif_sperio$series
ggtsdisplay(fracdif_sperio)

fit_sperio <- auto.arima(fracdif_sperio)
summary(fit_sperio)            #ARIMA(3,0,2) with non-zero mean 
coeftest(fit_sperio)           #intercept nesemnificativ
LjungBox(fit_sperio$residuals) #p-value pt lag 5 si 10 > 0.1 nu prezinta autocorelatie
ArchTest(fit_sperio$residuals) #p-value = 0.09905 > 0.05 no arch effects


fracdif_gph<-fd(training4,dval="GPH")
fracdif_gph #0.23832
fracdif_gph <- fracdif_gph$series
ggtsdisplay(fracdif_gph)

fit_gph <- auto.arima(fracdif_gph)
summary(fit_gph)               #ARIMA(3,0,2) with non-zero mean 
coeftest(fit_gph)              #toti semnificativi
LjungBox(fit_sperio$residuals) #p-value pt lag 5 si 10 > 0.1 nu prezinta autocorelatie
ArchTest(fit_sperio$residuals) #p-value = 0.09905 > 0.05 no arch effects


fracdif_hurst<-fd(training4,dval="Hurst")
fracdif_hurst #d=0.236
fracdif_hurst <- fracdif_hurst$series
ggtsdisplay(fracdif_hurst)
fracdif_hurst_ts <- ts(fracdif_hurst, start=2000, frequency = 12)

#Next, we fractionally differentiate the series considering the memory parameter
#d = 0.245, which is the average value of obtained semi-parametric estimators
y_fdiff_semiparametric<-fdiff(training4, d=0.245)
ggtsdisplay(y_fdiff_semiparametric)


#Models
# The candidate models are ARFIMA (2, d, 0), ARFIMA (3, d, 0),ARFIMA (4, d, 0), ARFIMA (9, d, 1)
# ARFIMA ([10], d, 0), ARFIMA (1, d, 1), ARFIMA (2,d, 1), and ARFIMA (2, d, 2)

fit_semiparametric1 <- Arima(y_fdiff_semiparametric, order=c(2,0,0))
summary(fit_semiparametric1)  #AIC=212.93   AICc=213.12   BIC=226.32
coeftest(fit_semiparametric1) #intercept nesemnificativ

fit_semiparametric2 <- Arima(y_fdiff_semiparametric, order=c(3,0,0))
summary(fit_semiparametric2)  #AIC=211.29   AICc=211.58   BIC=228.02
coeftest(fit_semiparametric2) #intercept nesemnificativ

fit_semiparametric3 <- Arima(y_fdiff_semiparametric, order=c(4,0,0)) 
summary(fit_semiparametric3)  #AIC=207.8   AICc=208.21   BIC=227.88
coeftest(fit_semiparametric3) #intercept nesemnificativ

fit_semiparametric4 <- Arima(y_fdiff_semiparametric, order=c(9,0,1))
summary(fit_semiparametric4)  #AIC=202.99   AICc=204.58   BIC=243.16
coeftest(fit_semiparametric4) #ar1 ar2 ar4 ar9  ma1 semnificativi, restu nu

fit_semiparametric5 <- Arima(y_fdiff_semiparametric, order=c(10,0,0))
summary(fit_semiparametric5)  #AIC=200.33   AICc=201.92   BIC=240.5
coeftest(fit_semiparametric5) #ar1 ar2 ar3 ar5 ar9 ar10 semnificativi, restu nu

fit_semiparametric6 <- Arima(y_fdiff_semiparametric, order=c(1,0,1))
summary(fit_semiparametric6)  #AIC=217.13   AICc=217.33   BIC=230.52
coeftest(fit_semiparametric6) #intercept nesemnificativ

fit_semiparametric7 <- Arima(y_fdiff_semiparametric, order=c(2,0,1))
summary(fit_semiparametric7)  #AIC=213.18   AICc=213.48   BIC=229.92
coeftest(fit_semiparametric7) #ma1 si intercept nesemnificativi

fit_semiparametric8 <- Arima(y_fdiff_semiparametric, order=c(2,0,2))
summary(fit_semiparametric8)  #AIC=205.1   AICc=205.51   BIC=225.18
coeftest(fit_semiparametric8) #intercept nesemnificativ



arfima_res_aic <- rep(0,8)
arfima_res_aic[1] <- AIC(fit_semiparametric1)
arfima_res_aic[2] <- AIC(fit_semiparametric2)
arfima_res_aic[3] <- AIC(fit_semiparametric3) 
arfima_res_aic[4] <- AIC(fit_semiparametric4)
arfima_res_aic[5] <- AIC(fit_semiparametric5)
arfima_res_aic[6] <- AIC(fit_semiparametric6) 
arfima_res_aic[7] <- AIC(fit_semiparametric7) 
arfima_res_aic[8] <- AIC(fit_semiparametric8) 
which(arfima_res_aic == min(arfima_res_aic)) #=>  5  ARFIMA ([10], d, 0)

arfima_res_bic <- rep(0,8)
arfima_res_bic[1] <-fit_semiparametric1$bic
arfima_res_bic[2] <- fit_semiparametric2$bic
arfima_res_bic[3] <- fit_semiparametric3$bic
arfima_res_bic[4] <- fit_semiparametric4$bic
arfima_res_bic[5] <- fit_semiparametric5$bic
arfima_res_bic[6] <- fit_semiparametric6$bic 
arfima_res_bic[7] <- fit_semiparametric7$bic 
arfima_res_bic[8] <- fit_semiparametric8$bic 
which(arfima_res_bic == min(arfima_res_bic)) #=>  8 


# Tests on residuals
ggtsdisplay(fit_semiparametric5$residuals)  # nu avem autocorelare in reziduuri conform ACF
checkresiduals(fit_semiparametric5)         #Ljung-Box test p-value = 0.000000005472 < 0.1 prezinta autocorelare
ArchTest(fit_semiparametric5$residuals)     #p-value = 0.0184  < 0.05  arch effects

Box.test(fit_semiparametric5$residuals, lag=1,type="Lj") #p > 0.1 nu avem autocorelare in reziduri
Box.test(fit_semiparametric5$residuals, lag=2,type="Lj")
Box.test(fit_semiparametric5$residuals, lag=3,type="Lj")
Box.test(fit_semiparametric5$residuals, lag=4,type="Lj")
Box.test(fit_semiparametric5$residuals, lag=5,type="Lj")
Box.test(fit_semiparametric5$residuals, lag=12,type="Lj") # avem



#Forecast
forecast_semi<-fit_semiparametric5 %>% forecast::forecast(h=54)
summary(forecast_semi)
autoplot(forecast_semi)

forecast_semip <- as.data.frame(forecast_semi[["mean"]])
forecast_semip <- ts(forecast_semip, start=c(2018, 7), frequency=12)



#Analiza acuratetii
# RMSE 
rmse(training,fit_test4[["modes"]][[1]][["fitted"]])       #6.82655
rmse(test,prediction_arfima_parametric[[1]][["Forecast"]]) #5.057542
summary(fit_semiparametric5) #0.3668172
rmse(test,forecast_semip) #4.688944

# MAPE
library(MLmetrics)
MAPE(training,fit_test4[["modes"]][[1]][["fitted"]])       #56.4509
MAPE(test,prediction_arfima_parametric[[1]][["Forecast"]]) #14.90027 ?
summary(fit_semiparametric5) #163.9397
MAPE(test,forecast_semip)    #177.4115


# MAE
mae(training,fit_test4[["modes"]][[1]][["fitted"]])       #6.767884
mae(test,prediction_arfima_parametric[[1]][["Forecast"]]) #5.008167
summary(fit_semiparametric5) #0.2781074
mae(test, forecast_semip)    #4.620584


# MASE
mase(training,fit_test4[["modes"]][[1]][["fitted"]])      #24.18553
mase(test,prediction_arfima_parametric[[1]][["Forecast"]])#26.28048
summary(fit_semiparametric5) #0.7998056
mase(test,forecast_semip)    #15.2664













