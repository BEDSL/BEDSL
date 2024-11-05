# Loading and installing relevent libraries
if(!require("readr")) {install.packages("readr"); library("readr")}
if(!require("readxl")) {install.packages("readxl"); library("readxl")}
if(!require("changepoint")) {install.packages("changepoint"); library("changepoint")}
if(!require("stats")) {install.packages("stats"); library("stats")}
if(!require("tibble")) {install.packages("tibble"); library("tibble")}
if(!require("strucchange")) {install.packages("strucchange"); library("strucchange")}
if(!require("xts")) {install.packages("xts"); library("xts")}
if(!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if(!require("skimr")) {install.packages("skimr"); library("skimr")}
if(!require("tseries")) {install.packages("tseries"); library("tseries")}
if(!require("FinTS")) {install.packages("FinTS"); library("FinTS")}
if(!require("rmgarch")) {install.packages("rmgarch"); library("rmgarch")}
if(!require("tidyquant")) {install.packages("tidyquant"); library("tidyquant")}
if(!require("data.table")) {install.packages("data.table"); library("data.table")}
if(!require("gt")) {install.packages("gt"); library("gt")}
if(!require("MLmetrics")) {install.packages("MLmetrics"); library("MLmetrics")}

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


#Reading data
x <- read_csv("D:/Ruxi/Facultate/Licenta/Articole pt licenta/brent.csv",col_names = TRUE)

# Creating logged oil price time series object called y for simplifying the code
z <- ts(x$Deflated, start=c(1988,1), frequency = 12)
y <-log(z)


#Graphs-----------------------------------------------------------------------
# Basic time series plot
autoplot(y) +
  ggtitle("The evolution of log real monthly crude oil price") +
  xlab("Year") +
  theme_bw() +
  ylab("%")

#Graphs of original and logged price series, separated
cbind("Monthly real Brent price" = z,
      "Logged monthly real Brent price"=y) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("") + theme_bw()

#Graph of original series: nominal vs real price
nominalBrent <- ts(x[,2], start=c(1988,1), frequency = 12)

autoplot(z, series="**Monthly real Brent price**", colour = 'black') +
  autolayer(nominalBrent, series="Monthly nominal Brent price") +
  xlab("Year") +
  ylab("Dollar") +
  ggtitle("Monthly Brent price") +
  guides(colour=guide_legend(title="Legend")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::extended_breaks(20))

#Descriptive Statistics---------------------------------------------------------
#Summary,Skewness and Kurtosis for original series
ggplot(y, aes(x=z)) + 
  geom_histogram(bins =25)+
  labs()+
  xlab("Dolari")+ylab("Frecventa") + theme_bw()
summary(z)
sd(z)
skewness(z[1:411])
kurtosis(z[1:411])
jarque.bera.test(z)  #p-value = 2.798e-14 < 0.05, series not normally distributed

#Summary,Skewness and Kurtosis for logged series
ggplot(y, aes(x=y)) + 
  geom_histogram(bins = 25)+
  labs(title = "Distribution of monthly logged real Brent price")+
  xlab("")+ylab("") + theme_bw()
summary(y)
sd(y)
skewness(y[1:411])
kurtosis(y[1:411])

jarque.bera.test(y)  #p-value = 2.798e-14 < 0.05, series not normally distributed

#Generating summary table
Series <- c("Sample","Observations","Mean","Median","Maximum","Minimum",
            "Std.Dev.","Skewness","Kurtosis","Jarque-Bera","Probability")
`Original Series` <- c("1988M01 - 2022M03", 411, 54.64, 44.21, 143.33, 14.20, 
                       29.86, 0.95, -0.10, 62.79, "2.309e-14")
`Logged Series` <- c("1988M01 - 2022M03", 411, 3.86, 3.78, 4.96, 2.65, 
                     0.52, 0.21, -0.89, 17.214,"0.0001828")
summary_statistics <- as.data.frame(cbind(Series,`Original Series`, `Logged Series`))
summary_statistics %>% gt() %>% tab_header(
  title = md("The **monthly real Brent price** statistics for the period 1988M1-2022M03"))


#Change points----------------------------------------------------------------------------
#Change points in mean
#binary segmentation method, using BIC and Q points
m_binseg <- cpt.mean(z, penalty = "BIC", method = "BinSeg", Q=13) #chose 13 due to better looking graphic

cpts(m_binseg) # function that returns change points where the series' mean starts changing
#59 142 201 233 242 249 257 275 319 323 358 386 401

# To make the graphic of changes in mean, xens takes the value of the change points found
# y and yend will be calculated as the mean of segments between change points
#Mean of segments calculated with mean()
mean(z[0:59])
mean(z[59:142])
mean(z[142:201])
mean(z[201:233])
mean(z[233:242])
mean(z[242:249])
mean(z[249:257])
mean(z[257:275])
mean(z[275:319])
mean(z[319:323])
mean(z[323:358])
mean(z[358:386])
mean(z[386:401])
mean(z[401:411])

#Graphic of change points in mean
yts <- ts(x$Deflated) 
forecast::autoplot(yts) +
  geom_segment(aes(x=0,xend=59,y=35,yend=35),colour='red')+
  geom_segment(aes(x=59,xend=142,y=26.06,yend=26.06),colour='red') +
  geom_segment(aes(x=142,xend=201,y=36.94,yend=36.94),colour='red') + 
  geom_segment(aes(x=201,xend=233,y=69.23,yend=69.23),colour='red') +
  geom_segment(aes(x=233,xend=242,y=92.34,yend=92.34),colour='red')+
  
  geom_segment(aes(x=242,xend=249,y=123.78,yend=123.78),colour='red')+
  geom_segment(aes(x=249,xend=257,y=61.72,yend=61.72),colour='red') +
  geom_segment(aes(x=257,xend=275,y=81.43,yend=81.43),colour='red') + 
  geom_segment(aes(x=275,xend=319,y=112.64,yend=112.64),colour='red') +
  geom_segment(aes(x=319,xend=323,y=94.23,yend=94.23),colour='red')+
  
  geom_segment(aes(x=323,xend=358,y=49.82,yend=49.82),colour='red')+
  geom_segment(aes(x=358,xend=386,y=62.33,yend=62.33),colour='red')+
  geom_segment(aes(x=386,xend=401,y=42.64,yend=42.64),colour='red')+
  geom_segment(aes(x=401,xend=411,y=69.96,yend=69.96),colour='red')+
  ggtitle("Change point in mean for Brent price") +
  ylab("Brent Price") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme_bw()

#Change points in variance, using method Pelt
v_pelt <- cpt.var(z, method = "PELT")
cpts(v_pelt) #236 323, these are the points where the series' variance stars changing

#Looking at variance of each segment
sd(z[0:236])  #21.21
sd(z[236:323])#13.02
sd(z[323:411])#16.05

#Graphic of change points in variance
forecast::autoplot(z) +
  geom_vline(xintercept = 2007.8, colour = 'red') + #xintercept takes the date value of the first changepoint
  geom_vline(xintercept = 2014.11, colour = 'red') + 
  ggtitle("Change point in variance for Brent Price") +
  ylab("Real Brent Price") +
  theme(plot.title=element_text(hjust=0.5)) +
  theme_bw()


#Structural breaks----------------------------------------------------------------------------
#Similar to change points, but they refer to changes in the regression's coefficients
# Test Quandt Likelihood Ratio (QLR)

# Step 1 - creating a tibble object that contains the original variable (lag0)
# and the original variable with one lag (lag1)
data_qlr <- tibble(ylag0 = y,  
                   ylag1 = lag(as.numeric(y))) 

# Step 2 - we apply a false regression where the dependent variable is lag0 and the independent one is lag1
qlr <- Fstats(ylag0 ~ ylag1, data = data_qlr)

# Step 3 - we estimate the point where there is a structural change
breakpoints(qlr) #183 

# Step 4 - we test the semnificance of the point
# if p < 0.1 it means we have a semnificant structural break point
sctest(qlr, type = "supF") #p = 0.02 semnificant for z, p = 0.11 no semnificance for y

# Step 5 - making the graphic
y[183] #3.67
# we identify the point on the graphic
# and approximate the value for drawing the line on the graphic

autoplot(y) +
  #geom_vline(xintercept = 183, colour = 'red') + 
  geom_hline(yintercept = 3.67, colour = 'red')+
  ggtitle("Structural break for Brent price") +
  ylab("Brent") +
  theme_bw()

# Structural breaks - Zivot Andrew
summary(ur.za(y))
za_intercept <- ur.za(y, model =  "intercept")
za_trend <- ur.za(y, model =  "trend")
za_both <- ur.za(y, model =  "both")
summary(za_intercept)
summary(za_trend)
summary(za_both)
plot(za_both) # lines not touched, so no structural breaks


#Stationarity tests-------------------------------------------------------------------------- 
#ADF
adf.test(y)  #p = 0.39 > 0.1 nonstationary ts
summary(ur.df(y, type='none', selectlags = c("AIC"))) # nonstationary ts   |0.12| < |-2.58| / |-1.95| / |-1.62|
summary(ur.df(y, type='drift', selectlags = c("AIC")))# nonstationary 
summary(ur.df(y, type='trend', selectlags = c("AIC")))# nonstationary
# KPSS
y %>% ur.kpss(type = c("mu")) %>% summary()  #Constant: t > critical values => nonstationary ts
y %>% ur.kpss(type = c("tau")) %>% summary() #Trend:    t > critical values => nonstationary ts
# Philips-Perron
PP.test(y)  # p = 0.25 > 0.05 => nonstationary ts
tseries::pp.test(y,type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(y,type = c("Z(t_alpha)"), lshort = TRUE)


#Stationarity tests for first difference
#ADF
summary(ur.df(diff(y), type='none', selectlags = c("AIC"))) # stationary ts #observe overdifferencing 
summary(ur.df(diff(y), type='drift', selectlags = c("AIC")))# stationary 
summary(ur.df(diff(y), type='trend', selectlags = c("AIC")))# stationary
# KPSS
diff(y) %>% ur.kpss(type = c("mu")) %>% summary()  #Constant: t < critical values =>stationary ts
diff(y) %>% ur.kpss(type = c("tau")) %>% summary() #Trend:    t < critical values =>stationary ts
# Philips-Perron
PP.test(diff(y))  # p = 0.01 < 0.05 => stationary ts
tseries::pp.test(diff(y),type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(diff(y),type = c("Z(t_alpha)"), lshort = TRUE)

#Table with stationarity tests
`Unit Root` <- c("ADF level","PP level",  "KPSS level", "ADF first difference","PP first difference","KPSSfirst difference")
`T&C` <- c("-3.265*", "0.255", "0.660", "-13.889***", "0.01***", "0.0403***" )
`C` <- c("-2.397",    "0.267", "3.661", "-13.905***", "0.01***", "0.043 ***" )
`None` <- c("0.1249", "",      "",      "-13.909***", "",        "" )

unit_root <- as.data.frame(cbind(`Unit Root`,`T&C`,`C`,`None`))
unit_root %>% gt() %>% tab_header(
  title = md("**Unit root analysis of the Real Monthly Brent Price**")) %>%
  tab_source_note(
    source_note = "Note: ***, **, * means stationary at 1%, 5% and 10%; T&C represents the most general model with a constant and trend; C is the model with a constant and without trend; None is the most restricted model without a drift and trend"
  )

#Seasonality tests ------------------------------------------------------------------------
# Hegy 
hegy.test(y)  # p = 0.36 > 0.1 for t_1 => to make the series stationary a single seasonal difference would be needed
# Canova Hansen
ch.test(y)    #95%  seasonal unit root, shows seasonality
# OCSB
ocsb.test(y, lag.method = c("fixed", "AIC", "BIC", "AICc"), maxlag = 0) # 21.5935 > 1.8 =>stationary seasonal unit root, so no seasonality

#Subseries graph for checking seasonality
ggsubseriesplot(y) +
  ylab("%") +
  theme_bw() +
  ggtitle("Seasonal subseries plot: log real monthly crude oil price") #no seasonality


#Looking for Long Memory---------------------------------------------------------
ggtsdisplay(y, theme = theme_bw())  #slow decreasing ACF
hurstexp(y)     # 0.5 < H < 1


#1. Methods applied to training and test------------------------------------------

#Separating data into test and training set for modelling
training <- window(y, start=c(1988,1), end=c(2019,1)) 
test <- tail(y, 38)

#---------------------------------------I ARIMA---------------------------------------------------
#inspecting the first difference 
training %>% diff() %>% ggtsdisplay(main="", theme = theme_bw())

#Stationarity tests 
#ADF
adf.test(diff(training))  #p = 0.01 < 0.1 stationary ts
summary(ur.df(diff(training), type='none', selectlags = c("AIC"))) # stationary ts #observe overdifferencing 
summary(ur.df(diff(training), type='drift', selectlags = c("AIC")))# stationary 
summary(ur.df(diff(training), type='trend', selectlags = c("AIC")))# stationary
# KPSS
diff(training) %>% ur.kpss(type = c("mu")) %>% summary()  #Constant: t < critical values =>stationary ts
diff(training) %>% ur.kpss(type = c("tau")) %>% summary() #Trend:    t < critical values =>stationary ts
# Philips-Perron
PP.test(diff(training))  # p = 0.01 < 0.05 => stationary ts
pp.test(diff(training),type = c("Z(alpha)"), lshort = TRUE)
pp.test(diff(training),type = c("Z(t_alpha)"), lshort = TRUE)


#Modelling
#potential models: ARIMA(1,1,0),ARIMA(4,1,0),ARIMA(6,1,0),ARIMA(0,1,1),ARIMA(0,1,5),ARIMA(1,1,1) 

fit1 <- Arima(training, order=c(1,1,0))
coeftest(fit1) #significant
summary(fit1)  #AIC=-771.45   AICc=-771.42   BIC=-763.61

fit2 <- Arima(training, order=c(4,1,0))
coeftest(fit2) #ar2 ar3 not significant
summary(fit2)

fit3 <- Arima(training, order=c(6,1,0))
coeftest(fit3) # ar2 ar3 si ar5 not significant
summary(fit3)  #AIC=-771.5   AICc=-771.19   BIC=-744.06

fit4 <- Arima(training, order=c(0,1,1))
coeftest(fit4) #significant
summary(fit4)  #AIC=-772.29   AICc=-772.26   BIC=-764.45

fit5 <- Arima(training, order=c(0,1,3))
coeftest(fit5) #ma2 ma3 not significant
summary(fit5)  #AIC=-772.29   AICc=-772.26   BIC=-764.45

fit6 <- Arima(training, order=c(1,1,1))
coeftest(fit6) #not significant
summary(fit6)  #AIC=-772.29   AICc=-772.26   BIC=-764.45

fit.arima <- auto.arima(training) #ARIMA(0,1,1)  #checking out what auto.arima()would suggest
coeftest(fit.arima) #ARIMA(0,1,1), significant

#Optimal model is ARIMA(0,1,1)

# Testing residuals 
res_arima1 <- fit4$residuals
# Autocorrelation
checkresiduals(fit4) #no autocorrelation
Box.test(residuals(fit4),lag = 1, type = 'Lj') #0.96 > 0.1, no autocorrelation at lag 1,guaranteed 99%
Box.test(residuals(fit4),lag = 2, type = 'Lj') #0.94
Box.test(residuals(fit4),lag = 3, type = 'Lj') #0.97
Box.test(residuals(fit4),lag = 4, type = 'Lj') #0.43

# Normality
jarque.bera.test(residuals(fit4))  #1.151e-08 < 0.1 residual aren't normally distributed

# Heteroscedasticity
ArchTest(residuals(fit4), lags = 1) #9.677e-06< 0.1 arch effects

# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`1` <- c("0.0015756", "0.9683***",   "19.574",   "0.000009677")
`2` <- c("0.10991", "0.9465***",   "19.968",   "0.00004613")
`3` <- c("0.2224", "0.9739***",   "20.035",   "0.000167")
`4` <- c("3.8122", "0.432***",   "20.955",   "0.0003232")
lb_archlm <- as.data.frame(cbind(Lags,`1`,`2`,`3`,`4`))
lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

# Forecast 
fit_arima_accuracy<- fit4 %>% forecast::forecast(h=50)        # 38 in test set + 12 in the future
forecast::accuracy(fit_arima_accuracy, test)                  # Check the accuracy on logged data
fit_arima_accuracy %>% forecast::forecast(h=50) %>%autoplot() # Forecast plot on logged data 

forecast_arima1<- as.data.frame(fit_arima_accuracy[["mean"]])
forecast_arima1<- ts(forecast_arima1, start=c(2019, 2), frequency=12) #prepared data for making plot

autoplot(exp(y)) + autolayer(exp(forecast_arima1),PI=FALSE)+
  guides(colour=guide_legend(title="Forecast")) +
  xlab("Time") + ylab(" % ") +
  ggtitle("Final forecasts of  Real Brent Price through Arima") +
  guides(colour=guide_legend(title="Legend")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::extended_breaks(20))

#Accuracy (in logged units)
AccuracyIndicators <- c("RMSE","MAE","MAPE", "MASE")
`trainingArima` <- c("0.0851 ", "0.065",   "0.009", '0.265')
`testArima` <- c("0.330", "0.222",   "6.104", '0.896')
accuracy_table <- as.data.frame(cbind(AccuracyIndicators, `trainingArima`, `testArima`))
accuracy_table %>% gt() %>% tab_header(
  title = md("**Accuracy indicators for training and test set**"))

#---------------------------------------II Parametric ARFIMA---------------------------------------------------

#A temporary d value is obtained by fitting ARFIMA (0, d, 0) model.
#The estimated d is 0.498.

fit <- arfima(training, order = c(0, 0, 0),numeach = c(1, 1), back=FALSE, dmean = FALSE)
summary(fit)   #0.498

y_fractal<-fdiff(training, d=0.498)
ggtsdisplay(y_fractal, theme=theme_bw()) #looking at plot to identify AR and MA parameters

#ADF
adf.test(y_fractal)  #p = 0.01 < 0.1 stationary ts
summary(ur.df(y_fractal, type='none', selectlags = c("AIC"))) # stationary ts   
summary(ur.df(y_fractal, type='drift', selectlags = c("AIC")))# stationary 
summary(ur.df(y_fractal, type='trend', selectlags = c("AIC")))# stationary
# Philips-Perron
PP.test(y_fractal)  # p = 0.01 < 0.05 stationary ts
tseries::pp.test(y_fractal,type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(y_fractal,type = c("Z(t_alpha)"), lshort = TRUE)


# According to the plot of ACF and PACF, the model candidates are 
#ARFIMA (3, d, 0), ARFIMA (2, d, 0), ARFIMA (1, d, 0)

#Modelling
fit_test <- arfima(training, order = c(3, 0, 0))
summary(fit_test)  #unsignificant ar2 ar3, AIC = -1822.66; BIC = -1799.13
#d.f 0.4418798

fit_test1 <- arfima(training, order = c(2, 0, 0))
summary(fit_test1) #unsignificant ar2,  AIC = -1823.57; BIC = -1803.97
#d.f 0.4503730

fit_test2 <- arfima(training, order = c(1, 0, 0))
summary(fit_test2)  #significant, AIC = -1825.42; BIC = -1809.73
#d.f   0.4566833

#The optimal model is ARFIMA(1, 0.4566833, 0)

#Test on residuals
res_param1 <-fit_test2[["modes"]][[1]][["residuals"]]
#Autocorrelation
checkresiduals(fit_test2[["modes"]][[1]], theme = theme_bw()) # no autocorelation 
LjungBox(fit_test2[["modes"]][[1]][["residuals"]],lags = 1)  #p-value = 0.77 > 0.1   no autocorelation at lag1
LjungBox(fit_test2[["modes"]][[1]][["residuals"]],lags = 2)  #p-value = 0.57 > 0.1   no autocorelation at lag2
LjungBox(fit_test2[["modes"]][[1]][["residuals"]],lags = 3)  #p-value = 0.65 > 0.1   no autocorelation at lag3
LjungBox(fit_test2[["modes"]][[1]][["residuals"]],lags = 4)  #p-value = 0.62 > 0.1   no autocorelation at lag4

#Normality
jarque.bera.test(fit_test2[["modes"]][[1]][["residuals"]])   #p-value < 0.05 residuals aren't normally distributed

#Heteroscedasticity
ArchTest(fit_test2[["modes"]][[1]][["residuals"]],lags = 1)  #p-value < 0.05  arch effects at lag 1

# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`1` <- c("0.079", "0.777***",   "10.93",   "0.000")
`2` <- c("1.091", "0.579***",   "11.219",   " 0.003")
`3` <- c("1.636", "0.651***",   "11.281",   "0.010")
`4` <- c("2.637", "0.620***",   "11.478",   "0.021")
lb_archlm <- as.data.frame(cbind(Lags,`1`,`2`,`3`,`4`))
lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

#Forecast
prediction_arfima_parametric <- predict(fit_test2, n.ahead = 50) # 38 in test + 12 in the future
forecast::accuracy(prediction_arfima_parametric[[1]][["Forecast"]],test)

fitted_values <-  fit_test2[["modes"]][[1]][["fitted"]] 
forecast_values <- prediction_arfima_parametric[[1]][["Forecast"]] 
forecasted_param1 <- ts(forecast_values, start=c(2019,2), frequency = 12)

autoplot(exp(y), series="Real monthly crude oil price", colour = 'black') +
  autolayer(exp(forecasted_param1)  , series="Forecasted monthly crude oil price") +
  xlab("Year") +
  ylab("Crude oil price") +
  ggtitle("Final forecasts of  Real Brent Price through parametric Arfima") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::extended_breaks(20))

#Accuracy
# RMSE 
rmse(y_fractal,fit_test2[["modes"]][[1]][["fitted"]])             #0.635 
rmse(test,prediction_arfima_parametric[[1]][["Forecast"]][1:38])  #0.33 

# MAE
mae(y_fractal,fit_test2[["modes"]][[1]][["fitted"]])                        #0.531
mae(test,prediction_arfima_parametric[[1]][["Forecast"]][1:38])             #0.228

# MAPE
MAPE(y_fractal,fit_test2[["modes"]][[1]][["fitted"]])                       #1.727
MAPE(test,prediction_arfima_parametric[[1]][["Forecast"]][1:38])            #0.056

# MASE
mase(y_fractal,fit_test2[["modes"]][[1]][["fitted"]])                        #6.913
mase(test,prediction_arfima_parametric[[1]][["Forecast"]][1:38])             #2.015

#Accuracy table (in logged units)
Accuracy <- c("RMSE","MAE","MAPE","MASE")
`trainingParametricArfima` <- c("0.635", "0.531",   "1.727",   "6.913")
`testParametricArfima` <- c("0.330", "0.228",   "0.056",   "2.015")
accuracy_table <- as.data.frame(cbind(Accuracy, `trainingParametricArfima`, `testParametricArfima`))
accuracy_table %>% gt() %>% tab_header(
  title = md("**Accuracy indicators for training and test set**"))


#---------------------------------------III Semiparametric ARFIMA---------------------------------------------------
options(scipen = 999)

#Looking at Sperio, GPH and Hurst
fracdif_sperio<-fd(y,dval="Sperio")
fracdif_sperio #0.8257531

fracdif_gph<-fd(y,dval="GPH")
fracdif_gph #0.7590759

fracdif_hurst<-fd(y,dval="Hurst")
fracdif_hurst #0.5084817

#Table with values obtained
`Method` <- c("Geweke dan Porter-Hudak (GPH)", "Smoothed GPH (Sperio)",   "R/S")
`Parameter` <- c("0.82575", "0.75907",   "0.50848")
param_table <- as.data.frame(cbind(`Method`, `Parameter`))
param_table %>% gt() 

#Next, we fractionally differentiate the series considering the memory parameter
#d = (0.8257531 + 0.7590759 + 0.5084817 )/3 - 0.5 = 0.197, which is the average value of obtained semi-parametric estimators - 0.5
y_fdiff_semiparametric<-fdiff(training, d=0.197)
ggtsdisplay(y_fdiff_semiparametric, theme = theme_bw())

#ADF
adf.test(y_fdiff_semiparametric)  #p = 0.01 < 0.1 stationary ts
# Philips-Perron
PP.test(y_fractal)  # p = 0.01 < 0.05 stationary ts
tseries::pp.test(y_fractal,type = c("Z(alpha)"), lshort = TRUE)
tseries::pp.test(y_fractal,type = c("Z(t_alpha)"), lshort = TRUE)

#Models
# The candidate models are ARFIMA (2, d, 0), ARFIMA (3, d, 0),ARFIMA (1, d, 0)
fit_semiparametric1 <- Arima(y_fdiff_semiparametric, order=c(1,0,0), include.mean = FALSE)
summary(fit_semiparametric1)  #AIC=-666.21   AICc=-666.17   BIC=-658.36
coeftest(fit_semiparametric1) #significant

fit_semiparametric2 <- Arima(y_fdiff_semiparametric, order=c(2,0,0), include.mean = FALSE)
summary(fit_semiparametric2)  #AIC=-676.37   AICc=-676.3   BIC=-664.6
coeftest(fit_semiparametric2) #significant

fit_semiparametric3 <- Arima(y_fdiff_semiparametric, order=c(3,0,0), include.mean = FALSE)
summary(fit_semiparametric3)  #AIC=-675.17   AICc=-675.07   BIC=-659.49
coeftest(fit_semiparametric3) #ar3 not significant

#The optimal model is ARFIMA(2, 0.197, 0)

#Tests on residuals
res_semip1 <-fit_semiparametric2$residuals
reziduuri <- as.ts(fit_semiparametric2$residuals)

#Autocorrelation
checkresiduals(reziduuri, theme = theme_bw()) #no autocorrelation
LjungBox(reziduuri,lags = 1)   #p-value = 0.26 > 0.1  no autocorrelation at lag1
LjungBox(reziduuri,lags = 2)   #p-value = 0.19 > 0.1  no autocorrelation at lag2
LjungBox(reziduuri,lags = 3)   #p-value = 0.24 > 0.1  no autocorrelation at lag4
Box.test(reziduuri, lag=1)     #p-value = 0.26> 0.1

#Normality
jarque.bera.test(reziduuri)   #p-value < 0.05 residuals aren't normally distributed

#Heteroscedasticity
ArchTest(reziduuri,lags = 1)  #p-value = 0.1066 > 0.05   no arch effects at lag 1

# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`1` <- c("1.238", "0.265***",   "2.603",   "0.106***")
`2` <- c("3.291", "0.192***",   "8.2682",   "0.016.")
`3` <- c("4.112", "0.249***",   "7.9782",   "0.046.")
`4` <- c("5.488", "0.240***",   "10.964",   "0.026.")
lb_archlm <- as.data.frame(cbind(Lags,`1`,`2`,`3`,`4`))
lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

#Forecast
forecast_semi<-fit_semiparametric2 %>% forecast::forecast(h=50) #test:38 + forecast:12
forecast_semip1 <- as.data.frame(forecast_semi[["mean"]])
forecast_semip1 <- ts(forecast_semip1, start=c(2019, 2), frequency=12)
mean(exp(y))#54.64

autoplot(exp(y), series="Real monthly crude oil price", colour='black') +
  autolayer(exp(forecast_semip1)+54.64, series="Forecasted price with semiparametric method") + 
  autolayer(exp(forecasted_param1), series="Forecasted price with parametric method") + 
  xlab("Year") +
  ylab("Crude oil price") +
  ggtitle("MonthlyCrude oil price") +
  guides(colour=guide_legend(title="Legend")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::extended_breaks(20))

#Accuracy
#RMSE
summary(fit_semiparametric2)                #0.09428043
rmse(test,forecast_semip1[1:38])    #2.97       

# MAPE
summary(fit_semiparametric2)                #5.795496
MAPE(test,forecast_semip1[1:38])    #2.9

# MAE
summary(fit_semiparametric2)                #0.07056159
mae(test, forecast_semip1[1:38])    #2.95

# MASE
summary(fit_semiparametric2)                #1.003182
mase(test,forecast_semip1[1:38])    #26.08

#Accuracy table (in logged units)
Accuracy <- c("RMSE","MAE","MAPE","MASE")
`trainingSemiParametricArfima` <- c("0.096", "0.070",   "5.795",   "0.994")
`testSemiParametricArfima` <-     c("2.970", "2.95",   "2.906",   "26.080")
accuracy_table <- as.data.frame(cbind(Accuracy, `trainingSemiParametricArfima`, `testSemiParametricArfima`))
accuracy_table %>% gt() %>% tab_header(
  title = md("**Accuracy indicators for training and test set**"))


#2. Methods applied to the entire analayzed period------------------------------------------

#---------------------------------------IV ARIMA---------------------------------------------------
#We know from before that the series is nonstationary and becomes stationary after differencing
#inspecting the first difference acf and pacf
y %>% diff() %>% ggtsdisplay(main="", theme = theme_bw())

#Modelling
fit1 <- Arima(y, order=c(1,1,0))
coeftest(fit1) #significant
summary(fit1)  #AIC=-757.37   AICc=-757.34   BIC=-749.33

fit2 <- Arima(y, order=c(1,1,1))
coeftest(fit2) #ar1 unsignificant
summary(fit2)  #AIC=-762.82   AICc=-762.76   BIC=-750.77

fit3 <- Arima(y, order=c(2,1,0))
coeftest(fit3) #significant
summary(fit3)  #AIC=-763.02   AICc=-762.96   BIC=-750.97

fit4 <- Arima(y, order=c(1,1,2))
coeftest(fit4) #significant
summary(fit4)  #AIC=-766.19   AICc=-766.09   BIC=-750.12

fit5 <- Arima(y, order=c(0,1,1))
coeftest(fit5) #significant
summary(fit5)  #AIC=-772.29   AICc=-772.26   BIC=-764.45

fit.arima <- auto.arima(y) #ARIMA(1,1,2) 
coeftest(fit.arima) #semnificativi
summary(fit.arima)  

#The optimal model is ARIMA(1,1,2), with significant coefficients and lowest AIC

# Testing residuals
res_arima2 <- fit4$residuals
#Autocorrelation
checkresiduals(fit4) #no autocorrelation
Box.test(residuals(fit4),lag = 1, type = 'Lj') #0.98 > 0.1 no autocorrelation at lag 1, 99%
Box.test(residuals(fit4),lag = 2, type = 'Lj') #0.97 > 0.1 no autocorrelation at lag 1, 99%
Box.test(residuals(fit4),lag = 3, type = 'Lj') #0.99 > 0.1 no autocorrelation at lag 1, 99%
Box.test(residuals(fit4),lag = 4, type = 'Lj') #0.57 > 0.1 no autocorrelation at lag 1, 99%

# Normality
jarque.bera.test(residuals(fit4)) # residuals aren't normally distributed

# Heteroscedasticity
ArchTest(residuals(fit4), lags = 1) #2.2e-16 < 0.1 arch effects

# Forecast
fit_arima_accuracy_notest<- fit4 %>% forecast::forecast(h=12)        
fit_arima_accuracy_notest %>% forecast::forecast(h=12) %>%autoplot() # Forecast plot logged data

forecastarima_notest<- as.data.frame(fit_arima_accuracy_notest[["mean"]])
forecastarima_notest<- ts(forecastarima_notest, start=c(2022, 4), frequency=12) #prepared data for making plot

autoplot(exp(y)) + autolayer(exp(forecastarima_notest),PI=FALSE)+
  guides(colour=guide_legend(title="Forecast")) +
  xlab("Time") + ylab(" % ") +
  ggtitle("Final forecasts of  Real Brent Price through Arima") +
  guides(colour=guide_legend(title="Legend")) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::extended_breaks(20))

#Accuracy (in logged units)
summary(fit4)
AccuracyIndicators <- c("RMSE","MAE","MAPE", 'MASE')
`trainingArima` <- c("0.0939 ", "0.0686", "1.839", '0.2614')
accuracy_table <- as.data.frame(cbind(AccuracyIndicators, `trainingArima`))
accuracy_table %>% gt() %>% tab_header(
  title = md("**Accuracy indicators for training and test set**"))


#---------------------------------------V Parametric ARFIMA---------------------------------------------------

fit <- arfima(y, order = c(0, 0, 0),numeach = c(1, 1), back=FALSE, dmean = FALSE)
summary(fit)   #0.499

y_fractal<-fdiff(y, d=0.499)
ggtsdisplay(y_fractal, theme = theme_bw())

# According to the plot of ACF and PACF, the model candidates are 
#ARFIMA (3, d, 0), ARFIMA (2, d, 0), ARFIMA (1, d, 0)

#Potential models
fit_test <- arfima(y, order = c(3, 0, 0))
summary(fit_test)  #significant ,AIC = -1922.43; BIC = -1898.32
#d.f 0.4481110

fit_test1 <- arfima(y, order = c(2, 0, 0))
summary(fit_test1) #significant ,  AIC = -1921.13; BIC = -1901.03
#d.f 0.4725299

fit_test2 <- arfima(y, order = c(1, 0, 0))
summary(fit_test2)  #significant, AIC = -1919.2; BIC = -1903.12
#d.f   0.4754367

#The optim model considering AIC is ARFIMA(3, 0.4481110 , 0)

#Testing residuals
res_param2 <- fit_test[["modes"]][[1]][["residuals"]]
#obs: fit_test2 has autocorrelation

#Autocorrelation
checkresiduals(fit_test[["modes"]][[1]], theme = theme_bw())  #no autocorrelation      
LjungBox(fit_test[["modes"]][[1]][["residuals"]],lags = 1)  #p-value = 0.96 > 0.1  no autocorrelation at lag1, 99%
LjungBox(fit_test[["modes"]][[1]][["residuals"]],lags = 2)  #p-value = 0.96 > 0.1  
LjungBox(fit_test[["modes"]][[1]][["residuals"]],lags = 3)  #p-value = 0.99 > 0.1  
LjungBox(fit_test[["modes"]][[1]][["residuals"]],lags = 4)  #p-value = 0.47 > 0.1  

#Normality
jarque.bera.test(fit_test[["modes"]][[1]][["residuals"]])   #p-value < 0.05 residuals aren't normally distributed

#Heteroscedasticity
ArchTest(fit_test[["modes"]][[1]][["residuals"]],lags = 1)  #p-value < 0.05  arch effects at lag 1

# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`1` <- c("0.0014", "0.969***",   "63.85",   "1.343e-15")
`2` <- c("0.0711", "0.965***",   "78.39",   "2.2e-16")
`3` <- c("0.0727", "0.994***",   "99.397",   "2.2e-16")
`4` <- c("3.5429", "0.4713813***",   "99.239",   "2.2e-16")
lb_archlm <- as.data.frame(cbind(Lags,`1`,`2`,`3`,`4`))
lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

#Forecast
prediction_arfima_parametric_notest <- predict(fit_test, n.ahead = 12) 

fitted_values <-  fit_test[["modes"]][[1]][["fitted"]] 
forecast_values <- prediction_arfima_parametric_notest[[1]][["Forecast"]] 
forecasted_parametric_arfima_notest <- ts(forecast_values, start=c(2022,4), frequency = 12)

autoplot(exp(y), series="Real monthly crude oil price", colour='black') +
  autolayer(exp(forecasted_parametric_arfima_notest), series="Forecasted monthly crude oil price") +
  xlab("Year") +
  ylab("Crude oil price") +
  ggtitle("MonthlyCrude oil price") +
  guides(colour=guide_legend(title="Legend")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = scales::extended_breaks(20)) +
  theme_bw() 

#Accuracy
rmse(y_fractal,fit_test[["modes"]][[1]][["fitted"]])  #0.62
mae(y_fractal,fit_test[["modes"]][[1]][["fitted"]])   #0.50
MAPE(y_fractal,fit_test[["modes"]][[1]][["fitted"]])  #2.3
mase(y_fractal,fit_test[["modes"]][[1]][["fitted"]])  #6.34

#Accuracy table (in logged units)
Accuracy <- c("RMSE","MAE","MAPE","MASE")
`trainingParametricArfima` <- c("0.622", "0.506",   "2.304",   "6.348")
accuracy_table <- as.data.frame(cbind(Accuracy, `trainingParametricArfima`))
accuracy_table %>% gt() %>% tab_header(
  title = md("**Accuracy indicators for training set**"))

#---------------------------------------VI Semiparametric ARFIMA---------------------------------------------------

# Finding the differencing parameter d is the same as before
# d = 0.197 from above

#Next, we fractionally differentiate the series considering the memory parameter
#d = (0.8257531 + 0.7590759 + 0.5084817 )/3 - 0.5 = 0.197, which is the average value of obtained semi-parametric estimators - 0.5
y_fdiff_semiparametric<-fdiff(y, d=0.197)
ggtsdisplay(y_fdiff_semiparametric, theme = theme_bw())

#Models
# The candidate models are ARFIMA (2, d, 0), ARFIMA (3, d, 0),ARFIMA (1, d, 0)
fit_semiparametric1 <- Arima(y_fdiff_semiparametric, order=c(1,0,0), include.mean = FALSE)
summary(fit_semiparametric1)  #AIC=-660.73   AICc=-660.7   BIC=-652.7
coeftest(fit_semiparametric1) #semnificativ

fit_semiparametric2 <- Arima(y_fdiff_semiparametric, order=c(2,0,0), include.mean = FALSE)
summary(fit_semiparametric2)  #AIC=-673.67   AICc=-673.61   BIC=-661.61
coeftest(fit_semiparametric2) #semnificativi

fit_semiparametric3 <- Arima(y_fdiff_semiparametric, order=c(3,0,0), include.mean = FALSE)
summary(fit_semiparametric3)  #AIC=-678.98   AICc=-678.88   BIC=-662.9
coeftest(fit_semiparametric3) #semnificativi

#The optim model is ARFIMA(3, 0.197, 0)

#Testing residuals
res_semip2 <- fit_semiparametric3$residuals
reziduuri <- as.ts(fit_semiparametric3$residuals)

#Autocorrelation
checkresiduals(reziduuri)      #no autocorrelation 
LjungBox(reziduuri,lags = 1)   #p-value = 0.333 >  0.1 no autocorrelation at lag1
LjungBox(reziduuri,lags = 2)   #p-value = 0.602 > 0.1  no autocorrelation at lag2
LjungBox(reziduuri,lags = 3)   #p-value = 0.753 > 0.1  no autocorrelation at lag3
LjungBox(reziduuri,lags = 4)   #p-value = 0.203 > 0.1  no autocorrelation at lag4
Box.test(reziduuri, lag=1)     #p-value = 0.33

#Normality
jarque.bera.test(reziduuri)   #p-value < 0.05 residuals aren't normally distributed

#Heteroscedasticity
ArchTest(reziduuri,lags = 1)  #p-value = 0.0007005 < 0.1 arch eff

# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`1` <- c("0.935", "0.333***",   "11.488",   "0.0007005")
`2` <- c("1.012", "0.602***",   "36.36",   " 1.272e-08")
`3` <- c("1.197", "0.753***",   "42.748",   "2.784e-09")
`4` <- c("5.943", "0.203***",   "63.79",   "4.628e-13")
lb_archlm <- as.data.frame(cbind(Lags,`1`,`2`,`3`,`4`))
lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

#Forecast
forecast_semi<-fit_semiparametric3 %>% forecast::forecast(h=12) 
forecast_semip_arfima_notest <- as.data.frame(forecast_semi[["mean"]])
forecast_semip_arfima_notest <- ts(forecast_semip_arfima_notest, start=c(2022, 4), frequency=12)

autoplot(exp(y), series="Real monthly crude oil price", colour='black') +
  autolayer(exp(exp(forecast_semip_arfima_notest)), series="Forecasted price with semiparametric method") + 
  autolayer(exp(forecasted_parametric_arfima_notest), series="Forecasted price with parametric method") +
  xlab("Year") +
  ylab("Crude oil price") +
  ggtitle("MonthlyCrude oil price predictions with parametric and semiparametric Arfima") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::extended_breaks(20))


#Comparisons----------------------------------------------------------------------------------------------------

autoplot(exp(y), series="Real monthly crude oil price", colour='black') +
  autolayer(exp(exp(forecast_semip_arfima_notest)), series="Forecasted price with semiparametric method on whole dataset") + 
  autolayer(exp(forecasted_parametric_arfima_notest), series="Forecasted price with parametric method on whole dataset") +
  autolayer(exp(forecast_semip1)+54.64, series="Forecasted price with semiparametric method on training") + 
  autolayer(exp(forecasted_param1), series="Forecasted price with parametric method on training") + 
  #autolayer(exp(forecastarima_notest),series="Forecasted price with ARIMA on whole dataset")+
  #autolayer(exp(forecast_arima1),series="Forecasted price with ARIMA on training")+
  xlab("Year") +
  ylab("Crude oil price") +
  ggtitle("MonthlyCrude oil price predictions with parametric and semiparametric Arfima") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 4)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::extended_breaks(20))
  #theme(legend.text=element_text(size=12))




#DIEBOLD MARIANO

res_arima1
res_param1
res_semip1
res_arima2
res_param2
res_semip2

#For models constructed by fitting training
# Arfima parametric and semiparametric
dm.test(res_param1,res_semip1, h=1) #  p = 0.18 > 0.1 accept H0,  parametric and semiparametric models don't have different forecasts

# Arfima parametric and arima
dm.test(res_param1,res_arima1, h=1) # p = 0.9481 > 0.1, accept H0,  parametric arfima and arima models don't have different forecasts

# Arfima  semiparametric and arima
dm.test(res_semip1,res_arima1, h=1)  #  p = 0.1823 > 0.1 accept H0, semiparametric arfima and arima models don't have different forecasts




