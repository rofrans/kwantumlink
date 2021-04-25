#+++++++++++++++++++++++++++++++++++++++++++++++++#
# MacroForecast is a demo project of KWANTUMLINK  #
# Published on GITHUB under the MIT License       #
#+++++++++++++++++++++++++++++++++++++++++++++++++#

# It tests different models for forecasting of macro economic time series 
# Use the R markdown script for generating the paper

#installation of packages:
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org") # correlation analysis
if(!require(keras)) install.packages("keras", repos = "http://cran.us.r-project.org") 
if(!require(imputeTS)) install.packages("imputeTS", repos = "http://cran.us.r-project.org") # for imputation of missing data in time series
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org") # for drawing the neural network structure
if(!require(fpp2)) install.packages("fpp2", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(fpp2)
library(forecast) # comes with fpp2 installation, required for auto.arima() e.a.
library(dplyr)
library(keras)
library(tensorflow)
library(neuralnet)


# 1) READING THE DATA                                                           ----
#.===================
# set the working directory to the directory with the data files
setwd("/Users/robert/OneDrive/CONSULTING/KWANTUMLINK/Github demo projects/MacroForecast")

# read the data into a data frame per file
# as such, the columns are variables and the rows are years (observations)

dataM3   <- (read.csv(file='MABMM301EZM189S.csv',  sep=',')) 
dataCPI  <- (read.csv(file='CPHPTT01EZM661N.csv',  sep=',')) 
dataGDP  <- (read.csv(file='EA19LORSGPORGYSAM.csv',sep=',')) 
dataIRL  <- (read.csv(file='IRLTLT01EZM156N.csv',  sep=',')) 
dataRESP <- (read.csv(file='QXMN628BIS.csv',       sep=',')) 
dataEMP  <- (read.csv(file='SLEMPTOTLSPZSEUU.csv', sep=',')) 
dataUEMP <- (read.csv(file='LRHUTTTTEZM156S.csv',  sep=',')) 
dataBALP <- (read.csv(file='EA19B6BLTT02STSAQ.csv',sep=',')) 
dataSTOXi<- (read.csv(file='Eurostoxx.csv',        sep=';')) 


# 2) PREPARE THE DATA AND PREPARE A CLEAN SET                                    ----                                  ----
#.============================================
# for each series: converting the date column into the same "date" format, and 
# simplifying the series titles

dataM3 <- dataM3 %>%
  mutate(date = as.Date(DATE), m3 = MABMM301EZM189S/1000000000) %>% # convert EUR into Bn EUR
  select(date, m3)
str(dataM3)

dataCPI <- dataCPI %>%
  mutate(date = as.Date(DATE), cpi = CPHPTT01EZM661N) %>%
  select(date, cpi)
str(dataCPI)

dataGDP <- dataGDP %>%
  mutate(date = as.Date(DATE), gdp = EA19LORSGPORGYSAM) %>%
  select(date, gdp)
str(dataGDP)

dataIRL <- dataIRL %>%
  mutate(date = as.Date(DATE), irl = IRLTLT01EZM156N) %>%
  select(date, irl)
str(dataIRL)

dataRESP <- dataRESP %>%
  mutate(date = as.Date(DATE), resp = QXMN628BIS) %>%
  select(date, resp)
str(dataRESP)

dataEMP <- dataEMP %>%
  mutate(date = as.Date(DATE), emp = SLEMPTOTLSPZSEUU) %>%
  select(date, emp)
str(dataEMP)

dataUEMP <- dataUEMP %>%
  mutate(date = as.Date(DATE), uemp = LRHUTTTTEZM156S) %>%
  select(date, uemp)
str(dataUEMP)

dataBALP <- dataBALP %>%
  mutate(date = as.Date(DATE), balp = EA19B6BLTT02STSAQ) %>%
  select(date, balp)
str(dataBALP)

# the locale should be set in English to interpret the data formats in English
# since my computer is set in pt_PT.UTF-8.... 
# (ex: should read the months as Feb and May instead of Fev and Mai)
Sys.getlocale(category="LC_ALL")
Sys.setlocale("LC_TIME", "en_US.UTF-8") 

# convert the specific date format of EuroSTOX into the same format as the other series
# it is "2010Jan" and should become "2010-01-01"
year <- substr(dataSTOXi$Period.Unit., 1, 4)
month <- substr(dataSTOXi$Period.Unit., 5, 7)
yearMonth01 <- paste(year, "-", month, "-01")
year_month_01 <- gsub(" ", "", yearMonth01)
str(year_month_01)

dataSTOX <- dataSTOXi %>%
  mutate(date = as.Date(year_month_01, format = '%Y-%b-%d'), 
         stox = as.numeric(gsub( ",", ".", X.Points..))) %>% 
  select(date, stox)

# one by one, combine all series into one dataframe
data <- full_join(dataM3, dataCPI,by=dataCPI$DATE)
data <- full_join(data, dataGDP,  by=data$DATE)
data <- full_join(data, dataIRL,  by=data$DATE)
data <- full_join(data, dataRESP, by=data$DATE)
data <- full_join(data, dataEMP,  by=data$DATE)
data <- full_join(data, dataUEMP, by=data$DATE)
data <- full_join(data, dataBALP, by=data$DATE)
data <- full_join(data, dataSTOX, by=data$DATE)

# select a scope of dates for which all data are available:
datasel <- filter(data, date >= '1999-01-01' & date <= '2020-12-01')
dim(datasel)

#3) DATA VISUALIZATION AND IMPUTATION OF MISSING VALUES                                                      ----
#.=====================================================

# plot the data series
# the series that are expressed in EUR
colors = c('m3' = 'purple')
EURplot <- datasel %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=m3, color='m3')) +
  labs(x = 'Date', y = 'Bn EUR', title='Money supply M3 (Bn EUR)', color = 'Legend') +
  scale_color_manual(values = colors)
EURplot

# the series that are expressed in percentage
colors <- c('irl' = 'blue', 'gdp' = 'black', 'emp' = 'brown', 'uemp' = 'red', 'balp' = 'green')
pctplot <- datasel %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=irl, color='irl')) +
  geom_line(aes(y=gdp, color='gdp')) +
  geom_point(aes(y=emp, color='emp'), size = 1) +
  geom_line(aes(y=uemp, color='uemp')) +
  geom_point(aes(y=balp, color='balp'), size = 0.5) +
  labs(x = 'Date', y = 'Index', color = 'Legend', title='Time series (percentages)') +
  scale_color_manual(values = colors)
pctplot

# the series that expressed as an index starting at 100
colors <- c('resp' = 'red', 'cpi' = 'black')
indexplot <- datasel %>%
  ggplot(aes(x=date)) +
  geom_point(aes(y=resp, color='resp'), size = 0.5) +
  geom_line (aes(y=cpi,  color='cpi')) +
  labs(x = 'Date', y = 'Index', color = 'Legend', title='Time series (index-based)') +
  scale_color_manual(values = colors)
indexplot

# the stock exchange series, which has a proper index
colors = c('stox' = 'blue')
stoxplot <- datasel %>%
  ggplot() +
  geom_line(aes(x=date, y=stox, color='stox')) +
  labs(x = 'Date', y = 'Index', color = 'Legend', title='Eurostox equity index') +
  scale_color_manual(values = colors)
stoxplot

# transformation of quarterly and yearly data to monthly series via imputation
#.----------------------------------------------------------------------------
# interpolation of the yearly and quarterly series for filling in missing data, 
# so that they become monthly series
interpolEMP <- na_interpolation(datasel$emp)

dataI <- datasel %>%
  mutate(empI = na_interpolation(emp),
         respI = na_interpolation(resp),
         balpI = na_interpolation(balp),
         gdpI  = na_interpolation(gdp)) %>% 
  select(-emp, -resp, -balp, -gdp)
         
# plot again the series that are expressed in percentage
colors <- c('irl' = 'blue', 'gdp' = 'black', 'emp' = 'brown', 'uemp' = 'red', 'balp' = 'green')
pctIplot <- dataI %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=irl, color='irl')) +
  geom_line(aes(y=gdpI, color='gdp')) +
  geom_line(aes(y=empI, color='emp')) +
  geom_line(aes(y=uemp, color='uemp')) +
  geom_line(aes(y=balpI, color='balp')) +
  labs(x = 'Month', y = 'Index', color = 'Legend', title='Time series (percentages)') +
  scale_color_manual(values = colors)
pctIplot

# plot again the series that expressed as an index starting at 100
colors <- c('m3' = 'purple', 'resp' = 'red', 'cpi' = 'black')
indexIplot <- dataI %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=respI, color='resp')) +
  geom_line (aes(y=cpi,  color='cpi')) +
  labs(x = 'Month', y = 'Index', color = 'Legend', title='Time series (index-based)') +
  scale_color_manual(values = colors)       
indexIplot


# Applying a differencing technique to make the data stationary
#.-------------------------------------------------------------
# The purpose of "differencing" is to make the data stationary
# the data are replaced by their difference with the next month
# and the difference for the first month is filled in artificially as the 
# difference between the first and the second month (otherwise the series would
# be one item too short, as compared to the other series)
dataID <- dataI %>%
  mutate(m3D = c(m3[2]-m3[1], diff(m3, diffferences = 1))) %>%
  mutate(respID = c(respI[2]-respI[1], diff(respI, diffferences = 1))) %>%
  mutate(cpiD =   c(cpi[2]-cpi[1], diff(cpi, diffferences = 1))) %>%
  mutate(stoxD =  c(stox[2]-stox[1], diff(stox, diffferences = 1))) %>%
  select(-m3, -respI, -cpi, -stox)

# plot again the series that were "differenced"
colors <- c('m3D' = 'purple', 'respD' = 'red', 'cpiD' = 'black', 'stoxD' = 'blue')
differencedplot <- dataID %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=m3D, color='m3D')) +
  geom_line(aes(y=respID, color='respD')) +
  geom_line (aes(y=cpiD,  color='cpiD')) +
  geom_line (aes(y=stoxD,  color='stoxD')) +
  labs(x = 'Month', y = 'Index', color = 'Legend', title='Time series (differenced)') +
  scale_color_manual(values = colors)       
differencedplot



# 4) CORRELATION CHECK                                                          ----
#.====================
# plot a correlation heat map
library(GGally)
corrmap <- dataID %>%
  select(-date) %>%
  ggcorr(method = c("pairwise", "spearman"), 
         nbreaks = 10,
         hjust = 0.9, 
         label = TRUE,
         label_size = 3,
         color = "grey20")
corrmap



# 5) APPROACH 1: TIME SERIES FORECASTING WITH ARIMA                             ----
#.=================================================

# 5A) CPI data (index-series)
#.............................

# prepare a time series object
cpi_ts <- ts(dataI$cpi, start=1999, end=2020, frequency=12)

fit_cpi <- auto.arima(cpi_ts)
summary(fit_cpi)
checkresiduals(fit_cpi)

# forecast
fcast_cpi <- forecast(fit_cpi, h = 36)
plot(fcast_cpi)


# 5B) CPID data (differenced series)
#...................................

# prepare a time series object
cpiD_ts <- ts(dataID$cpi, start=1999, end=2020, frequency=12)

fit_cpiD <- auto.arima(cpiD_ts)
summary(fit_cpiD)
checkresiduals(fit_cpiD)

# forecast
fcast_cpiD <- forecast(fit_cpiD, h = 36)
plot(fcast_cpiD)



  # 6) FURTHER DATA PREPARATION FOR THE MULTIVARIATE FORECASTING              ----
#.==============================================================

# 6.A) ADDING AN "ECONOMIC CLIMATE VARIABLE"                                    ----
#.------------------------------------------
# 6.A.1) on the index series
#...........................
# adding a "economic climate" variable with a value +1 or -1 for the periods of 
# the crisis of 2008 and 2020
dataIC <- dataI %>%
  mutate(climate = case_when(date >= '2008-08-01' & date < '2010-01-01' ~ -1,
                             date >= '2020-03-01' ~ -1,
                             TRUE ~ 1))

dataNC <- select(dataIC, -date)
which(is.na(dataNC))

# 6.A.2) on the differenced series
#.................................
# adding a "economic climate" variable with a value +1 or -1 for the periods of 
# the crisis of 2008 and 2020
dataICD <- dataID %>%
  mutate(climate = case_when(date >= '2008-08-01' & date < '2010-01-01' ~ -1,
                             date >= '2020-03-01' ~ -1,
                             TRUE ~ 1))

dataNCD <- select(dataICD, -date)
which(is.na(dataNCD))


# 6.B) PREPARATION OF A LEADING VARIABLE FOR CPI (CPI1, CPI2, CPI3)                             ----
#.-----------------------------------------------------------------
# 6.B.1) on the Index series                                                    ----
#.--------------------------
# select the cpi column from the data matrix
cpi <- dataNC[,2]
# add 3 new columns with the same cpi data
a = cbind(cpi, cpi, cpi, cpi)
# shift the cpi values 1,2 or 3 years backwards
# technique used from https://stackoverflow.com/questions/48968161/individual-shift-of-each-column-in-a-matrix
b <- matrix(sapply(1:dim(a)[2], function(x){c(a[x:dim(a)[1], x], rep(0, (x - 1) ))}), ncol = dim(a)[2], nrow = dim(a)[1])
colnames(b) <- c('cpi', 'cpi1', 'cpi2', 'cpi3')
b <- b[,-1] # delete the first column "cpi", that we have already in dataNC
data2NC <- cbind(dataNC, b)
tail(data2NC)

# 6.B.2) on the Differenced series                                                    ----
#.--------------------------------
# select the cpi column from the data matrix
cpiD <- dataNCD[,2]
# add 3 new columns with the same cpi data
a = cbind(cpiD, cpiD, cpiD, cpiD)
# shift the cpi values 1,2 or 3 years backwards
# from https://stackoverflow.com/questions/48968161/individual-shift-of-each-column-in-a-matrix
b <- matrix(sapply(1:dim(a)[2], function(x){c(a[x:dim(a)[1], x], rep(0, (x - 1) ))}), ncol = dim(a)[2], nrow = dim(a)[1])
colnames(b) <- c('cpiD', 'cpi1D', 'cpi2D', 'cpi3D')
b <- b[,-1] # delete the first column "cpi", that we have already in dataNC
data2NCD <- cbind(dataNCD, b)
tail(data2NCD)



# 7) APPROACH 2: MULTIVARIATE GLM                                               ----
#.===============================

# 7A)  ON THE INDEX SERIES                                                      ----
#-------------------------

# 7A.1) Base model                                                                ----
#.--------------
modelglm <- glm(cpi ~ m3+gdpI+irl+uemp+stox+empI+respI+balpI, 
                data=as.data.frame(dataI))
summary(modelglm)

RMSE1 <- sqrt(mean(modelglm$residuals^2))

rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

#initiate the rsquare overview table
rsquare.results1 <- data_frame(Method = "GLM with 8 vars", 
                               Dep_var = "cpi",  
                               R2 = rsquare,
                               RMSE = RMSE1)

# produce the first overview table
rsquare.results1 %>% knitr::kable()

# 7A.2 Model with climate variable
#.--------------------------------
# adding the same "climate"variable as above in the deep learning model
modelglm <- glm(cpi ~ m3+gdpI+irl+uemp+stox+empI+respI+balpI+climate, 
                data=as.data.frame(dataNC))
summary(modelglm)

RMSE1 <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results1 <- bind_rows(rsquare.results1,
                             data_frame(Method = "GLM with 8 vars + climate variable",  
                                        Dep_var = "cpi",  
                                        R2 = rsquare,
                                        RMSE = RMSE1))

# produce the overview table
rsquare.results1 %>% knitr::kable()


# 7A.3 Testing the predictability for the cpi in the next three years
#.-------------------------------------------------------------------
# predicting cpi one year ahead (cpi1)
modelglm <- glm(cpi1 ~ m3+gdpI+irl+uemp+stox+empI+respI+balpI+climate, 
                data=as.data.frame(data2NC))
summary(modelglm)

RMSE2 <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)


#initiate the rsquare overview table
rsquare.results2 <- data_frame(Method = "Predicting CPI 1 year  ahead", 
                               Dep_var = "cpi", 
                               R2 = rsquare,
                               RMSE = RMSE2)

# produce the first overview table
rsquare.results2 %>% knitr::kable()

# predicting cpi two years ahead (cpi2)
modelglm <- glm(cpi2 ~ m3+gdpI+irl+uemp+stox+empI+respI+balpI+climate, 
                data=as.data.frame(data2NC))
summary(modelglm)

RMSE2 <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results2 <- bind_rows(rsquare.results2,
                             data_frame(Method = "Predicting CPI 2 years ahead", 
                                        Dep_var = "cpi",  
                                        R2 = rsquare,
                                        RMSE = RMSE2))

# produce the overview table
rsquare.results2 %>% knitr::kable()

# predicting cpi three years ahead (cpi3)
modelglm <- glm(cpi3 ~ m3+gdpI+irl+uemp+stox+empI+respI+balpI+climate, 
                data=as.data.frame(data2NC))
summary(modelglm)

RMSE2 <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results2 <- bind_rows(rsquare.results2,
                             data_frame(Method = "Predicting CPI 3 years ahead", 
                                        Dep_var = "cpi",  
                                        R2 = rsquare,
                                        RMSE = RMSE2))

# produce the first overview table
rsquare.results2 %>% knitr::kable()



# 7B)  ON THE DIFFERENCED SERIES                                                ----
#-------------------------------

# 7B.1) Base model                                                              ----
#.----------------
modelglm <- glm(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI, 
                data=as.data.frame(dataID))
summary(modelglm)

RMSE1D <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

#initiate the rsquare overview table
rsquare.results1D <- data_frame(Method = "GLM with 8 vars", 
                               Dep_var = "cpiD",  
                               R2 = rsquare,
                               RMSE = RMSE1D)

# produce the first overview table
rsquare.results1D %>% knitr::kable()

# 7B.2 Model with climate variable
#.--------------------------------
# adding the same "climate"variable as above in the deep learning model
modelglm <- glm(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate, 
                data=as.data.frame(dataNCD))
summary(modelglm)

RMSE1D <- sqrt(mean(modelglm$residuals^2))
rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results1D <- bind_rows(rsquare.results1D,
                              data_frame(Method = "GLM with 8 vars + climate variable",  
                                         Dep_var = "cpiD",  
                                         R2 = rsquare,
                                         RMSE = RMSE1D))

# produce the overview table
rsquare.results1D %>% knitr::kable()


# 7B.3 Testing the predictability for the cpi in the next three years
#.--------------------------------------------------------------
# predicting cpi one year ahead (cpi1)
modelglm <- glm(cpi1D ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate, 
                data=as.data.frame(data2NCD))
summary(modelglm)
RMSE2D <- sqrt(mean(modelglm$residuals^2))

rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)


#initiate the rsquare overview table
rsquare.results2D <- data_frame(Method = "Predicting CPID 1 year  ahead", 
                               Dep_var = "cpiD", 
                               R2 = rsquare,
                               RMSE = RMSE2D)

# produce the first overview table
rsquare.results2D %>% knitr::kable()

# predicting cpi two years ahead (cpi2)
modelglm <- glm(cpi2D ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate, 
                data=as.data.frame(data2NCD))
summary(modelglm)
RMSE2D <- sqrt(mean(modelglm$residuals^2))

rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results2D <- bind_rows(rsquare.results2D,
                              data_frame(Method = "Predicting CPID 2 years ahead", 
                                         Dep_var = "cpiD",  
                                         R2 = rsquare,
                                         RMSE = RMSE2D))

# produce the overview table
rsquare.results2 %>% knitr::kable()

# predicting cpi three years ahead (cpi3)
modelglm <- glm(cpi3D ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate, 
                data=as.data.frame(data2NCD))
summary(modelglm)
RMSE2D <- sqrt(mean(modelglm$residuals^2))

rsquare <- with(summary(modelglm), 1 - deviance/null.deviance)

# complete the overview table
rsquare.results2D <- bind_rows(rsquare.results2D,
                              data_frame(Method = "Predicting CPID 3 years ahead", 
                                         Dep_var = "cpiD",  
                                         R2 = rsquare,
                                         RMSE = RMSE2D))

# produce the overview table
rsquare.results2D %>% knitr::kable()




# 8) APPROACH 3: APPLYING A DEEP LEARNING NEURAL NETWORK MODEL                  ----
#.============================================================
# The neural model will only be used with the differenced series
# This works better for this method (stationary data) and facilitates the 
# comparison with of the predictability with the GLM method on the differenced
# series.


# 8.A) PREPARATION                                                              ----
#.----------------

# 8.A.1) all variables should be numeric
#.......................................
# therefore we withdraw the "date" column
dataND <- select(dataID, -date)
str(dataND)
which(is.na(dataND))



# 8.A.2) split the set into training and test set
#................................................
dataND <- as.matrix(dataND)
set.seed(2021)

index <- sample(2, nrow(dataID), replace = TRUE, prob = c(0.8, 0.2))
training <- dataND[index==1,1:9] # remark: the number of columns is to be increased from 8 to 9, due to the index that was added
test <- dataND[index==2, 1:9]


# 8.A.3) normalization of the data
#.................................
means <- colMeans(training)
stds <- apply(training, 2, sd)
training <- scale(training, center = means, scale = stds)
test     <- scale(test,     center = means, scale = stds)


# quick verification of the normalization:
summary(training) # all means are zero => OK
sd(training) # standard deviation is globally close to 1 => OK

# 8.B) SIMPLEST MODEL                                                               ----
#.-------------------
#https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/

# Construction of the neural model
# - input layer: 8 nodes, one for each independent variable
# - one hidden layer with only one node
# - one output layer, for the dependent variable (CPI)
# the model is trained on the training data
# the relations are set to "linear"
nn <- neuralnet(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI,
                data = training,
                hidden = c(1),
                linear.output = T, 
                algorithm = "rprop+",
                threshold=0.01, 
                lifesign= 'full',
                rep=1)



# we extract the error value from the list of the weights:
nn$result.matrix[1]

# visualizing the neural network
plot(nn,
     col.hidden = 'blue',
     col.hidden.synapse = 'blue',
     show.weights = F,
     information = T,
     fill = 'lightblue')

# The “compute” function then creates the prediction variable
temp_test <- subset(test, select = c('m3D', 'gdpI','irl','uemp','stoxD','empI','respID','balpI'))
# "compute" function is degraded, use predict instead: https://rdrr.io/cran/neuralnet/man/predict.nn.html

nn.results <- predict(nn, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = nn.results)

# calculation of the RMSE
rmse.simple <- sqrt(mean((results$actual - results$prediction)^2))

#initiate the model comparison table
neural.results <- data_frame(Neural_Model_Structure = "0. input8 + dense1 + output1",
                            Dep_Var = "cpiD",
                            RMSE = rmse.simple)

# produce the first overview table
neural.results %>% knitr::kable()

# 8.C) BASE MODEL: ONE HIDDEN LAYER                                                               ----
#.---------------------------------
#https://datascienceplus.com/neuralnet-train-and-test-neural-networks-using-r/

# Construction of the neural model
# - input layer: 8 nodes, one for each independent variable
# - one hidden layer of 8 nodes
# - one output layer, for the dependent variable (CPI)
# the model is trained on the training data
# the relations are set to "linear"
nn <- neuralnet(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI,
                data = training,
                hidden = c(8),
                linear.output = T, 
                algorithm = "rprop+",
                threshold=0.01,  
                lifesign= 'full',
                rep=1)



# we extract the error value from the list of the weights:
nn$result.matrix[1]

# visualizing the neural network
plot(nn,
     col.hidden = 'blue',
     col.hidden.synapse = 'blue',
     show.weights = F,
     information = T,
     fill = 'lightblue')

# The “compute” function then creates the prediction variable
temp_test <- subset(test, select = c('m3D', 'gdpI','irl','uemp','stoxD','empI','respID','balpI'))
# "compute" function is degraded, use predict instead: https://rdrr.io/cran/neuralnet/man/predict.nn.html

nn.results <- predict(nn, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = nn.results)

# calculation of the RMSE
rmse.base <- sqrt(mean((results$actual - results$prediction)^2))

# complete the overview table
neural.results <- bind_rows(neural.results,
                            data_frame(Neural_Model_Structure = "1. input8 + dense8 + output1",
                                       Dep_Var = "cpiD",
                                       RMSE = rmse.base))

# produce the first overview table
neural.results %>% knitr::kable()


# 8.D) ADDING AN EXTRA LAYER                                                              ----
#.--------------------------
# Construction of the neural model
# - input layer: 8 nodes, one for each independent variable
# - one hidden layer of 40 nodes
# - one hidden layer of 8 nodes
# - one output layer, for the dependent variable (CPI)
# the model is trained on the training data
# the relations are set to "linear"


nn <- neuralnet(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI,
                data = training,
                hidden = c(40,8),
                linear.output = T, 
                algorithm = "rprop+",
                threshold=0.01, 
                lifesign= 'full',
                rep=1)

# we extract the error value from the list of the weights:
nn$result.matrix[1]

# visualizing the neural network
# plot(nn,
#      col.hidden = 'blue',
#      col.hidden.synapse = 'blue',
#      show.weights = F,
#      information = T,
#      fill = 'lightblue')

# The “compute” function then creates the prediction variable
temp_test <- subset(test, select = c('m3D', 'gdpI','irl','uemp','stoxD','empI','respID','balpI'))
# "compute" function is degraded, use predict instead: https://rdrr.io/cran/neuralnet/man/predict.nn.html

nn.results <- predict(nn, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = nn.results)

# calculation of the RMSE
rmse.2 <- sqrt(mean((results$actual - results$prediction)^2))

# complete the overview table
neural.results <- bind_rows(neural.results,
                           data_frame(Neural_Model_Structure = "2. input8 + dense40 + dense8 + output1",
                                      Dep_Var = "cpiD",
                                      RMSE = rmse.2))

# produce the first overview table
neural.results %>% knitr::kable()


# 8.E) ADDING THE CLIMATE VARIABLE                                                             ----
#.--------------------------------
# 8.D.1) all variables should be numeric
#.......................................
# therefore we withdraw the "date" column
dataNCD <- select(dataICD, -date)
str(dataNCD)
which(is.na(dataNCD))



# 8.D.2) split the set into training and test set
#................................................
dataNCD <- as.matrix(dataNCD)
set.seed(2021)

index <- sample(2, nrow(dataICD), replace = TRUE, prob = c(0.8, 0.2))
training <- dataNCD[index==1,1:10] # remark: the number of columns is to be increased from 9 to 10, due to the index that was added
test <- dataNCD[index==2, 1:10]

# 8.D.3) normalization of the data
#.................................
means <- colMeans(training)
stds <- apply(training, 2, sd)
training <- scale(training, center = means, scale = stds)
test     <- scale(test,     center = means, scale = stds)

# quick verification of the normalization:
summary(training) # all means are zero => OK
sd(training) # standard deviation is globally close to 1 => OK


# 8.D.4) Applying the neural model
#.................................
nn <- neuralnet(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate,
                data = training,
                hidden = c(40, 8),
                linear.output = T, 
                algorithm = "rprop+",
                threshold=0.01, 
                lifesign= 'full',
                rep=1)

# we extract the error value from the list of the weights:
nn$result.matrix[1]

# visualizing the neural network
# plot(nn,
#      col.hidden = 'blue',
#      col.hidden.synapse = 'blue',
#      show.weights = F,
#      information = T,
#      fill = 'lightblue')

# The “compute” function then creates the prediction variable
temp_test <- subset(test, select = c('m3D', 'gdpI','irl','uemp','stoxD','empI','respID','balpI', 'climate'))
# "compute" function is degraded, use predict instead: https://rdrr.io/cran/neuralnet/man/predict.nn.html

nn.results <- predict(nn, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = nn.results)

# calculation of the RMSE
rmse.c <- sqrt(mean((results$actual - results$prediction)^2))

# complete the overview table
neural.results <- bind_rows(neural.results,
                            data_frame(Neural_Model_Structure = "3. input8 + dense40 + dense8 + output1, with climate var",
                                       Dep_Var = "cpiD",
                                       RMSE = rmse.c))

# produce the first overview table
neural.results %>% knitr::kable()




# 9) APPLYING A MACHINE LEARNING TECHNIQUE: RANDOM FOREST                                                 ----
#.=======================================================

library(randomForest)

# a) application without the climate variable
rf <- randomForest(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI, data = training, ntree=20)

predictions_RF = predict(rf, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = predictions_RF)

# calculation of the RMSE
rmse.rf <- sqrt(mean((results$actual - results$prediction)^2))

#initiate the model comparison table
rf.results <- data_frame(Random_Forest = "Random Forest, n=20",
                             Dep_Var = "cpiD",
                             RMSE = rmse.rf)


# produce the first overview table
rf.results %>% knitr::kable()


# b) application with the climate variable
rf <- randomForest(cpiD ~ m3D+gdpI+irl+uemp+stoxD+empI+respID+balpI+climate, data = training, ntree=20)

predictions_RF = predict(rf, temp_test)

# A “results” variable then compares the predicted data with the actual data
results <- data.frame(actual = as.data.frame(test)$cpiD, prediction = predictions_RF)

# calculation of the RMSE
rmse.rf <- sqrt(mean((results$actual - results$prediction)^2))

# complete the overview table


# complete the overview table
rf.results <- bind_rows(rf.results,
                        data_frame(Random_Forest = "Random Forest, n=20, with climate variable",
                                   Dep_Var = "cpiD",
                                   RMSE = rmse.rf))

# produce the overview table
rf.results %>% knitr::kable()


# 10) COMPARISON OF THE DIFFERENT MODELS                                                  ----
#.======================================
#see markdown text
