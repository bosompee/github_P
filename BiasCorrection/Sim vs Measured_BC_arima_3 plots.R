library(zoo)
library(dplyr)
library(magrittr)
library(tidyverse)
library(lubridate)

windowsFonts(Arial=windowsFont("TT Arial"))
theme_scz <- function(...){
  theme_bw(base_size=10, base_family="Arial") + 
    theme(
      text=element_text(color="black"),
      plot.title=element_text(face="bold", size=rel(1)),
      axis.title=element_text(face="bold", size=rel(1)),
      axis.text=element_text(size=rel(1)),
      strip.text=element_text(size=rel(1)),
      legend.title=element_text(face="bold", size=rel(1)),
      legend.text=element_text(size=rel(1)),
      panel.grid=element_blank(),
      plot.margin=unit(c(1,1,1,1), "mm"),
      strip.background=element_blank())
}

theme_set(theme_scz())

# Arrange
library(ggpubr)
par(mfrow = c(2, 2))

## read in csv file of stream gages
df_gages <- read_csv("C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/Bias correction_NHM/newNC.csv")

#look at column names
names(df_gages)

# get a vector of all ID numbers
all_gages <- df_gages$Site_no.
all_gages

gage <- all_gages[7]
gage

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

seg <- all_gages2[7]
seg
####################################################################
path_to_usgs_data <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"

# USGS data
mf <- read.csv(paste0(path_to_usgs_data, "Discharge_", gage, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mf$Date <-mdy(mf$Date)
mf$Year <- year(mf$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1 <- subset(mf, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

# NHM data
df <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colname <- paste0("X", seg)
df_seg <- df[,c("Date", seg_colname)]

df_seg$Date <-mdy(df_seg$Date)
df_seg$Year <- year(df_seg$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1 <- subset(df_seg, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_seg1)[colnames(df_seg1)=="X34442"]<-"Sim"      #change seg ID

#JOIN data
df_joined <- left_join(df_seg1, mf1, by = "Date")

#converting data from daily to monthly
df_joined1 <- df_joined       #Duplicate data
df_joined1$year_month <- floor_date(df_joined1$Date, "month")  #Create year-month column

df_joined_aggr <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Q= mean(Q)) %>%
  as.data.frame()

df_joined_aggr1 <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Sim= mean(Sim)) %>%     
  as.data.frame()


#JOIN data again
df_joined <- left_join(df_joined_aggr, df_joined_aggr1, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joined)[1]<-"Date"

############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_joined$diff <- df_joined$Q - df_joined$Sim


#ACF and PACF before bias correction
acf(df_joined$diff, main="Gage A")
pacf(df_joined$diff, main="")

library(patchwork)
"A" + "B" + "C" + "D"

par(mfrow = c(1, 2))


#checking if the dataset is a time series
class(df_joined)  #data is in "dataframe" form

#converting the data first to time series
df_joinedtime=ts(df_joined[,4],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedtime)

acf(df_joinedtime)
pacf(df_joinedtime)

adf.test(df_joinedtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
S = auto.arima(df_joinedtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
#S = auto.arima(df_joined$diff, allowmean=FALSE, allowdrift=FALSE, trace=TRUE) #use this for ACF PACF plots
S
checkresiduals(S)
s <- S$residuals
s

acf(s)

###################################################
#### Forecast with ARIMA model
###################################################
forecast <- forecast(S, h=100)
forecast
autoplot(forecast)+
  labs(x="Time", y="Residuals", title="Gage A")
checkresiduals(forecast)

#increase print limit to 2500 values
options(max.print=2500)

#increase print limit to max allowed by your machine
options(max.print = .Machine$integer.max)

#attempt to print entire data frame again
forecast

### PLOTTING THE ACF AND PACF ON SAME GRAPH
### ACF and PACF after bias-correction
#ACF and PACF before bias correction
acf(df_joined$diff, main="ACF of residuals before bias correction")
pacf(df_joined$diff, main="PACF of residuals before bias correction")

par(mfrow = c(2, 2))

acf(s, main="ACF of residuals after bias correction")
pacf(s, main="PACF of residuals after bias correction")

#autoplot(S)
plot.ts(s)
gghistogram(s)

d <- cbind(df_joined, s)

d1 <- transform(d, SimCorrected= s+Sim)

SimCorrectednew <- d1 $SimCorrected                     # Duplicate data frame
SimCorrectednew[d1 $SimCorrected  < 0] <- 0     # Set negative values to 0
SimCorrectednew

#Add to dataframe
d1 <- cbind(d1, SimCorrectednew)

##calculate fit stats
# package: hydroGOF
library(hydroGOF)
library(ggplot2)

gof(sim=d1$SimCorrectednew, obs=d1$Q)

#############################
# d1$Date <- mdy(d1$Date)

bxp <- ggplot(data=d1, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrectednew, color= "SimCorrected"))+
  geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Bias Correction', 'Simulated', 'Measured'), values = c('SimCorrected'='red', 'Sim'='blue', 'Q'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  guides(color= guide_legend(title = 'Gage 07141300'))+
  ggtitle('Gage 07108900')+
  theme(legend.position = "top")+
  guides(color= guide_legend(title = ''))
#guides(color= guide_legend)
bxp

##########################################################################################
##########################################################################################

# get a vector of all ID numbers

gageC <- all_gages[20]
gageC

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

segC <- all_gages2[20]
segC
####################################################################
path_to_usgs_data <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"

# USGS data
mfC <- read.csv(paste0(path_to_usgs_data, "Discharge_", gage, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mfC$Date <-mdy(mfC$Date)
mfC$Year <- year(mfC$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1C <- subset(mfC, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

# NHM data
dfC <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colnameC <- paste0("X", segC)
df_segC <- df[,c("Date", seg_colnameC)]

df_segC$Date <-mdy(df_segC$Date)
df_segC$Year <- year(df_segC$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1C <- subset(df_segC, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_seg1C)[colnames(df_seg1C)=="X37684"]<-"Sim"      #change seg ID

#JOIN data
df_joinedC <- left_join(df_seg1C, mf1C, by = "Date")

#converting data from daily to monthly
df_joined1C <- df_joinedC       #Duplicate data
df_joined1C$year_month <- floor_date(df_joined1C$Date, "month")  #Create year-month column

df_joined_aggrC <- df_joined1C |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Q= mean(Q)) |>    
  as.data.frame()


df_joined_aggr1C <- df_joined1C |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Sim= mean(Sim)) |>     
  as.data.frame()

#JOIN data again
df_joinedC <- left_join(df_joined_aggrC, df_joined_aggr1C, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joinedC)[1]<-"Date"

############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_joinedC$diff <- df_joinedC$Q - df_joinedC$Sim


#checking if the dataset is a time series
class(df_joinedC)  #data is in "dataframe" form

#converting the data first to time series
df_joinedCtime=ts(df_joinedC[,4],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedCtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedCtime)

acf(df_joinedCtime)
pacf(df_joinedCtime)

adf.test(df_joinedCtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
SC = auto.arima(df_joinedCtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
SC
sC <- SC$residuals
sC

###################################################
#### Forecast with ARIMA model
###################################################
forecastB <- forecast(SC, h=300)
forecastB
autoplot(forecastB)+
  labs(x="Time", y="Residuals", title="Gage B")
checkresiduals(forecastB)


dC <- cbind(df_joinedC, sC)

d1C <- transform(dC, SimCorrected= sC+Sim)

SimCorrectednewC <- d1C $SimCorrected                 # Duplicate data frame
SimCorrectednewC[d1C $SimCorrected  < 0] <- 0     # Set negative values to 0
SimCorrectednewC

#Add to dataframe
d1C <- cbind(d1C, SimCorrectednewC)

##calculate fit stats
# package: hydroGOF
library(hydroGOF)
library(ggplot2)

gof(sim=d1C$SimCorrectednewC, obs=d1C$Q)

#############################
# d1$Date <- mdy(d1$Date)

dens <- ggplot(data=d1C, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrectednewC, color= "SimCorrected"))+
  geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('After Bias Correction', 'Simulated', 'Measured'), values = c('SimCorrected'='red', 'Sim'='blue', 'Q'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage 07137500')+
  theme(legend.position="none")

dens

######################################################################################
######################################################################################

# get a vector of all ID numbers

gageB <- all_gages[23]
gageB

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

segB <- all_gages2[23]
segB
####################################################################
path_to_usgs_data <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"

# USGS data
mfB <- read.csv(paste0(path_to_usgs_data, "Discharge_", gage, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mfB$Date <-mdy(mfB$Date)
mfB$Year <- year(mfB$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1B <- subset(mfB, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

# NHM data
dfB <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colnameB <- paste0("X", segB)
df_segB <- df[,c("Date", seg_colnameB)]

df_segB$Date <-mdy(df_segB$Date)
df_segB$Year <- year(df_segB$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1B <- subset(df_segB, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_seg1B)[colnames(df_seg1B)=="X37137"]<-"Sim"      #change seg ID

#JOIN data
df_joinedB <- left_join(df_seg1B, mf1B, by = "Date")

#converting data from daily to monthly
df_joined1B <- df_joinedB       #Duplicate data
df_joined1B$year_month <- floor_date(df_joined1B$Date, "month")  #Create year-month column

df_joined_aggrB <- df_joined1B |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Q= mean(Q)) |>    
  as.data.frame()


df_joined_aggr1B <- df_joined1B |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Sim= mean(Sim)) |>     
  as.data.frame()

#JOIN data again
df_joinedB <- left_join(df_joined_aggrB, df_joined_aggr1B, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joinedB)[1]<-"Date"


############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_joinedB$diff <- df_joinedB$Q - df_joinedB$Sim


#checking if the dataset is a time series
class(df_joinedB)  #data is in "dataframe" form

#converting the data first to time series
df_joinedBtime=ts(df_joinedB[,4],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedBtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedBtime)

acf(df_joinedBtime)
pacf(df_joinedBtime)

adf.test(df_joinedBtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
SB = auto.arima(df_joinedBtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
SB
sB <- SB$residuals
sB

###################################################
#### Forecast with ARIMA model
###################################################
forecastC <- forecast(SB, h=100)
forecastC
autoplot(forecastC)+
  labs(x="Time", y="Residuals", title="Gage C")
checkresiduals(forecastC)


dB <- cbind(df_joinedB, sB)

d1B <- transform(dB, SimCorrected= sB+Sim)

SimCorrectednewB <- d1B $SimCorrected                 # Duplicate data frame
SimCorrectednewB[d1B $SimCorrected  < 0] <- 0     # Set negative values to 0
SimCorrectednewB

#Add to dataframe
d1B <- cbind(d1B, SimCorrectednewB)

##calculate fit stats
# package: hydroGOF
library(hydroGOF)
library(ggplot2)

gof(sim=d1B$SimCorrectednewB, obs=d1B$Q)

#############################
# d1$Date <- mdy(d1$Date)

dp <- ggplot(data=d1B, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrectednewB, color= "SimCorrected"))+
  geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Bias Correction', 'Simulated', 'Measured'), values = c('SimCorrected'='red', 'Sim'='blue', 'Q'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage 07141300')+
  theme(legend.position="none")
dp


# read in csv file of stream gages
df_gages <- read_csv("C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/Bias correction_NHM/newNC.csv")

#look at column names
names(df_gages)

# get a vector of all ID numbers
all_gages <- df_gages$Site_no.
all_gages

gage <- all_gages[27]
gage

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

seg <- all_gages2[27]
seg
####################################################################
path_to_usgs_data <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"

# USGS data
mf <- read.csv(paste0(path_to_usgs_data, "Discharge_", gage, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mf$Date <-mdy(mf$Date)
mf$Year <- year(mf$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1 <- subset(mf, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

# NHM data
df <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colname <- paste0("X", seg)
df_seg <- df[,c("Date", seg_colname)]

df_seg$Date <-mdy(df_seg$Date)
df_seg$Year <- year(df_seg$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1 <- subset(df_seg, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_seg1)[colnames(df_seg1)=="X33743"]<-"Sim"      #change seg ID

#JOIN data
df_joined <- left_join(df_seg1, mf1, by = "Date")

#converting data from daily to monthly
df_joined1 <- df_joined       #Duplicate data
df_joined1$year_month <- floor_date(df_joined1$Date, "month")  #Create year-month column

df_joined_aggr <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Q= mean(Q)) %>%
  as.data.frame()

df_joined_aggr1 <- df_joined1 %>%   #Agrregate data
  group_by(year_month) %>%
  dplyr::summarize(Sim= mean(Sim)) %>%     
  as.data.frame()


#JOIN data again
df_joined <- left_join(df_joined_aggr, df_joined_aggr1, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joined)[1]<-"Date"

############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_joined$diff <- df_joined$Q - df_joined$Sim


#ACF and PACF before bias correction
acf(df_joined$diff, main="Gage A")
pacf(df_joined$diff, main="")

library(patchwork)
"A" + "B" + "C" + "D"

par(mfrow = c(1, 2))


#checking if the dataset is a time series
class(df_joined)  #data is in "dataframe" form

#converting the data first to time series
df_joinedtime=ts(df_joined[,4],start=c(1980,10),frequency = 12)

#checking again if the dataset is a time series
class(df_joinedtime)  #data is in "ts" now

library(forecast)
library(tseries)
plot(df_joinedtime)

acf(df_joinedtime)
pacf(df_joinedtime)

adf.test(df_joinedtime)

#df_joinedModel= auto.arima(df_joinedtime,ic="aic",trace = TRUE)


#################
SD = auto.arima(df_joinedtime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
#S = auto.arima(df_joined$diff, allowmean=FALSE, allowdrift=FALSE, trace=TRUE) #use this for ACF PACF plots
SD
checkresiduals(S)
sD <- S$residuals
sD

acf(s)

###################################################
#### Forecast with ARIMA model
###################################################
forecastD <- forecast(S, h=100)
forecastD
autoplot(forecastD)+
  labs(x="Time", y="Residuals", title="Gage D")
checkresiduals(forecastD)


# Arrange
library(ggpubr)
library(gridExtra)
legend <- g_legend(figure)

theme_set(theme_pubr())
# ::::::::::::::::::::::::::::::::::::::::::::::::::
ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
# Use a common legend for multiple plots
ggarrange(bxp, dp,  common.legend = TRUE)

figure <- ggarrange(bxp, dens, dp,
                    labels = c("(a)", "(b)", "(c)"),
                    ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
figure
