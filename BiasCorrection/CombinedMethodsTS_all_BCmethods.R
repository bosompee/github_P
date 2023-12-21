library(zoo)
library(dplyr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(patchwork) # for combining plots: https://patchwork.data-imaginist.com/


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

df_joined_aggr <- df_joined1 |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Q= mean(Q)) |>
  as.data.frame()

df_joined_aggr1 <- df_joined1 |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Sim= mean(Sim)) |>     
  as.data.frame()


#JOIN data again
df_joined <- left_join(df_joined_aggr, df_joined_aggr1, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joined)[1]<-"Date"

###################################################

library(EcoHydRology)

library(hydrostats)

library(hydroTSM)
library(gridExtra)

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
#probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
probSim <- fdc(df_joined$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE, col="blue")
probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
df_sim_fdc <- df_joined[,c("Date", "Sim")]
df_sim_fdc$probSim <- probSim
df_sim_fdc$probSimRound <- round(df_sim_fdc$probSim, 3)
df_sim_fdc_summarize <-
  summarize(group_by(df_sim_fdc, probSimRound), Qsim_mean = mean(Sim))

#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdc <- df_joined[,c("Date", "Q")]
df_obs_fdc$probObs <- probObs
df_obs_fdc$probObsRound <- round(df_obs_fdc$probObs, 3)
df_obs_fdc_summarize <-
  summarize(group_by(df_obs_fdc, probObsRound), Qobs_mean = mean(Q))

df_fdc_joined <- left_join(df_sim_fdc_summarize, df_obs_fdc_summarize, by = c("probSimRound" = "probObsRound"))

# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joined$Qobs_meanInt <- na.approx(df_fdc_joined$Qobs_mean)
probObsInt <- fdc(df_fdc_joined$Qobs_meanInt,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Bias- corrected Simulated Flow Duration Curve',thr.shw=TRUE, col="red")
df_fdc_joined$probObsInt <- probObsInt

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joined$probObsIntRound <- round(df_fdc_joined$probObsInt, 3)
df_corrected <- left_join(df_sim_fdc, df_fdc_joined[,c("probSimRound", "Qobs_meanInt")], by = "probSimRound")
df_corrected_with_obs <- left_join(df_corrected, df_obs_fdc[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obs)[5]<-"FDCSimCorrected"


############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obs$diff <- df_corrected_with_obs$Q - df_corrected_with_obs$Sim

#converting the residual data first to time series
df_corrected_with_obstime=ts(df_corrected_with_obs[,7],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstime)  #data is in "dataframe" form

library(forecast)
library(tseries)
plot(df_corrected_with_obstime)

acf(df_corrected_with_obstime, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstime, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstime)


#################
Rmodel = auto.arima(df_corrected_with_obstime, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
Rmodel
Residual <- Rmodel$residuals
Residual

#checking again if the dataset is a time series
class(Residual)  #data is in "ts" now

plot(Residual)

acf(Residual, main = "ACF of residuals after bias correction")
pacf(Residual, main = "PACF of residuals after bias correction")

adf.test(s)

#adding the residuals to the dataset
df_corrected_with_obs <- cbind(df_corrected_with_obs, Residual)

df_corrected_with_obs <- transform(df_corrected_with_obs, ARIMASimCorrected= Residual+Sim)

#replacing the negative values with zero
ARIMASimCorrectednew <- df_corrected_with_obs $ARIMASimCorrected                     # Duplicate data frame
ARIMASimCorrectednew[df_corrected_with_obs $ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
ARIMASimCorrectednew

#Add to dataframe
df_corrected_with_obs <- cbind(df_corrected_with_obs, ARIMASimCorrectednew)


############## FDC-ARIMA ###############
# FDC has already been done so the Sim corrected FDC will be used as input for the Sim for the ARIMA
############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obs$FDC_ARIMAdiff <- df_corrected_with_obs$Q - df_corrected_with_obs$FDCSimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obs)  #data is in "dataframe" form

#converting the residual data first to time series
df_corrected_with_obstime2=ts(df_corrected_with_obs[,11],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstime2)  #data is in "dataframe" form

acf(df_corrected_with_obstime2, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstime2, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstime2)
#################
#################
Rmodel2 = auto.arima(df_corrected_with_obstime2, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
Rmodel2
FDC_ARIMA_Residual <- Rmodel2$residuals

#ACF and PACF after bias correction
acf(FDC_ARIMA_Residual, main = "ACF of residuals after bias correction")
pacf(FDC_ARIMA_Residual, main = "PACF of residuals after bias correction")

adf.test(FDC_ARIMA_Residual)

#adding the residuals to the dataset
df_corrected_with_obs <- cbind(df_corrected_with_obs, FDC_ARIMA_Residual)

df_corrected_with_obs <- transform(df_corrected_with_obs, FDC_ARIMASimCorrected= FDC_ARIMA_Residual+Sim)

#replacing the negative values with zero
FDC_ARIMASimCorrectednew <- df_corrected_with_obs $FDC_ARIMASimCorrected                     # Duplicate data frame
FDC_ARIMASimCorrectednew[df_corrected_with_obs $FDC_ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
FDC_ARIMASimCorrectednew

#Add to dataframe
df_corrected_with_obs <- cbind(df_corrected_with_obs, FDC_ARIMASimCorrectednew)

#converting from ft3/s to m3/s
df_corrected_with_obs$FDCSimCorrectedm3_s <- df_corrected_with_obs$FDCSimCorrected*0.0283
df_corrected_with_obs$ARIMASimCorrectednewm3_s <- df_corrected_with_obs$ARIMASimCorrectednew*0.0283
df_corrected_with_obs$FDC_ARIMASimCorrectednewm3_s <- df_corrected_with_obs$FDC_ARIMASimCorrectednew*0.0283
df_corrected_with_obs$Qm3_s <- df_corrected_with_obs$Q*0.0283

col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc

dp <- ggplot(data=df_corrected_with_obs, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = FDCSimCorrectedm3_s, color= "FDC"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(ARIMASimCorrectednewm3_s), color= "ARIMA"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(FDC_ARIMASimCorrectednewm3_s), color= "FDC-ARIMA"), show.legend = FALSE)+
  #geom_line(mapping = aes(y = Sim, color= "NHM Simulated"))+     #change y value
  geom_line(mapping = aes(y = Qm3_s, color= "Measured"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('FDC', 'ARIMA', 'FDC-ARIMA',  'Measured'), 
                     values = c('FDC'=col.cat.red,'ARIMA'=col.cat.grn,'FDC-ARIMA'=col.cat.yel,'Measured'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (m3/s)')+
  ggtitle('Gage A')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15)) # move title inside plot
  #theme(legend.position="bottom")+
  labs(colour = NULL)
  
dp

######################################################################
############################  GAGE B   ###############################
######################################################################
gageB <- all_gages[20]
gageB

segB <- all_gages2[20]
segB
####################################################################
# USGS data
mfB <- read.csv(paste0(path_to_usgs_data, "Discharge_", gageB, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mfB$Date <-mdy(mfB$Date)
mfB$Year <- year(mfB$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1B <- subset(mfB, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

seg_colnameB <- paste0("X", segB)
df_segB <- df[,c("Date", seg_colnameB)]

df_segB$Date <-mdy(df_segB$Date)
df_segB$Year <- year(df_segB$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_segB <- subset(df_segB, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_segB)[colnames(df_segB)=="X37684"]<-"Sim"      #change seg ID

#JOIN data
df_joinedB <- left_join(df_segB, mf1B, by = "Date")

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

###################################################

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
#probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
probSimB <- fdc(df_joinedB$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE, col="blue")
probObsB <- fdc(df_joinedB$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
df_sim_fdcB <- df_joinedB[,c("Date", "Sim")]
df_sim_fdcB$probSimB <- probSimB
df_sim_fdcB$probSimRoundB <- round(df_sim_fdcB$probSimB, 3)
df_sim_fdc_summarizeB <-
  summarize(group_by(df_sim_fdcB, probSimRoundB), Qsim_mean = mean(Sim))

#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdcB <- df_joinedB[,c("Date", "Q")]
df_obs_fdcB$probObsB <- probObsB
df_obs_fdcB$probObsRoundB <- round(df_obs_fdcB$probObsB, 3)
df_obs_fdc_summarizeB <-
  summarize(group_by(df_obs_fdcB, probObsRoundB), Qobs_mean = mean(Q))

df_fdc_joinedB <- left_join(df_sim_fdc_summarizeB, df_obs_fdc_summarizeB, by = c("probSimRoundB" = "probObsRoundB"))

# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joinedB$Qobs_meanIntB <- na.approx(df_fdc_joinedB$Qobs_mean)
probObsIntB <- fdc(df_fdc_joinedB$Qobs_meanIntB,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Bias- corrected Simulated Flow Duration Curve',thr.shw=TRUE, col="red")
df_fdc_joinedB$probObsIntB <- probObsIntB

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joinedB$probObsIntRoundB <- round(df_fdc_joinedB$probObsIntB, 3)
df_correctedB <- left_join(df_sim_fdcB, df_fdc_joinedB[,c("probSimRoundB", "Qobs_meanIntB")], by = "probSimRoundB")
df_corrected_with_obsB <- left_join(df_correctedB, df_obs_fdcB[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obsB)[5]<-"FDCSimCorrected"


############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsB$diff <- df_corrected_with_obsB$Q - df_corrected_with_obsB$Sim

#converting the residual data first to time series
df_corrected_with_obstimeB=ts(df_corrected_with_obsB[,7],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstimeB)  #data is in "dataframe" form

plot(df_corrected_with_obstimeB)

acf(df_corrected_with_obstimeB, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstimeB, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstimeB)


#################
RmodelB = auto.arima(df_corrected_with_obstimeB, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
RmodelB
ResidualB <- RmodelB$residuals
ResidualB

#checking again if the dataset is a time series
class(ResidualB)  #data is in "ts" now

plot(ResidualB)

acf(ResidualB, main = "ACF of residuals after bias correction")
pacf(ResidualB, main = "PACF of residuals after bias correction")

adf.test(ResidualB)

#adding the residuals to the dataset
df_corrected_with_obsB <- cbind(df_corrected_with_obsB, ResidualB)

df_corrected_with_obsB <- transform(df_corrected_with_obsB, ARIMASimCorrected= ResidualB+Sim)

#replacing the negative values with zero
ARIMASimCorrectednewB <- df_corrected_with_obsB $ARIMASimCorrected                     # Duplicate data frame
ARIMASimCorrectednewB[df_corrected_with_obsB $ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
ARIMASimCorrectednewB

#Add to dataframe
df_corrected_with_obsB <- cbind(df_corrected_with_obsB, ARIMASimCorrectednewB)


############## FDC-ARIMA ###############
# FDC has already been done so the Sim corrected FDC will be used as input for the Sim for the ARIMA
############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsB$FDC_ARIMAdiff <- df_corrected_with_obsB$Q - df_corrected_with_obsB$FDCSimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obsB)  #data is in "dataframe" form

#converting the residual data first to time series
df_corrected_with_obstime2B=ts(df_corrected_with_obsB[,11],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstime2B)  #data is in "dataframe" form

acf(df_corrected_with_obstime2B, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstime2B, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstime2B)
#################
#################
Rmodel2B = auto.arima(df_corrected_with_obstime2B, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
Rmodel2B
FDC_ARIMA_ResidualB <- Rmodel2B$residuals

#ACF and PACF after bias correction
acf(FDC_ARIMA_ResidualB, main = "ACF of residuals after bias correction")
pacf(FDC_ARIMA_ResidualB, main = "PACF of residuals after bias correction")

adf.test(FDC_ARIMA_ResidualB)

#adding the residuals to the dataset
df_corrected_with_obsB <- cbind(df_corrected_with_obsB, FDC_ARIMA_ResidualB)

df_corrected_with_obsB <- transform(df_corrected_with_obsB, FDC_ARIMASimCorrected= FDC_ARIMA_ResidualB+Sim)

#replacing the negative values with zero
FDC_ARIMASimCorrectednewB <- df_corrected_with_obsB $FDC_ARIMASimCorrected                     # Duplicate data frame
FDC_ARIMASimCorrectednewB[df_corrected_with_obsB $FDC_ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
FDC_ARIMASimCorrectednewB

#Add to dataframe
df_corrected_with_obsB <- cbind(df_corrected_with_obsB, FDC_ARIMASimCorrectednew)

#converting from ft3/s to m3/s
df_corrected_with_obsB$FDCSimCorrectedm3_s <- df_corrected_with_obsB$FDCSimCorrected*0.0283
df_corrected_with_obsB$ARIMASimCorrectednewm3_s <- df_corrected_with_obsB$ARIMASimCorrectednew*0.0283
df_corrected_with_obsB$FDC_ARIMASimCorrectednewm3_s <- df_corrected_with_obsB$FDC_ARIMASimCorrectednew*0.0283
df_corrected_with_obsB$Qm3_s <- df_corrected_with_obsB$Q*0.0283


dpB <- ggplot(data=df_corrected_with_obsB, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = FDCSimCorrectedm3_s, color= "FDC"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(ARIMASimCorrectednewm3_s), color= "ARIMA"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(FDC_ARIMASimCorrectednewm3_s), color= "FDC-ARIMA"), show.legend = FALSE)+
  #geom_line(mapping = aes(y = Sim, color= "NHM Simulated"))+     #change y value
  geom_line(mapping = aes(y = Qm3_s, color= "Measured"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('FDC', 'ARIMA', 'FDC-ARIMA',  'Measured'), 
                     values = c('FDC'=col.cat.red,'ARIMA'=col.cat.grn,'FDC-ARIMA'=col.cat.yel,'Measured'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (m3/s)')+
  ggtitle('Gage B')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15)) # move title inside plot
  #theme(legend.position="bottom")+
  labs(colour = NULL)

dpB

######################################################################
############################  GAGE C   ###############################
######################################################################
gageC <- all_gages[23]
gageC

segC <- all_gages2[23]
segC
####################################################################
# USGS data
mfC <- read.csv(paste0(path_to_usgs_data, "Discharge_", gageC, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mfC$Date <-mdy(mfC$Date)
mfC$Year <- year(mfC$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1C <- subset(mfC, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

seg_colnameC <- paste0("X", segC)
df_segC <- df[,c("Date", seg_colnameC)]

df_segC$Date <-mdy(df_segC$Date)
df_segC$Year <- year(df_segC$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_segC <- subset(df_segC, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_segC)[colnames(df_segC)=="X37137"]<-"Sim"      #change seg ID

#JOIN data
df_joinedC <- left_join(df_segC, mf1C, by = "Date")

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

###################################################

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
#probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
probSimC <- fdc(df_joinedC$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE, col="blue")
probObsC <- fdc(df_joinedC$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
df_sim_fdcC <- df_joinedC[,c("Date", "Sim")]
df_sim_fdcC$probSimC <- probSimC
df_sim_fdcC$probSimRoundC <- round(df_sim_fdcC$probSimC, 3)
df_sim_fdc_summarizeC <-
  summarize(group_by(df_sim_fdcC, probSimRoundC), Qsim_mean = mean(Sim))

#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdcC <- df_joinedC[,c("Date", "Q")]
df_obs_fdcC$probObsC <- probObsC
df_obs_fdcC$probObsRoundC <- round(df_obs_fdcC$probObsC, 3)
df_obs_fdc_summarizeC <-
  summarize(group_by(df_obs_fdcC, probObsRoundC), Qobs_mean = mean(Q))

df_fdc_joinedC <- left_join(df_sim_fdc_summarizeC, df_obs_fdc_summarizeC, by = c("probSimRoundC" = "probObsRoundC"))

# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joinedC$Qobs_meanIntC <- na.approx(df_fdc_joinedC$Qobs_mean)
probObsIntC <- fdc(df_fdc_joinedC$Qobs_meanIntC,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Bias- corrected Simulated Flow Duration Curve',thr.shw=TRUE, col="red")
df_fdc_joinedC$probObsIntC <- probObsIntC

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joinedC$probObsIntRoundC <- round(df_fdc_joinedC$probObsIntC, 3)
df_correctedC <- left_join(df_sim_fdcC, df_fdc_joinedC[,c("probSimRoundC", "Qobs_meanIntC")], by = "probSimRoundC")
df_corrected_with_obsC <- left_join(df_correctedC, df_obs_fdcC[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obsC)[5]<-"FDCSimCorrected"


############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsC$diff <- df_corrected_with_obsC$Q - df_corrected_with_obsC$Sim

#converting the residual data first to time series
df_corrected_with_obstimeC=ts(df_corrected_with_obsC[,7],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstimeC)  #data is in "dataframe" form

plot(df_corrected_with_obstimeC)

acf(df_corrected_with_obstimeC, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstimeC, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstimeC)


#################
RmodelC = auto.arima(df_corrected_with_obstimeC, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
RmodelC
ResidualC <- RmodelC$residuals
ResidualC

#checking again if the dataset is a time series
class(ResidualC)  #data is in "ts" now

plot(ResidualC)

acf(ResidualC, main = "ACF of residuals after bias correction")
pacf(ResidualC, main = "PACF of residuals after bias correction")

adf.test(ResidualC)

#adding the residuals to the dataset
df_corrected_with_obsC <- cbind(df_corrected_with_obsC, ResidualC)

df_corrected_with_obsC <- transform(df_corrected_with_obsC, ARIMASimCorrected= ResidualC+Sim)

#replacing the negative values with zero
ARIMASimCorrectednewC <- df_corrected_with_obsC $ARIMASimCorrected                     # Duplicate data frame
ARIMASimCorrectednewC[df_corrected_with_obsC $ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
ARIMASimCorrectednewC

#Add to dataframe
df_corrected_with_obsC <- cbind(df_corrected_with_obsC, ARIMASimCorrectednewC)


############## FDC-ARIMA ###############
# FDC has already been done so the Sim corrected FDC will be used as input for the Sim for the ARIMA
############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsC$FDC_ARIMAdiff <- df_corrected_with_obsC$Q - df_corrected_with_obsC$FDCSimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obsC)  #data is in "dataframe" form

#converting the residual data first to time series
df_corrected_with_obstime2C=ts(df_corrected_with_obsC[,11],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstime2C)  #data is in "dataframe" form

acf(df_corrected_with_obstime2C, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstime2C, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstime2C)
#################
#################
Rmodel2C = auto.arima(df_corrected_with_obstime2C, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
Rmodel2C
FDC_ARIMA_ResidualC <- Rmodel2C$residuals

#ACF and PACF after bias correction
acf(FDC_ARIMA_ResidualC, main = "ACF of residuals after bias correction")
pacf(FDC_ARIMA_ResidualC, main = "PACF of residuals after bias correction")

adf.test(FDC_ARIMA_ResidualC)

#adding the residuals to the dataset
df_corrected_with_obsC <- cbind(df_corrected_with_obsC, FDC_ARIMA_ResidualC)

df_corrected_with_obsC <- transform(df_corrected_with_obsC, FDC_ARIMASimCorrected= FDC_ARIMA_ResidualC+Sim)

#replacing the negative values with zero
FDC_ARIMASimCorrectednewC <- df_corrected_with_obsC $FDC_ARIMASimCorrected                     # Duplicate data frame
FDC_ARIMASimCorrectednewC[df_corrected_with_obsC $FDC_ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
FDC_ARIMASimCorrectednewC

#Add to dataframe
df_corrected_with_obsC <- cbind(df_corrected_with_obsC, FDC_ARIMASimCorrectednew)

#converting from ft3/s to m3/s
df_corrected_with_obsC$FDCSimCorrectedm3_s <- df_corrected_with_obsC$FDCSimCorrected*0.0283
df_corrected_with_obsC$ARIMASimCorrectednewm3_s <- df_corrected_with_obsC$ARIMASimCorrectednew*0.0283
df_corrected_with_obsC$FDC_ARIMASimCorrectednewm3_s <- df_corrected_with_obsC$FDC_ARIMASimCorrectednew*0.0283
df_corrected_with_obsC$Qm3_s <- df_corrected_with_obsC$Q*0.0283


dpC <- ggplot(data=df_corrected_with_obsC, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = FDCSimCorrectedm3_s, color= "FDC"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(ARIMASimCorrectednewm3_s), color= "ARIMA"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(FDC_ARIMASimCorrectednewm3_s), color= "FDC-ARIMA"), show.legend = FALSE)+
  #geom_line(mapping = aes(y = Sim, color= "NHM Simulated"))+     #change y value
  geom_line(mapping = aes(y = Qm3_s, color= "Measured"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('FDC', 'ARIMA', 'FDC-ARIMA',  'Measured'), 
                     values = c('FDC'=col.cat.red,'ARIMA'=col.cat.grn,'FDC-ARIMA'=col.cat.yel,'Measured'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (m3/s)')+
  ggtitle('Gage C')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15)) # move title inside plot
#theme(legend.position="bottom")+
labs(colour = NULL)

dpC

######################################################################
############################  GAGE D   ###############################
######################################################################
gageD <- all_gages[27]
gageD

segD <- all_gages2[27]
segD
####################################################################
# USGS data
mfD <- read.csv(paste0(path_to_usgs_data, "Discharge_", gageD, "_RawC.csv"))
#head(mf)

#changing the date format to standard unambigious standard
mfD$Date <-mdy(mfD$Date)
mfD$Year <- year(mfD$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

mf1D <- subset(mfD, Date >= "1980-10-01" & Year <= 2016)

##########################################################################

seg_colnameD <- paste0("X", segD)
df_segD <- df[,c("Date", seg_colnameD)]

df_segD$Date <-mdy(df_segD$Date)
df_segD$Year <- year(df_segD$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_segD <- subset(df_segD, Date >= "1980-10-01" & Year <= 2016)

# rename one column name
colnames(df_segD)[colnames(df_segD)=="X33743"]<-"Sim"      #change seg ID

#JOIN data
df_joinedD <- left_join(df_segD, mf1D, by = "Date")

#converting data from daily to monthly
df_joined1D <- df_joinedD       #Duplicate data
df_joined1D$year_month <- floor_date(df_joined1D$Date, "month")  #Create year-month column

df_joined_aggrD <- df_joined1D |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Q= mean(Q)) |>
  as.data.frame()

df_joined_aggr1D <- df_joined1D |>   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Sim= mean(Sim)) |>     
  as.data.frame()


#JOIN data again
df_joinedD <- left_join(df_joined_aggrD, df_joined_aggr1D, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joinedD)[1]<-"Date"

###################################################

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
#probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
probSimD <- fdc(df_joinedD$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Simulated Flow Duration Curve',thr.shw=TRUE, col="blue")
probObsD <- fdc(df_joinedD$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Measured Flow Duration Curve',thr.shw=TRUE, col="black")
df_sim_fdcD <- df_joinedD[,c("Date", "Sim")]
df_sim_fdcD$probSimD <- probSimD
df_sim_fdcD$probSimRoundD <- round(df_sim_fdcD$probSimD, 3)
df_sim_fdc_summarizeD <-
  summarize(group_by(df_sim_fdcD, probSimRoundD), Qsim_mean = mean(Sim))

#summarize(group_by(df_sim_fdc, probSimRound, Date), Qsim_mean = mean(Sim))
df_obs_fdcD <- df_joinedD[,c("Date", "Q")]
df_obs_fdcD$probObsD <- probObsD
df_obs_fdcD$probObsRoundD <- round(df_obs_fdcD$probObsD, 3)
df_obs_fdc_summarizeD <-
  summarize(group_by(df_obs_fdcD, probObsRoundD), Qobs_mean = mean(Q))

df_fdc_joinedD <- left_join(df_sim_fdc_summarizeD, df_obs_fdc_summarizeD, by = c("probSimRoundD" = "probObsRoundD"))

# interpolate to fill gaps in probObsRound
#interpolate missing values in 'Qobs_mean' column
df_fdc_joinedD$Qobs_meanIntD <- na.approx(df_fdc_joinedD$Qobs_mean)
probObsIntD <- fdc(df_fdc_joinedD$Qobs_meanIntD,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Bias- corrected Simulated Flow Duration Curve',thr.shw=TRUE, col="red")
df_fdc_joinedD$probObsIntD <- probObsIntD

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joinedD$probObsIntRoundD <- round(df_fdc_joinedD$probObsIntD, 3)
df_correctedD <- left_join(df_sim_fdcD, df_fdc_joinedD[,c("probSimRoundD", "Qobs_meanIntD")], by = "probSimRoundD")
df_corrected_with_obsD <- left_join(df_correctedD, df_obs_fdcD[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obsD)[5]<-"FDCSimCorrected"


############## ARIMA #########################################
#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsD$diff <- df_corrected_with_obsD$Q - df_corrected_with_obsD$Sim

#converting the residual data first to time series
df_corrected_with_obstimeD=ts(df_corrected_with_obsD[,7],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstimeD)  #data is in "dataframe" form

plot(df_corrected_with_obstimeD)

acf(df_corrected_with_obstimeD, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstimeD, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstimeD)


#################
RmodelD = auto.arima(df_corrected_with_obstimeD, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
RmodelD
ResidualD <- RmodelD$residuals
ResidualD

#checking again if the dataset is a time series
class(ResidualD)  #data is in "ts" now

plot(ResidualD)

acf(ResidualD, main = "ACF of residuals after bias correction")
pacf(ResidualD, main = "PACF of residuals after bias correction")

adf.test(ResidualD)

#adding the residuals to the dataset
df_corrected_with_obsD <- cbind(df_corrected_with_obsD, ResidualD)

df_corrected_with_obsD <- transform(df_corrected_with_obsD, ARIMASimCorrected= ResidualD+Sim)

#replacing the negative values with zero
ARIMASimCorrectednewD <- df_corrected_with_obsD $ARIMASimCorrected                     # Duplicate data frame
ARIMASimCorrectednewD[df_corrected_with_obsD $ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
ARIMASimCorrectednewD

#Add to dataframe
df_corrected_with_obsD <- cbind(df_corrected_with_obsD, ARIMASimCorrectednewD)


############## FDC-ARIMA ###############
# FDC has already been done so the Sim corrected FDC will be used as input for the Sim for the ARIMA
############## ARIMA ###############

#First, the residual series was obtained by subtracting the simulated flow from that of the observed.
df_corrected_with_obsD$FDC_ARIMAdiff <- df_corrected_with_obsD$Q - df_corrected_with_obsD$FDCSimCorrected


#checking if the dataset is a time series
class(df_corrected_with_obsD)  #data is in "dataframe" form

#converting the residual data first to time series
df_corrected_with_obstime2D=ts(df_corrected_with_obsD[,11],start=c(1980,10),frequency = 12)

#checking if the residual dataset is a time series
class(df_corrected_with_obstime2D)  #data is in "dataframe" form

acf(df_corrected_with_obstime2D, main= "ACF of residuals before bias correction")
pacf(df_corrected_with_obstime2D, main = "PACF of residuals before bias correction")

adf.test(df_corrected_with_obstime2D)
#################
#################
Rmodel2D = auto.arima(df_corrected_with_obstime2D, allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
Rmodel2D
FDC_ARIMA_ResidualD <- Rmodel2D$residuals

#ACF and PACF after bias correction
acf(FDC_ARIMA_ResidualD, main = "ACF of residuals after bias correction")
pacf(FDC_ARIMA_ResidualD, main = "PACF of residuals after bias correction")

adf.test(FDC_ARIMA_ResidualD)

#adding the residuals to the dataset
df_corrected_with_obsD <- cbind(df_corrected_with_obsD, FDC_ARIMA_ResidualD)

df_corrected_with_obsD <- transform(df_corrected_with_obsD, FDC_ARIMASimCorrected= FDC_ARIMA_ResidualD+Sim)

#replacing the negative values with zero
FDC_ARIMASimCorrectednewD <- df_corrected_with_obsD $FDC_ARIMASimCorrected                     # Duplicate data frame
FDC_ARIMASimCorrectednewD[df_corrected_with_obsD $FDC_ARIMASimCorrected  < 0] <- 0     # Set negative values to 0
FDC_ARIMASimCorrectednewD

#Add to dataframe
df_corrected_with_obsD <- cbind(df_corrected_with_obsD, FDC_ARIMASimCorrectednew)

#converting from ft3/s to m3/s
df_corrected_with_obsD$FDCSimCorrectedm3_s <- df_corrected_with_obsD$FDCSimCorrected*0.0283
df_corrected_with_obsD$ARIMASimCorrectednewm3_s <- df_corrected_with_obsD$ARIMASimCorrectednew*0.0283
df_corrected_with_obsD$FDC_ARIMASimCorrectednewm3_s <- df_corrected_with_obsD$FDC_ARIMASimCorrectednew*0.0283
df_corrected_with_obsD$Qm3_s <- df_corrected_with_obsD$Q*0.0283

dpD <- ggplot(data=df_corrected_with_obsD, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = FDCSimCorrectedm3_s, color= "FDC"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(ARIMASimCorrectednewm3_s), color= "ARIMA"), show.legend = FALSE)+
  geom_line(mapping = aes(y = as.numeric(FDC_ARIMASimCorrectednewm3_s), color= "FDC-ARIMA"), show.legend = FALSE)+
  #geom_line(mapping = aes(y = Sim, color= "NHM Simulated"))+     #change y value
  geom_line(mapping = aes(y = Qm3_s, color= "Measured"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('FDC', 'ARIMA', 'FDC-ARIMA',  'Measured'), 
                     values = c('FDC'=col.cat.red,'ARIMA'=col.cat.grn,'FDC-ARIMA'=col.cat.yel,'Measured'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (m3/s)')+
  ggtitle('Gage D')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15)) # move title inside plot
#theme(legend.position="bottom")+
labs(colour = NULL)

dpD

m <- (dp + dpB + dpC + dpD) +
  plot_layout(ncol = 1, guides = "collect") & theme(legend.position = 'bottom')

ggsave("Map_ExampleABCD.png", width = 190, height = 190, units = "mm")


probSim
probObs
# Basic scatter plot
V<- ggplot(df_corrected_with_obs, aes(x=probSim, y=FDCSimCorrected)) + geom_point(colour = "red")+
  #geom_line(size=0.5)+
  labs(title = "Gage A")+
  labs(x = "Simulated FDC", y = "Bias-Corrected FDC")
V

(probSim + probObs + V) + plot_layout(nrow = 3, ncol = 1, guides = "collect")

#library(ggpubr)

#See ?ggarrange

combined_plot <- ggarrange(probSim,
                           V,
                           nrow = 2,
                           ncol = 2) #nrow & ncol depend on how you want to 
combined_plot
#organize your plots


#(probSim + plot_spacer() + probObs) / (plot_spacer() + V + plot_spacer())
