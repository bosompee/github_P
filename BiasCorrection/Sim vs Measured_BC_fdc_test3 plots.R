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

mf1 <- subset(mf, Date >= "2001-01-01" & Year <= 2016)

##########################################################################

# NHM data
df <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colname <- paste0("X", seg)
df_seg <- df[,c("Date", seg_colname)]

df_seg$Date <-mdy(df_seg$Date)
df_seg$Year <- year(df_seg$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1 <- subset(df_seg, Date >= "2001-01-01" & Year <= 2016)

# rename one column name
colnames(df_seg1)[colnames(df_seg1)=="X34442"]<-"Sim"      #change seg ID

#JOIN data
df_joined <- left_join(df_seg1, mf1, by = "Date")

#converting data from daily to monthly
df_joined1 <- df_joined       #Duplicate data
df_joined1$year_month <- floor_date(df_joined1$Date, "month")  #Create year-month column

df_joined_aggr <- df_joined1 %>%   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Q= mean(Q)) |>
  as.data.frame()

df_joined_aggr1 <- df_joined1 %>%   #Agrregate data
  group_by(year_month) |>
  dplyr::summarize(Sim= mean(Sim)) |>     
  as.data.frame()


#JOIN data again
df_joined <- left_join(df_joined_aggr, df_joined_aggr1, by = "year_month")

#change column name "year_month" to "Date"
colnames(df_joined)[1]<-"Date"

library(EcoHydRology)

library(hydrostats)

library(hydroTSM)
library(gridExtra)


# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
probObs <- fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
probSim <- fdc(df_joined$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)

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
probObsInt <- fdc(df_fdc_joined$Qobs_meanInt,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
df_fdc_joined$probObsInt <- probObsInt

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joined$probObsIntRound <- round(df_fdc_joined$probObsInt, 3)
df_corrected <- left_join(df_sim_fdc, df_fdc_joined[,c("probSimRound", "Qobs_meanInt")], by = "probSimRound")
df_corrected_with_obs <- left_join(df_corrected, df_obs_fdc[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obs)[5]<-"SimCorrected"

#############################
# d1$Date <- mdy(d1$Date)

bxp <- ggplot(data=df_corrected_with_obs, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrected, color= "SimCorrected"))+
  geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Bias Correction', 'Simulated', 'Measured'), values = c('SimCorrected'='red', 'Sim'='blue', 'Q'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  guides(color= guide_legend(title = ''))+
  ggtitle('Gage 07108900')+
  theme(legend.position = "top")+
  guides(color= guide_legend(title = ''))
#guides(color= guide_legend)
bxp

##########################################################################################
##########################################################################################

# get a vector of all ID numbers

gageC <- all_gages[17]
gageC

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

segC <- all_gages2[17]
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

mf1C <- subset(mfC, Date >= "2001-01-01" & Year <= 2016)

##########################################################################

# NHM data
dfC <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colnameC <- paste0("X", segC)
df_segC <- df[,c("Date", seg_colnameC)]

df_segC$Date <-mdy(df_segC$Date)
df_segC$Year <- year(df_segC$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1C <- subset(df_segC, Date >= "2001-01-01" & Year <= 2016)

# rename one column name
colnames(df_seg1C)[colnames(df_seg1C)=="X37513"]<-"Sim"      #change seg ID

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

# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
probObsC <- fdc(df_joinedC$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
probSimC <- fdc(df_joinedC$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)

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
probObsIntC <- fdc(df_fdc_joinedC$Qobs_meanIntC,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
df_fdc_joinedC$probObsIntC <- probObsIntC

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joinedC$probObsIntRoundC <- round(df_fdc_joinedC$probObsIntC, 3)
df_correctedC <- left_join(df_sim_fdcC, df_fdc_joinedC[,c("probSimRoundC", "Qobs_meanIntC")], by = "probSimRoundC")
df_corrected_with_obsC <- left_join(df_correctedC, df_obs_fdcC[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obsC)[5]<-"SimCorrected"

#############################
# d1$Date <- mdy(d1$Date)

dens <- ggplot(data=df_corrected_with_obsC, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrected, color= "SimCorrected"))+
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

gageB <- all_gages[20]
gageB

# get a vector of all ID numbers
all_gages2 <- df_gages$Segment
all_gages2

segB <- all_gages2[20]
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

mf1B <- subset(mfB, Date >= "2001-01-01" & Year <= 2016)

##########################################################################

# NHM data
dfB <- read.csv("C:/Users/p739b253/Documents/nhm_sf.csv")
#head(df)
seg_colnameB <- paste0("X", segB)
df_segB <- df[,c("Date", seg_colnameB)]

df_segB$Date <-mdy(df_segB$Date)
df_segB$Year <- year(df_segB$Date)
#mf <- subset(mf, Year >= 1980 & Year <= 2016)

df_seg1B <- subset(df_segB, Date >= "2001-01-01" & Year <= 2016)

# rename one column name
colnames(df_seg1B)[colnames(df_seg1B)=="X37684"]<-"Sim"      #change seg ID

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


# FDC for Sim and Obs
#fdc(df_joined$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Upstream monthly duration curve',thr.shw=TRUE)
probObsB <- fdc(df_joinedB$Q,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
probSimB <- fdc(df_joinedB$Sim,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)

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
probObsIntB <- fdc(df_fdc_joinedB$Qobs_meanIntB,plot = T,lQ.thr=0.8,ylab='Q, [ft3/s]',main='Flow Duration Curve',thr.shw=TRUE)
df_fdc_joinedB$probObsIntB <- probObsIntB

# to correct df_joined, make a column in df_joined called probSimRound where probSim is rounded to 3 places
df_fdc_joinedB$probObsIntRoundB <- round(df_fdc_joinedB$probObsIntB, 3)
df_correctedB <- left_join(df_sim_fdcB, df_fdc_joinedB[,c("probSimRoundB", "Qobs_meanIntB")], by = "probSimRoundB")
df_corrected_with_obsB <- left_join(df_correctedB, df_obs_fdcB[,c("Date", "Q")], by = "Date")

#change column name "Qobs_meanInt" to "SimBC"
colnames(df_corrected_with_obsB)[5]<-"SimCorrected"

#############################
# d1$Date <- mdy(d1$Date)

dp <- ggplot(data=df_corrected_with_obsB, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = SimCorrected, color= "SimCorrected"))+
  geom_line(mapping = aes(y = Sim, color= "Sim"))+     #change y value
  geom_line(mapping = aes(y = Q, color= "Q"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('After Bias Correction', 'Simulated', 'Measured'), values = c('SimCorrected'='red', 'Sim'='blue', 'Q'='black'))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage 07141300')+
  theme(legend.position="none")

dp

# Arrange
library(ggpubr)
#library(gridExtra)

theme_set(theme_pubr())
# ::::::::::::::::::::::::::::::::::::::::::::::::::
ggarrange(bxp, dp, dens, ncol = 2, nrow = 2)
# Use a common legend for multiple plots
ggarrange(bxp, dp,  common.legend = TRUE)

figure <- ggarrange(bxp, dens, dp,
                    labels = c("(a)", "(b)", "(c)"),
                    ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom")
figure
