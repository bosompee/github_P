library(zoo)
library(dplyr)
library(magrittr)
library(tidyverse)
library(lubridate)
library(patchwork) # for combining plots: https://patchwork.data-imaginist.com/

col.cat.grn <- "#3cb44b"   # green
col.cat.yel <- "#ffe119"   # yellow
col.cat.org <- "#f58231"   # orange
col.cat.red <- "#e6194b"   # red
col.cat.blu <- "#0082c8"   # blue
col.gray <- "gray65"       # gray for annotation lines, etc


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

dp <- ggplot(data=df_joined, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = Q, color= "Measured"), show.legend = FALSE)+
  geom_line(mapping = aes(y = Sim, color= "NHM Simulated"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Measured', 'NHM Simulated'), 
                     values = c('Measured'='black', 'NHM Simulated'=col.cat.blu))+
  #labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage A')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15)) # move title inside plot
  #theme(legend.position="bottom")+
  labs(colour = NULL)+
  theme(legend.position = "none")

dp

ggtitle("Plot Title") +
  theme(plot.title = element_text(hjust = 1, vjust = -10))

############################################################################################################
##########################    Gage B    ####################################################################
############################################################################################################
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

dpB <- ggplot(data=df_joinedB, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = Q, color= "Measured"), show.legend = FALSE)+
  geom_line(mapping = aes(y = Sim, color= "NHM Simulated"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Measured', 'NHM Simulated'), 
                     values = c('Measured'='black', 'NHM Simulated'=col.cat.blu))+
  #labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage B')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15))
  theme(legend.position="bottom")+
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

dpC <- ggplot(data=df_joinedC, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = Q, color= "Measured"), show.legend = FALSE)+
  geom_line(mapping = aes(y = Sim, color= "NHM Simulated"), show.legend = FALSE)+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Measured', 'NHM Simulated'), 
                     values = c('Measured'='black', 'NHM Simulated'=col.cat.blu))+
  #labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage C')+
  theme(axis.title.x=element_blank())+
  #axis.text.x=element_blank())+
  theme(plot.title = element_text(vjust = -15))
  theme(legend.position="bottom")+
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

dpD <- ggplot(data=df_joinedD, mapping=aes( x = Date))+
  geom_line(mapping = aes(y = Q, color= "Measured"))+
  geom_line(mapping = aes(y = Sim, color= "NHM Simulated"))+
  scale_x_date(expand = c(0,0)) +
  #scale_x_continuous(breaks = seq(1980,2016,4))+
  scale_color_manual(labels=c('Measured', 'NHM Simulated'), 
                     values = c('Measured'='black', 'NHM Simulated'=col.cat.blu))+
  labs(x= 'Time [monthly data]', y= 'Q (ft3/s)')+
  ggtitle('Gage D')+
  theme(plot.title = element_text(vjust = -15))+
  theme(legend.position="bottom")+
  labs(colour = NULL)

dpD

(dp + dpB + dpC + dpD) +
  plot_layout(ncol = 1, guides = "collect") & theme(legend.position = 'bottom')

ggsave("Map_ExampleABCD1.png", width = 190, height = 190, units = "mm")




