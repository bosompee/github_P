#################STREAMFLOW DATA DOWNLOAD IN CARB############################
library(sf)
library(dplyr)
library(readr)
library(sp)
library(rgdal)
library(ggplot2)

############ Getting Metadata about gages (elevation, drainage area etc)############
library(dataRetrieval)

## read in shapefile ofstream gages
df_gages <- read_csv("C:/Users/p739b253/Documents/newStreamGagesCARB_2a.csv")  #Gage IDs must start with zero ie. 07148400

#look at column names
names(df_gages)

# get a vector of all ID numbers
all_gages <- df_gages$Site_no.
gage <- all_gages[4]

# loopthrough gages
for (gage in all_gages){
  gage <- all_gages[4]
  daily_raw <-
    dataRetrieval::readNWISdv(siteNumbers = gage,
                              parameterCd = "00060", # discharge
                              statCd = "00003") # daily mean
  daily_trimmed <- select(daily_raw, Date, X_00060_00003, X_00060_00003_cd)
  names(daily_trimmed) <- c("Date", "discharge_cfs", "discharge_flag")
  output_path <- "C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/NHM_data/streamflowdata/"
  output_name <- paste0("Discharge_", gage, "_Raw.csv")
  write_csv(daily_trimmed, paste0(output_path, output_name))
}
