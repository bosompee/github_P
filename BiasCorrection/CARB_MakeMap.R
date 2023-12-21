# CARB_MakeMap.R
#' This script is intended to load geospatial data, join it with performance 
#' metrics from a CSV file, and produce a map.

# setwd to folder with data
#setwd("C:/Users/s947z036/Desktop/CARB files_Patience")
setwd("C:/Users/p739b253/OneDrive - University of Kansas/FEWtures/CARB files_Patience")

# load packages and set theme/colors
library(tidyverse)
library(sf)
library(patchwork) # for combining plots: https://patchwork.data-imaginist.com/

## ggplot theme
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

# load and process data
## watershed boundary - get bounding box for plot
sf_watershed <- st_read("CARB_Watershed_Final/CARB_Watershed_Final.shp")
CARB_bbox <- st_bbox(sf_watershed)

## states - trim to just region of interest
sf_states <- st_read("US_States_shp/tl_2017_us_state.shp") |> 
  subset(STUSPS %in% c("KS", "CO", "TX", "OK", "NM"))

## streams - trim to just CARB
sf_streams <- st_read("nsegment_11/nsegment_11.shp")
streams_CARB <- st_within(sf_streams, sf_watershed)
i_streams_CARB <- which(lapply(streams_CARB, length) != 0)
sf_streams_CARB <- sf_streams[i_streams_CARB, ]

## NSE - turn into SF object
df_fit <- read_csv("rawdata_NSE2new.csv")
sf_fit <- st_as_sf(df_fit, coords = c("Longitude", "Latitude"), crs = st_crs(sf_watershed))
### pivot into long format for faceting
sf_fit_long <- pivot_longer(sf_fit, c("rawDATA", "FDC", "ARIMA", "FDC_ARIMA", "FDC1980_2000", "FDC2001_2016"),
                            names_to = "DataType", values_to = "NSE")
### set NSE breaks
sf_fit_long$NSE_cut <- cut(sf_fit_long$NSE, breaks = c(-Inf, -1, -0.5, 0, 0.25, 0.5, 0.75, 1),
                           labels = c("< -1", "-1 - -0.5", "-0.5 - 0", "0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1"))

### set up color scale - from https://colorbrewer2.org/#type=sequential&scheme=YlGn&n=5
pal_NSE <- c("#f03b20", "#feb24c", "#ffeda0", "#c2e699", "#78c679", "#31a354", "#006837")


# EXAMPLE: make a map of just one metric
raw<- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "rawDATA"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F)+
  labs(subtitle = "(a) Simulated versus Measured")+
  guides(colour = guide_legend(reverse=T))
raw

ggsave("Map_Example1.png", width = 190, height = 100, units = "mm")

# EXAMPLE 2: combine two maps including legends
p_FDC <- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "FDC"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F) +
  labs(subtitle = "(b) FDC Method")+
  guides(colour = guide_legend(reverse=T))
p_FDC

p_ARIMA <- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "ARIMA"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F) +
  labs(subtitle = "(c) ARIMA Method")+
  guides(colour = guide_legend(reverse=T))
p_ARIMA

p_FDC_ARIMA <- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "FDC_ARIMA"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F) +
  labs(subtitle = "(d) FDC-ARIMA Method")+
  guides(colour = guide_legend(reverse=T))
p_FDC_ARIMA

p_FDC_1980_2000 <- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "FDC1980_2000"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F) +
  labs(subtitle = "(a) FDC 1980-2000")+
  guides(colour = guide_legend(reverse=T))
p_FDC_1980_2000


p_FDC_2001_2016 <- 
  ggplot() +
  geom_sf(data = sf_states, color = "gray35", fill = NA) +
  geom_sf(data = sf_watershed, color = "black", fill = "gray65") +
  geom_sf(data = sf_streams_CARB, color = "#0082c8") +
  geom_sf(data = subset(sf_fit_long, DataType == "FDC2001_2016"), 
          aes(color = NSE_cut),
          size = 2) +
  coord_sf(xlim = CARB_bbox[c(1,3)], ylim = CARB_bbox[c(2,4)],
           expand = TRUE) +
  scale_color_manual(name = "NSE", values = pal_NSE, drop = F) +
  labs(subtitle = "(b) FDC 2001-2016")+
  guides(colour = guide_legend(reverse=T))
p_FDC_2001_2016

(p_FDC_1980_2000 + p_FDC_2001_2016) +
  plot_layout(ncol = 1, guides = "collect")

ggsave("Map_Example3.png", width = 190, height = 190, units = "mm")

(raw + p_FDC + p_ARIMA + p_FDC_ARIMA) +
  plot_layout(ncol = 2, guides = "collect")

ggsave("Map_Example2.png", width = 190, height = 190, units = "mm")
