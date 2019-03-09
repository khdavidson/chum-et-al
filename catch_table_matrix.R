# Catch table matrix creation 


# What I want: CPUE will be values to start, eventually IAs. 
#______________________________________________________________
#   Event         |   Bay 2     |    Bay 6     |     Bay 11    |
#_________________|_____________|______________|_______________|
# 20170403-0600   |   0.000     |              |               |
#          0601   |   0.000     |              |               |   
#          0602   |   0.000     |              |               |   
#          0615   |   0.000     |              |               |   
#          0616   |     x       |       x      |       x       |   
#          0620   |             |    0.000     |               |   
#          0631   |             |    0.000     |               |   
#          0632   |             |    0.000     |               |   
#          0635   |             |    0.000     |               |   
#          0636   |     x       |       x      |       x       |   
#          0640   |             |              |     0.000     |   
#          0641   |             |              |     0.000     |   
#          0642   |             |              |     0.000     |   
#          0655   |             |              |     0.000     |   
#           ...   |     ...     |      ...     |      ...      |        
#______________________________________________________________


# Set wd 
setwd("~/`Stock assessment/Analysis/Data files")

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(withr)

# Read data
data <- read.csv("TEB_leftjoin.csv")

# Create proper run time length
data <- data %>%
  select(everything()) %>%
  mutate_at(vars(c(17)), funs(as.character)) %>%
  mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
  mutate_at(vars(c(17)), funs(as.numeric)) %>%
  mutate(run_time = run_time*60)


#######################################################################################################################################
#######################################################################################################################################


#                                                   THE FOLLOWING CODE CORRESPONDS TO
#                                               KD_202019_MissionSockeyeSmoltCPUECalibrations


#######################################################################################################################################

# First, simple subset data 
data <- data %>% 
  filter(trap_type == "RST") %>% 
  print()

#------------------------------------------------------------------------

####
# 1. Create time series (to be joined later)
####
ts.min <- data.frame(value=seq(ymd_hm("2017-04-03 06:07", tz=""), ymd_hm("2017-04-19 15:18", tz=""), by="1 min") )
#ts.min$date_time <- format(as.POSIXct(ts.min$date_time), '%Y-%m-%d %H:%M')


####
# 2. Calculate CPUE for each run (to be time of day eventually)
####
data <- data %>%
  filter(trap_type =="RST") %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  print()

# Summarize catch by USID and date, calculate CPUE by RUN  
CPUE <- data %>% 
  group_by(date, USID, fished_vol, set_start, set_end) %>% 
  summarize(total_n = unique(sockeye_smolt_total)) %>% 
  mutate(CPUE_run = total_n/fished_vol) %>%
  print()

####
# 3. Reformat time series 
####
CPUE$set_start <- as.character(CPUE$set_start)
CPUE$set_end <- as.character(CPUE$set_end)

# Add zero to start of 3 digit numbers 
CPUE$set_start <- with_options(c(scipen = 999), str_pad(CPUE$set_start, 4, pad = "0"))
CPUE$set_end <- with_options(c(scipen = 999), str_pad(CPUE$set_end, 4, pad = "0"))

# Add colon after ever 2 digits
CPUE <- CPUE %>% 
  ungroup() %>%
  mutate(set_start = gsub('(.{2})', '\\1:', set_start)) %>%
  mutate(set_end = gsub('(.{2})', '\\1:', set_end))

    # Remove trailing colon
    CPUE$set_start <- str_sub(CPUE$set_start, 1, str_length(CPUE$set_start)-1)
    CPUE$set_end <- str_sub(CPUE$set_end, 1, str_length(CPUE$set_end)-1)

# Make as time 
CPUE$date_start = as.POSIXct(paste(CPUE$date, CPUE$set_start),tz = "")
CPUE$date_end = as.POSIXct(paste(CPUE$date, CPUE$set_end),tz = "")

# Restructure 
CPUE <- CPUE %>% 
  select(-set_start, -set_end) %>%
  gather("time", "value", 6:7) %>% 
  print()

table <- left_join(ts.min, CPUE, by="value")


####
# 4. Join with empty time series above 
####

table <- left_join(ts.min, CPUE, by="date_time")



### IT IS JOINING BUT NOT COPYING - FIGURE THIS OUT MONDAY! 
















