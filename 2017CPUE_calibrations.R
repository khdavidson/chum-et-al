## Calibrating CPUE by daily discharge  

# WORK directory
setwd("~/`Stock assessment/Analysis/Data files")

# Load data
data <- read.csv("TEB_leftjoin.csv")

# Fix variable formats 
data$date <- as.Date(data$date)

# Libraries to use
library(dplyr)
library(ggplot2)


################################################################################################################################################

# Calibrate daily total catch by daily discharge @ Hope
CPUE_discharge <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID,run) %>% 
  filter(sockeye_smolt_total != "NR") %>% 
  group_by(date, USID, discharge_m3s) %>%
  summarize(unique_SO = unique(sockeye_smolt_total)) %>%
  select(date, USID, discharge_m3s, unique_SO) %>% 
  group_by(date) %>% 
  summarize(total_SO = sum(unique_SO), discharge = mean(discharge_m3s, na.rm=T)) %>% 
  mutate(fish_m3s = total_SO/discharge) %>%
  print()

ggplot(CPUE_discharge, aes(x=date, y=fish_m3s)) + 
  geom_line() + 
  geom_point() + 
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle=45, hjust=1))



