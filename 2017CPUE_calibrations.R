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
library(tidyr)


################################################################################################################################################

# Calibrate daily total catch by daily discharge @ Hope
CPUE_discharge <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID, run, run_time) %>% 
  filter(sockeye_smolt_total != "NR", run_time == "0:15") %>%                                                                # Remove the messy NR entries and select only 15 minute runs
  group_by(date, USID, discharge_m3s) %>%                                                                                    # Group by date, sampling event and discharge
  summarize(unique_SO = unique(sockeye_smolt_total, na.rm=T), unique_runs=unique(run)) %>%                                            # Create new variables to select the total number of sockeye in each sampling event and the number of runs
  select(date, USID, discharge_m3s, unique_SO, unique_runs) %>%                                                              # Select those new variables as well as other ones
  group_by(date) %>%                                                                                                         # Group by date
  summarize(total_SO = sum(unique_SO, na.rm=T), discharge = mean(discharge_m3s, na.rm=T), nruns=n_distinct(unique_runs)) %>%          # Create new variables to sum the daily total sockeye, daily discharge and number of runs in a day 
  mutate(fish_m3s = (total_SO/discharge)*1000) %>%                                                                                  # Create new column to divide daily total sockeye/daily discharge
  mutate(fish_run = (total_SO/nruns)*10) %>%                                                                                      # Create new column to divide daily total sockeye/daily number of runs (original CPUE method)
  print()

# Plotting original CPUE (total daily fish/daily number of runs) and new CPUE (total daily fish/daily discharge)
    # First restructure data so ggplot2 can deal with it
    CPUE_discharge <- gather(CPUE_discharge,series,value,-date)
    CPUE_discharge <- CPUE_discharge %>%
      filter(series != "discharge", series != "nruns")


ggplot(CPUE_discharge, aes(x=date)) +
  geom_bar(aes(y=total_SO, fill="Total number of sockeye"), stat="identity", colour="gray70", alpha=0.2) +
  geom_line(aes(y=fish_run, group=1, colour="Total sockeye/runs (*10)", linetype="Total sockeye/runs (*10)"), size=1) +
  geom_line(aes(y=fish_m3s, group=2, colour="Total sockeye/m3/s (*1000)", linetype = "Total sockeye/m3/s (*1000)"), size=1) + 
  scale_colour_manual("", values=c("Total sockeye/runs (*10)" = "gray20", "Total sockeye/m3/s (*1000)" = "black")) +
  scale_fill_manual("", values="gray60") +
  scale_linetype_manual("", values = c("Total sockeye/runs (*10)" = 3, 
                                       "Total sockeye/m3/s (*1000)" = 1)) +
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  theme_bw()+
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=15,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.title.y = element_text(margin=margin(t=0,r=7,b=0,l=0), face="bold", size=12),
        axis.text.y = element_text(colour="black", size=10),
        axis.title.x = element_text(margin=margin(t=7,r=0,b=2,l=0), face="bold", size=12),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=10),
        legend.text = element_text(size=12),
        legend.position = c(0.11,0.8)) +
  ylab("Daily catch") +
  xlab("Date")
  
#  geom_point(aes(y=fish_m3s), size=2, pch=21, fill="black",  colour="black") + 
# geom_point(aes(y=fish_run), size=1, pch=21, fill="gray30", colour="black") +




ggplot(CPUE_discharge, aes(x=date)) + 
  geom_line(aes(y=fish_m3s), size=1, linetype="solid", colour="black") + 
  geom_point(aes(y=fish_m3s), size=3, pch=21, fill="black",  colour="black") + 
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  theme_bw() + 
  theme(text = element_text(colour="black", size=13),
        axis.text.y = element_text(colour="black"),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.text.x = element_text(colour="black", angle=45, hjust=1)) 

