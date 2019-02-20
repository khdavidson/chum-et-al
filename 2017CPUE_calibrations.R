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
cols    <- c( "c1" = "gray90", "c2" = "gray30", "c2" = "black")
shapes  <- c("s1" = 22, "s2" = 21, "s3" = 21)

ggplot(CPUE_discharge, aes(x=date)) + 
  geom_bar(aes(y=total_SO), stat="identity", fill="gray90", colour="gray70") +
  geom_line(aes(y=fish_run), size=1, linetype="dotted", colour="gray30") +
  geom_point(aes(y=fish_run), size=1, pch=21, fill="gray30", colour="black") +
  geom_line(aes(y=fish_m3s), size=1, linetype="solid", colour="black") + 
  geom_point(aes(y=fish_m3s), size=2, pch=21, fill="black",  colour="black") + 
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
#  scale_y_continuous(limits = c(0,500), sec.axis = sec_axis(~., name = "Fish per m3/s (*1000)")) +
  theme_bw() + 
  theme(plot.margin=margin(t=15,r=2,b=2,l=2),
        text = element_text(colour="black", size=12),
        axis.text.y = element_text(colour="black"),
        axis.title.y = element_text(margin=margin(t=0,r=7,b=0,l=0), face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.text.x = element_text(colour="black", angle=45, hjust=1),
        axis.title.y.right = element_text(margin=margin(t=0,r=0,b=0,l=7)),
        legend.justification = c(1, 0), 
        legend.position      = c(1, 0.1),
        legend.text          = element_text(size = 8)) +
  scale_color_manual(breaks = c("c1", "c2", "c3"), 
                                values = cols,
                                labels = c("Total daily catch", "Daily catch/number runs", "Daily catch/m3/s")) +
  scale_shape_manual(name = "Sex", 
                              breaks = c("s1", "s2", "s3"),
                              values = shapes,
                     labels = c("Total daily catch", "Daily catch/number runs", "Daily catch/m3/s")) +
  ylab("Total number of sockeye smolts and \n Daily total/number runs") +
  xlab("Date")
  





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

