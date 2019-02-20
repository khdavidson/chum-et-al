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
library(magrittr)
library(stringr)

################################################################################################################################################

# Note: the code below corresponds to the word doc "Mission Sockeye smolt CPUE calibrations"


################################################################################################################################################


# METHOD 1: Daily catch/daily discharge (scale: day)

# Calibrate daily total catch by daily discharge @ Hope
CPUE_discharge <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID, run, run_time) %>% 
  filter(sockeye_smolt_total != "NR", run_time == "0:15") %>%                                                                         # Remove the messy NR entries and select only 15 minute runs
  group_by(date, USID, discharge_m3s) %>%                                                                                             # Group by date, sampling event and discharge
  summarize(unique_SO = unique(sockeye_smolt_total, na.rm=T), unique_runs=unique(run)) %>%                                            # Create new variables to select the total number of sockeye in each sampling event and the number of runs
  select(date, USID, discharge_m3s, unique_SO, unique_runs) %>%                                                                       # Select those new variables as well as other ones
  group_by(date) %>%                                                                                                                  # Group by date
  summarize(total_SO = sum(unique_SO, na.rm=T), discharge = mean(discharge_m3s, na.rm=T), nruns=n_distinct(unique_runs)) %>%          # Create new variables to sum the daily total sockeye, daily discharge and number of runs in a day 
  mutate(fish_m3s = (total_SO/discharge)*1000) %>%                                                                                    # Create new column to divide daily total sockeye/daily discharge
  mutate(fish_run = (total_SO/nruns)*10) %>%                                                                                          # Create new column to divide daily total sockeye/daily number of runs (original CPUE method)
  print()

# Plot dyplyr results (Figure 1) 
CPUE_discharge$date <- as.Date(as.character(CPUE_discharge$date))

ggplot(CPUE_discharge, aes(x=date)) +
  geom_bar(aes(y=total_SO, fill="Total number of smolts"), stat="identity", colour="gray30", alpha=0.4) +
  geom_line(aes(y=fish_run, group=1, 
                colour="CPUE: Total smolts/runs (*10)", 
                linetype="CPUE: Total smolts/runs (*10)",
                size="CPUE: Total smolts/runs (*10)")) +
  geom_line(aes(y=fish_m3s, group=2, 
                colour="CPUE: Total smolts/m3/s (*1000)", 
                linetype = "CPUE: Total smolts/m3/s (*1000)",
                size="CPUE: Total smolts/m3/s (*1000)")) + 
  scale_fill_manual("", values="gray60") +
  scale_colour_manual("", values=c("CPUE: Total smolts/runs (*10)" = "black", 
                                   "CPUE: Total smolts/m3/s (*1000)" = "black")) +
  scale_linetype_manual("", values = c("CPUE: Total smolts/runs (*10)" = 3, 
                                       "CPUE: Total smolts/m3/s (*1000)" = 1)) +
  scale_size_manual("", values = c("CPUE: Total smolts/runs (*10)"=1.5, 
                                   "CPUE: Total smolts/m3/s (*1000)"=1.5)) +
  scale_x_date(limits=as.Date(c("2017-04-03", "2017-06-14")), date_breaks = "5 day", date_labels = "%m-%d") + 
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,by=100), labels = seq(0,500, by=100),
                     sec.axis = sec_axis(~., name = "CPUE")) +
  theme_bw() +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=15,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white", colour="black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "white"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y = element_text(margin=margin(t=0,r=15,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        axis.title.y.right = element_text(margin=margin(t=0,r=0,b=0,l=15), angle=90, size=30),
        legend.text = element_text(size=25),
        legend.position = c(0.76,0.88),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.spacing.y = unit(-3.5, "mm"), 
        legend.key.height = unit(2, "line"),
        legend.key.width = unit(2, "line")) +
  guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +
  ylab("Total number of smolts") +
  xlab("Date")
  



# Method 1.1: Daily catch/daily discharge ACCOUNTING FOR SAMPLING TIME (scale: day)

# Calibrate daily total catch by daily discharge @ Hope - ACCOUNT FOR SAMPLING EFFORT (TOTAL RUN TIME in sec)
CPUE_discharge_run <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID, run, run_time) %>% 
  transform(run_time=str_replace(run_time,"0:","")) %>%
  mutate_at(vars(c(5)), funs(as.numeric)) %>%
  filter(sockeye_smolt_total != "NR") %>%                                                                        
  group_by(date, USID, discharge_m3s, run, run_time) %>%                                        
  summarize(unique_SO = unique(sockeye_smolt_total, na.rm=T)) %>%
  group_by(date, run, discharge_m3s) %>%
  summarize(total_SO = sum(unique_SO), unique_time = unique(run_time)) %>%
  mutate_at(vars(c(5)), funs(as.numeric)) %>%
  group_by(date) %>% 
  summarize(total_SO = sum(total_SO), total_time = sum(unique_time*60), 
            discharge = mean(discharge_m3s, na.rm=T), runs = n_distinct(run)) %>%
  mutate(fish_run = total_SO/runs*10,
         fish_m3s = total_SO/discharge*1000,
         fish_m3 = ((total_SO/discharge)/total_time)*10000000) %>%
  print()

# Plot dyplyr results (Figure 2)
CPUE_discharge_run$date <- as.Date(as.character(CPUE_discharge_run$date))

ggplot(CPUE_discharge_run, aes(x=date)) +
  geom_line(aes(y=fish_run, group=1, 
                colour="CPUE: Total smolts/runs", 
                linetype="CPUE: Total smolts/runs",
                size="CPUE: Total smolts/runs"), alpha = 1) +
  geom_line(aes(y=fish_m3s, group=2, 
                colour="CPUE: Total smolts/m3/s", 
                linetype = "CPUE: Total smolts/m3/s",
                size="CPUE: Total smolts/m3/s"), alpha = 1) + 
  geom_line(aes(y=fish_m3, group=3, 
                colour="CPUE: Total smolts/m3", 
                linetype = "CPUE: Total smolts/m3",
                size="CPUE: Total smolts/m3"), alpha = 0.7) + 
  scale_colour_manual("", values=c("CPUE: Total smolts/runs" = "black", 
                                   "CPUE: Total smolts/m3/s" = "black",
                                   "CPUE: Total smolts/m3" = "blue")) +
  scale_linetype_manual("", values = c("CPUE: Total smolts/runs" = 3, 
                                       "CPUE: Total smolts/m3/s" = 1,
                                       "CPUE: Total smolts/m3" = 1)) +
  scale_size_manual("", values = c("CPUE: Total smolts/runs" = 1.5, 
                                   "CPUE: Total smolts/m3/s" = 1.5,
                                   "CPUE: Total smolts/m3" = 1.5)) +
  scale_x_date(limits = as.Date(c("2017-04-03", "2017-06-14")), date_breaks = "5 day", date_labels = "%m-%d") + 
  scale_y_continuous(limits=c(0,325), breaks=seq(0,325,by=75), labels = seq(0,325, by=75)) +
  theme_bw() +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=10,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y = element_text(margin=margin(t=0,r=15,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=25),
        legend.position = c(0.8,0.85),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.spacing.y = unit(-3.5, "mm"), 
        legend.key.height = unit(2, "line"),
        legend.key.width = unit(2, "line")) +
  guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +
  ylab("CPUE") +
  xlab("Date")






