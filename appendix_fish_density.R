## Calibrating CPUE  

# WORK directory
setwd("~/`Stock assessment/Analysis/Data files")

# Load data
data <- read.csv("TEB_leftjoin.csv")

# Fix variable formats 
data$date <- as.Date(data$date)

# Packages to use
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
library(grid)
library(gridExtra)
library(scales)

# Need to make a new column for run time (sec) - the old one was derived from Basecamp tracks and might not be actually "true" - instead
# convert the rounded run times (min) to represent run times in seconds. A coarse estimate, given the resolution is at the minute-level,
# but probably more representative of actual sampling time. 
data <- data %>%
  select(everything()) %>%
  mutate_at(vars(c(17)), funs(as.character)) %>%
  mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
  mutate_at(vars(c(17)), funs(as.numeric)) %>%
  mutate(run_time_der = run_time*60)


################################################################################################################################################
################################################################################################################################################


# Daily catch/daily discharge (scale: day)


# Calibrate daily total catch by daily discharge @ Hope
CPUE_discharge <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID, run, run_time) %>% 
  filter(sockeye_smolt_total != "NR", run_time == "15") %>%                                                                         # Remove the messy NR entries and select only 15 minute runs
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
  geom_bar(aes(y=total_SO, fill="Total number of Fish"), stat="identity", colour="gray30", alpha=0.4) +
  geom_line(aes(y=fish_run, group=1, 
                colour="CPUE: Total Fish/runs (*10)", 
                linetype="CPUE: Total Fish/runs (*10)",
                size="CPUE: Total Fish/runs (*10)")) +
  geom_line(aes(y=fish_m3s, group=2, 
                colour="CPUE: Total Fish/m3/s (*1000)", 
                linetype = "CPUE: Total Fish/m3/s (*1000)",
                size="CPUE: Total Fish/m3/s (*1000)")) + 
  scale_fill_manual("", values="gray60") +
  scale_colour_manual("", values=c("CPUE: Total Fish/runs (*10)" = "black", 
                                   "CPUE: Total Fish/m3/s (*1000)" = "black")) +
  scale_linetype_manual("", values = c("CPUE: Total Fish/runs (*10)" = 3, 
                                       "CPUE: Total Fish/m3/s (*1000)" = 1)) +
  scale_size_manual("", values = c("CPUE: Total Fish/runs (*10)"=1.5, 
                                   "CPUE: Total Fish/m3/s (*1000)"=1.5)) +
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
  ylab("Total number of fish") +
  xlab("Date")




# Method 1.2: Daily catch/daily discharge ACCOUNTING FOR SAMPLING TIME (scale: day)

# Calibrate daily total catch by daily discharge @ Hope - ACCOUNT FOR SAMPLING EFFORT (TOTAL RUN TIME in sec)
CPUE_discharge_run <- data %>%
  select(sockeye_smolt_total, discharge_m3s, date, USID, run, run_time_der) %>% 
  filter(sockeye_smolt_total != "NR") %>%                                                                        
  group_by(date, USID, discharge_m3s, run, run_time_der) %>%                                        
  summarize(unique_SO = unique(sockeye_smolt_total, na.rm=T)) %>%
  group_by(date, run, discharge_m3s) %>%
  summarize(total_SO = sum(unique_SO), unique_time = unique(run_time_der)) %>%
  group_by(date) %>% 
  summarize(total_SO = sum(total_SO), total_time = sum(unique_time), 
            discharge = mean(discharge_m3s, na.rm=T), runs = n_distinct(run)) %>%
  mutate(fish_run = total_SO/runs*10,
         fish_m3s = total_SO/discharge*1000,
         fish_m3 = ((total_SO/discharge)/total_time)*10000000) %>%
  print()

# Plot dyplyr results (Figure 2)
ggplot(CPUE_discharge_run, aes(x=date)) +
  geom_line(aes(y=fish_run, group=1, 
                colour="CPUE: Total Fish/runs", 
                linetype="CPUE: Total Fish/runs",
                size="CPUE: Total Fish/runs"), alpha = 1) +
  geom_line(aes(y=fish_m3s, group=2, 
                colour="CPUE: Total Fish/m3/s", 
                linetype = "CPUE: Total Fish/m3/s",
                size="CPUE: Total Fish/m3/s"), alpha = 1) + 
  geom_line(aes(y=fish_m3, group=3, 
                colour="CPUE: Total Fish/m3", 
                linetype = "CPUE: Total Fish/m3",
                size="CPUE: Total Fish/m3"), alpha = 0.7) + 
  scale_colour_manual("", values=c("CPUE: Total Fish/runs" = "black", 
                                   "CPUE: Total Fish/m3/s" = "black",
                                   "CPUE: Total Fish/m3" = "blue")) +
  scale_linetype_manual("", values = c("CPUE: Total Fish/runs" = 3, 
                                       "CPUE: Total Fish/m3/s" = 1,
                                       "CPUE: Total Fish/m3" = 1)) +
  scale_size_manual("", values = c("CPUE: Total Fish/runs" = 1.5, 
                                   "CPUE: Total Fish/m3/s" = 1.5,
                                   "CPUE: Total Fish/m3" = 1.5)) +
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



#########################################################################################################################################


# CALIBRATE CATCH BY VELOCITY - Calculate velocity for each date-run event 


# Extract current velocity and # sockeye to calculate # FISH/M/S
bay_flow <- data %>% 
  select(date, USID, run, bay, current_speed_mps, NEW_set_start, sockeye_smolt_total) %>% 
  filter(current_speed_mps != "#DIV/0!", current_speed_mps > 0) %>%                                      # 135 entries lost due to no GPS data
  group_by(date, USID, run, bay, NEW_set_start) %>% 
  summarize(current = unique(current_speed_mps), catch = unique(sockeye_smolt_total, na.rm=T)) %>% 
  group_by(date, run, bay, NEW_set_start) %>%
  summarize(current = unique(current), total = sum(catch, na.rm=T)) %>% 
  mutate(fish_ms = total/current) %>%
  group_by(date, bay) %>% 
  summarize(sum_total = sum(total), sum_fish_ms=sum(fish_ms)) %>% 
  print()


# FIGURE 4: Plot original total catch and fish/m/s  
bay_flow$bay <- factor(bay_flow$bay, levels=c("B2", "B6", "B11"), ordered=T)
# Fig 4A
total<-ggplot(bay_flow, aes(colour=bay, fill=bay)) +
  geom_vline(aes(xintercept = date), col="gray60") +
  geom_bar(stat="identity", aes(x=date, y=sum_total), position="dodge") +
  scale_x_date(limits=as.Date(c("2017-04-22", "2017-05-18")), breaks = "2 day", date_labels = "%m-%d") +
  scale_y_continuous(limits=c(0,400)) +
  scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                    name="",
                    breaks=c("B2", "B6", "B11"),
                    labels=c("Bay 2", "Bay 6", "Bay 11")) +
  scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                      name="",
                      breaks=c("B2", "B6", "B11"),
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=10,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y = element_text(margin=margin(t=50,r=25,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
        legend.title = element_blank(),
        legend.text = element_text(size=25),
        legend.position = c(0.1,0.7),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylab("Total \n smolts")
# Fig 4B
fish_ms<-ggplot(bay_flow, aes(colour=bay,fill=bay, group=bay)) +
  geom_vline(aes(xintercept = date), col="gray60") +
  geom_bar(stat="identity", aes(x=date, y=sum_fish_ms), position="dodge") +
  scale_x_date(limits=as.Date(c("2017-04-22", "2017-05-18")), breaks = "2 day", date_labels = "%m-%d") +
  scale_y_continuous(limits=c(0,400)) +
  scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                    name="",
                    breaks=c("B2", "B6", "B11"),
                    labels=c("Bay 2", "Bay 6", "Bay 11")) +
  scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                      name="",
                      breaks=c("B2", "B6", "B11"),
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=10,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y = element_text(margin=margin(t=0,r=25,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
        legend.position = "none") +
  ylab("Total \n smolts/m/s")


# Also consider sampling time lengths - Standardizing by bay flow, accounting for run length (sec)
# Extract variables as above, as well as sampling time (sec) 
bay_flow_sec <- data %>% 
  select(date, USID, run, bay, current_speed_mps, NEW_set_start, sockeye_smolt_total, run_time_der) %>% 
  filter(current_speed_mps != "#DIV/0!", current_speed_mps > 0) %>%                                      # 135 entries lost due to no GPS data
  group_by(date, USID, run, bay, NEW_set_start) %>% 
  summarize(current = unique(current_speed_mps), catch = unique(sockeye_smolt_total, na.rm=T), unq_sec = unique(run_time_der)) %>% 
  group_by(date, run, bay, NEW_set_start) %>%
  summarize(current = unique(current), total = sum(catch, na.rm=T), run_sec = unique(unq_sec)) %>% 
  mutate(fish_ms = total/current) %>%
  mutate(fish_m = fish_ms/run_sec) %>% 
  group_by(date, bay) %>% 
  summarize(sum_total = sum(total), sum_fish_ms=sum(fish_ms), sum_fish_m = sum(fish_m)*100) %>% 
  print()

#Fig 4C
fish_m<-ggplot(bay_flow_sec, aes(colour=bay,fill=bay, group=bay)) +
  geom_vline(aes(xintercept = date), col="gray60") +
  geom_bar(stat="identity", aes(x=date, y=sum_fish_m), position="dodge") +
  scale_x_date(limits=as.Date(c("2017-04-22", "2017-05-18")), breaks = "2 day", date_labels = "%m-%d") +
  scale_y_continuous(limits=c(0,50)) +
  scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                    name="",
                    breaks=c("B2", "B6", "B11"),
                    labels=c("Bay 2", "Bay 6", "Bay 11")) +
  scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                      name="",
                      breaks=c("B2", "B6", "B11"),
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=10,r=10,b=2,l=2),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y = element_text(margin=margin(t=0,r=25,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=2,l=0), face="bold", size=30),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
        legend.position = "none") +
  xlab("Date") +
  ylab("Total \n smolts/m")

# Fig 4ABC draw
grid.newpage()
grid.draw(rbind(ggplotGrob(total), ggplotGrob(fish_ms), ggplotGrob(fish_m), size="last"))
grid.arrange(total, fish_ms, ncol=2)
# Looking at Figure 4, dramatic changes occur when accounting for run time - suddenly the catch in Bay 11 is highest, 
# which doesn't seem correct. -- I don't think it is correct. For now should just deal with 15 minute runs only for simplicity 

# Are there differences in run time length among bays? 
bay_runs <- data %>% 
  select(date, USID, run, bay, current_speed_mps, NEW_set_start, sockeye_smolt_total, run_time_der) %>% 
  filter(current_speed_mps != "#DIV/0!", current_speed_mps > 0) %>%                                      # 135 entries lost due to no GPS data
  group_by(date, USID, run, bay, NEW_set_start) %>% 
  summarize(current = unique(current_speed_mps), catch = unique(sockeye_smolt_total, na.rm=T), unq_sec = unique(run_time_der)) %>% 
  group_by(date, run, bay, NEW_set_start) %>%
  summarize(current = unique(current), total = sum(catch, na.rm=T), run_sec = unique(unq_sec)) %>%
  group_by(bay) %>% 
  print()

lm1<-lm(bay_runs$run_sec ~ bay_runs$bay)
r1<-resid(lm1)
plot(r1)
hist(r1)
qqnorm(r1)
qqline(r1)

a1 <- aov(bay_runs$run_sec ~ bay_runs$bay)
summary(lm1)
TukeyHSD(a1)

