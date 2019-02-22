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
library(grid)
library(gridExtra)

################################################################################################################################################
################################################################################################################################################


# Note: the code below corresponds to the word doc "Mission Sockeye smolt CPUE calibrations"


################################################################################################################################################


# METHOD 1.1: Daily catch/daily discharge (scale: day)

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
  



# Method 1.2: Daily catch/daily discharge ACCOUNTING FOR SAMPLING TIME (scale: day)

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


################################################################################################################################################


# METHOD 2: Standardizing by bay flow (scale: bay, 3 levels)



# FIRST SOME EXPLORATORY RELATIONSHIPS
# Extract current speed ~ bay, date, time
flow <- data %>% 
  select(date, USID, bay, run, current_speed_mps, NEW_set_start) %>% 
  filter(current_speed_mps != "#DIV/0!", current_speed_mps > 0) %>%                                    # 135 entries lost due to no GPS data
  group_by(date, USID, run, bay, NEW_set_start) %>% 
  summarize(unq_flow = unique(current_speed_mps), unq_start = unique(NEW_set_start)) %>% 
  group_by(date, bay, unq_start) %>% 
  summarize(flow = unique(unq_flow)) %>%
  mutate_at(vars(c(3)), funs(as.character)) %>%
  mutate(unq_start = paste(gsub(":", "", unq_start))) %>%
  mutate_at(vars(c(3)), funs(as.numeric)) %>%
  print()

    # Test: flow ~ bay + date + time of day (based on start time) 
    lm1 <- lm(log(flow) ~ unq_start, data=flow)
    r1 <- resid(lm1)
    plot(r1)
    hist(r1)
    qqnorm(r1)
    qqline(r1)
    
    lm2 <- lm(log10(flow) ~ bay, data=flow)
    r2 <- resid(lm2)
    plot(r2)
    hist(r2)
    qqnorm(r2)
    qqline(r2)
    a2 <- aov(log10(flow) ~ bay, data=flow)
    summary(a2)
    TukeyHSD(a2)
    
    lm3 <- lm(flow ~ date*bay, data=flow)
    r3 <- resid(lm3)
    plot(lm3)
    plot(r3)
    hist(r3)
    qqnorm(r3)
    qqline(r3)



# Afterthought - also get current speed ~ discharge 
flow_dis <- data %>% 
  select(date, discharge_m3s, current_speed_mps, bay) %>% 
  group_by(date, bay) %>% 
  summarize(discharge = mean(discharge_m3s, na.rm=T), current = mean(current_speed_mps, na.rm=T))
    
    
    # Plot current speed ~ date, current speed ~ time of day, current speed ~ discharge
    date<-ggplot(flow, aes(fill=bay)) + 
      geom_point(aes(x=date, y=flow), pch=21, size=5) +
      scale_x_date(limits = as.Date(c("2017-04-03", "2017-06-14")), date_breaks = "7 day", date_labels = "%m-%d") +
      scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                        name="",
                        breaks=c("B2", "B6", "B11"),
                        labels=c("Bay 2", "Bay 6", "Bay 11")) +
      theme_bw() +
      theme(text = element_text(colour="black", size=12),
            plot.margin=margin(t=10,r=10,b=2,l=2),
            panel.background = element_rect(fill = "white", colour = "black", size=2),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.grid.major = element_line(colour = "transparent"),
            plot.background = element_rect(fill = "transparent"),
            axis.ticks = element_line(size=1.2),
            axis.ticks.length = unit(0.5, "line"),
            axis.title.y = element_text(margin=margin(t=0,r=5,b=0,l=5), face="bold", size=30),
            axis.text.y = element_text(colour="black", size=25),
            axis.title.x = element_text(margin=margin(t=5,r=0,b=2,l=0), face="bold", size=30),
            axis.text.x = element_text(colour="black", size=25),
            legend.title = element_blank(),
            legend.text = element_text(size=25),
            legend.position = c(0.1,0.7),
            legend.background = element_blank(),
            legend.box.background = element_rect(colour = "black"))+
          #  legend.spacing.y = unit(-3.5, "mm"), 
          #  legend.key.height = unit(2, "line"),
           # legend.key.width = unit(2, "line")) +
      xlab("Date") +
      ylab("")
    
    start<-ggplot(flow, aes(fill=bay)) + 
      geom_point(aes(x=unq_start, y=flow), pch=21, size=5) +
      scale_x_continuous(breaks=seq(600, 1530,150), labels=seq(600,1530,150), limits=c(600,1530)) +
      scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                        name="",
                        breaks=c("B2", "B6", "B11"),
                        labels=c("Bay 2", "Bay 6", "Bay 11")) +
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
            axis.title.x = element_text(margin=margin(t=5,r=0,b=2,l=0), face="bold", size=30),
            axis.text.x = element_text(colour="black", size=25),
            legend.position="none") +
      ylab("Current velocity (m/s)") +
      xlab("Time of day (24 hr)")
    
    dcharge<-ggplot(flow_dis, aes(x=discharge, y=current, fill=bay)) +
      geom_smooth(aes(colour=bay)) +
      geom_point(pch=21, size=5) +
      scale_x_continuous(limits=c(1600,10000), breaks=seq(1600,10000, by=2000), labels=seq(1600,10000,by=2000)) +
      scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                        name="",
                        breaks=c("B2", "B6", "B11"),
                        labels=c("Bay 2", "Bay 6", "Bay 11")) +
      scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), 
                        name="",
                        breaks=c("B2", "B6", "B11"),
                        labels=c("Bay 2", "Bay 6", "Bay 11")) +
      theme_bw() +
      theme(text = element_text(colour="black", size=12),
            plot.margin=margin(t=10,r=10,b=2,l=2),
            panel.background = element_rect(fill = "white", colour = "black", size=2),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.grid.major = element_line(colour = "transparent"),
            plot.background = element_rect(fill = "transparent"),
            axis.ticks = element_line(size=1.2),
            axis.ticks.length = unit(0.5, "line"),
            axis.title.y = element_text(margin=margin(t=0,r=5,b=0,l=5), face="bold", size=30),
            axis.text.y = element_text(colour="black", size=25),
            axis.title.x = element_text(margin=margin(t=10,r=0,b=5,l=0), face="bold", size=30),
            axis.text.x = element_text(colour="black", size=25),
            legend.text = element_text(size=25),
            legend.position = "none") +
      xlab(expression(bold(paste("Discharge (m"^3, "/", s,")", sep="")))) +   
      ylab("")
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(date), ggplotGrob(start), ggplotGrob(dcharge), size="last"))



### 

# Calculate current speed for each date-run event
  # Current varies:
    # Intra-annually             (~Date, due to discharge)
    # Over the course of a day   (~Date-time, due to tide)
    # Horizontally               (~Date-run, due to shoreline effects)
  # This is at the run scale so dont need to account for sampling time (unless scaled up to daily CPUE)
    
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
  


# Plot original total catch and fish/m/s
bay_flow$bay <- factor(bay_flow$bay, levels=c("B2", "B6", "B11"), ordered=T)

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
        axis.title.y = element_text(margin=margin(t=30,r=5,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
        legend.title = element_blank(),
        legend.text = element_text(size=25),
        legend.position = c(0.1,0.7),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  ylab("Total smolts")

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
        axis.title.y = element_text(margin=margin(t=0,r=5,b=0,l=5), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=2,l=0), face="bold", size=30),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
        legend.position = "none") +
  xlab("Date") +
  ylab("Total smolts/m/s")

grid.newpage()
grid.draw(rbind(ggplotGrob(total), ggplotGrob(fish_ms), size="last"))
grid.arrange(total, fish_ms, ncol=2)







