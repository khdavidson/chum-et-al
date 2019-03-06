## CPUE doc Appendix I: Enviro data code 

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
  mutate(run_time_s = run_time*60)


######################################################################################################################################


####################
#
# BUT FIRST
# SOME EXPLORATORY RELATIONSHIPS to understand how current velocity changes with bay, date, time, etc. 
# 
####################

# Extract current velocity ~ date + time of day (used start time, arbitrary) + Bay
  # But to consider time of day (as related to tides), need to create a new column that is a 'date-time' combination
flow <- data %>% 
  mutate(datetime = paste(date, NEW_set_start, sep=" ")) %>%
  mutate(datetime = lubridate::ymd_hm(datetime)) %>%
  separate(datetime, into=c("date", "time"), sep = " (?=[^ ]+$)") %>% 
  mutate(datetime = paste(date, time, sep=" ")) %>%
  mutate(time = as.factor(time)) %>%
  select(date, time, datetime, USID, bay, run, current_speed_mps) %>% 
  filter(current_speed_mps != "#DIV/0!", current_speed_mps > 0) %>%   
  group_by(date, USID, run, bay, time, datetime) %>% 
  summarize(unq_flow = unique(current_speed_mps)) %>% 
  group_by(date, bay, time, run, datetime) %>% 
  summarize(flow = unique(unq_flow)) %>%
  print()

# Afterthought - also get current speed ~ discharge 
flow_dis <- data %>% 
  select(date, discharge_m3s, current_speed_mps, bay) %>% 
  group_by(date, bay) %>% 
  summarize(discharge = mean(discharge_m3s, na.rm=T), current = mean(current_speed_mps, na.rm=T))

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

    
  # FIGURE 1: Plot current speed ~ date, current speed ~ time of day, current speed ~ discharge 
  # Fig 1A
  flow$date <- as.Date(flow$date)
  date<-ggplot(flow, aes(fill=bay)) + 
    geom_point(aes(x=date, y=flow), pch=21, size=5) +
    scale_x_date(limits = as.Date(c("2017-04-03", "2017-06-14")), date_breaks = "7 day", date_labels = "%h %d") +
    scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
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
          axis.text.x = element_text(angle=45, hjust=1,colour="black", size=25),
          legend.title = element_blank(),
          legend.text = element_text(size=25),
          legend.position = c(0.1,0.75),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
        xlab("Date") +
        ylab("")
  
  # Fig 1B    
  flow$time <- as.POSIXct(flow$time, format="%H:%M:%S")
  start<-ggplot(flow, aes(fill=bay)) + 
    geom_point(aes(x=time, y=flow), pch=21, size=5) +
    scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
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
  #Omitted from Fig 3
  datetime<-ggplot(flow, aes(fill=bay)) +                           # Not super informative - trends obviously more driven by discharge than tide apparently
    geom_point(aes(x=datetime, y=flow), pch=21, size=5) +
    scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
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
  
  # Fig 1C
  dcharge<-ggplot(flow_dis, aes(x=discharge, y=current, fill=bay)) +
    geom_point(pch=21, size=5) +
    scale_x_continuous(limits=c(1600,10000), breaks=seq(1600,10000, by=2000), labels=seq(1600,10000,by=2000)) +
    scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
                      breaks=c("B2", "B6", "B11"), 
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
    scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
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
  
  # Fig 1D
  discharge <- data %>% 
  select(date, discharge_m3s) %>% 
  group_by(date) %>% 
  summarize(discharge = mean(discharge_m3s, na.rm=T))

  fig1d<-ggplot(data=discharge, aes(x=date, y=discharge)) +
    geom_line(size=1.2) +
    scale_x_date(date_breaks = "7 day", date_labels = "%h %d") +
    scale_y_continuous(breaks=seq(1800,18000,by=2500)) +
    theme_bw() +
    theme(text = element_text(colour="black", size=12),
          plot.margin=margin(t=10,r=10,b=2,l=2),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=5), face="bold", size=30),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_text(margin=margin(t=10,r=0,b=2,l=0), face="bold", size=30),
          axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
          legend.title = element_blank(),
          legend.text = element_text(size=25),
          legend.position = c(0.1,0.7),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
        xlab("Date") +
        ylab(expression(bold(paste("Discharge (m"^3, "/", s,")", sep=""))))  
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(date), ggplotGrob(start),size="last"))
  grid.newpage()
  grid.draw(rbind(ggplotGrob(dcharge), ggplotGrob(fig1d),size="last"))

  
###
# Sea level data 
###  
  
# Download and extract tidal data from EC to examine Bay current velocity ~ tide height
sealvl <- read.csv("7654-ALL-2017_slev.csv")                # Sea level data for April at New Westminster (Stn # 7654) 

# Split Date and Time into 2 columns to compare with sampling data easier 
sealvl <- sealvl %>% 
  rename(sea_level_m = SLEV.metres., date = Obs_date) %>%
  separate(date, into = c("date", "time"), sep = " (?=[^ ]+$)") %>%
  mutate(date = lubridate::dmy(date)) %>%
  unite(datetime, date, time, sep=" ") %>%
  mutate(datetime = lubridate::ymd_hm(datetime)) %>%
  separate(datetime, into = c("date", "time"), sep = " (?=[^ ]+$)") %>% 
  mutate(datetime = paste(date, time, sep=" ")) %>%
  filter(datetime > "2017-04-03 09:28:00", datetime < "2017-06-14 12:26:00") %>%
  mutate_at(c(vars(1)), funs(as.Date))

# Gather original data, need run as well as other parameters already included in "flow" table 
data2 <- data %>% 
  mutate(datetime = paste(date, NEW_set_start, sep=" ")) %>%
  mutate(datetime = lubridate::ymd_hm(datetime)) %>%
  separate(datetime, into=c("date", "time"), sep = " (?=[^ ]+$)") %>% 
  mutate(datetime = paste(date, time, sep=" ")) %>%
  select(date, bay, datetime, time, run) %>%
  group_by(date, bay, run, datetime) %>%
  summarize(time = unique(time)) %>%
  print()

# Merge "sealvl" and "data2"
data2$date <- as.Date(data2$date)
lvl.merge <- inner_join(data2, sealvl, by=c("date", "time", "datetime"))

# Merge lvl.merge with above flow table so I can look at velocity ~ sea level height 
flow$date <- as.Date(flow$date)
flow.lvl.merge <- left_join(flow, lvl.merge, by=c("date", "time", "bay", "datetime", "run"))

  
  #Fig 4A
  flow.lvl.merge$datetime <- as.POSIXct(as.character(flow.lvl.merge$datetime),format = "%Y-%m-%d %H:%M:%S")
  
  sea<-ggplot(flow.lvl.merge, aes(colour=bay)) +                          
    geom_line(aes(x=datetime, y=sea_level_m), size=2, alpha=0.8) +
    scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
                        breaks=c("B2", "B6", "B11"), 
                        labels=c("Bay 2", "Bay 6", "Bay 11")) +
    scale_x_datetime(date_breaks="100 hours", date_labels = "%h %d (%H:%M)") +
    theme_bw() +
    theme(text = element_text(colour="black", size=12),
          plot.margin=margin(t=10,r=20,b=2,l=2),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "gray60"),
          panel.grid.major.y = element_line(colour="transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_text(margin=margin(t=0,r=15,b=0,l=5), face="bold", size=30),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_text(margin=margin(t=10,r=0,b=20,l=0), face="bold", size=30),
          axis.text.x = element_text(angle=45, hjust=1, colour="black", size=25),
          legend.title = element_blank(),
          legend.text = element_text(size=25),
          legend.position = c(0.9,0.2),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black")) +
    ylab("Sea level (m)") +
    xlab("Date and time (24 hr)") 
  
  # FIG 4B 
  sea_vel<-ggplot(flow.lvl.merge, aes(fill=bay)) +                           # Not super informative - trends obviously more driven by discharge than tide apparently
    geom_point(aes(x=sea_level_m, y=flow), pch=21, size=5) +
    scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"), name="", 
                      breaks=c("B2", "B6", "B11"), 
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
    theme_bw() +
    theme(text = element_text(colour="black", size=12),
          plot.margin=margin(t=10,r=20,b=2,l=2),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_text(margin=margin(t=10,r=15,b=0,l=5), face="bold", size=30),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_text(margin=margin(t=5,r=0,b=10,l=0), face="bold", size=30),
          axis.text.x = element_text(colour="black", size=25),
          legend.position="none") +
    ylab("Current velocity (m/s)") +
    xlab("Sea level (m)")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(sea), ggplotGrob(sea_vel),size="last"))
  
# Given these plots, we can see that current velocity varies considerably by: 
    # Bay: Bay 6 current velocity is significantly higher than Bays 2 and 11. But Bays 2 and 11 do not differ significantly in current velocity
    # Date: Current velocity increases over time, due to increasing discharge. This increase is not proportional for all Bays: Bay 6 current velocity increases much more than Bays 2 and 11 over time.
    # Discharge: As above, increasing discharge over time increases current velocity in Bay 6 much more than Bays 2 and 11
    # Time of day: Although this graph doesn't show a clear affect of time of day on current velocity, I need to combine date-time events as this change is linked to tidal cycles. Or, gather tide date. TBD.
   

######
#
# / END exploratory analysis
#
######
    