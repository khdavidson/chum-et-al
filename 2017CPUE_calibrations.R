## Calibrating CPUE by daily discharge  

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


################################################################################################################################################
################################################################################################################################################


# Note: the code below corresponds to the word doc "Mission Sockeye smolt CPUE calibrations"


################################################################################################################################################


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
    
  
#########################################################################################################################################
  
  

# CALIBRATE CATCH USING CSA/WATER VOLUME FISHED 
  # RECALL, current varies:
    # Intra-annually                (~Date, due to discharge)
    # Over the course of a day      (~Time, due to tide)
    # Over the course of date-time  (~Date-time, due to tide)
    # Horizontally                  (~Date-run, due to shoreline effects)
  # This is at the run scale so dont need to account for sampling time (unless scaled up to daily CPUE)
  
  

                                  #######################################################################      
                                  #         1. RST, Depth=1.13, Not expanded for subsampling           #
                                  #######################################################################   

  
# Three step calculation:
  # 1. Number of fish/m3 fished (in 900 s, but calculation allows for other run lengths)
  # 2. Water volume in Bay (per 900 s, but calculation allows for other run lengths)
  # 3. CPUE fish catch (1*2)

CPUEdailycatch <- data %>% 
  select(USID, date, trap_type, UFID, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time_s, run, current_speed_mps) %>%
  filter(trap_type =="RST", sockeye_smolt_total != "NR") %>%                                                                                               # Just RST for now
  group_by(date, USID) %>%                                                                                                                                 # Group by date and sampling event
  summarize(unq_catch = unique(sockeye_smolt_total), run=unique(run), run_time_s=unique(run_time_s), current=unique(current_speed_mps, na.rm=T)) %>%       # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600", "1243.836",                                                                                     # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                        ifelse(run_time_s=="1020", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                               ifelse(run_time_s=="1080", "2238.905",
                                                      ifelse(run_time_s=="1140", "2363.288",
                                                             ifelse(run_time_s=="1200", "2487.672",
                                                                    ifelse(run_time_s=="1260", "2612.056",
                                                                           ifelse(run_time_s=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(fish_m3 = unq_catch/fished_vol) %>%                                                                                                               # Step 1: Calculate # fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                            # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                             # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3s = bay_width*bay_depth*current*run_time_s) %>%                                                                                      # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3*bay_volume_m3s) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print()    
  

      # CHILKO CHECK
      chilko <- c("Chilko Combined", "Chilko (S)", "Chilko (ES)")                                                                                                   # Create vector of Chilko CUs to call later
      chilko_dailycatch <- data %>% 
        select(USID, date, trap_type, UFID, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time_s, run, current_speed_mps) %>%
        filter(trap_type =="RST", CU_final %in% chilko) %>%                                                                                                         # Just RST and Chilko fish
        group_by(date, USID) %>%                                                                                                                                    # Group by date and sampling event
        summarize(unq_catch = unique(sockeye_smolt_total), run=unique(run), run_time_s=unique(run_time_s), current=unique(current_speed_mps, na.rm=T)) %>%          # Create unique variables for # fish, run, run length, and current velocity
        mutate(fished_vol = as.numeric(ifelse(run_time_s=="600", "1243.836", "1865.71"))) %>%                                                                       # Fill in volume fished for the 600 and 900 s runs
        mutate(bay_width = 440/3) %>%                                                                                                                               # Total river width is 440m therefore bay width is 440/3
        mutate(bay_depth = 1.13) %>%                                                                                                                                # For now, Bay depth is RST depth (1.13m)
        mutate(fish_m3 = unq_catch/fished_vol) %>%                                                                                                                  # Step 1: # fish/m3 (per run length)
        mutate(bay_volume_m3s = bay_width*bay_depth*current*run_time_s) %>%                                                                                         # Step 2: Volume of water in the Bay for a whole run (bay area*current velocity*run length)
        mutate(CPUE = fish_m3*bay_volume_m3s) %>%                                                                                                                   # Step 3: CPUE fish calculation (Step 1*Step 2)
        group_by(date) %>%                                                                                                                                          # Regroup by date                
        summarize(daily_CPUE = sum(CPUE)*100, daily_count=sum(unq_catch)*1000) %>%                                                                                  # Summarize the daily 'raw' number of fish and the daily CPUE
        print()
      
      # Load Chilko fence data and reformat quickly 
      chilko_fence <- read.csv("chilko_fence.csv")
      chilko_fence <- chilko_fence %>%                                                   
        rename(date = DATE,
               daily_count_chilko = Daily.Count...Chilko,                            # Renaming columns for easy call in R 
               daily_propn_chilko = Daily.Proportion...Chilko,
               cuml_count_chilko = Cumulative.Chilko.Total,
               cuml_propn_chilko = X..Cumulative.Chilko) %>% 
        mutate(date = lubridate::dmy(date))                                          # Convert date format to be yyyy-mm-dd
      

      # FIG 5. Daily count at Mission, daily count at Chilko, and CPUE 
      ggplot() +
        geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, 
                                         colour="Daily count at Chilko",                                                               # Colours and linetypes are categorial. Required for ggplot2 to create legend from different data series 
                                         linetype = "Daily count at Chilko"), 
                  size=1.5, alpha=0.4) +
        geom_bar(data=chilko_dailycatch, aes(x=date, y=daily_count,
                                             fill="Daily count at Mission (Chilko only)"), 
                 stat="identity", colour="black", width=1, alpha=0.8) +
        geom_line(data=chilko_dailycatch, aes(x=date, y=daily_CPUE,
                                              colour="CPUE at Mission (Chilko only)",
                                              linetype="CPUE at Mission (Chilko only)"), 
                  size=2, alpha=0.7) +
        scale_colour_manual("", values=c("Daily count at Chilko" = "gray40", 
                                         "CPUE at Mission (Chilko only)" = "black")) +
        scale_linetype_manual("", values=c("Daily count at Chilko" = 1,
                                           "CPUE at Mission (Chilko only)" = 1)) +
        scale_fill_manual("", values=c("Daily count at Mission (Chilko only)" = "gray40")) +
        scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
        scale_y_continuous(labels=comma, sec.axis = sec_axis(~.,                                                                       # Secondary axis call
                                                             name = "CPUE at Mission (*100) and \n Daily count at Mission (*1000)", 
                                                             breaks = seq(0,3000000, by=600000), 
                                                             labels = comma)) +
        theme_bw() +
        theme(text = element_text(colour="black", size=12),
              plot.margin=margin(t=10,r=10,b=2,l=2),
              panel.background = element_rect(fill = "white", colour = "black", size=2),
              panel.grid.minor = element_line(colour = "transparent"),
              panel.grid.major = element_line(colour = "transparent"),
              plot.background = element_rect(fill = "transparent"),
              axis.ticks = element_line(size=1.2),
              axis.ticks.length = unit(0.5, "line"),
              axis.title.y.right = element_text(margin=margin(t=0,r=0,b=0,l=20), face="bold", size=30),
              axis.title.y.left = element_text(margin=margin(t=0,r=20,b=0,l=5), face="bold", size=30),
              axis.text.y = element_text(colour="black", size=25),
              axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=30),
              axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
              legend.text = element_text(size=25),
              legend.position = c(0.68,0.87),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"),
              legend.spacing.y = unit(-3.5, "mm"),                                                                                    # Reduce extra white space around legend 
              legend.key.height = unit(2, "line"),                                                                                    # Adjusts space between line series in legend 
              legend.key.width = unit(2, "line")) +                                                                                   # Adjust width of line label in legend
        guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                                # Adjusts width of bar label in legend
        ylab("Daily count at Chilko") +
        xlab("Date")


      
                                  #######################################################################      
                                  #           2. RST, Depth=1.13, expanded for subsampling              #
                                  #######################################################################   
 

# Next step would be to see how these CPUE calculations apply when counts by CU are expanded for sub-sampling ("CU expansions" as in
# Townsend et al. 2017) 

####
## FIRST calculate expansion factors as done previously 
####
      
# DATA 1: Total number of fish caught, sampled and released daily 
dailycatch <- data %>% 
  select(USID, date, trap_type, UFID, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time_s, run, current_speed_mps) %>%
  filter(trap_type =="RST", sockeye_smolt_total != "NR") %>% 
  group_by(date, USID) %>%                                                                                                                # Group by date and USID
  summarize(unq_run_time = unique(run_time_s), unq_vel = unique(current_speed_mps),                                                       # Summarize variables for run time, current velocity, number of SO caught, and number of SO released
            unq_catch = unique(sockeye_smolt_total), unq_release = unique(sockeye_smolt_release)) %>% 
  group_by(date, USID) %>%                                                                                                                # Group again by date and USID
  summarize(sum_run_time = sum(unq_run_time), velocity = sum(unq_vel),                                                                    # Summarize the run time, current velocity, number of fish caught and number released, and add new column to calculate number sampled (caught - released) 
            sum_SO = sum(unq_catch), sum_release = sum(unq_release), sum_sampled = sum_SO-sum_release) %>%
  print()

    # Also replace the one entry on Apr 6 where the fish was both sampled and released - for our purposes, it's a sampled fish so change sampled = 1 and unsampled = 1
    dailycatch[49, 7] = 1                                                                         # Makes the "0" sampled into "1" sampled
    dailycatch[49, 6] = 0                                                                         # Makes the "1" released into "0" released (for our purposes this fish was sampled because it has a GSID)
    
# DATA 2: original number of fish caught from each CU (just sampled fish) 
dailysampled <- data %>% 
  select(USID, date, trap_type, UFID, CU_final, run_time_s, run) %>%         
  filter(trap_type == "RST", CU_final != "NA") %>%                                                # Select only RST catches, don't include entries where CU_final is "NA"
  group_by(date, USID, CU_final) %>%                                                              # Group by date, USID and CU            
  summarize(CU_n = n_distinct(UFID)) %>%                                                          # Summarize to count the number of sampled fish in each CU, for each sampling event
  print()

    # Join 'dailycatch' with 'dailysampled'
    catch.sample.merge <- left_join(dailycatch, dailysampled, by=c("date", "USID"))               # Merge the overall catch summary data (dailycatch) with the specific CU data (dailysampled)               

    
# Create new column for proportion of SAMPLED fish belonging to CU 
catch.sample.merge <- catch.sample.merge %>%
  mutate(propn_obs = CU_n/sum_sampled)                                                            # Create a new column to determine the proportion of fish belonging to each CU (just sampled fish at this point) 

# Calculate the expansion for SUBSAMPLED fish based on above proportion of sampled fish belonging to each CU
catch.sample.merge <- catch.sample.merge %>% 
  mutate(releasedxpropn = sum_release * propn_obs) %>%                                            # Create a new column "releasedxpropn" that expands the number of unsampled fish by the expansion factor to calculate the number of fish that needs to be added
  mutate(n_CU_exp = CU_n + releasedxpropn) %>%                                                    # Create a new column that adds the expanded numbers to the original catch numbers
  print()

    # Replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
    catch.sample.merge$n_CU_exp <- ifelse(is.na(catch.sample.merge$n_CU_exp), catch.sample.merge$sum_SO, catch.sample.merge$n_CU_exp)                # Again, some CUs didn't have expansion factors, so just replace NAs generated with their original abundances

    
# Now we have the number of expanded fish caught per day, per run (i.e., per sampling event), per CU (or not, including zero catches). 
  # This is probably the finest scale catch data we could have. It would make sense to start here, with these expanded 'raw' catches to 
  # then calculate CPUE. We would ultimately obtain the number of fish from each CU passing by at each sampling event (which I believe 
  # is the goal). ** still for the RST trap only though :( ** 


####
## SECOND apply the previous CPUE calculations to these EXPANDED catches 
####
      
# Expanded catches dataframe: catch.sample.merge 

# Add CPUE calculation columns as above 
CPUEexp <- catch.sample.merge %>% 
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                        ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                               ifelse(sum_run_time=="1080", "2238.905",
                                                      ifelse(sum_run_time=="1140", "2363.288",
                                                             ifelse(sum_run_time=="1200", "2487.672",
                                                                    ifelse(sum_run_time=="1260", "2612.056",
                                                                           ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(fish_m3 = n_CU_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate #fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*sum_run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3*bay_volume_m3) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print() 


    # CHILKO CHECK
    # Load Chilko fence data and reformat quickly 
    chilko_fence <- read.csv("chilko_fence.csv")                                                    # Load in the Chilko fence data (csv extracted from Excel file)
    
    chilko_fence <- chilko_fence %>% 
      rename(date = DATE,
             daily_count_chilko = Daily.Count...Chilko,                                             # Rename columns for nice R handling
             daily_propn_chilko = Daily.Proportion...Chilko,
             cuml_count_chilko = Cumulative.Chilko.Total,
             cuml_propn_chilko = X..Cumulative.Chilko) %>% 
        mutate(date = lubridate::dmy(date))                                                         # Reformat date to be yyyy-mm-dd
      
    # Subset the CPUEexp dataframe for just Chilko fish
    chilko <- c("Chilko Combined", "Chilko (S)", "Chilko (ES)")                                     # Create vector of Chilko CUs to call later
    
    chilko_CPUE <- CPUEexp %>% 
      filter(CU_final %in% chilko) %>%                                                              # Selecting only Chilko CUs using above vector from the CPUEexp dataframe above
      group_by(date) %>%                                                                            # Group these by date
      summarize(daily_CPUE = sum(CPUE)*100) %>%                                                     # Summarize CPUE for each day (scale of comparison for Chilko fence data), and multiply by 100 to scale it for plotting
      print()

    #FIG 6. Daily count at Chilko and expanded CPUE for Chilko
    ggplot() +
      geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, 
                                       colour="Daily count at Chilko",                                                              # Colours and linetypes are categorical to facilitate creating a legend from plotting different data series
                                       linetype = "Daily count at Chilko"), size=1.5, alpha=0.4) +
      geom_line(data=chilko_CPUE, aes(x=date, y=daily_CPUE,
                                            colour="CPUE at Mission (Chilko only)",
                                            linetype="CPUE at Mission (Chilko only)"), size=2, alpha=0.7) +
      scale_colour_manual("", values=c("Daily count at Chilko" = "gray40", 
                                       "CPUE at Mission (Chilko only)" = "black")) +
      scale_linetype_manual("", values=c("Daily count at Chilko" = 1,
                                         "CPUE at Mission (Chilko only)" = 1)) +
      scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
      scale_y_continuous(labels=comma, sec.axis = sec_axis(~., 
                                                           name = "CPUE at Mission (*100)", 
                                                           breaks = seq(0,3000000, by=600000), 
                                                           labels = comma)) +
      theme_bw() +
      theme(text = element_text(colour="black", size=12),
              plot.margin=margin(t=10,r=10,b=2,l=2),
              panel.background = element_rect(fill = "white", colour = "black", size=2),
              panel.grid.minor = element_line(colour = "transparent"),
              panel.grid.major = element_line(colour = "transparent"),
              plot.background = element_rect(fill = "transparent"),
              axis.ticks = element_line(size=1.2),
              axis.ticks.length = unit(0.5, "line"),
              axis.title.y.right = element_text(margin=margin(t=0,r=0,b=0,l=20), face="bold", size=30),
              axis.title.y.left = element_text(margin=margin(t=0,r=20,b=0,l=5), face="bold", size=30),
              axis.text.y = element_text(colour="black", size=25),
              axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=30),
              axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
              legend.text = element_text(size=25),
              legend.position = c(0.72,0.9),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"),
              legend.spacing.y = unit(-3.5, "mm"),                                                                                  # Removes some extra white space around legend
              legend.key.height = unit(2, "line"),                                                                                  # Adds space between legend labels
              legend.key.width = unit(2, "line")) +                                                                                 # Makes line label wider in legend
      ylab("Daily count at Chilko") +
      xlab("Date")




            #######################################################################################################################      
            # (code: C1)    RST and VT-0, Depth=1.13 and 1.01, Expanded for subsampling, Scaling factor applied to 6’ and 12’     #
            #######################################################################################################################   

#                                                   *** I don't like this method ***

####
## FIRST re-calculate expansions for each CU, trap type, depth, etc. to account for sub-sampling. 
####

# Total number of fish caught, sampled and released daily 
m3_dailycatch <- data %>% 
  select(USID, date, trap_type, depth_ft, run, bay, UFID, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time_s, run, current_speed_mps) %>%
  filter(sockeye_smolt_total != "NR") %>% 
  group_by(date, USID, trap_type, depth_ft, run, bay) %>%                                                                                                                # Group by date and USID
  summarize(unq_run_time = unique(run_time_s), unq_vel = unique(current_speed_mps),                                                       # Summarize variables for run time, current velocity, number of SO caught, and number of SO released
            unq_catch = unique(sockeye_smolt_total), unq_release = unique(sockeye_smolt_release)) %>% 
  group_by(date, USID, trap_type, depth_ft, run, bay) %>%                                                                                                                # Group again by date and USID
  summarize(sum_run_time = sum(unq_run_time), velocity = sum(unq_vel),                                                                    # Summarize the run time, current velocity, number of fish caught and number released, and add new column to calculate number sampled (caught - released) 
            sum_SO = sum(unq_catch), sum_release = sum(unq_release), sum_sampled = sum_SO-sum_release) %>%
  print()

    # Also replace the one entry on Apr 6 where the fish was both sampled and released - for our purposes, it's a sampled fish so change sampled = 1 and unsampled = 1
    m3_dailycatch[120, 11] = 1                                                                         # Makes the "0" sampled into "1" sampled
    m3_dailycatch[120, 10] = 0                                                                         # Makes the "1" released into "0" released (for our purposes this fish was sampled because it has a GSID)
    
# DATA 2: original number of fish caught from each CU (just sampled fish) 
m3_dailysampled <- data %>% 
  select(USID, date, trap_type, depth_ft, UFID, CU_final, run_time_s, run, bay) %>%         
  filter(CU_final != "NA") %>%                                                # Select only RST catches, don't include entries where CU_final is "NA"
  group_by(date, USID, CU_final, trap_type, depth_ft, run, bay) %>%                                                              # Group by date, USID and CU            
  summarize(CU_n = n_distinct(UFID)) %>%                                                          # Summarize to count the number of sampled fish in each CU, for each sampling event
  print()

    # Join 'dailycatch' with 'dailysampled'
    m3.cs.merge <- left_join(m3_dailycatch, m3_dailysampled, by=c("date", "USID", "trap_type", "depth_ft", "run", "bay"))               # Merge the overall catch summary data (dailycatch) with the specific CU data (dailysampled)               

# Create new column for proportion of SAMPLED fish belonging to CU 
m3.cs.merge <- m3.cs.merge %>%
  mutate(propn_obs = CU_n/sum_sampled)                                                            # Create a new column to determine the proportion of fish belonging to each CU (just sampled fish at this point) 

# Calculate the expansion for SUBSAMPLED fish based on above proportion of sampled fish belonging to each CU
m3.cs.merge <- m3.cs.merge %>% 
  mutate(releasedxpropn = sum_release * propn_obs) %>%                                            # Create a new column "releasedxpropn" that expands the number of unsampled fish by the expansion factor to calculate the number of fish that needs to be added
  mutate(n_CU_exp = CU_n + releasedxpropn) %>%                                                    # Create a new column that adds the expanded numbers to the original catch numbers
  print()

    # Replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
    m3.cs.merge$n_CU_exp <- ifelse(is.na(m3.cs.merge$n_CU_exp), m3.cs.merge$sum_SO, m3.cs.merge$n_CU_exp)                # Again, some CUs didn't have expansion factors, so just replace NAs generated with their original abundances

# Write into .csv 
write.csv(m3.cs.merge, "mission_SO_expanded_CU_counts.csv", row.names = F)
    

# PAUSE  
  # Now we have the number of expanded fish caught per day, per run (i.e., per sampling event), per CU (or not, including zero catches). 
  # This is probably the finest scale catch data we could have. It would make sense to start here, with these expanded 'raw' catches to 
  # then calculate CPUE. We would ultimately obtain the number of fish from each CU passing by at each sampling event (which I believe 
  # is the goal). This dataframe includes all traps. 


####
## SECOND, use expanded catches dataframe (m3.cs.merge // mission_SO_expanded_CU_counts.csv) to calculate this CPUE 
####    

data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")

# Calculating CPUE for each depth using trap depths as Bay depths (1.13m for RST, 1.01 for VT) 
RSTVT <- data_exp %>% 
  select(USID, date, run, bay, trap_type, depth_ft, sum_run_time, velocity, CU_final, n_CU_exp, sum_SO) %>%
  filter(trap_type !="IPT") %>%                                                                                               # Omit IPT trap for now
  group_by(date, USID, trap_type, depth_ft, run, bay, CU_final) %>%                                                                                                                                 # Group by date and sampling event
  summarize(run_time_s=unique(sum_run_time), current=unique(velocity, na.rm=T), 
            unq_catch = unique(sum_SO), unq_catch_exp = unique(n_CU_exp)) %>%                                                                 # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600" & trap_type=="RST", "1243.836",
                                 ifelse(run_time_s=="1020"& trap_type=="RST", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time_s=="1080"& trap_type=="RST", "2238.905",
                                 ifelse(run_time_s=="1140"& trap_type=="RST", "2363.288",
                                 ifelse(run_time_s=="1200"& trap_type=="RST", "2487.672",
                                 ifelse(run_time_s=="1260"& trap_type=="RST", "2612.056",
                                 ifelse(run_time_s=="1320"& trap_type=="RST", "2736.439", 
                                  ifelse(run_time_s=="600" & trap_type=="Vertical", "545.95",
                                  ifelse(run_time_s=="900" & trap_type=="Vertical", "818.92",
                                  ifelse(run_time_s=="1020" & trap_type=="Vertical", "928.11",
                                  ifelse(run_time_s=="1080" & trap_type=="Vertical", "982.70",
                                  ifelse(run_time_s=="1140" & trap_type=="Vertical", "1037.30",
                                  ifelse(run_time_s=="1200" & trap_type=="Vertical", "1091.891",
                                  ifelse(run_time_s=="1260" & trap_type=="Vertical", "1146.49",
                                  ifelse(run_time_s=="1320" & trap_type=="Vertical", "1201.08", "1865.71"))))))))))))))))) %>%                             # Nested ifelse() command to apply volume of water fished for each trap type and run length. There has got to be a better way to do this >:(
  mutate(fish_m3 = unq_catch/fished_vol) %>%                                                                                                               # Step 1: Calculate # fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                            # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = as.numeric(ifelse(trap_type=="Vertical", 1.01, 1.13))) %>%                                                                                                                             # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3s = bay_width*bay_depth*current*run_time_s) %>%                                                                                      # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3*bay_volume_m3s) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print()  




####
# Code to apply scaling factors to surface catches 
####
  
# First just RST and VT-0 added catches
RSTVT0 <- RSTVT %>% 
  filter(depth_ft=="0") %>% 
  group_by(date, run, bay) %>% 
  summarize(n_surface = sum(CPUE, na.rm=T)) %>% 
  mutate(depth_ft=0) %>%
  print()

# Next just VT-6 and -12 (don't need to add catches beacuse here we are just applying scaling factor to surface catches and not using
  # observed catches)
RSTVT612 <- RSTVT %>% 
  filter(depth_ft != "0") %>% 
  group_by(date, run, bay, depth_ft) %>% 
  summarize(summary_column = "empty column") %>% 
  print()

# Join surface and depth frames 
RSTVT_m3 <- full_join(RSTVT0, RSTVT612, by=c("date", "run", "bay", "depth_ft"))

# Create new unqiue DRB index  
RSTVT_m3 <- RSTVT_m3 %>% 
  select(-summary_column) %>%
  mutate(DRB = paste(gsub("-", "", date), run, bay, sep="-")) %>% 
  mutate(n_surface = ifelse(is.na(n_surface), 0, n_surface)) 

# 
RSTVT_m3.spread <- spread(RSTVT_m3, depth_ft, n_surface)

RSTVT_m3.spread<-RSTVT_m3.spread %>% 
  rename(zero = "0",
         six = "6",
         twelve = "12") %>% 
  mutate(six = zero*6.8,                                # Based on line 692 above
         twelve=zero*7.0)                               # Based on line 693 above







                                  #######################################################################      
                                  #     3. RST and VT, Depth=1.13 and 1.01, expanded for subsampling    #
                                  #######################################################################   


# Just doing as in #1 but with VT as well (just each trap's 'slice' of volume)
data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")

# Calculating CPUE for each depth using trap depths as Bay depths (1.13m for RST, 1.01 for VT) 
RSTVT_m3 <- data_exp %>% 
  select(USID, date, run, bay, trap_type, depth_ft, sum_run_time, velocity, CU_final, n_CU_exp, sum_SO) %>%
  filter(trap_type !="IPT") %>%                                                                                               # Omit IPT trap for now
  group_by(date, USID, trap_type, depth_ft, run, bay, CU_final) %>%                                                                                                                                 # Group by date and sampling event
  summarize(run_time_s=unique(sum_run_time), current=unique(velocity, na.rm=T), 
            unq_catch = unique(sum_SO), unq_catch_exp = unique(n_CU_exp)) %>%                                                                 # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600" & trap_type=="RST", "1243.836",
                                 ifelse(run_time_s=="1020"& trap_type=="RST", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time_s=="1080"& trap_type=="RST", "2238.905",
                                 ifelse(run_time_s=="1140"& trap_type=="RST", "2363.288",
                                 ifelse(run_time_s=="1200"& trap_type=="RST", "2487.672",
                                 ifelse(run_time_s=="1260"& trap_type=="RST", "2612.056",
                                 ifelse(run_time_s=="1320"& trap_type=="RST", "2736.439", 
                                  ifelse(run_time_s=="600" & trap_type=="Vertical", "545.95",
                                  ifelse(run_time_s=="900" & trap_type=="Vertical", "818.92",
                                  ifelse(run_time_s=="1020" & trap_type=="Vertical", "928.11",
                                  ifelse(run_time_s=="1080" & trap_type=="Vertical", "982.70",
                                  ifelse(run_time_s=="1140" & trap_type=="Vertical", "1037.30",
                                  ifelse(run_time_s=="1200" & trap_type=="Vertical", "1091.891",
                                  ifelse(run_time_s=="1260" & trap_type=="Vertical", "1146.49",
                                  ifelse(run_time_s=="1320" & trap_type=="Vertical", "1201.08", "1865.71"))))))))))))))))) %>%                             # Nested ifelse() command to apply volume of water fished for each trap type and run length. There has got to be a better way to do this >:(
  mutate(fish_m3 = unq_catch_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate # fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                            # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = as.numeric(ifelse(trap_type=="Vertical", 1.01, 1.13))) %>%                                                                                                                             # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3s = bay_width*bay_depth*current*run_time_s) %>%                                                                                      # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3*bay_volume_m3s)


####
# Sumamrize by daily total (CPUEs just added together)
####
daily_RSTVT_m3 <- RSTVT_m3 %>% 
  group_by(date) %>% 
  summarize(daily_total_CPUE = sum(CPUE, na.rm=T)) %>% 
  print()


####
# Sumamrize by % daily total at surface
####
depth_m3 <- RSTVT_m3 %>% 
    group_by(date, depth_ft) %>% 
    summarize(catch_depth = sum(CPUE, na.rm=T)) %>% 
    mutate(propn_daily_total = catch_depth/sum(catch_depth, na.rm=T)) %>% 
    print()


####
# Sumamrize by just total numbers at each depth
####
  n_m3 <- RSTVT_m3 %>% 
    group_by(depth_ft) %>% 
    summarize(total_depth = sum(CPUE, na.rm=T)) %>% 
    print()


  
  
  
   
##################################################################################
# Effort summary (RST+VT) at each depth: total time, events, volume, and catch   #
   effort_summary <- RSTVT %>% 
    select(date, trap_type, USID, run, bay, depth_ft, run_time_s, fished_vol, unq_catch_exp) %>% 
    filter(trap_type == "Vertical") %>%
    group_by(depth_ft) %>% 
    summarize(sum_vol = sum(fished_vol), total_run = (sum(run_time_s)/60)/60, n_events = n_distinct(USID), total_catch = sum(unq_catch_exp)) %>%
    mutate(vol_run = sum_vol/n_events) %>%
    print()
                                                                              #
##################################################################################
  

  
  
  
  
  
  
  
             ##########################################################################################################     
             #    4. RST and VT, Depth=1.13 and 1.01, expanded for subsampling, infer current at depth (Vernon 1966)  #
             ##########################################################################################################   

# Using current profiles in Vernon (1966; Figure 9 and Table 12), we can estimate that the current at 6 ft is 98% of the surface current,
  # and at 12 ft it is 93.5% of the surface current. 
# Still using just the 'slice' for each trap's volume, but just with slightly altered current estimates. 

####
# FIRST: replace surface current entries for traps at depth with the proportional currents
####
data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")

RSTVT_m4 <- data_exp %>% 
  select(USID, date, run, bay, trap_type, depth_ft, sum_run_time, velocity, CU_final, n_CU_exp, sum_SO) %>%
  filter(trap_type !="IPT") %>%                                                                                               # Omit IPT trap for now
  group_by(date, USID, trap_type, depth_ft, run, bay, CU_final) %>%                                                                                                                                 # Group by date and sampling event
  summarize(run_time_s=unique(sum_run_time), current=unique(velocity, na.rm=T), 
            unq_catch = unique(sum_SO), unq_catch_exp = unique(n_CU_exp)) %>%                                                                 # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600" & trap_type=="RST", "1243.836",
                                 ifelse(run_time_s=="1020"& trap_type=="RST", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time_s=="1080"& trap_type=="RST", "2238.905",
                                 ifelse(run_time_s=="1140"& trap_type=="RST", "2363.288",
                                 ifelse(run_time_s=="1200"& trap_type=="RST", "2487.672",
                                 ifelse(run_time_s=="1260"& trap_type=="RST", "2612.056",
                                 ifelse(run_time_s=="1320"& trap_type=="RST", "2736.439", 
                                  ifelse(run_time_s=="600" & trap_type=="Vertical", "545.95",
                                  ifelse(run_time_s=="900" & trap_type=="Vertical", "818.92",
                                  ifelse(run_time_s=="1020" & trap_type=="Vertical", "928.11",
                                  ifelse(run_time_s=="1080" & trap_type=="Vertical", "982.70",
                                  ifelse(run_time_s=="1140" & trap_type=="Vertical", "1037.30",
                                  ifelse(run_time_s=="1200" & trap_type=="Vertical", "1091.891",
                                  ifelse(run_time_s=="1260" & trap_type=="Vertical", "1146.49",
                                  ifelse(run_time_s=="1320" & trap_type=="Vertical", "1201.08", "1865.71"))))))))))))))))) %>%                             # Nested ifelse() command to apply volume of water fished for each trap type and run length. There has got to be a better way to do this >:(
  mutate(current_scaled = as.numeric(ifelse(depth_ft=="6", current*0.98,
                                     ifelse(depth_ft=="12", current*0.935, current)))) %>%
  mutate(fish_m3 = unq_catch_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate # fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                            # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = as.numeric(ifelse(trap_type=="Vertical", 1.01, 1.13))) %>%                                                                                                                             # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3s = bay_width*bay_depth*current_scaled*run_time_s) %>%                                              ## NOTE CURRENT SCALED USED HERE!!!!                                        # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3*bay_volume_m3s)


####
# Sumamrize & plot by daily catch for method 3 and 4 (just added CPUE)
####
daily_RSTVT_m4 <- RSTVT_m4 %>% 
  group_by(date) %>% 
  summarize(daily_total_CPUE = sum(CPUE, na.rm=T)) %>% 
  print()

  #Plot
  daily_RSTVT_m3$date <- as.Date(daily_RSTVT_m3$date)
  daily_RSTVT_m4$date <- as.Date(daily_RSTVT_m4$date)
  
  ggplot() +
    geom_line(data=daily_RSTVT_m4, aes(x=date, y=daily_total_CPUE), colour="red", size=1.2) +
    geom_line(data=daily_RSTVT_m3, aes(x=date, y=daily_total_CPUE), size=1) +                                                                                # Makes line label wider in legend
      ylab("Total CPUE") +
      xlab("Date")


####
# Sumamrize % plot by % daily total catch at each depth for method 3 and 4
####
  depth_m4 <- RSTVT_m4 %>% 
    group_by(date, depth_ft) %>% 
    summarize(catch_depth = sum(CPUE, na.rm=T)) %>% 
    mutate(propn_daily_total = catch_depth/sum(catch_depth, na.rm=T)) %>% 
    print()

  # Plot
  p4.1<-ggplot() +
    geom_bar(data=depth_m3, aes(x=date,y=propn_daily_total, fill=depth_ft), stat="identity") 
  p4.2<-ggplot() +
    geom_bar(data=depth_m4, aes(x=date,y=propn_daily_total, fill=depth_ft), stat="identity")

  grid.newpage()
  grid.draw(rbind(ggplotGrob(p4.1), ggplotGrob(p4.2), size="last"))


####
# Sumamrize by just total numbers at each depth for method 3 and 4
####
  n_m4 <- RSTVT_m4 %>% 
    group_by(depth_ft) %>% 
    summarize(total_depth = sum(CPUE, na.rm=T)) %>% 
    print()





  
  
  
  
             ##########################################################################################################     
             #    5. RST and VT, Depth=1.13 and 1.01, expanded, inferred current (Vernon 1966), RST-VT0 scaled        #
             ##########################################################################################################   


# Relate RST and VT0 catches. This is the basic method of finding a factor that describes how much more efficient the RST trap is than
  # the VT0. 
  
####
# FIRST, need to calculate fish/m3 for all depths - for this simple exercise, ignore CUs
####
data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")

RSTVT_m5 <- data_exp %>% 
  select(USID, date, run, bay, trap_type, depth_ft, sum_run_time, velocity, CU_final, n_CU_exp, sum_SO) %>%
  filter(trap_type !="IPT") %>%                                                                                               # Omit IPT trap for now
  group_by(date, USID, trap_type, depth_ft, run, bay) %>%                                                                                                                                 # Group by date and sampling event
  summarize(run_time_s=unique(sum_run_time), current=unique(velocity, na.rm=T), 
            unq_catch = unique(sum_SO)) %>%                                                                 # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600" & trap_type=="RST", "1243.836",
                                 ifelse(run_time_s=="1020"& trap_type=="RST", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time_s=="1080"& trap_type=="RST", "2238.905",
                                 ifelse(run_time_s=="1140"& trap_type=="RST", "2363.288",
                                 ifelse(run_time_s=="1200"& trap_type=="RST", "2487.672",
                                 ifelse(run_time_s=="1260"& trap_type=="RST", "2612.056",
                                 ifelse(run_time_s=="1320"& trap_type=="RST", "2736.439", 
                                  ifelse(run_time_s=="600" & trap_type=="Vertical", "545.95",
                                  ifelse(run_time_s=="900" & trap_type=="Vertical", "818.92",
                                  ifelse(run_time_s=="1020" & trap_type=="Vertical", "928.11",
                                  ifelse(run_time_s=="1080" & trap_type=="Vertical", "982.70",
                                  ifelse(run_time_s=="1140" & trap_type=="Vertical", "1037.30",
                                  ifelse(run_time_s=="1200" & trap_type=="Vertical", "1091.891",
                                  ifelse(run_time_s=="1260" & trap_type=="Vertical", "1146.49",
                                  ifelse(run_time_s=="1320" & trap_type=="Vertical", "1201.08", "1865.71"))))))))))))))))) %>%                             # Nested ifelse() command to apply volume of water fished for each trap type and run length. There has got to be a better way to do this >:(
  mutate(current_scaled = as.numeric(ifelse(depth_ft=="6", current*0.98,
                                     ifelse(depth_ft=="12", current*0.935, current)))) %>%
  mutate(fish_m3 = unq_catch/fished_vol) 


####
# SECOND, just select 0m, and only times when there are catches in both the RST and VT0 
####
surface_m5 <- RSTVT_m5 %>% 
  ungroup() %>%
  select(date, run, bay, depth_ft, trap_type, fish_m3) %>%
  unite(DRB, date, run, bay) %>%
  filter(depth_ft == "0", fish_m3 >0)

# Brute force pull out all the dates with both RST and VT0 catches because I'm too brain dead to figure out an elegant way 
events <- c("2017-04-22_R7_B11", "2017-04-27_R1_B2", "2017-04-27_R2_B6", "2017-05-01_R6_B6", "2017-05-07_R4_B6", "2017-05-07_R9_B2", 
            "2017-05-08_R6_B6", "2017-05-31_R10_B2")

# Filter by 'events' and remove the one event where there wasn't paired RST and VT0 sampling
surface_m5 <- surface_m5 %>% 
  filter(DRB %in% events) %>%
  filter(DRB != "2017-05-31_R10_B2") %>%
  print()

# Reformat for easy calculation
surface_m5.spread <- spread(surface_m5, trap_type, fish_m3)

# Calculate the factor difference for each sampling event, and then summarize to overall mean +/- SD factor
surface_m5.spread <- surface_m5.spread %>% 
  mutate(factor = RST/Vertical) %>%
  summarize(mean_factor = mean(factor), sd_factor = sd(factor)) %>%
  print()

  # On average, the RST caught 11.8 (+/- 14.1)x more fish/m3 than the VT0. Therefore we could use that to convert VT6 and VT12 catches
    # into imagined RST6 and RST12 catches


####
# THIRD, apply factor (11.8) to fish/m3 for VT6 and VT12 
####
RSTVT_m5_fac <- data_exp %>% 
  select(USID, date, run, bay, trap_type, depth_ft, sum_run_time, velocity, CU_final, n_CU_exp, sum_SO) %>%
  filter(trap_type !="IPT") %>%                                                                                               # Omit IPT trap for now
  group_by(date, USID, trap_type, depth_ft, run, bay) %>%                                                                                                                                 # Group by date and sampling event
  summarize(run_time_s=unique(sum_run_time), current=unique(velocity, na.rm=T), 
            unq_catch = unique(sum_SO)) %>%                                                                 # Create unique variables for number of fish, run, run length, and current velocity
  mutate(fished_vol = as.numeric(ifelse(run_time_s=="600" & trap_type=="RST", "1243.836",
                                 ifelse(run_time_s=="1020"& trap_type=="RST", "2114.521",                                                                             # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time_s=="1080"& trap_type=="RST", "2238.905",
                                 ifelse(run_time_s=="1140"& trap_type=="RST", "2363.288",
                                 ifelse(run_time_s=="1200"& trap_type=="RST", "2487.672",
                                 ifelse(run_time_s=="1260"& trap_type=="RST", "2612.056",
                                 ifelse(run_time_s=="1320"& trap_type=="RST", "2736.439", 
                                  ifelse(run_time_s=="600" & trap_type=="Vertical", "545.95",
                                  ifelse(run_time_s=="900" & trap_type=="Vertical", "818.92",
                                  ifelse(run_time_s=="1020" & trap_type=="Vertical", "928.11",
                                  ifelse(run_time_s=="1080" & trap_type=="Vertical", "982.70",
                                  ifelse(run_time_s=="1140" & trap_type=="Vertical", "1037.30",
                                  ifelse(run_time_s=="1200" & trap_type=="Vertical", "1091.891",
                                  ifelse(run_time_s=="1260" & trap_type=="Vertical", "1146.49",
                                  ifelse(run_time_s=="1320" & trap_type=="Vertical", "1201.08", "1865.71"))))))))))))))))) %>%                             # Nested ifelse() command to apply volume of water fished for each trap type and run length. There has got to be a better way to do this >:(
  mutate(current_scaled = as.numeric(ifelse(depth_ft=="6", current*0.98,
                                     ifelse(depth_ft=="12", current*0.935, current)))) %>%
  mutate(fish_m3 = unq_catch/fished_vol) %>% 
  mutate(fish_m3_fac = ifelse(depth_ft != "0", fish_m3*11.8, fish_m3)) %>% 
  mutate(bay_width = 440/3) %>%                                                                                                                            # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                         # NOTE: as we are 'imagining' the RST is fishing at depths, we should be using the RST depth!!                                                                             # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3s = bay_width*bay_depth*current_scaled*run_time_s) %>%                                              ## NOTE CURRENT SCALED USED HERE!!!!                                        # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(CPUE = fish_m3_fac*bay_volume_m3s)


####
# Sumamrize & plot by daily catch for method 3 and 4 (just added CPUE)
####
daily_RSTVT_m5 <- RSTVT_m5_fac %>% 
  group_by(date) %>% 
  summarize(daily_total_CPUE = sum(CPUE, na.rm=T)) %>% 
  print()

  #Plot
  daily_RSTVT_m3$date <- as.Date(daily_RSTVT_m3$date)
  daily_RSTVT_m4$date <- as.Date(daily_RSTVT_m4$date)
  daily_RSTVT_m5$date <- as.Date(daily_RSTVT_m5$date)
  
  ggplot() +
    geom_line(data=daily_RSTVT_m5, aes(x=date, y=daily_total_CPUE), colour="blue", size=1.5) +
    geom_line(data=daily_RSTVT_m4, aes(x=date, y=daily_total_CPUE), colour="red", size=1.2) +
    geom_line(data=daily_RSTVT_m3, aes(x=date, y=daily_total_CPUE), size=1) +                                                                                # Makes line label wider in legend
      ylab("Total CPUE") +
      xlab("Date")


####
# Sumamrize % plot by % daily total catch at each depth for method 3 and 4
####
  depth_m5 <- RSTVT_m5_fac %>% 
    group_by(date, depth_ft) %>% 
    summarize(catch_depth = sum(CPUE, na.rm=T)) %>% 
    mutate(propn_daily_total = catch_depth/sum(catch_depth, na.rm=T)) %>% 
    print()

  
  # FIGURE 5
  depth_m3$depth_ft <- factor(depth_m3$depth_ft, levels = c("0", "6", "12"), ordered=T)
  depth_m4$depth_ft <- factor(depth_m4$depth_ft, levels = c("0", "6", "12"), ordered=T)
  depth_m5$depth_ft <- factor(depth_m5$depth_ft, levels = c("0", "6", "12"), ordered=T)
  depth_m3$date <- as.Date(depth_m3$date)
  depth_m4$date <- as.Date(depth_m4$date)
  depth_m5$date <- as.Date(depth_m5$date)

  # FIG 5a
  pm3<-ggplot() +
    geom_bar(data=depth_m3, aes(x=date,y=propn_daily_total, fill=depth_ft), width=1, colour="black", stat="identity") +
    scale_x_date(date_breaks = "5 day", date_labels = "%h %d") +
    scale_fill_manual(labels = c("0", "1.8", "3.7"), breaks=c("0", "6", "12"),values=c("#7fd2ea", "#0063b3", "#002b64")) +
    theme_bw() +
    theme(plot.margin=margin(t=10,r=20,b=0,l=45),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")                                                                                   # Reduce extra white space around legend 
  # FIG 5b
  pm4<-ggplot() +
    geom_bar(data=depth_m4, aes(x=date,y=propn_daily_total, fill=depth_ft), width=1, colour="black", stat="identity") +
    scale_x_date(date_breaks = "5 day", date_labels = "%h %d") +
    scale_fill_manual(labels = c("0", "1.8", "3.7"), breaks=c("0", "6", "12"),values=c("#7fd2ea", "#0063b3", "#002b64")) +
    theme_bw() +
    theme(plot.margin=margin(t=10,r=20,b=0,l=45),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_text(margin=margin(t=0,r=70,b=0,l=10), face="bold", size=30),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_text(face="bold", size=20),
          legend.text = element_text(size=20),
          legend.position=c(0.8765,0.5),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill="transparent", colour="black", size=1)) +                                                                                   # Reduce extra white space around legend 
    guides(fill=guide_legend(title="Depth (m)")) +
    ylab("Proportion of abundance index") 
  # FIG 5c
  pm5<-ggplot()+
    geom_bar(data=depth_m5, aes(x=date,y=propn_daily_total, fill=depth_ft), width=1, colour="black", stat="identity") +
    scale_x_date(date_breaks = "5 day", date_labels = "%h %d") +
    scale_fill_manual(labels = c("0", "1.8", "3.7"), breaks=c("0", "6", "12"),values=c("#7fd2ea", "#0063b3", "#002b64")) +
    theme_bw() +
    theme(plot.margin=margin(t=10,r=20,b=0,l=45),
          panel.background = element_rect(fill = "white", colour = "black", size=2),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          plot.background = element_rect(fill = "transparent"),
          axis.ticks = element_line(size=1.2),
          axis.ticks.length = unit(0.5, "line"),
          axis.title.y = element_blank(),
          axis.text.y = element_text(colour="black", size=25),
          axis.title.x = element_text(margin=margin(t=15,r=0,b=0,l=10), face="bold", size=30),
          axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
          legend.position = "none") +                                                                                   # Reduce extra white space around legend 
    xlab("Date")  
  #fig 5 draw
  grid.newpage()
  grid.draw(rbind(ggplotGrob(pm3), ggplotGrob(pm4), ggplotGrob(pm5),size="last"))


####
# Sumamrize by just total numbers at each depth for method 3 and 4
####
  n_m5 <- RSTVT_m5_fac %>% 
    group_by(depth_ft) %>% 
    summarize(total_depth = sum(CPUE, na.rm=T)) %>% 
    print()































