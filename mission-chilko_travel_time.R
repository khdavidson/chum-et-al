setwd("~/`Stock assessment/Analysis/Scripts/chum-et-al")

library(dplyr)
library(lubridate)
library(scales)

#                                                       Chilko travel time calculations 

################
# Mission data #
################

mission.dat <- read.csv("mission_SO_expanded_CU_counts.csv")      


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CHILKO ONLY  

####
# IA/CPUE per run 
####
mission.cko <- mission.dat %>%
  filter(trap_type =="RST") %>%
  mutate_at(vars(c(12)), funs(as.character)) %>%
  mutate(CU_simple = ifelse(CU_final=="Chilko (S)", "Chilko", 
                     ifelse(CU_final=="Chilko (ES)", "Chilko", 
                     ifelse(CU_final=="Chilko Combined", "Chilko", CU_final)))) %>%
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*sum_run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE*bay_volume_m3) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print()


# Subset Chilko only
mission.cko <- mission.cko %>% 
  filter(CU_simple == "Chilko") %>% 
  print()


####
# Create the dataframe where abundance estimates will be calculated
####
# Calculate daily average count (expanded) and daily average IA for CHILKO
mission.cko.avg <- mission.cko %>% 
  group_by(date, USID) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), IA=mean(IA), CPUE=mean(CPUE)) %>%
  group_by(date) %>%
  summarize(MIS_total_n_exp=sum(n_CU_exp), MIS_daily_mean_CPUE=mean(CPUE), MIS_daily_mean_IA=mean(IA)) %>%
  print()


####
# Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), MIS_daily_mean_IA=0)
# Join 
mission.cko.avg$date <- as.Date(mission.cko.avg$date)
mission.cko.avg <- full_join(ts.df, mission.cko.avg, by=c("date", "MIS_daily_mean_IA"))

# Remove duplicates, first re-arranging
mission.cko.avg <- mission.cko.avg %>%
  arrange(date,desc(MIS_daily_mean_IA)) %>%
  distinct(date, .keep_all = T) %>%                # This preserves days with 0 catch and removes the duplicated date row
  print()

# Cuml calcs 
mission.cko.avg <- mission.cko.avg %>% 
  mutate(cuml_IA=cumsum(MIS_daily_mean_IA)) %>% 
  mutate(cuml_IA_propn=cuml_IA/sum(MIS_daily_mean_IA))

#####################
# Chilko fence data #
#####################

chilko.dat <- read.csv("chilko_fence.csv")

chilko.dat <- chilko.dat %>%                                                   
  rename(date = DATE,
         CKO_daily_count = Daily.Count...Chilko,                            
         CKO_daily_propn = Daily.Proportion...Chilko,
         CKO_cuml_count = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  select(-c(3,5)) %>% 
  mutate(CKO_cuml_propn = CKO_cuml_count/sum(CKO_daily_count))

####
# Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), CKO_daily_count=0)
# Join 
chilko.dat$date <- as.Date(chilko.dat$date)
chilko.dat <- full_join(ts.df, chilko.dat, by=c("date", "CKO_daily_count"))

# Remove duplicates, first re-arranging
chilko.dat <- chilko.dat %>%
  arrange(date,desc(CKO_daily_count)) %>%
  distinct(date, .keep_all = T) %>%                # This preserves days with 0 catch and removes the duplicated date row
  print()



################
# Full dataset #
################

full.cko <- left_join(mission.cko.avg, chilko.dat, by="date")

# Objectively find peaks 
findpeaks(full.cko$CKO_daily_count, npeaks=3, threshold=4, sortstr=TRUE)
    # Peaks at rows 25, 21, 28 
    # These rows are: Apr 27, Apr 23, Apr 30
findpeaks(full.cko$MIS_daily_mean_IA, npeaks=3, threshold=4, sortstr=TRUE)
    # Peaks at rows 38, 35, 40 
    # These rows are: May 10, May 7, May 12

  # Doesn't work for Mission CPUE or n (returns error)

# Ordering data and just pulling out what seems to be peaks: 
  # For IA, peaks seem to be at Apr 27, May 7, May 10
  # For Chilko, peaks seem to be at Apr 23, Apr 27, May 6


# Temporal normal 
ggplot() +
  geom_line(data=full.cko, aes(x=date, y=CKO_daily_count, colour="Chilko"), 
            size=1.8, alpha=0.7) +
  geom_line(data=full.cko, aes(x=date, y=MIS_daily_mean_IA*1000, colour="Mission"), 
                 size=1.8, alpha=0.9) +
  geom_point(aes(x=as.Date("2017-04-23"), y=6663431), size=4, colour="red") +
  geom_point(aes(x=as.Date("2017-04-27"), y=7753619), size=4, colour="red") +
  geom_point(aes(x=as.Date("2017-05-06"), y=4616294), size=4, colour="red") +
  geom_point(aes(x=as.Date("2017-04-27"), y=325.43888*1000), size=4, colour="red") +
  geom_point(aes(x=as.Date("2017-05-07"), y=663.20670*1000), size=4, colour="red") +
  geom_point(aes(x=as.Date("2017-05-10"), y=730.02419*1000), size=4, colour="red") +
  scale_colour_manual("", values=c("Chilko" = "black", "Mission" = "blue")) +
  scale_x_date(date_breaks = "5 day", date_labels = "%h %d") +
  scale_y_continuous(labels=comma, sec.axis = sec_axis(~., name = "Estimate of abundance at Mission (*1000)", 
                                                       breaks=seq(0,1000000,by=500000), labels=comma)) +
  theme(plot.margin=margin(t=90,r=10,b=2,l=2),
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
        legend.position = c(0.8,0.80),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.spacing.y = unit(-4, "mm"),                                                                              
        legend.key.height = unit(3, "line"),                                                                                    
        legend.key.width = unit(2, "line")) +                                                                                   
  guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
  ylab("Daily count at Chilko") +
  xlab("Date")


# Temporal cumulative 
ggplot() +
  geom_hline(yintercept = 0.5, colour="gray40", linetype="dashed")+
  geom_line(data=full.cko, aes(x=date, y=CKO_cuml_propn, colour="Chilko"), 
            size=1.8, alpha=0.7) +
  geom_line(data=full.cko, aes(x=date, y=cuml_IA_propn, colour="Mission"), 
            size=1.8, alpha=0.8) +
  scale_colour_manual("", values=c("Chilko" = "black", "Mission" = "blue")) +
  scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
  theme(plot.margin=margin(t=90,r=10,b=2,l=2),
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
        legend.position = c(0.85,0.20),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.spacing.y = unit(-4, "mm"),                                                                              
        legend.key.height = unit(3, "line"),                                                                                    
        legend.key.width = unit(2, "line")) +                                                                                   
  guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
  ylab("Cumulative proportion") +
  xlab("Date")










