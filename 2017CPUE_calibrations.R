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
library(lubridate)

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

# Code to inform analysis:

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
  # Sampling effort is:                                                          #
    # Higher at the surface                                                      #
    # Unequal among Bays                                                         #
##################################################################################
  

##############################################################################################################################
# Daily expanded catch                                                                                                       #
                                                                                                                             #
# DATA 1: Total number of fish caught, sampled and released daily                                                            #
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
                                                                                                                          #
# DATA 2: original number of fish caught from each CU (just sampled fish)                                                    #
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
                                                                                                                          #
# Calculate the expansion for SUBSAMPLED fish based on above proportion of sampled fish belonging to each CU                 #
catch.sample.merge <- catch.sample.merge %>% 
  mutate(releasedxpropn = sum_release * propn_obs) %>%                                            # Create a new column "releasedxpropn" that expands the number of unsampled fish by the expansion factor to calculate the number of fish that needs to be added
  mutate(n_CU_exp = CU_n + releasedxpropn) %>%                                                    # Create a new column that adds the expanded numbers to the original catch numbers
  print()

    # Replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
    catch.sample.merge$n_CU_exp <- ifelse(is.na(catch.sample.merge$n_CU_exp), catch.sample.merge$sum_SO, catch.sample.merge$n_CU_exp)                # Again, some CUs didn't have expansion factors, so just replace NAs generated with their original abundances
                                                                                                                          #
data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")                                                                    #
# Now we have the number of expanded fish caught per day, per run (i.e., per sampling event), per CU (or not, including      #
  # zero catches). This is probably the finest scale catch data we could have. It would make sense to start here, with these #
  # expanded 'raw' catches to then calculate CPUE. We would ultimately obtain the number of fish from each CU passing by at  #
  # each sampling event (which I believe is the goal). ** still for the RST trap only though :( **                           #  
##############################################################################################################################




#                                                   THE FOLLOWING CODE CORRESPONDS TO
#                                               KD_202019_MissionSockeyeSmoltCPUECalibrations


# CALIBRATE CATCH USING CSA/WATER VOLUME FISHED 
  # RECALL, current varies:
    # Intra-annually                (~Date, due to discharge)
    # Over the course of a day      (~Time, due to tide)
    # Over the course of date-time  (~Date-time, due to tide)
    # Horizontally                  (~Date-run, due to shoreline effects)


#--------------------------------------------------------------------------------------------------------------------------------------
   
                                  #_____________________________________________________________________#   
                                  #                                                                     #
                                  #  1. RST expanded for subsampling, daily discharge, total day sec    #
                                  #_____________________________________________________________________#   

# Calculate:
  # CPUE (RST catch only, expanded)
  # Average daily CPUE*daily discharge*total daily run time length
  # Just for CHILKO! 

data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")                                                                    #

####
# FIRST: CPUE
####
chilko <- c("Chilko (S)", "Chilko (ES)", "Chilko Combined")
m1_CPUE <- data_exp %>%
  filter(trap_type =="RST", CU_final %in% chilko) %>%
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%
  print()


####
# SECOND: Create dataframe for daily average CPUE, discharge, total run time 
####
# Gather variables 
  # subset
  m1_1 <- m1_CPUE %>% 
    group_by(date, USID) %>% 
    summarize(sum_SO=unique(sum_SO), n_CU_exp=sum(n_CU_exp), CPUE=sum(CPUE)) %>%
    group_by(date) %>%
    summarize(sum_SO = sum(sum_SO), n_CU_exp=sum(n_CU_exp)*1000, daily_mean_CPUE=mean(CPUE), sec_day=86400) %>%
    print()

  # Extract daily discharge
  discharge <- data %>% 
    select(date, discharge_m3s) %>% 
    group_by(date) %>% 
    summarize(daily_mean_discharge = mean(discharge_m3s, na.rm=T))  
  
# Link daily discharge and daily CPUE/run length
m1_1$date <- as.Date(m1_1$date)
m1_df <- left_join(m1_1, discharge, by = "date")


####
# THIRD: Calculate 
####
# Calculate
m1_df <- m1_df %>% 
  mutate(IA_mean_day = daily_mean_CPUE*daily_mean_discharge*sec_day) %>%                        
  print()

# Fill in missing days with 0
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), IA_mean_day=0)

# Join 
m1_df.join <- full_join(ts.df, m1_df, by=c("date", "IA_mean_day"))

# Remove duplicates 
m1_df.join <- m1_df.join %>%
  arrange(date,desc(IA_mean_day)) %>%
  distinct(date, .keep_all = T) %>%
  print()


####
# FOURTH: PLOT. Fig 1.1. Chilko  
####
# Load Chilko fence data and reformat quickly 
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date))                                          

#Plot
ggplot() +
  geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
            size=1.8, alpha=0.8) +
  geom_point(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko"), 
             size=0.1, alpha=0.3) +
  geom_line(data=m1_df.join, aes(x=date,y=IA_mean_day, colour="Daily estimate at Mission \n (Chilko only)", 
                            linetype="Daily estimate at Mission \n (Chilko only)"), 
            size=1.8, alpha=0.8) +
  geom_point(data=m1_df.join, aes(x=date, y=IA_mean_day, colour="Daily estimate at Mission \n (Chilko only)"), 
             size=0.1, alpha=0.3) +
  geom_bar(data=m1_df.join, aes(x=date, y=n_CU_exp, fill="Count at Mission (Chilko only)"), stat="identity", 
           colour="black", width=1, alpha=0.6) +
  scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                   "Daily estimate at Mission \n (Chilko only)" = "blue")) +
  scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                     "Daily estimate at Mission \n (Chilko only)" = 1)) +
  scale_fill_manual("", values=c("Count at Mission (Chilko only)" = "gray40")) +
  scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
  scale_y_continuous(labels=comma, 
                     sec.axis = sec_axis(~., name = "Total daily count at Mission (*1000)", breaks=seq(0,400000,by=400000), labels=comma)) +
  theme(text = element_text(colour="black", size=12),
              plot.margin=margin(t=100,r=10,b=2,l=2),
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
              legend.position = c(0.7,0.80),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black"),
              legend.spacing.y = unit(-4, "mm"),                                                                              
              legend.key.height = unit(3, "line"),                                                                                    
              legend.key.width = unit(2, "line")) +                                                                                   
  guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
  ylab("Daily count at Chilko and \n Average daily estimate at Mission") +
  xlab("Date")


####
# FIFTH: Cumulative run timings comparison (feeds into Fig 2.2 below)
####
# Overall df (not just Chilko like before)
m1_CPUE_cuml <- data_exp %>%
  filter(trap_type =="RST") %>%
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%
  print()

# 1. Gather variables 
  # subset
  m1_2 <- m1_CPUE_cuml %>% 
    group_by(date, USID, CU_final) %>% 
    summarize(n_CU_exp=sum(n_CU_exp), CPUE=sum(CPUE)) %>%
    group_by(date, CU_final) %>%
    summarize(n_CU_exp=sum(n_CU_exp), daily_mean_CPUE=mean(CPUE), sec_day=86400) %>%
    print()

  # Extract daily discharge
  discharge <- data %>% 
    select(date, discharge_m3s) %>% 
    group_by(date) %>% 
    summarize(daily_mean_discharge = mean(discharge_m3s, na.rm=T))  
  
# 2. Link daily discharge and daily CPUE/run length
m1_2$date <- as.Date(m1_2$date)
m1_2_df <- left_join(m1_2, discharge, by = "date")

# 3. Calculate CPUE*discharge*sec in a day
m1_2_df <- m1_2_df %>% 
  mutate(IA_mean_day = daily_mean_CPUE*daily_mean_discharge*sec_day) %>%                        
  print()

# 2. Summarize by day 
m1_cuml_day <- m1_2_df %>% 
  group_by(date, CU_final) %>%
  summarize(n_CU_exp=sum(n_CU_exp), daily_mean_IA=mean(IA_mean_day)) %>%
  print() 

# 3. Determine total number fish in each CU 
m1_CU_totals <- m1_cuml_day %>% 
  group_by(CU_final) %>%                                                                   
  summarize(CU_total_n = sum(n_CU_exp), CU_total_IA = sum(daily_mean_IA)) %>%                                                   
  print()

# 4. Join, make new column to divide daily # each CU by CU total   
CU.total.merge <- left_join(m1_cuml_day, m1_CU_totals, by="CU_final")                           

m1_final_date <- CU.total.merge %>% 
  mutate(daily_perc_n=n_CU_exp/CU_total_n, daily_perc_IA=daily_mean_IA/CU_total_IA) %>%                                                 
  print()

# 5. Calculate cumulative proportions 
m1_final_date_cuml <- m1_final_date %>% 
  group_by(CU_final) %>%                                                               
  mutate(cuml_n = cumsum(n_CU_exp), cuml_IA=cumsum(CU_total_IA),
         cuml_propn_n=cumsum(daily_perc_n), cuml_propn_IA=cumsum(daily_perc_IA)) %>%
  print()

# 6. Just choose top 5 CUs 
top_5 <- c("Chilko (S)", "Francois-Fraser (S)", "Chilko (ES)", "Chilko Combined", "Quesnel (S)")
m1_final_date_cuml_5 <- final_date_cuml %>% 
  filter(CU_final %in% top_5) %>%                                                               
  print()

# 7. Find migration timing info 
dates <- m1_final_date_cuml_5 %>% 
  group_by(CU_final) %>%                                                                   
  summarize(first = min(date), last = max(date))  %>%                                     
  print()



#--------------------------------------------------------------------------------------------------------------------------------------



                                  #_____________________________________________________________________#      
                                  #                                                                     #
                                  #           2. RST, Depth=1.13, expanded for subsampling              #
                                  #_____________________________________________________________________#   

# Calculate:
  # CPUE (RST catch only, expanded)
  # Bay water vol
  # Adjusted abundance
  # Just for CHILKO! 

data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")      

####
## FIRST apply Equations 1-3 to these EXPANDED catches 
####
chilko <- c("Chilko (S)", "Chilko (ES)", "Chilko Combined")
m2_CPUE <- data_exp %>% 
  filter(trap_type == "RST", CU_final %in% chilko) %>% 
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE_fish_m3 = n_CU_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate #fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*sum_run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE_fish_m3*bay_volume_m3) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print() 


####
# SECOND: Summarize for daily catch stats Chilko only 
####
# Gather variables 
  # subset
  m2_1 <- m2_CPUE %>% 
    group_by(date, USID) %>% 
    summarize(sum_SO=unique(sum_SO), n_CU_exp=sum(n_CU_exp), IA=sum(IA)) %>%
    group_by(date) %>%
    summarize(sum_SO = sum(sum_SO), n_CU_exp=sum(n_CU_exp)*1000, daily_mean_IA=mean(IA)*1000, daily_total_IA=sum(IA)*100) %>%
    print()

# Fill in missing days with 0
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), daily_total_IA=0, daily_mean_IA=0)

# Join 
m2_1$date <- as.Date(m2_1$date)
m2_df.join <- full_join(ts.df, m2_1, by=c("date", "daily_total_IA", "daily_mean_IA"))

# Remove duplicates 
m2_df.join <- m2_df.join %>%
  arrange(date, desc(daily_total_IA)) %>%
  distinct(date, .keep_all = T) %>%
  print()


####
# THIRD: PLOT. Fig 2.1. Chilko  
####
# Load Chilko fence data and reformat quickly 
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date))                                          

#Plot
ggplot() +
  geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
            size=1.6, alpha=0.6) +
  geom_line(data=m2_df.join, aes(x=date, y=daily_total_IA, colour= "Daily total estimate at Mission \n (Chilko only)",
                                 linetype = "Daily total estimate at Mission \n (Chilko only)"), 
            size=1.8, alpha=0.7) +
  geom_line(data=m2_df.join, aes(x=date,y=daily_mean_IA, colour="Daily average estimate at Mission \n (Chilko only)", 
                                 linetype="Daily average estimate at Mission \n (Chilko only)"), 
            size=1.8, alpha=0.8) +
  scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                   "Daily average estimate at Mission \n (Chilko only)" = "blue",
                                   "Daily total estimate at Mission \n (Chilko only)" = "black")) +
  scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                     "Daily average estimate at Mission \n (Chilko only)" = 1,
                                     "Daily total estimate at Mission \n (Chilko only)" = 1)) +
  scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
  scale_y_continuous(labels=comma, 
                     sec.axis = sec_axis(~., name = "Daily average estimate at Mission (*1000) and \n Daily total estimate at Mission (*100)", 
                                         breaks=seq(0,3000000,by=750000), labels=comma)) +
  theme(text = element_text(colour="black", size=12),
              plot.margin=margin(t=100,r=10,b=2,l=2),
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
              legend.position = c(0.7,0.80),
              legend.background = element_blank(),
              legend.box.background = element_rect(colour = "black", fill="white"),
              legend.spacing.y = unit(-5, "mm"),                                                                              
              legend.key.height = unit(4, "line"),                                                                                    
              legend.key.width = unit(2, "line")) +                                                                                   
  ylab("Daily total count at Chilko") +
  xlab("Date")



####
# FOURTH: Cumulative run timing methods comparison
####
# Overall df
m2_cuml <- data_exp %>% 
  filter(trap_type == "RST") %>% 
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE_fish_m3 = n_CU_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate #fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*sum_run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE_fish_m3*bay_volume_m3) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print() 

# 1. Summarize by sampling event
m2_cuml_day <- m2_cuml %>% 
  group_by(date, USID, CU_final) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), IA=sum(IA))

    # Replace NAs with text n/a so easier to filter
    #m2_cuml_day$CU_final <- as.character(m2_cuml_day$CU_final)
    #m2_cuml_day$CU_final <- ifelse(is.na(m2_cuml_day$CU_final), "n/a", m2_cuml_day$CU_final)            

# 2. Summarize by day 
m2_cuml_day <- m2_cuml_day %>% 
  group_by(date, CU_final) %>%
  summarize(n_CU_exp=sum(n_CU_exp), daily_mean_IA=mean(IA), daily_total_IA=sum(IA)) %>%
  print() 

# 3. Determine total number fish in each CU 
CU_totals <- m2_cuml_day %>% 
  group_by(CU_final) %>%                                                                   
  summarize(CU_total_n = sum(n_CU_exp), CU_mean_IA=mean(daily_mean_IA), CU_total_IA = sum(daily_total_IA)) %>%                                                   
  print()

# 4. Join, make new column to divide daily # each CU by CU total   
CU.total.merge <- left_join(m2_cuml_day, CU_totals, by="CU_final")                           

final_date <- CU.total.merge %>% 
  mutate(daily_perc_n=n_CU_exp/CU_total_n, 
         daily_mean_perc_IA=daily_mean_IA/CU_total_IA, 
         daily_total_perc_IA=daily_total_IA/CU_total_IA) %>%                                                 
  print()

# 5. Calculate cumulative proportions 
final_date_cuml <- final_date %>% 
  group_by(CU_final) %>%                                                               
  mutate(cuml_propn_n=cumsum(daily_perc_n), 
         cuml_propn_mean_IA=cumsum(daily_mean_perc_IA),
         cuml_propn_total_IA=cumsum(daily_total_perc_IA)) %>%
  print()

# 6. Just choose top 7 CUs 
top_7 <- c("Chilko (S)", "Francois-Fraser (S)", "Chilko (ES)", "Chilko Combined", "Quesnel (S)")
final_date_cuml_7 <- final_date_cuml %>% 
  filter(CU_final %in% top_7) %>%                                                               
  print()

# 7. Find migration timing info 
dates <- final_date_cuml_7 %>% 
  group_by(CU_final) %>%                                                                   
  summarize(first = min(date), last = max(date))  %>%                                     
  print()




#--------------------------------------------------------------------------------------------------------------------------------------
    




# PLOT: Fig 2.2/2.3a
final_date_cuml_7$date <- as.Date(final_date_cuml_7$date)

ggplot() +
  geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
  
  geom_line(data=final_date_cuml_7, aes(x=date, y=cuml_propn_n, group=CU_final, colour=CU_final), linetype=2, size=2, alpha=0.5) +
  geom_point(data=final_date_cuml_7, aes(x=date, y=cuml_propn_n, fill=CU_final),colour="black", size=4, pch=21, alpha=0.4) +
  
  #geom_line(data=m1_final_date_cuml_5, aes(x=date, y=cuml_propn_IA, group=CU_final, colour=CU_final), size=2) +
  #geom_point(data=m1_final_date_cuml_5, aes(x=date, y=cuml_propn_IA, fill=CU_final), size=4, pch=21) +
  
  geom_line(data=final_date_cuml_7, aes(x=date, y=cuml_propn_IA, group=CU_final, colour=CU_final), size=2) +
  geom_point(data=final_date_cuml_7, aes(x=date, y=cuml_propn_IA, fill=CU_final),colour="black", size=4, pch=21) +
  
  scale_x_date(date_breaks = "4 day", date_labels = ("%h %d")) +
  scale_colour_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +                 # dark blue, aquamarine, pink, green, orange
  scale_fill_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +
  theme(text = element_text(colour="black", size=12),
        plot.margin=margin(t=10,r=25,b=0,l=10),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y.left = element_text(margin=margin(t=0,r=20,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=25),
        legend.title = element_text(size=30, face="bold"),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.80,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("Cumulative proportion") +
  xlab("Date") 


    # PLOT: Fig 2.2b - zoomed in at messy 50% intersecting point
    final_date_cuml_7$date <- as.Date(final_date_cuml_7$date)
    
    ggplot() +
      geom_hline(aes(yintercept=0.5), linetype="dotted", colour="black", size=2) +
      
      geom_line(data=final_date_cuml_7, aes(x=date, y=cuml_propn_n, group=CU_final, colour=CU_final), linetype=2, size=3, alpha=0.55) +
      geom_point(data=final_date_cuml_7, aes(x=date, y=cuml_propn_n, fill=CU_final),colour="black", size=5, pch=21, alpha=0.55) +
      
      geom_line(data=m1_final_date_cuml_5, aes(x=date, y=cuml_propn_IA, group=CU_final, colour=CU_final), size=3) +
      geom_point(data=m1_final_date_cuml_5, aes(x=date, y=cuml_propn_IA, fill=CU_final), size=5, pch=21) +

      
   #   geom_line(data=final_date_cuml_7, aes(x=date, y=cuml_propn_IA, group=CU_final, colour=CU_final), size=3) +
    #  geom_point(data=final_date_cuml_7, aes(x=date, y=cuml_propn_IA, fill=CU_final),colour="black", size=5, pch=21) +
      
      scale_x_date(limits=as.Date(c("2017-05-05", "2017-05-08")), date_breaks = "1 day", date_labels = ("%h %d")) +
      scale_y_continuous(limits=c(0.10,0.90), breaks=seq(0.10,0.90, by=0.2))+
      scale_colour_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +                 # dark blue, aquamarine, pink, green, orange
      scale_fill_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +
      theme(text = element_text(colour="black", size=45),
              plot.margin=margin(t=15,r=15,b=0,l=0),
              panel.background = element_rect(fill = "white", colour = "black", size=2),
              panel.grid.minor = element_line(colour = "transparent"),
              panel.grid.major = element_line(colour = "transparent"),
              plot.background = element_rect(fill = "transparent"),
              axis.ticks = element_line(size=1.2),
              axis.ticks.length = unit(0.5, "line"),
              axis.title.y.left = element_text(margin=margin(t=0,r=0,b=0,l=0), face="bold", size=30),
              axis.text.y = element_text(colour="black", size=45),
              axis.title.x = element_text(margin=margin(t=0,r=0,b=0,l=0), face="bold", size=30),
              axis.text.x = element_text(colour="black", angle=45, hjust=1, size=45),
            legend.position = "none") +                                                               # Position order is: horizontal adjustment, vertical adjustment   
      ylab("") +
      xlab("") 



  
  

  
  
  
  
  
  
  





























