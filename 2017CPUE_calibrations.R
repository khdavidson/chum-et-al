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


data_exp <- read.csv("mission_SO_expanded_CU_counts.csv")                                                                    #

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CHILKO ONLY FIRST 

####
# 1. CPUE per run per CU per day (for simple Chilko)
####
m1_CPUE <- data_exp %>%
  filter(trap_type =="RST") %>%
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%
  mutate_at(vars(c(12)), funs(as.character)) %>%
  mutate(CU_simple = ifelse(CU_final=="Chilko (S)", "Chilko", 
                            ifelse(CU_final=="Chilko (ES)", "Chilko", 
                                   ifelse(CU_final=="Chilko Combined", "Chilko", CU_final))))%>%
  print()

####### CHILKO NORMAL TEMPORAL 

m1_CPUE_cko <- m1_CPUE %>% 
  filter(CU_simple == "Chilko") %>% 
  print()

####
# 2. Create the dataframe where abundance estimates will be calculated
####
# Calculate daily average count (expanded) and daily average CPUE for CHILKO
m1_avgCPUE_cko <- m1_CPUE_cko %>% 
  group_by(date, USID) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), CPUE=mean(CPUE)) %>%
  group_by(date) %>%
  summarize(total_n_exp=sum(n_CU_exp), daily_mean_CPUE=mean(CPUE)) %>%
  mutate(sec_day=86400) %>%
  print()

  # Extract daily discharge from raw data
  discharge <- data %>% 
    select(date, discharge_m3s) %>% 
    group_by(date) %>% 
    summarize(daily_mean_discharge = mean(discharge_m3s, na.rm=T))  

# Link daily discharge and CPUE df
m1_avgCPUE_cko$date <- as.Date(m1_avgCPUE_cko$date)
m1_df_cko <- left_join(m1_avgCPUE_cko, discharge, by = "date")

####
# 3. Calculate abundance estimates
####
m1_df_cko <- m1_df_cko %>% 
  group_by(date) %>%
  mutate(daily_IA = daily_mean_CPUE*daily_mean_discharge*sec_day) %>%  
  print()

####
# 4. Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), daily_IA=0)
# Join 
m1_df_cko <- full_join(ts.df, m1_df_cko, by=c("date", "daily_IA"))

# Remove duplicates, first re-arranging
m1_df_cko <- m1_df_cko %>%
  arrange(date,desc(total_n_exp)) %>%
  distinct(date, .keep_all = T) %>%                # This preserves days with 0 catch and removes the duplicated date row
  print()

####
# 5. Chilko fence data
####
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date))

    #PLOT: Fig 1.1a CHILKO TEMPORAL NORMAL 
    ggplot() +
      geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
                size=1.8, alpha=0.8) +
      geom_point(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko"), 
                 size=0.1, alpha=0.3) +
      geom_line(data=m1_df_cko, aes(x=date,y=daily_IA, colour="Daily abundance estimate at Mission \n (Chilko only)", 
                                linetype="Daily abundance estimate at Mission \n (Chilko only)"), 
                size=1.8, alpha=0.8) +
      geom_point(data=m1_df_cko, aes(x=date, y=daily_IA, colour="Daily abundance estimate at Mission \n (Chilko only)"), 
                 size=0.1, alpha=0.3) +
      geom_bar(data=m1_df_cko, aes(x=date, y=total_n_exp*1000, fill="Count at Mission (Chilko only)"), stat="identity", 
               colour="black", width=1, alpha=0.8) +
      scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                       "Daily abundance estimate at Mission \n (Chilko only)" = "blue")) +
      scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                         "Daily abundance estimate at Mission \n (Chilko only)" = 1)) +
      scale_fill_manual("", values=c("Count at Mission (Chilko only)" = "black")) +
      scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
      scale_y_continuous(labels=comma, 
                         sec.axis = sec_axis(~., name = "Total daily count at Mission (*1000)", breaks=seq(0,400000,by=400000), labels=comma)) +
      theme(plot.margin=margin(t=100,r=10,b=2,l=2),
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


########### CHILKO CUMULATIVE

####
# 4. Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df2 <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-04-19"), by="day"), daily_IA=0)
# Join 
m1_df_cko_cuml <- full_join(ts.df2, m1_df_cko, by=c("date", "daily_IA"))

# Fill in numeric NAs
m1_df_cko_cuml <- m1_df_cko_cuml %>% 
  mutate_at(c(2:3), funs(replace(., is.na(.), 0))) %>% 
  print()

####
# 5. Calculate cumulative data 
####
m1_df_cko_cuml <- m1_df_cko_cuml %>% 
  mutate(cuml_n = cumsum(total_n_exp), cuml_IA = cumsum(daily_IA)) %>% 
  mutate(cuml_n_propn = cuml_n/sum(total_n_exp), cuml_IA_propn=cuml_IA/sum(daily_IA)) %>%
  print()

####
# 6. Chilko fence data with cumulative calculations
####
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  select(-c(3,5)) %>% 
  mutate(cuml_propn_chilko = cuml_count_chilko/sum(daily_count_chilko))

    #PLOT: Fig 1.1b CHILKO TEMPORAL CUMULATIVE
    ggplot() +
      geom_line(data=chilko_fence, aes(x=date, y=cuml_propn_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
                size=1.8, alpha=0.5) +
      geom_point(data=chilko_fence, aes(x=date, y=cuml_propn_chilko, fill="Count at Chilko"), 
                 colour="black", pch=21, size=4, alpha=0.5) +
      geom_line(data=m1_df_cko_cuml, aes(x=date, y=cuml_n_propn, colour="Count at Mission (Chilko only)",
                                         linetype = 'Count at Mission (Chilko only)'),  
               size=1.8, alpha=0.6) +
      geom_point(data=m1_df_cko_cuml, aes(x=date, y=cuml_n_propn, fill="Count at Mission (Chilko only)"), 
                 colour="black", pch=21, size=4, alpha=0.8) +
      geom_line(data=m1_df_cko_cuml, aes(x=date,y=cuml_IA_propn, colour="Daily abundance estimate at \n Mission (Chilko only)", 
                                linetype="Daily abundance estimate at \n Mission (Chilko only)"), 
                size=1.8, alpha=0.8) +
      geom_point(data=m1_df_cko_cuml, aes(x=date, y=cuml_IA_propn, fill="Daily abundance estimate at \n Mission (Chilko only)"), 
                 colour="black", pch=21, size=4, alpha=0.8) +
      scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                       "Count at Mission (Chilko only)" = "black",
                                       "Daily abundance estimate at \n Mission (Chilko only)" = "blue")) +
      scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                         "Count at Mission (Chilko only)" = 1,
                                         "Daily abundance estimate at \n Mission (Chilko only)" = 1)) +
      scale_fill_manual("", values=c("Count at Chilko" = "gray40",
                                         "Count at Mission (Chilko only)" = "black",
                                         "Daily abundance estimate at \n Mission (Chilko only)" = "blue")) +
      scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
      theme(plot.margin=margin(t=10,r=10,b=2,l=2),
                  panel.background = element_rect(fill = "white", colour = "black", size=2),
                  panel.grid.minor = element_line(colour = "transparent"),
                  panel.grid.major = element_line(colour = "transparent"),
                  plot.background = element_rect(fill = "transparent"),
                  axis.ticks = element_line(size=1.2),
                  axis.ticks.length = unit(0.5, "line"),
                  axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=20), face="bold", size=35),
                  axis.text.y = element_text(colour="black", size=30),
                  axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=35),
                  axis.text.x = element_text(colour="black", angle=45, hjust=1, size=30),
                  legend.text = element_text(size=30),
                  legend.position = c(0.75,0.2),
                  legend.background = element_blank(),
                  legend.box.background = element_rect(colour = "black"),
                  legend.spacing.y = unit(-4, "mm"),                                                                              
                  legend.key.height = unit(3, "line"),                                                                                    
                  legend.key.width = unit(2, "line")) +                                                                                   
      guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
      ylab("Cumulative proportion") +
      xlab("Date")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ALL CUs - cumulative 

####
# 1. CPUE per run per CU per day
####
m1_CPUE <- data_exp %>%
  filter(trap_type =="RST") %>%
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%
  mutate_at(vars(c(12)), funs(as.character)) %>%
  print()

####
# 2. Create the dataframe where abundance estimates will be calculated
####
# Calculate daily average count (expanded) and daily average CPUE for CHILKO
m1_avgCPUE <- m1_CPUE %>% 
  group_by(date, USID, CU_final) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), CPUE=sum(CPUE)) %>%
  group_by(date, CU_final) %>%
  summarize(total_n_exp=sum(n_CU_exp), daily_mean_CPUE=mean(CPUE)) %>%
  mutate(sec_day=86400) %>%
  print()

  # Extract daily discharge from raw data
  discharge <- data %>% 
    select(date, discharge_m3s) %>% 
    group_by(date) %>% 
    summarize(daily_mean_discharge = mean(discharge_m3s, na.rm=T))  

# Link daily discharge and CPUE df
m1_avgCPUE$date <- as.Date(m1_avgCPUE$date)
m1_df <- left_join(m1_avgCPUE, discharge, by = "date")

####
# 3. Calculate abundance estimates
####
m1_df <- m1_df %>% 
  group_by(date) %>%
  mutate(daily_IA = daily_mean_CPUE*daily_mean_discharge*sec_day) %>%  
  print()
    
####
# 4. Calculate cumulative data 
####
# Calculate cumulative values
m1_df_cuml <- m1_df %>%
  group_by(CU_final) %>%
  mutate(cuml_n = cumsum(total_n_exp), cuml_IA = cumsum(daily_IA)) %>%
  print()
    
  # Determine CU totals
  CU_totals <- m1_df %>% 
    group_by(CU_final) %>% 
    summarize(sum_n_exp = sum(total_n_exp), sum_daily_IA = sum(daily_IA)) %>% 
    print()
    
  # Join m1_df_cuml with CU_totals
  m1_df_cuml <- left_join(m1_df_cuml, CU_totals, by="CU_final")
    
# Calculate cumulative PROPORTIONS (for plotting)
m1_df_cuml <- m1_df_cuml %>% 
  mutate(cuml_n_propn = cuml_n/sum_n_exp, cuml_IA_propn = cuml_IA/sum_daily_IA) %>% 
  print()
    
####
# 5. Choose top 5 and find migration timing info 
####
top_5 <- c("Chilko (S)", "Francois-Fraser (S)", "Chilko (ES)", "Chilko Combined", "Quesnel (S)")
m1_df_cuml5 <- m1_df_cuml %>% 
  filter(CU_final %in% top_5) %>%                                                               
  print()

# Find migration timing info 
dates <- m1_df_cuml5 %>% 
  group_by(CU_final) %>%                                                                   
  summarize(first = min(date), last = max(date))  %>%                                     
  print()

    #PLOT: Fig 1.2a ALL CUs CUMULATIVE
    ggplot() +
      geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
      geom_line(data=m1_df_cuml5, aes(x=date, y=cuml_n_propn, group=CU_final, colour=CU_final), linetype=2, size=2, alpha=0.6) +
      geom_point(data=m1_df_cuml5, aes(x=date, y=cuml_n_propn, fill=CU_final), colour="black", size=4, pch=21, alpha=0.6) +
      geom_line(data=m1_df_cuml5, aes(x=date, y=cuml_IA_propn, group=CU_final, colour=CU_final), size=2) +
      geom_point(data=m1_df_cuml5, aes(x=date, y=cuml_IA_propn, fill=CU_final), colour="black", size=4, pch=21) +
      scale_x_date(date_breaks = "4 day", date_labels = ("%h %d")) +
      scale_colour_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +                 # dark blue, aquamarine, pink, green, orange
      scale_fill_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +
      theme(plot.margin=margin(t=10,r=25,b=0,l=10),
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

        # PLOT: Fig 1.2b - zoomed in at messy 50% intersecting point
        ggplot() +
          geom_hline(aes(yintercept=0.5), linetype="dotted", colour="black", size=2) +
          geom_line(data=m1_df_cuml5, aes(x=date, y=cuml_n_propn, group=CU_final, colour=CU_final), linetype=2, size=4, alpha=0.55) +
          geom_point(data=m1_df_cuml5, aes(x=date, y=cuml_n_propn, fill=CU_final),colour="black", size=9, pch=21, alpha=0.55) +
          geom_line(data=m1_df_cuml5, aes(x=date, y=cuml_IA_propn, group=CU_final, colour=CU_final), size=4) +
          geom_point(data=m1_df_cuml5, aes(x=date, y=cuml_IA_propn, fill=CU_final), size=9, pch=21) +
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# CHILKO ONLY FIRST 

####
# 1. CPUE per run per CU per day (for simple Chilko)
####
m2_IA <- data_exp %>%
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


####### CHILKO NORMAL TEMPORAL 

m2_IA_cko <- m2_IA %>% 
  filter(CU_simple == "Chilko") %>% 
  print()


####
# 2. Create the dataframe where abundance estimates will be calculated
####
# Calculate daily average count (expanded) and daily average IA for CHILKO
m2_avgIA_cko <- m2_IA_cko %>% 
  group_by(date, USID) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), IA=mean(IA)) %>%
  group_by(date) %>%
  summarize(total_n_exp=sum(n_CU_exp), daily_mean_IA=mean(IA)) %>%
  print()


####
# 3. Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-06-14"), by="day"), daily_mean_IA=0)
# Join 
m2_avgIA_cko$date <- as.Date(m2_avgIA_cko$date)
m2_df_cko <- full_join(ts.df, m2_avgIA_cko, by=c("date", "daily_mean_IA"))

# Remove duplicates, first re-arranging
m2_df_cko <- m2_df_cko %>%
  arrange(date,desc(total_n_exp)) %>%
  distinct(date, .keep_all = T) %>%                # This preserves days with 0 catch and removes the duplicated date row
  print()


####
# 4. Chilko fence data
####
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date))


    #PLOT: Fig 2.1a CHILKO TEMPORAL NORMAL 
    ggplot() +
      geom_line(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
                size=1.8, alpha=0.7) +
      geom_point(data=chilko_fence, aes(x=date, y=daily_count_chilko, colour="Count at Chilko"), 
                 size=0.1, alpha=0.01) +
      geom_line(data=m1_df_cko, aes(x=date,y=daily_IA, colour="M1 Daily abundance \n estimate at Mission", 
                                linetype="M1 Daily abundance \n estimate at Mission"), 
                size=1.8, alpha=0.7) +
      geom_point(data=m1_df_cko, aes(x=date, y=daily_IA, colour="M1 Daily abundance \n estimate at Mission"), 
                 size=0.1, alpha=0.3) +
      geom_line(data=m2_df_cko, aes(x=date,y=daily_mean_IA*1000, colour="M2 Daily abundance \n estimate at Mission (*1000)", 
                                linetype="M2 Daily abundance \n estimate at Mission (*1000)"), 
                size=1.8, alpha=0.7) +
      geom_point(data=m2_df_cko, aes(x=date, y=daily_mean_IA*1000, colour="M2 Daily abundance \n estimate at Mission (*1000)"), 
                 size=0.1, alpha=0.01) +
      geom_bar(data=m2_df_cko, aes(x=date, y=total_n_exp*1000, fill="Count at Mission (*1000)"), stat="identity", 
               colour="black", width=1, alpha=0.8) +
      scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                       "M1 Daily abundance \n estimate at Mission" = "blue",
                                       "M2 Daily abundance \n estimate at Mission (*1000)" = "red")) +
      scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                         "M1 Daily abundance \n estimate at Mission" = 1,
                                         "M2 Daily abundance \n estimate at Mission (*1000)" = 1)) +
      scale_fill_manual("", values=c("Count at Mission (*1000)" = "black")) +
      scale_x_date(date_breaks = "5 day", date_labels = "%h %d") +
      scale_y_continuous(labels=comma, 
                         sec.axis = sec_axis(~., name = "Daily count at Mission \n Daily abundance estimate at Mission", 
                                             breaks=seq(0,2500000,by=750000), labels=comma)) +
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
                  legend.position = c(0.74,0.80),
                  legend.background = element_blank(),
                  legend.box.background = element_rect(colour = "black"),
                  legend.spacing.y = unit(-4, "mm"),                                                                              
                  legend.key.height = unit(3, "line"),                                                                                    
                  legend.key.width = unit(2, "line")) +                                                                                   
      guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
      ylab("Daily count at Chilko") +
      xlab("Date")



########### CHILKO CUMULATIVE

####
# 3. Time series cleanup
####
# Fill in days with 0 to eventually make Fig 2.1a
ts.df2 <- data.frame(date=seq(ymd("2017-04-03"), ymd("2017-04-19"), by="day"), daily_mean_IA=0)
# Join 
m2_avgIA_cko$date <- as.Date(m2_avgIA_cko$date)
m2_df_cko_cuml <- full_join(ts.df2, m2_avgIA_cko, by=c("date", "daily_mean_IA"))

# Fill in NAs with 0s
m2_df_cko_cuml <- m2_df_cko_cuml %>% 
  mutate_at(c(2:3), funs(replace(., is.na(.), 0))) %>% 
  print()

####
# 4. Calculate cumulative data 
####
m2_df_cko_cuml <- m2_df_cko_cuml %>% 
  mutate(cuml_n = cumsum(total_n_exp), cuml_IA = cumsum(daily_mean_IA)) %>% 
  mutate(cuml_n_propn = cuml_n/sum(total_n_exp), cuml_IA_propn=cuml_IA/sum(daily_mean_IA)) %>%
  print()


####
# 5. Chilko fence data with cumulative calculations
####
chilko_fence <- read.csv("chilko_fence.csv")
chilko_fence <- chilko_fence %>%                                                   
  rename(date = DATE,
         daily_count_chilko = Daily.Count...Chilko,                            
         daily_propn_chilko = Daily.Proportion...Chilko,
         cuml_count_chilko = Cumulative.Chilko.Total,
         cuml_propn_chilko = X..Cumulative.Chilko) %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  select(-c(3,5)) %>% 
  mutate(cuml_propn_chilko = cuml_count_chilko/sum(daily_count_chilko))


    #PLOT: Fig 2.1b CHILKO TEMPORAL CUMULATIVE
    ggplot() +
      geom_line(data=chilko_fence, aes(x=date, y=cuml_propn_chilko, colour="Count at Chilko", linetype = "Count at Chilko"), 
                size=1.8, alpha=0.5) +
      geom_point(data=chilko_fence, aes(x=date, y=cuml_propn_chilko, fill="Count at Chilko"), 
                 colour="black", pch=21, size=4, alpha=0.5) +
      geom_line(data=m2_df_cko_cuml, aes(x=date, y=cuml_n_propn, colour="Count at Mission (Chilko only)",
                                         linetype = 'Count at Mission (Chilko only)'),  
               size=1.8, alpha=0.6) +
      geom_point(data=m2_df_cko_cuml, aes(x=date, y=cuml_n_propn, fill="Count at Mission (Chilko only)"), 
                 colour="black", pch=21, size=4, alpha=0.8) +
      
      geom_line(data=m1_df_cko_cuml, aes(x=date,y=cuml_IA_propn, colour="M1 Daily abundance estimate at \n Mission (Chilko only)", 
                                linetype="M1 Daily abundance estimate at \n Mission (Chilko only)"), 
                size=1.8, alpha=0.7) +
      geom_point(data=m1_df_cko_cuml, aes(x=date, y=cuml_IA_propn, fill="M1 Daily abundance estimate at \n Mission (Chilko only)"), 
                 colour="black", pch=21, size=4, alpha=0.7) +
      
      geom_line(data=m2_df_cko_cuml, aes(x=date,y=cuml_IA_propn, colour="M2 Daily abundance estimate at \n Mission (Chilko only)", 
                                linetype="M2 Daily abundance estimate at \n Mission (Chilko only)"), 
                size=1.8, alpha=0.7) +
      geom_point(data=m2_df_cko_cuml, aes(x=date, y=cuml_IA_propn, fill="M2 Daily abundance estimate at \n Mission (Chilko only)"), 
                 colour="black", pch=21, size=4, alpha=0.7) +
      
      scale_colour_manual("", values=c("Count at Chilko" = "gray40",
                                       "Count at Mission (Chilko only)" = "black",
                                       "M1 Daily abundance estimate at \n Mission (Chilko only)" = "blue",
                                       "M2 Daily abundance estimate at \n Mission (Chilko only)" = "red")) +
      scale_linetype_manual("", values=c("Count at Chilko" = 1,
                                         "Count at Mission (Chilko only)" = 1,
                                         "M1 Daily abundance estimate at \n Mission (Chilko only)" = 1,
                                         "M2 Daily abundance estimate at \n Mission (Chilko only)" = 1)) +
      scale_fill_manual("", values=c("Count at Chilko" = "gray40",
                                     "Count at Mission (Chilko only)" = "black",
                                     "M1 Daily abundance estimate at \n Mission (Chilko only)" = "blue",
                                     "M2 Daily abundance estimate at \n Mission (Chilko only)" = "red")) +
      scale_x_date(date_breaks = "5 day", date_labels = "%h-%d") +
      theme(plot.margin=margin(t=10,r=10,b=2,l=2),
                  panel.background = element_rect(fill = "white", colour = "black", size=2),
                  panel.grid.minor = element_line(colour = "transparent"),
                  panel.grid.major = element_line(colour = "transparent"),
                  plot.background = element_rect(fill = "transparent"),
                  axis.ticks = element_line(size=1.2),
                  axis.ticks.length = unit(0.5, "line"),
                  axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=20), face="bold", size=35),
                  axis.text.y = element_text(colour="black", size=30),
                  axis.title.x = element_text(margin=margin(t=15,r=0,b=2,l=0), face="bold", size=35),
                  axis.text.x = element_text(colour="black", angle=45, hjust=1, size=30),
                  legend.text = element_text(size=30),
                  legend.position = c(0.74,0.2),
                  legend.background = element_blank(),
                  legend.box.background = element_rect(colour = "black"),
                  legend.spacing.y = unit(-4, "mm"),                                                                              
                  legend.key.height = unit(3, "line"),                                                                                    
                  legend.key.width = unit(2, "line")) +                                                                                   
      guides(fill=guide_legend(keywidth=0.35, keyheight=0.4, default.unit="inch")) +                                              
      ylab("Cumulative proportion") +
      xlab("Date")


    
    
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FOR ALL CUS! 
        
####
# 1. Apply Equations 1-3 to expanded counts for all CUs
####
m2_IA <- data_exp %>% 
  filter(trap_type == "RST") %>% 
  mutate(fished_vol = as.numeric(ifelse(sum_run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(sum_run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(sum_run_time=="1080", "2238.905",
                                 ifelse(sum_run_time=="1140", "2363.288",
                                 ifelse(sum_run_time=="1200", "2487.672",
                                 ifelse(sum_run_time=="1260", "2612.056",
                                 ifelse(sum_run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = n_CU_exp/fished_vol) %>%                                                                                                               # Step 1: Calculate #fish/m3 (in x seconds)
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*sum_run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE*bay_volume_m3) %>%                                                                                                                # Step 3: CPUE fish abundance (Step 1*Step 2)                                        
  print() 
        
####
# 2. Create the dataframe where abundance estimates will be calculated
####
m2_avgIA <- m2_IA %>% 
  group_by(date, USID, CU_final) %>% 
  summarize(n_CU_exp=sum(n_CU_exp), IA=mean(IA)) %>%
  group_by(date, CU_final) %>%
  summarize(total_n_exp=sum(n_CU_exp), daily_mean_IA=mean(IA)) %>%
  print()
     
####
# 3. Calculate cumulative data 
####
# Calculate cumulative values
m2_df_cuml <- m2_avgIA %>%
  group_by(CU_final) %>%
  mutate(cuml_n = cumsum(total_n_exp), cuml_IA = cumsum(daily_mean_IA)) %>%
  print()
    
  # Determine CU totals
  CU_totals <- m2_avgIA %>% 
    group_by(CU_final) %>% 
    summarize(sum_n_exp = sum(total_n_exp), sum_daily_IA = sum(daily_mean_IA)) %>% 
    print()
    
  # Join m1_df_cuml with CU_totals
  m2_df_cuml <- left_join(m2_df_cuml, CU_totals, by="CU_final")
    
# Calculate cumulative PROPORTIONS (for plotting)
m2_df_cuml <- m2_df_cuml %>% 
  mutate(cuml_n_propn = cuml_n/sum_n_exp, cuml_IA_propn = cuml_IA/sum_daily_IA) %>% 
  print()
    
####
# 4. Choose top 5 and find migration timing info 
####
top_5 <- c("Chilko (S)", "Francois-Fraser (S)", "Chilko (ES)", "Chilko Combined", "Quesnel (S)")
m2_df_cuml5 <- m2_df_cuml %>% 
  filter(CU_final %in% top_5) %>%                                                               
  print()

# Find migration timing info
m2_df_cuml5$date <- as.Date(m2_df_cuml5$date)
dates <- m2_df_cuml5 %>% 
  group_by(CU_final) %>%                                                                   
  summarize(first = min(date), last = max(date))  %>%                                     
  print()
        
    #PLOT: Fig 2.2a ALL CUs CUMULATIVE
    ggplot() +
      geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
      geom_line(data=m2_df_cuml5, aes(x=date, y=cuml_n_propn, group=CU_final, colour=CU_final), linetype=2, size=2, alpha=0.6) +
      geom_point(data=m2_df_cuml5, aes(x=date, y=cuml_n_propn, fill=CU_final), colour="black", size=4, pch=21, alpha=0.6) +
      geom_line(data=m2_df_cuml5, aes(x=date, y=cuml_IA_propn, group=CU_final, colour=CU_final), size=2) +
      geom_point(data=m2_df_cuml5, aes(x=date, y=cuml_IA_propn, fill=CU_final), colour="black", size=4, pch=21) +
      scale_x_date(date_breaks = "4 day", date_labels = ("%h %d")) +
      scale_colour_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +                 # dark blue, aquamarine, pink, green, orange
      scale_fill_manual(name="WSP Conservation Unit", values = c("#0c1cf2", "#4CF0F4", "#f95353", "#4ebf15", "#ff8d00")) +
      theme(plot.margin=margin(t=10,r=25,b=0,l=10),
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
        ggplot() +
          geom_hline(aes(yintercept=0.5), linetype="dotted", colour="black", size=2) +
          geom_line(data=m2_df_cuml5, aes(x=date, y=cuml_n_propn, group=CU_final, colour=CU_final), linetype=2, size=4, alpha=0.55) +
          geom_point(data=m2_df_cuml5, aes(x=date, y=cuml_n_propn, fill=CU_final),colour="black", size=9, pch=21, alpha=0.55) +
          geom_line(data=m2_df_cuml5, aes(x=date, y=cuml_IA_propn, group=CU_final, colour=CU_final), size=4) +
          geom_point(data=m2_df_cuml5, aes(x=date, y=cuml_IA_propn, fill=CU_final), size=9, pch=21) +
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
        
        
        
        
        
        
        
        
        
        
        
        
        
        














































