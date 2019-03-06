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
  


################################################################################################################################################
################################################################################################################################################


# Note: the code below corresponds to the word doc "Mission Sockeye smolt CPUE calibrations"


################################################################################################################################################

# CALIBRATE CATCH USING CSA/WATER VOLUME FISHED 
  # RECALL, current varies:
    # Intra-annually                (~Date, due to discharge)
    # Over the course of a day      (~Time, due to tide)
    # Over the course of date-time  (~Date-time, due to tide)
    # Horizontally                  (~Date-run, due to shoreline effects)
  # Sampling effort is:
    # Higher at the surface
    # Unequal among Bays

#--------------------------------------------------------------------------------------------------------------------------------------
   











   
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

    #FIG 2.2. Daily count at Chilko and expanded CPUE for Chilko
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




#--------------------------------------------------------------------------------------------------------------------------------------
    





  
  

  
  
  
  
  
  
  





























