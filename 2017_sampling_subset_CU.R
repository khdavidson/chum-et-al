# Sampling interval analysis 

# Based on 2017 data, can we sample every 2nd day and still catch roughly the same stocks? 

# Read in big 2017 dataframe 
data <- read.csv("TEB_leftjoin.csv")
# Make 'date' as.Date for R
data$date <- as.Date(data$date)

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringr)
library(grid)
library(gridExtra)
library(openair)


################################################################################################################################################
################################################################################################################################################

# SUBSETTING MEGA DATAFRAME TO CREATE EXPANDED DAILY PROPORTIONS AND COUNTS

# Total number of fish caught daily 
dailycatch <- data %>% 
  select(USID, date, trap_type, UFID, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time, run) %>%
  filter(trap_type =="RST", run_time == "0:15", sockeye_smolt_total != "NR") %>% 
  group_by(date, USID, USID) %>% 
  summarize(unq_catch = unique(sockeye_smolt_total), unq_release = unique(sockeye_smolt_release)) %>% 
  group_by(date, USID) %>% 
  summarize(sum_SO = sum(unq_catch), sum_release = sum(unq_release), sum_sampled = sum_SO-sum_release) %>%
  print()

    # Also replace the one entry on Apr 6 where the fish was both sampled and released - for our purposes, it's a sampled fish so change sampled = 1 and unsampled = 1
    dailycatch[49, 5] = 1
    dailycatch[49, 4] = 0
    
# Number of fish caught in each CU (sampled fish) 
dailysampled <- data %>% 
  select(USID, date, trap_type, UFID, CU_final, run_time_min, run) %>% 
  filter(trap_type == "RST", run_time_min =="15", CU_final != "", CU_final != "Unconfirmed") %>%          
  group_by(date, USID, CU_final) %>%                                                      
  summarize(CU_n = n_distinct(UFID)) %>% 
  print()

    # Rename CU's that are "" as "BLANK" 

# Join 'dailycatch' with 'dailysampled'
catch.sample.merge <- left_join(dailycatch, dailysampled, by=c("date", "USID"))

# Create new column for obseved (sampled) proportion for each CU 
catch.sample.merge <- catch.sample.merge %>%
  mutate(propn_obs = CU_n/sum_sampled)

# Determine NUMBER of subsampled fish to add to totals based on KNOWN CU PROPORTIONS
catch.sample.merge <- catch.sample.merge %>% 
  mutate(n_unsampled_CU = sum_release * propn_obs) %>%                                       # Create a new column "n_unsampled_assgn" that exands the number of unsampled fish by the expansion factor to calculate the number of fish that needs to be added
  mutate(n_CU_exp = CU_n+n_unsampled_CU) %>%
  print()

    # Replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
    catch.sample.merge$n_CU_exp <- ifelse(is.na(catch.sample.merge$n_CU_exp), catch.sample.merge$sum_SO, catch.sample.merge$n_CU_exp)           # Again, some CUs didn't have expansion factors, so just replace NAs generated with their original abundances

    
# Calculate expanded number of fish PER CU PER DAY 
exp_CU <- catch.sample.merge %>%
  filter(CU_final != "") %>%
  group_by(date, CU_final) %>% 
  summarize(CU_n_exp = sum(n_CU_exp))

# Use catch.sample.merge to calculate the total number of EXPANDED fish caught daily  
exp_total <- catch.sample.merge %>% 
  group_by(date) %>% 
  summarize(total_n_exp = sum(n_CU_exp)) %>% 
  print()

# Join daily total and CU total (both expanded)
exp.merge <- left_join(exp_total, exp_CU, by="date")

# Final group by date and % fish from each CU 
date_total <- exp.merge %>% 
  group_by(date, CU_final, CU_n_exp) %>% 
  summarize(CU_exp_propn = CU_n_exp/total_n_exp) %>% 
  print()


# NOW SUBSAMPLE EVERY SECOND ROW 
second_day <- selectByDate(date_total, start = "2017-04-03", end = "2017-06-14", day = seq(1,31,by=2))   # start Apr 3
second_day_alt <- selectByDate(date_total, start = "2017-04-04", end = "2017-06-14", day = seq(1,31,by=2))   # start Apr 3



# Calculate overall % each CU 
  # From sampling every day 
  CU_total <- date_total %>% 
    group_by(CU_final) %>% 
    summarize(sum_CU = sum(CU_n_exp, na.rm=T)) %>% 
    mutate(sum_propn = sum_CU/sum(sum_CU)) %T>%
    write.csv("CU_propn_every_day.csv")
  
  
  # From sampling every second day
  CU_second <- second_day %>% 
    group_by(CU_final) %>% 
    summarize(sum_CU = sum(CU_n_exp, na.rm=T)) %>% 
    mutate(sum_propn = sum_CU/sum(sum_CU)) %T>%
    write.csv("CU_propn_second_day.csv")

  # From sampling every second day-alternative
  CU_second_alt <- second_day_alt %>% 
    group_by(CU_final) %>% 
    summarize(sum_CU = sum(CU_n_exp, na.rm=T)) %>% 
    mutate(sum_propn = sum_CU/sum(sum_CU)) %T>%
    write.csv("CU_propn_second_day_alt.csv")
  
  
# Plot results 
every_day<-ggplot(date_total, aes(x=date,y=CU_exp_propn, fill=CU_final)) +
  geom_bar(stat="identity")
scnd_day<-ggplot(second_day, aes(x=date,y=CU_exp_propn, fill=CU_final)) +
  geom_bar(stat="identity")
    
grid.newpage()
grid.draw(rbind(ggplotGrob(every_day), ggplotGrob(second_day), size="last"))
    
    
    
    
    
    
    
 
