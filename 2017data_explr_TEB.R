# Merging TALLY and ENV data from previous "2017data_cleaning" script
# 18-Feb-2019

# Set WORK working directory folder
setwd("~/`Stock assessment/Analysis/Data files")
# Set HOME working directory 
setwd("~/DFO BI02 Stock assessment data analysis/Analysis/Data files")

# Load packages to use 
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(grid)
library(magrittr)
library(compare)
library(lme4)


##################################################################################################################################################
##################################################################################################################################################

#     The following code can be skipped as it deals with merging dataframes - already done, just load file on line 192

##################################################################################################################################################
##################################################################################################################################################



#####################
#
#  Data selection/subsetting
#
#####################



########## TALLY DATA
tally.df <- read.csv("Mission_Sockeye_TALLY_2017_clean.csv")

# Quickly just make date as.Date
tally.df$date <- as.Date(tally.df$date)

# Select variables of interest for reducted TALLY dataframe - focus on the key data and metadata
pipe_printt = function(tally.abbrv) {print(tally.abbrv); tally.abbrv}          # Function to print dplyr results at end of pipe below.
tally.abbrv <- tally.df %>% 
  select(c(1:21,27,29,30,32,47,68,69)) %>%                                     # Select column numbers of interest based on column numbers (quicker than typing out all column names, but you could do that too). Omitted other species for now
  pipe_printt()




########## ENVIRONMENTAL DATA -- first need to join discharge file to ENV data file as they were separate 
env.df <- read.csv("Mission_Sockeye_ENV_2017_clean.csv")
dis.df <- read.csv("Mission_Sockeye_HOPEDISCHARGE_2017_clean.csv")

# Quickly just make date as.Date
env.df$date <- as.Date(env.df$date)
dis.df$date <- as.Date(dis.df$date)

# Merge ENV + DISCHARGE 
ed.merge <- left_join(env.df, dis.df, by="date")

# Select variables of interest for reducted ENV dataframe - focus on the key data and metadata
pipe_printe = function(env.abbrv) {print(env.abbrv); env.abbrv}          # Function to print dplyr results at end of pipe below.
env.abbrv <- ed.merge %>% 
  select(c(1:20)) %>%                                                    # Select column numbers of interest based on column numbers (quicker than typing out all column names, but you could do that too). All of them for now
  filter(run != "RNA") %>%                                               # Remove a weird NA that was included
  pipe_printe()





#####################
#
#  MERGE TALLY + ENV(+discharge) dataframes
#
#####################

# Goal: merge TALLY and ENV dataframes into one database for 2017 

# Join env.abbrv and tally.abbrv by env_index
te.merge <- left_join(tally.abbrv, ed.merge, by="env_index")                # Joining dataframes. Syntax is: left_join(x, y, by="relational key") Will give warnings because is coercing USID from factors into characters, not an issue 

# Clean up merged file for duplicate columns
# left_join(x,y) creates duplicate columns with suffix .x and .y to identify duplicated columns from x and y (tally.abbrv and bio.abbrv, respectively)
col_old <- colnames(te.merge)                                                # Store all the column names from tb.merge dataframe in "col_old" as essentially a text vector (these are no longer associated with data)
col_new <- gsub(pattern = "[.]x$", replacement="", x=col_old)                # Dropping the suffix ".x" from the columns
colnames(te.merge) <- col_new                                                # Apply new column names as column names of tb.merge - Note syntax order here matters! It's essentially pasting the new column names back on top of the dataframe so they are now associated with data again.

col_new <- colnames(te.merge)                                                # Store the new column names of tb.merge in "col_new" - as above, just a vector of text now
col_new_new <- gsub(pattern = "[.]y$", replacement="", x=col_new)            # Dropping the suffix ".y" from the columns 
colnames(te.merge) <- col_new_new                                            # Apply new column names as column names of tb.merge

# Now we have duplicate date, run, bay, etc. columns, so we want to drop all duplicated columns
te.merge <- te.merge[, !duplicated(colnames(te.merge))]                      # For the column names in tb.merge, remove duplicates (indicated by "!duplicated") - ! usually means remove             

# Export as .csv
write.csv(te.merge, "TE_leftjoin.csv", row.names = F)                        # Export this merged dataframe as a .csv just because 







#####################
#
#  SUPERMERGE (TALLY + ENV(+discharge)) + BIO dataframes
#
#####################

# Read in the dataframes 
te.merge <- read.csv("TE_leftjoin.csv")
bio.df <- read.csv("Mission_Sockeye_BIO_2017_clean.csv")

# Quickly switch back dates to as.Date
te.merge$date <- as.Date(te.merge$date)
bio.df$date <- as.Date(bio.df$date)


########## BIO DATA

# Select variables of interest for reducted BIO dataframe - there are many empty or (what appear to be) old columns, so we will just drop them 
# and focus on the key data and metadata
pipe_printb = function(bio.abbrv) {print(bio.abbrv); bio.abbrv}               # Function to print dplyr results at end of pipe below.
bio.abbrv <- bio.df %>% 
  select(c(1:68,84,85)) %>%                                                   # Select column numbers of interest based on column numbers
  pipe_printb()



# Goal: merge TALLY+ENV and BIO dataframes into one database for 2017 

# Join te.merge and bio.abbrv by USID
teb.merge <- left_join(te.merge, bio.abbrv, by="USID")                        # Joining dataframes. Syntax is: left_join(x, y, by="relational key") Will give warnings because is coercing USID from factors into characters, not an issue 

# Clean up merged file for duplicate columns
# left_join(x,y) creates duplicate columns with suffix .x and .y to identify duplicated columns from x and y (tally.abbrv and bio.abbrv, respectively)
col_old <- colnames(teb.merge)                                                # Store all the column names from tb.merge dataframe in "col_old" as essentially a text vector (these are no longer associated with data)
col_new <- gsub(pattern = "[.]x$", replacement="", x=col_old)                 # Dropping the suffix ".x" from the columns
colnames(teb.merge) <- col_new                                                # Apply new column names as column names of tb.merge - Note syntax order here matters! It's essentially pasting the new column names back on top of the dataframe so they are now associated with data again.

col_new <- colnames(teb.merge)                                                # Store the new column names of tb.merge in "col_new" - as above, just a vector of text now
col_new_new <- gsub(pattern = "[.]y$", replacement="", x=col_new)             # Dropping the suffix ".y" from the columns 
colnames(teb.merge) <- col_new_new                                            # Apply new column names as column names of tb.merge

# Now we have duplicate date, run, bay, etc. columns, so we want to drop all duplicated columns
teb.merge <- teb.merge[, !duplicated(colnames(teb.merge))]                    # For the column names in tb.merge, remove duplicates (indicated by "!duplicated") - ! usually means remove             

# Export as .csv
write.csv(teb.merge, "TEB_leftjoin.csv", row.names = F)                       # Export this merged dataframe as a .csv just because 




############################################################################################################################################################
############################################################################################################################################################



# README FIRST BEFORE GOING FORWARD! 

# A few notes on the data structure of "teb.merge": 
# Think of the TALLY, ENV and BIO data as essentially nested: 
# Every Tally entry (i.e., sampling event) has at least one ENV entry, but some ENV entries are associated with duplicate Tally entries - 
# That is, some sampling events have two Env entries associated with them. These are linked by an Environmental Index "env_index" which 
# links sampling events with associated Environmental data. This is a concatenated "date-run" label. 
# AS WELL, TALLY does not always have unique fish IDs (UFID(s)) associated with every unique sampling event (USID) (i.e., not every sampling 
# event resulted in a fish being sampled), but in BIO, an individual fish (UFID) will always have an associated sampling event (USID). 
# Therefore, there are many sampling events, some with associated fish and some without. 
# In general, the left_join(x,y,by = "") function takes the left, or x, dataframe (e.g., TALLY+ENV), and expands it out to incorporate all cases 
# where there is a matching observation in the right, or y, dataframe (e.g., BIO). We match these observations using, for example, USID 
# ('by = "USID"'). 

# The relational keys are: 
# Tally - Environmental: "env_index" = te.merge
# te.merge - Biosample: "USID" = teb.merge

# The result is one large dataframe where all sampling events (USIDs) are duplicated for the number of fish sampled (UFIDs) and environmental 
# observations (env_index). This means if you want to quickly calculate the total number of sockeye smolts on any given day, or for any trap, 
# or whatever, 
# *** YOU NEED TO USE THE unique() function first *** 
# (rather than sum()) - otherwise you will end up with millions of fish as it is summarzing every single entry, potentially those duplicated! 
# After you have done that, you can likely use sum(), depending on your grouping structure.



############################################################################################################################################################


# Now some data exploration! 
# Load TALLY + ENVIRONMENTAL + BIOSAMPLE merged superfile!
teb.merge <- read.csv("TEB_leftjoin.csv")
teb.merge$date <- as.Date(teb.merge$date)



# Total sockeye smolt catch by TRAP TYPE
teb.merge %>% 
  select(trap_type, sockeye_smolt_total, run_time, run_time_min, USID) %>%                 # In general, this function selects which variables (i.e., column names) I want to consider in this (or any subsequent) subsetting exercises. 
  group_by(trap_type, USID) %>%                                                            # Group by unique sampling event (USID) and trap type
  summarize(no_SO = unique(sockeye_smolt_total, na.rm=T)) %>%                              # Create a new variable ("no_SO") that counts the unique total catch of sockeye 
  group_by(trap_type) %>%                                                                  # Then group by trap type
  summarize(sum=sum(no_SO, na.rm=T)) %>%                                                   # Create a new variable ("sum") that sums the total number of sockeye in each trap
  print() %>%                                                                              # Print a table of the summarized results
  ggplot(aes(x=trap_type, y=sum)) +                                                      # Pipe right into ggplot for quick visual results
  geom_bar(stat="identity", fill="gray80", colour="black") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black", size=20)) +
  theme(text = element_text(size=15)) +
  ylab("Total sockeye smolts") +
  xlab("Trap type")





# Total sockeye smolt catch by DATE
teb.merge %>% 
  select(date, sockeye_smolt_total, USID, trap_type) %>% 
  filter(trap_type == "RST") %>%                                                            # Filter to select RST only
  group_by(date, USID) %>%                                                                  # Group by sampling event (USID) and date
  summarize(no_SO = unique(sockeye_smolt_total, na.rm=T)) %>%                               # Create a new variable ("no_SO") that counts the number of unique total sockeye smolt counts
  group_by(date) %>%                                                                        # Then group again by date
  summarize(sum=sum(no_SO)) %>%                                                             # Create a new variable ("sum") that sums the total number of sockeye for each date 
  ggplot(aes(x=date, y=sum)) +
  geom_bar(stat="identity", fill="gray70", colour="black") + 
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(text = element_text(size=15),
        plot.margin=margin(t=10,r=17,b=10,l=10),                                        # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(colour = "black", size=20),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0)),                    # Margin spacing order is: top, right, bottom, left
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0))) +
  ylab("Number of sockeye smolts") +
  xlab("Date")




# Re-make "CPUE-SAMPLING EFFORT" (Fig 8 in 2017): CPUE, # Sockeye smolts and # runs
# METHOD 1: 
teb.merge %>% 
  select(date, sockeye_smolt_total, run, trap_type, USID) %>%                                
  filter(trap_type == "RST") %>%                                                                      # RST only
  group_by(date, USID) %>%                                                                            # Group by sampling event and date
  summarize(nruns = unique(run), no_SO = unique(sockeye_smolt_total, na.rm=T)) %>%                    # Create new variables ("nruns", and "no_SO") to summarize the number of runs and the number of sockeye caught in each sampling event on each date
  group_by(date) %>%                                                                                  # Then just group by date
  summarize(nruns = n_distinct(nruns), sum = sum(no_SO), CPUE = (sum/nruns)*10) %>%                   # Create new variables/overwrite old one ("nruns" and "sum") to count the number of runs and total number of sockeye on each date
  ggplot(aes(x=date)) +
  geom_bar(aes(y=sum), stat="identity", fill="gray70", colour="black") +
  geom_line(aes(y=nruns), size=1) +
  geom_line(aes(y=CPUE), linetype="longdash", size=1) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  scale_y_continuous(limits = c(0,500),
                     sec.axis = sec_axis(~., name = "CPUE (*10) and \n Number of runs")) +
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                        # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12,),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0), size=15, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, colour="black", size = 12),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=0,l=0), size = 15, face="bold"),        # Margin spacing order is: top, right, bottom, left
        axis.title.y.right = element_text(margin=margin(t=0,r=0,b=0,l=7), size=15, angle=90)) +
  ylab("Number of sockeye smolts") +
  xlab("Date")





# METHOD 2: If you want to make the same graph as above but split in 3 panels
p <- teb.merge %>% 
  select(date, sockeye_smolt_total, run, trap_type, USID) %>%                                # Same grouping as above, but saved in "p"
  filter(trap_type == "RST") %>% 
  group_by(date, USID) %>% 
  summarize(nruns = unique(run), no_SO = unique(sockeye_smolt_total, na.rm=T)) %>% 
  group_by(date) %>% 
  summarize(nruns = n_distinct(nruns), sum = sum(no_SO), CPUE = (sum/nruns)*10)

p1 <- ggplot(p, aes(x=date, y=sum)) +                                                        # Each data series must be its own plot to stack plots. p1: Total # sockeye smolts
  geom_bar(stat="identity", fill="gray60", colour="black") +
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.margin=margin(t=10,r=17,b=10,l=20),                                             # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(colour = "black", size=12),
        axis.title.x = element_blank(),                  
        axis.title.y = element_text(margin=margin(t=0,r=28,b=0,l=0)))+
  ylab("Total number of \n sockeye smolts")
p2 <- ggplot(p, aes(x=date, y=CPUE)) +                                                      # p2: CPUE
  geom_line(size=1, linetype="longdash") +
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.margin=margin(t=10,r=17,b=10,l=20),                                            # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(colour = "black", size=12),
        axis.title.x = element_blank(),      
        axis.title.y = element_text(margin=margin(t=0,r=17,b=0,l=0)))+
  ylab("CPUE (*10)")
p3 <- ggplot(p, aes(x=date, y=nruns)) +                                                     # p3: Number of runs
  geom_line(size=1) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(text = element_text(size=13),
        plot.margin=margin(t=10,r=17,b=10,l=20),                                            # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(colour = "black", size=12),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0)),                       # Axis title spacing order is: top, right, bottom, left
        axis.title.y = element_text(margin=margin(t=0,r=17,b=0,l=0)))+
  ylab("Number of runs") +
  xlab("Date")
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))             # Combines all three plots into stacked multipanel plot




# Re-make "CU - LENGTH" (Figure 15 in 2017): Mean smolt length ~ CU 
# First determine if CU's all have at least 3 fish 
teb.merge %>%
  select(CU_final, sockeye_smolt_total) %>% 
  filter(CU_final != "", CU_final != "Unconfirmed") %>%                                       # Remove garbage CUs
  group_by(CU_final) %>%                                                                      # Group by remaining CUs
  summarize(n = n()) %>%                                                                      # Count the number of sockeye smolts in each CU
  arrange(n, desc(n))                                                                         # Arrange in descending order to see the CUs with the lowest number of fish at the top
# Harrison D/S (L) and Nadina-Francois (ES) both have n = 1 smolts therefore remove them for the purposes of length comparisons 

teb.merge %>% 
  select(CU_final, fork_mm, trap_type) %>% 
  filter(trap_type == "RST" & CU_final != "" & CU_final != "Unconfirmed" & 
           CU_final != "Harrison D/S (L)" & CU_final != "Nadina-Francois (ES)") %>%               # RST only, removing garbage CUs, along with the CUs with < 3 fish (see lines 267-272)
  group_by(CU_final) %>%                                                                        # Group by remaining CUs
  summarize_at(vars(fork_mm), funs(mean=round(mean(.),2), sd=round(sd(.),2))) %>%               # Create new variables ("mean" and "sd") for fork length summary stats. Note, here we use summarize_at() instead of summarize() because we apply a function ("funs") to round the summary stats to 2 decimal places
  ggplot(aes(x=reorder(CU_final, +mean), y=mean)) +
  geom_bar(stat="identity", fill="gray70", colour="black") +
  geom_errorbar(stat="identity", aes(ymin = mean-sd, ymax=mean+sd, width=0.2)) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150, by=25)) +
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0), size=15),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=0,l=0), size = 15)) +           # Margin spacing order is: top, right, bottom, left
  geom_text(aes(label=mean), nudge_y=-45) +
  ylab("Fork length") +
  xlab("Conservation unit") +
  coord_flip()                                                                              # Flip to be a horizontal bar graph. Omitting would make a normal vertical bar graph.






# Re-make FIG 17: % Total catch (VT) ~ time by depth        

# Re-make FIG 21: % Trap catch (RST) ~ length class by bay 





# Re-make TABLE 6: CUs and expansion 
teb.merge$UFID <- as.character(teb.merge$UFID)                                                # Change UFID to be a character (rather than factor) to replace NAs 
teb.merge$UFID <- ifelse(is.na(teb.merge$UFID), "n/a", teb.merge$UFID)                         # Replace all NAs with text "n/a" so it is easier to filter

# Step 1: Count sampled fish for each CU by sampling run (USID)
sampled <- teb.merge %>% 
  select(USID, date, trap_type, sockeye_smolt_release, sockeye_smolt_total, UFID, CU_final) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != "NR" & UFID != "n/a") %>%           # Only RST 
  group_by(USID, CU_final) %>%                                                            # Group by sampling event and CU
  summarize(n_assgn = n_distinct(UFID)) %>%                                               # Count number of distinct UFIDs (i.e., fish) in each CU for each sampling date - i.e., number of assigned fish
  mutate(n_sampled = sum(n_assgn)) %>%                                                    # Add new column ("n_sampled") and calculate the total number of assigned fish for each CU for each sampling event
  mutate(propn = n_assgn/n_sampled) %>%                                                   # Add new column ("propn") and calculate the proportion of assigned fish belonging to each CU on each sampling event
  print()

# Step 2: Count unsampled fish for each sampling run (USID)
unsampled <- teb.merge %>% 
  select(USID, date, trap_type, sockeye_smolt_release, sockeye_smolt_total, UFID, CU_final) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != 0 & sockeye_smolt_release != "NR") %>%           # RST only, excluding entries where the number of sockeye caught was 0 (i.e., select only sampling events where sockeye were caught) or NR
  group_by(USID) %>%                                                                                   # Group by sampling event (USID)
  summarize(n_total = unique(sockeye_smolt_total), n_released = unique(sockeye_smolt_release))         # Create new variable (n_total) that counts the total number of sockeye, and the number of released (i.e., unsampled) sockeye at each sampling event

# Step 3: Join "sampled" and "unsampled" together for quick math
sampling.merge <- left_join(unsampled, sampled, by="USID")                                # Join together the previous two tables ('sampled' and 'unsampled')

expansion <- sampling.merge %>% 
  select(USID, CU_final, n_total, n_released, n_sampled, n_assgn, propn) %>% 
  mutate(n_unsampled_assgn = n_released * propn) %>%                                      # Create new column ("n_unsampled_assgn") that expands the number of unsampled fish based on the proportion of sampled fish belonging to each CU
  group_by(CU_final) %>%                                                                  # Then group by CU
  summarize(exp_n_to_add = sum(n_unsampled_assgn)) %>%                                    # Create new variable ("exp_n_to_add") that calculated the number of new fish (expansion) to add to each CU
  print()

# Step 4: Count up only the sampled fish for each CU (basically the same as step 1) ***Start of re-making Table 6 in 2017***
CU <- teb.merge %>% 
  select(CU_final, sockeye_smolt_total, sockeye_smolt_release, UFID, trap_type) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != "NR" & UFID != "n/a") %>% 
  group_by(CU_final) %>%                                                                  # Group by CU
  summarize(old_sum = n_distinct(UFID)) %>%                                               # Create a new variable ("old_sum") that count the total number of sampled fish in each CU (not expanded)
  mutate(old_propn = (old_sum/sum(old_sum))) %>%                                          # Create a new column ("old_propn") that calculates the original proportion of sampled fish belonging to each CU (not expanded)
  print()

# Step 5: Join the original CU # and % to expanded CU 
CU.merge <- left_join(CU, expansion, by="CU_final")                                       # Join the expansion factor table ("expansion") with the original sampled fish table ("CU")

final <- CU.merge %>%                                                                     
  select(CU_final, old_sum, old_propn, exp_n_to_add) %>% 
  mutate(new_sum = old_sum + exp_n_to_add)                                                # Create a new column ("new_sum") that calculates the new number of fish in each CU (old number + expanded number = new sum)

# Time out to replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
final$new_sum <- ifelse(is.na(final$new_sum), final$old_sum, final$new_sum)         # Some CUs didn't have an expansion factor, so just replace NA's generated with their original number of fish

final <- final %>%
  select(CU_final, old_sum, old_propn, exp_n_to_add, new_sum) %>%
  mutate(new_propn = (new_sum/sum(new_sum))) %>%                                          # Create a new column ("new_propn") that calculates the new proportion of total catch taking into account expansion
  print()

# Step 6: Find migration timing metadata 
dates <- teb.merge %>% 
  select(CU_final, date, trap_type) %>% 
  filter(trap_type == "RST") %>% 
  mutate(date = ymd(date)) %>%                                                            # Make "date" as.Date for R
  group_by(CU_final) %>%                                                                  # Grouping by CU
  summarize(first = min(date), last = max(date))  %>%                                     # Create new variables ("first" and "last") that store the first and last migration date of each CU
  print()

migration1 <- teb.merge %>% 
  select(CU_final, date, trap_type, sockeye_smolt_total, UFID) %>% 
  filter(trap_type == "RST", sockeye_smolt_total != "NR", sockeye_smolt_total != "n/a") %>% 
  group_by(CU_final, date) %>%                                                            # Grouping by CU and date
  summarize(peak_n = n_distinct(UFID)) %>%                                                # Create new variable ("peak_n") that counts the number of fish (as indexed by the UFID) caught for each CU on each date
  group_by(CU_final) %>%                                                                  # Then just group by CU
  filter(peak_n == max(peak_n)) %>%                                                       # Filter out so just the maximum number of fish caught for each CU on each date are listed (this gives us PEAK MIGRATION DATE)
  print()

# Step 7: Join migration1 + final
final <- left_join(final, migration1, by="CU_final")                                      # Finally, join the expansion calculations table with the migration date table

# Time out to rename columns quickly for clarity
colnames(final)[colnames(final)=="date"] <- "peak_migration_date"                     # Rename column for clarity to express "date" is in fact the "peak migration date"
colnames(final)[colnames(final)=="peak_n"] <- "peak_migration_number"                 # Rename column for clarity to express "peak_n" is actually the "peak migration number"


# step 8: 50% migration date







# Re-make FIG 11: Cumulative catch (% total) by CU 
# Step 1: count sampled fish for each CU by sampling run (USID)      ** BASICALLY THE SAME AS ABOVE, JUST ADDING "date" 
sampled_date <- teb.merge %>% 
  select(USID, date, trap_type, sockeye_smolt_release, sockeye_smolt_total, UFID, CU_final) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != "NR" & UFID != "n/a") %>%           # RST only
  group_by(date, USID, CU_final) %>%                                                      # Grouping by sampling event, date, and CU
  summarize(n_assgn = n_distinct(UFID)) %>%                                               # Create new variable called "n_assgn" which calculates the number of sampled fish assigned to each CU on each sampling event and date 
  mutate(n_sampled = sum(n_assgn)) %>%                                                    # Then create new column "n_sampled" that summarizes the total number of sampled fish 
  mutate(propn = n_assgn/n_sampled) %>%                                                   # Create new column "propn" that calculates the original catch proportion of sampled fish at each CU, for each sampling event and date
  print()

# Step 2: Count unsampled fish for each sampling run (USID)   ** BASICALLY THE SAME AS ABOVE, JUST ADDING "date"
unsampled_date <- teb.merge %>% 
  select(USID, date, trap_type, sockeye_smolt_release, sockeye_smolt_total, UFID, CU_final) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != 0 & sockeye_smolt_release != "NR") %>% 
  group_by(date,USID) %>%                                                                                 # Group by sampling event and date
  summarize(n_total = unique(sockeye_smolt_total), n_released = unique(sockeye_smolt_release))            # Create new variables ("n_total" and "n_released") that summarize the total number of sockeye caught, and total number of sockeye released (i.e., unsampled) respectively  

# Step 3: Join "sampled" and "unsampled" together for quick math
sampling.merge.date <- left_join(unsampled_date, sampled_date, by="USID")                  # Join together the "sampled" and "unsampled" tables

expansion_date <- sampling.merge.date %>% 
  select(USID, CU_final, n_total, n_released, n_sampled, n_assgn, propn, date.x) %>% 
  mutate(n_unsampled_assgn = n_released * propn) %>%                                       # Create a new column "n_unsampled_assgn" that exands the number of unsampled fish by the expansion factor to calculate the number of fish that needs to be added
  group_by(date.x, CU_final) %>%                                                           # Then group by "date.x" and CU  --  Note: "date.x" is a weird artifact label left over from the result of the join in line 401. It refers to the dates where fish were subsampled
  summarize(exp_n_to_add = sum(n_unsampled_assgn, na.rm=T)) %>%                                     # Create a new variable ("exp_n_to_add") which calculates the total number of expanded fish to be added to each CU by each sampling date
  print()

# Step 4: Count up only the sampled fish for each CU (basically the same as step 1) 
CU_date <- teb.merge %>% 
  select(CU_final, sockeye_smolt_total, sockeye_smolt_release, UFID, trap_type,date) %>% 
  filter(trap_type == "RST", sockeye_smolt_release != "NR" & UFID != "n/a") %>% 
  group_by(date, CU_final) %>%                                                             # Group by date and CU
  summarize(old_sum = n_distinct(UFID)) %>%                                                # Create new variable "old_sum" that calculates the original number of sampled fish for each CU on each sampling date
  mutate(old_propn = (old_sum/sum(old_sum))) %>%                                           # Create a new column ("old_propn") that calculates the old proportion of total catch by each CU on each sampling date
  print()      

# Step 5: Join the original CU # and % to expanded CU 
colnames(expansion_date)[colnames(expansion_date)=="date.x"] <- "date"                     # Before joining, quickly rename "date.x" column for clarity to just be "date", and so it corresponds to "date" in the CU_table for the join
CU.merge.date <- left_join(CU_date, expansion_date, by=c("CU_final","date"))               # Join the original CU table with the expansion table

final_date <- CU.merge.date %>% 
  select(CU_final, old_sum, old_propn, exp_n_to_add, date) %>% 
  mutate(new_sum = old_sum + exp_n_to_add) %>%                                            # Create a new column ("new_sum") which calculates the new (i.e., expanded) number of fish caught in each CU on each sampling date
  print()

# Time out to replace the new NA's with the original numbers (no expansion factor was added so it produced an NA)
final_date$new_sum <- ifelse(is.na(final_date$new_sum), final_date$old_sum, final_date$new_sum)           # Again, some CUs didn't have expansion factors, so just replace NAs generated with their original abundances

# Step 6: Determine total # fish from each CU (eventually to calculate daily % of total fish)
CU_totals <- final_date %>% 
  select(date, CU_final, old_sum, old_propn, exp_n_to_add, new_sum) %>% 
  group_by(CU_final) %>%                                                                   # Group by CU
  summarize(CU_total = sum(new_sum)) %>%                                                   # Create new variable ("CU_total") that counts the total number of fish caught (expanded) in each CU
  print()

# Step 7: Join, make new column to divide daily # each CU by CU total   
CU.total.merge <- left_join(final_date, CU_totals, by="CU_final")                          # Join the expansion table with the total number of fish in each CU table

final_date <- CU.total.merge %>% 
  select(date, CU_final, old_sum, old_propn, exp_n_to_add, new_sum, CU_total) %>% 
  mutate(daily_perc = new_sum/CU_total) %>%                                                # Create a new column ("daily_perc") that calculates the daily proportion of total catch for each CU
  print()

# Step 8: Clean up - select only ~top 10 CUs 
levels(final_date$CU_final)                                                                # We generally just want a few key CUs, so first just check to make sure no undesirable ones - like "Unconfirmed" and ""

# Remove "Unconfirmed" and "", and then calculate total # fish (expanded) for each CU to see which CU's have more fish
final_date %>% 
  select(date, CU_final, old_sum, old_propn, exp_n_to_add, new_sum, CU_total, daily_perc) %>% 
  filter(CU_final != "", CU_final != "Unconfirmed") %>%                                # Filter out "" and "Unconfirmed" CUs
  group_by(CU_final) %>%                                                               # Then group by the remaining CUs
  summarize(sum = sum(new_sum)) %>%                                                    # Calculate a new variable ("sum") 
  arrange(desc(sum)) %>%                                                               # Arrange CUs in descending order of most abundant. Top 10: Chilko (S), Francois-Fraser (S), Chilko (ES), Chilko combo, Quesnel (S), Kamloops (S), Anderson-Seton (ES), Harrison U/S (L), Chilliwack (ES), Nahatlatch (ES)
  filter(sum > 19)                                                                     # Filter out CUs with mroe than 19 fish (easy cut off just based on numbers of fish - Nahatlatch lowest with 19.5 fish)

CU_10 <- c("Chilko (S)", "Francois-Fraser (S)", "Chilko (ES)", "Chilko combined", "Quesnel (S)", "Kamloops (S)",         
           "Anderson-Seton (ES)", "Harrison U/S (L)", "Chilliwack (ES)", "Nahatlatch (ES)")                         # Selecting the Top 10 CUs of interest

final_date <- final_date %>% 
  select(date, CU_final, old_sum, old_propn, exp_n_to_add, new_sum, CU_total, daily_perc) %>% 
  filter(CU_final %in% CU_10) %>%                                                      # Filter the expanded catch by date table to only include those top 10 CUs
  print()

# Step 9: Calculate cumulative proportions 
final_date_cuml <- final_date %>% 
  select(date, CU_final, old_sum, old_propn, exp_n_to_add, new_sum, CU_total) %>% 
  filter(CU_final %in% CU_10) %>%                                                # Filter by Top 10 CUs of interest 
  group_by(CU_final) %>%                                                               # Group by remaining top 10 CUs
  mutate(cuml_sum = cumsum(new_sum)) %>%                                               # Create a new column ("cuml_sum") that takes the cumulative catch over time for each CU by sampling date 
  mutate(cuml_propn = cuml_sum/CU_total)                                               # Create a new column ("cuml_propn") that calcaultes the cumulate catch proportion for each CU by sampling date

# Step 10: Plot 
# If we want run % over time (non-cumulative, daily)
ggplot(final_date, aes(x=date, y=daily_perc, group = CU_final)) +
  geom_point(aes(fill=CU_final), pch=21, colour="black") +                                 # Plotting points
  geom_line(aes(colour=CU_final), size=1) +                                                # Connecting points with lines
  scale_x_date(date_breaks = "5 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                               # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0), size=15),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=0,l=0), size = 15)) +          # Margin spacing order is: top, right, bottom, left
  ylab("Daily proportion of total catch") +
  xlab("Date")

# If we want run % over time (cumulative, daily)
ggplot(final_date_cuml, aes(x=date, y=cuml_propn, group = CU_final)) +
  geom_hline(aes(yintercept=0.5), colour="gray40", linetype="dotted") +
  geom_line(aes(y=cuml_propn, colour=CU_final), size=1.3) +
  geom_point(aes(fill=CU_final), size=3, pch=21) +
  #geom_smooth(aes(colour=CU_final), se=F, span=5) +
  scale_colour_brewer()+
  scale_x_date(date_breaks = "3 day", date_labels = ("%m-%d")) +
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                           # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0), size=15, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size = 12),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=0,l=0), size = 15, face= "bold"),          # Margin spacing order is: top, right, bottom, left
        legend.title = element_text(size =13, face="bold"),
        legend.text = element_text(size=12),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.11,0.8)) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  scale_colour_discrete(name="Conservation Unit") +
  scale_fill_discrete(name="Conservation Unit") +
  ylab("Cumulative proportion of catch") +
  xlab("Date") 

# Extract 50% run timing - TBD ggplot2 can't handle :(
m50 <- final_date_cuml %>% 
  select(date, CU_final, cuml_propn) %>% 
  filter(CU_final == "Harrison U/S (L)") %>%                          # Just select Harrison U/S (L)
  print()                                                             # Visually see that Harrison 50% migration date occurred May 8, 2017




######### scrap working code
        final_date_cuml$date <- as.character(final_date_cuml$date)
        final_date_cuml$date <- as.factor(final_date_cuml$date)
        v0.5 <- 0.5
        f1 <- approxfun(final_date_cuml$date, final_date_cuml$cuml_propn)
        optimize(function(t0) abs(f1(t0) - v0.5), interval = range(as.Date(final_date_cuml$date)))
        
        conc.est <- approx(x = as.numeric(final_date_cuml$date),
                           y = final_date_cuml$cuml_propn, xout=0.5)$cuml_propn

######################################



# Re-make "LENGTH - TRAP TABLES" (Table 7 in 2017): Length and trap 
teb.merge$UFID <- as.character(teb.merge$UFID)                                                      # As in lines 313-314
teb.merge$UFID <- ifelse(is.na(teb.merge$UFID), "n/a", teb.merge$UFID)                               # As in lines 313-314 - Could do at beginning in retrospect

teb.merge %>% 
  select(trap_type, depth_ft, mod_stat, fork_mm, UFID) %>% 
  filter(UFID != "n/a") %>%                                                                                                 # Remove any entry without a UFID
  group_by(trap_type, depth_ft, mod_stat) %>%                                                                               # Group by trap type, depth and modification status (mod_stat not really necessary, but was done in Excel example)
  summarize(total = n_distinct(UFID), mean = mean(fork_mm), sd = sd(fork_mm), min = min(fork_mm), max = max(fork_mm))       # Create new variables ("total", "mean", "sd", "min", and "max") to calculate number of sampled fish, and all length summary statistics 

# Note numbers are a bit off for RST when comparing to the Excel spreadsheet - check if any UFID's repeated       
n_occur <- data.frame(table(teb.merge$UFID))                                                 # Evaluate the number of occurrences for UFID
n_occur[n_occur$Freq >1,]                                                                   # Determine if any UFIDs have an occurrence frequency >1 (i.e., occur more than once) - Fish 2017-769 occurrs twice    

# Have checked for doubles, now check that the UFIDs are consequtive and in sequence (i.e., were any ID numbers skipped)
teb.merge$ID <- as.numeric(teb.merge$ID)                                                      # Just consider the ID column as we need a numeric sequence (not a character or factor) - and make it numeric
teb.merge$ID <- ifelse(is.na(teb.merge$ID), "n/a", teb.merge$ID)                               # Replace NAs with text "n/a" so doesn't throw error (R doesn't like numerical sequences with NAs)

setdiff(1:2356, teb.merge$ID)                                                                # Fish 1830 is missing - in BIO sheet is entered as being from May10 run2, bay6, but there is no entry for that on tally sheet, just May10, run2, BAY11 therefore was dropped upon merging the BIO and TALLY dataframes and we lost that fish entry. 

# NOTE: In retrospect, these are corrections I would make when first cleaning/loading the data in future. This hasn't been applied to theo verall       






# ENV data manipulation 
teb.merge$water_temp_C <- as.character(teb.merge$water_temp_C)
teb.merge$water_temp_C <- as.numeric(teb.merge$water_temp_C)
teb.merge$discharge_m3s <- as.numeric(teb.merge$discharge_m3s)
teb.merge$water_clarity_in <- as.character(teb.merge$water_clarity_in)
teb.merge$water_clarity_in <- as.numeric(teb.merge$water_clarity_in)

e<-teb.merge %>% 
  select(date, env_index, water_temp_C, time, water_clarity_in, discharge_m3s) %>%
  group_by(date) %>%
  summarize(mean_temp = mean(water_temp_C, na.rm=T), mean_clarity_cm = (mean(water_clarity_in, na.rm=T)*2.54), discharge=mean(discharge_m3s,na.rm=T)) %>% 
  print() 

t<-ggplot(e, aes(x=date, y=mean_temp)) +
  geom_line(col="black", size=1) +
  #  geom_point(fill="gray70", colour="black", pch=21, size=3) +
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  scale_y_continuous(limits=c(2,14), breaks=seq(2,14, by=3), labels=seq(2,14,by=3)) +
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                           # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=10), size=13, face="bold"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +          
  ylab(expression(bold(paste("Water temperature ("~degree~C,")", sep=""))))
c<-ggplot(e, aes(x=date, y=mean_clarity_cm)) +
  geom_line(col="black", size=0.8) +
  geom_point(fill="gray60", colour="black", pch=21, size=3) +
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                           # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0), size=13, face="bold"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +          
  ylab("Water clarity (cm)") 
d<-ggplot(e, aes(x=date, y=discharge)) +
  geom_line(col="black", size=0.8) +
  geom_point(fill="black", colour="black", pch=21, size=3) +
  scale_x_date(date_breaks = "5 day", date_labels = "%m-%d") + 
  theme_bw() +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5),                                                           # Margin spacing order is: top, right, bottom, left
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.y = element_text(margin=margin(t=0,r=5,b=0,l=0), size=13, face="bold"),
        axis.text.x = element_text(angle=45, hjust=1, colour="black", size = 12),
        axis.title.x = element_text(margin=margin(t=5,r=0,b=0,l=0), size = 13, face= "bold")) +          # Margin spacing order is: top, right, bottom, left
  ylab(expression(bold(paste("Discharge ("~m^3, "/", s,")", sep="")))) +   
  xlab("Date")
grid.draw(rbind(ggplotGrob(t), ggplotGrob(c), ggplotGrob(d), size = "last"))










# Test of CPUE ~ discharge 
test<-teb.merge %>% 
  select(date, sockeye_smolt_total, run, trap_type, USID, discharge_m3s) %>%                                
  filter(trap_type == "RST") %>%                                                                                                     # RST only
  group_by(date, USID) %>%                                                                                                           # Group by sampling event and date
  summarize(nruns = unique(run), no_SO = unique(sockeye_smolt_total, na.rm=T), mean_discharge=mean(discharge_m3s, na.rm=T)) %>%      # Create new variables ("nruns", and "no_SO") to summarize the number of runs and the number of sockeye caught in each sampling event on each date
  group_by(date) %>%                                                                                                                 # Then just group by date
  summarize(nruns = n_distinct(nruns), sum = sum(no_SO), CPUE = (sum/nruns), mean_discharge=mean(mean_discharge, na.rm=T))           # Calculate daily CPUE and daily discharge      

# Preliminary test of CPUE ~ discharge to see if sign
plot(test$CPUE ~ test$mean_discharge)
lm1 <- lm(test$CPUE ~ test$mean_discharge)                                    # Create linear model to extract residuals 
r1 <- resid(lm1)                                                              # Extract residuals of linear model to assess for normality, variance
plot(r1)                                                                      # Plot residuals to look at distribution 
qqnorm(r1)                                                                    # QQ-plot to help assess normality
qqline(r1)                                                                    # Add reference line to QQ-plot
hist(r1)                                                                      # Histogram of residuals to assess distribution

# Data are non-normal so use GLM to accommodate     
glm1<-glm((CPUE+1) ~ mean_discharge, data=test, family = Gamma(link="inverse"))            # GLM: add 1 to CPUE to remove 0s (not allowed in Gamma distribution)
summary(glm1)                                                                              # Print results of GLM

















