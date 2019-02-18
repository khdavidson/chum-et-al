# Data import from Excel .csv export and cleaning for nice handling in R 
# 14-Feb-2019

# Set WORK working directory
setwd("~/`Stock assessment/Analysis/Data files")
# Set HOME working directory 
setwd("~/DFO BI02 Stock assessment data analysis/Analysis/Data files")

# Load important packages to use
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(lubridate)


#######################
#
# TALLY data cleaning
#
#######################

# Read data 
tally.df <- read.csv("2017_MissionRST_Sockeye_20180406_TALLY.csv")

# Rename column headers, reformat data for easier handling in R, create USID. Will be done with pipes for quick running 
tally.df <- tally.df %>% 
rename(date = Date,                                             # used this extended way of renaming columns so can see what old column names correspond to in case of error
         survey_type = Survey.Type,                                               
         observers = Observers..Initials.Only.,
         daily_sheet = Daily.Sheet..,
         run = Run..,
         bay = Bay..,
         direction_travel = GPS...Direction.of.Travel,
         dist_travel_m = GPS...Distance.Traveled..m.,
         run_time_sec = GPS...Total.Run.Time..sec.,
         vessel_speed_mps = GPS...Vessel.Speed..m.s.,
         current_speed_mps = GPS...Current.Speed..m.s.,
         time_block = Time.Block..Based.on.Start.Time.,
         set_start = Set.Start.Time..24.hr.Clock.,
         set_end = Set.End.Time..24.hr.Clock.,
         NEW_set_start = NEW.Set.Start.Time,
         NEW_set_end = NEW.Set.End.Time,
         run_time = Total.Run.Time,
         shift_seg = Shift.Segment,
         trap_type = TrapType,
         depth_ft = Depth..ft..Vertical.Only,
         mod_stat = Original.or.Modifed.Nets,
         pink_fry_total = Total.Pink.Fry,
         chum_fry_total = Total.Chum.Fry,
         chinook_fry_total = Total.Chinook.Fry,
         chinook_smolt_total = Total.Chinook.Smolts,
         coho_smolt_total = Total.Coho.Smolts,
         sockeye_smolt_total = Total.Sockeye.Smolts,
         sockeye_smolt_bio = Sockeye.Smolt.Biosampled,
         sockeye_smolt_release = Sockeye.Smolt.Released,
         sockeye_fry_total = Total.Sockeye.Fry,
         sockeye_fry_bio = Sockeye.Fry.Biosampled,
         sockeye_fry_release = Sockeye.Fry.Released,
         coho_fry_total = Total.Coho.fry,
         coho_fry_AFC = Coho.fry..AFC.,
         sth_AFC = STH..AFC.,  
         chinook_fry_sampled_total = Total.Chinook.Fry.Retained.For.Samples,
         chinook_fry_release = Total.Chinook.Fry.Released,
         trap_type_old = Trap.Type,
         histo_AFC = Histo..Disease..AFC..RNA,
         net_type = Net.Type,
         comments_tally = Comments) %>% 
  mutate_at(vars(c(7:11)), funs(as.character)) %>%                                                 # Change columns 7-11 to character before can change to numeric
  mutate_at(vars(c(8:11)), funs(as.numeric)) %>%                                                   # Change columns 8-10 to be numeric 
  mutate_at(vars(c(22:34)), funs(as.character)) %>%                                                # Change columns 22-34 to be character before integer to preserve values
  mutate_at(vars(c(22:34)), funs(as.integer)) %>%                                                  # Change columns 22-34 to be integer
  mutate_at(vars(c(20)), funs(as.character)) %>%                                                   # Change column 20 to be character
  mutate(dist_travel_m = ifelse(dist_travel_m < 0, -dist_travel_m, dist_travel_m)) %>%             # Turn negative distances into positives 
  mutate(depth_ft = as.integer(replace(depth_ft, depth_ft=="Surface", 0))) %>%                     # Change depth to be integer and change "surface" to "0" 
  mutate(date = lubridate::dmy(date)) %>%                                                          # Convert old dd-mmm-yy to yyyy-mm-dd, makes as.Date for better handling and future plotting
  mutate(run = paste("R", run, sep = ""), bay = paste("B", bay, sep = "")) %>%                     # Add "R" and "B" before run and bay (respectively) to help make USID in next step
  mutate(USID = paste(paste(gsub("-", "", date)), run, bay, trap_type, depth_ft, sep="-")) %>%     # Create a unique sampling id (USID) that describes each sampling event over the summer. For simplicity right now, will be combined date-run-bay-trap-depth. This will correspond to the same USID in the bio.df. Note: when pasting date, I copied it over while simulatenously removing the "-" separator for easier reading.
  mutate(env_index = paste(paste(gsub("-", "", date)), run, sep="-")) %T>%                         # Create another unique index (env_index) to link TALLY with ENV dataframe. Just date-run
  write.csv("Mission_Sockeye_TALLY_2017_clean.csv", row.names = F)                                 # Export all this as a .csv file. Use row.names=F otherwise R will add unique row entry numbers (not helpful) and your first column will just be numbers

  # Note: will return warnings. This is only because fish counts include NAs, which R doesn't like when considering data as integers. We will
  # likely remove these NAs later, but for now they are kept to preserve original entries. 



#####################
#
# BIOSAMPLE data cleaning
#
#####################

# Read original csv data from excel 
bio.df <- read.csv("2017_MissionRST_Sockeye_20180406_BIO.csv")

# Rename column headers, reformat data for easier handling in R, create USID and UFID. Will be done with pipes for quick running 
bio.df <- bio.df %>% 
  rename(date = Date,                                                                 # Used this extended way of renaming columns so can see what old column names correspond to in case of error
         survey_type = Survey.Type,
         observers = Observers..Initials.Only.,
         daily_sheet = Daily.Sheet..,
         run = Run..,
         bay = Bay..,
         set_start = Run.Start.Time.I...24.hr.Clock.,
         set_end = Run.End.Time.I...24.hr.Clock.,
         NEW_set_start = Run.Start.Time.II,
         NEW_set_end = Run.End.Time.II,
         run_time = Total.Run.Duration..Time.,
         run_time_min = Total.Run.Duration..Minutes.,
         time_block = Time.Block..Based.on.Start.Time.,
         shift_seg = Shift.Segment,
         trap_type = Trap..Type,
         depth_ft = Depth...feet.,
         vt_net_type = Net.Type....VT.Only.,
         species_ID_field = Species.ID.in.the.Field,
         life_stage_field = Species.Life.Stage.ID...Field,
         ID = Fish.ID..,
         DNA_vial = DNA.......vial...,
         fork_mm = Fork.Length..mm.,
         size_class_mm = Size.Class..mm.,
         weight_g = Weight..g.,
         AFC = AFC..y.n.,
         histo_sample = Histo.Sample..y.n.,
         histo_jar = Histo.Jar..,
         spag_tag = Spaghetti.Tag..,
         fish_ID_DNA = Fish.ID.....DNA.Lab,
         GSI_submitted = Was.the.sample.submitted.for.GSI.processing....Yes.or.No.,
         reason_not_submitted = Reason.if.sample.not.submitted,
         GSI_processed = Was.the.sample.GSI.processed....Yes.or.No.,
         reason_not_processed = Reason.if.sample.not.processed,
         GSI_received = Did.we.receive.a.GSI.result...Yes.or.No.,
         reason_not_received = Reason.if.no.GSI.result,
         species_ID_DNA = Species.ID...DNA.Lab,
         ID_error = Was.a.species.ID.error.made....Yes.or.No.,
         ID_current_20180327 = Species.ID...Current...as.of...2018.03.27.,
         tally_df_adj = Has.the.Tally.Sheet.been.adjusted...Yes.or.No.,
         fish_ID_DNA2 = FISH.ID..DNA.LAB,
         CU1_number = CU.Assignment1...CU...with.the.highest.probability,
         CU1_name = CU.Assignment1...CU.Name.with.the.highest.probability,
         CU1_prob = CU.Assignment1...Probability,
         CU2_number = CU.Assignment2...CU...with.the.highest.probability,
         CU2_name = CU.Assignment2...CU.Name.with.the.highest.probability,
         CU2_prob = CU.Assignment2...Probability,
         CU3_number = CU.Assignment3...CU...with.the.highest.probability,
         CU3_name = CU.Assignment3...CU.Name.with.the.highest.probability,
         CU3_prob = CU.Assignment3...Probability,
         CU4_number = CU.Assignment4...CU...with.the.highest.probability,
         CU4_name = CU.Assignment4...CU.Name.with.the.highest.probability,
         CU4_prob = CU.Assignment4...Probability,
         CU5_number = CU.Assignment5...CU...with.the.highest.probability,
         CU5_name = CU.Assignment5...CU.Name.with.the.highest.probability,
         CU5_prob = CU.Assignment5...Probability,
         fish_ID_test1 = Fish.ID.Test.Column.1,
         fish_ID_test2 = Fish.ID.Test.Column.2,
         CU_assgn1_60 = Is.the.1st.CU.assignment.probability...0.600,
         CU_diff_assgn1_2 = The.difference.btw.the.1st.and.2nd.CU.assignments,
         CU_diff_assgn1_2_20 = Is.the.difference.btw.the.1st.and.2nd.CU.assignments...0.200,
         CU_rule = Rule.trigger.used.to.assign.a.CU,
         CU_assigned = Assigned.CU.based.on.GSI.Analysis.I..60.20.Rule.Applied.,
         CU_chilko_combo_method = Combined.Chiko.probability,
         CU_chilko_combo_prob = Combining.Chiko..ES....Chilko..S..probabilities..assignments.used,
         CU_chilko_gr_th_60 = If.the.combined.Chilko.probablility...0.6..is.the.diff.btw.the.combined.and.the.2nd.largest.CU.prob...or...0.2.,
         CU_final = Assigned.CU.based.on.GSI.Analysis.II..60.20...Chilko.Combined.Rules.Applied.,
         comments_bio = Comments) %>% 
  mutate_at(vars(c(22)), funs(as.numeric)) %>%                                                         # Make column 22 numeric            
  mutate_at(vars(c(16)), funs(as.character)) %>%                                                       # Change column 20 to be character
  mutate(depth_ft = as.integer(replace(depth_ft, depth_ft=="Surface", 0))) %>%                         # Change depth to be integer and change "surface" to "0" 
  mutate(date = lubridate::dmy(date)) %>%                                                              # Convert old dd-mmm-yy to yyyy-mm-dd, makes as.Date for better handling. 
  mutate(run = paste("R", run, sep = ""), bay = paste("B", bay, sep = "")) %>%                         # Add "R" and "B" before run and bay (respectively) to help make USID in next step
  mutate(USID = paste(paste(gsub("-", "", date)), run, bay, trap_type, depth_ft, sep="-")) %>%         # Create a unique sampling id (USID) that describes each sampling event over the summer. For simplicity right now, will be combined date-run-bay-trap-depth. This will correspond to the same USID in the bio.df
  mutate(UFID = paste("2017", ID, sep="-")) %T>%                                                       # Create a unique fish id (UFID) - this is not so important in this dataset as it is only 2017 fish. However, may want to consider assigning UFID's if planning to compare biometrics across years. Add "2017" as prefix for now
  write.csv("Mission_Sockeye_BIO_2017_clean.csv", row.names = F)                                       # Export it all as a .csv

  



#####################
#
# ENVIRONMENTAL data cleaning
#
#####################

# Read original csv data from excel 
env.df <- read.csv("2017_MissionRST_Sockeye_20180406_ENV.csv")

# Rename column headers, reformat data for easier handling in R, create USID and UFID. Will be done with pipes for quick running 
env.df <- env.df %>% 
  select(-c(Flow.........m.s., BAY)) %>%
  rename(date = Date,                                                              # Used this extended way of renaming columns so can see what old column names correspond to in case of error
         RPM = RPM,
         run = Run..,
         time = Time,
         wx_description = Weather.Description,
         cc_perc = X..Cloud.Cover,
         brightness = Brightness,
         precipitation = Precipitation,
         surface_chop = Surface.Chop,
         water_temp_C = Water.Temp..oC.,
         air_temp_C = Air.Temp..oC.,
         water_clarity_in = Water.Clarity..inches.,
         debris = Debris,
         flow_in = Initial.Flow.Reading,
         flow_out = Secondary.Flow.Reading,
         flow_ms = Flow.Calculation..m.s.,
         flow_diff = difference,
         comments_env = Comments) %>% 
  mutate(date = lubridate::dmy(date)) %>%                                          # Convert old dd-mmm-yy to yyyy-mm-dd, makes as.Date for better handling. 
  mutate(run = paste("R", run, sep = "")) %>%                                      # Add "R" before run to help make env_index in next step
  mutate(env_index = paste(paste(gsub("-", "", date)), run, sep="-")) %T>%         # Create a unique index ("env_index") to link to TALLY dataframe. Just date-run
  write.csv("Mission_Sockeye_ENV_2017_clean.csv", row.names = F)                   # Export it all as a .csv




## THESE DATAFRAMES STILL ESSENTIALLY REPRESENT THE ORIGINAL EXCEL SPREADSHEETS. FURTHER SELECTION WILL OCCUR IN "data_explr" script. 

