# Catch table matrix creation 


# What I want: CPUE will be values to start, eventually IAs. 
#______________________________________________________________
#   Event         |   Bay 2     |    Bay 6     |     Bay 11    |
#_________________|_____________|______________|_______________|
# 20170403-0600   |   0.000     |              |               |
#          0601   |   0.000     |              |               |   
#          0602   |   0.000     |              |               |   
#          0615   |   0.000     |              |               |   
#          0616   |     x       |       x      |       x       |   
#          0620   |             |    0.000     |               |   
#          0631   |             |    0.000     |               |   
#          0632   |             |    0.000     |               |   
#          0635   |             |    0.000     |               |   
#          0636   |     x       |       x      |       x       |   
#          0640   |             |              |     0.000     |   
#          0641   |             |              |     0.000     |   
#          0642   |             |              |     0.000     |   
#          0655   |             |              |     0.000     |   
#           ...   |     ...     |      ...     |      ...      |        
#______________________________________________________________


# Set wd 
setwd("~/`Stock assessment/Analysis/Data files")

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(withr)
library(stringr)
library(padr)
library(lme4)
library(forecast)
library(ggfortify)
library(changepoint)
library(strucchange)
library(ggpmisc)
library(purrr)
library(imputeTS)
library(xts)


# Read data
data <- read.csv("TEB_leftjoin.csv")

# Create proper run time length
data <- data %>%
  select(everything()) %>%
  mutate_at(vars(c(17)), funs(as.character)) %>%
  mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
  mutate_at(vars(c(17)), funs(as.numeric)) %>%
  mutate(run_time = run_time*60)


#######################################################################################################################################
#######################################################################################################################################


#                                                   THE FOLLOWING CODE CORRESPONDS TO
#                                               KD_202019_MissionSockeyeSmoltCPUECalibrations


#######################################################################################################################################

# First, simple subset data 
data <- data %>% 
  filter(trap_type == "RST") %>% 
  print()

#-------------------------------------------------------------------------------------------------------------------------------------


#########################
# CREATE MONSTER MATRIX #
#########################



####
# 1. Calculate CPUE for each run (to be time of day eventually)
####
# Create dataframe: select RST data, apply fished volumes
data <- data %>%
  filter(trap_type =="RST") %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  print()

# Summarize catch by USID and date, calculate CPUE by RUN  
CPUE <- data %>% 
  group_by(date, USID, fished_vol, set_start, set_end, bay) %>% 
  summarize(total_n = unique(sockeye_smolt_total)) %>%                           # Just used raw catch here as expanded isn't required
  mutate(CPUE_run = total_n/fished_vol) %>%                                      # Calculate CPUE 
  mutate(bay = ifelse(bay=="B2", "bay2",                                         # Rename Bays
               ifelse(bay=="B6", "bay6", "bay11"))) %>%
  print()


####
# 2. Reformat CPUE date-times 
####
# First set as character
CPUE$set_start <- as.character(CPUE$set_start)                                   
CPUE$set_end <- as.character(CPUE$set_end)

# Add zero to start of 3 digit numbers (i.e., 945 becomes 0945)
CPUE$set_start <- with_options(c(scipen = 999), str_pad(CPUE$set_start, 4, pad = "0"))
CPUE$set_end <- with_options(c(scipen = 999), str_pad(CPUE$set_end, 4, pad = "0"))

# Add colon after ever 2 digits
CPUE <- CPUE %>% 
  ungroup() %>%
  mutate(set_start = gsub('(.{2})', '\\1:', set_start)) %>%
  mutate(set_end = gsub('(.{2})', '\\1:', set_end))

    # Remove trailing colon
    CPUE$set_start <- str_sub(CPUE$set_start, 1, str_length(CPUE$set_start)-1)
    CPUE$set_end <- str_sub(CPUE$set_end, 1, str_length(CPUE$set_end)-1)

# Make as date-time (POSIXct) for minute intervals
CPUE$date_start = as.POSIXct(paste(CPUE$date, CPUE$set_start),tz = "")
CPUE$date_end = as.POSIXct(paste(CPUE$date, CPUE$set_end),tz = "")

####
# 3. Restructure, reformat dataframe
####
# Remove extra unneeded columns 
CPUE2 <- CPUE %>% 
  select(-c(fished_vol, total_n, date)) %>%
  print()

# Restructure start/end to be long form and rename columns
CPUE2 <- CPUE2 %>%
  gather("date_start", "date_end", 6:7) %>%                                           # Long form
  rename(key = date_start,
         value = date_end) %>% 
  print()

####
# 4. Expanding sampling time series
####
# Pad date-time minute intervals between sampling event start/end times 
CPUE_sub2 <- pad(CPUE2, interval="min", group="USID")

# Fill values between sampling event start and end (i.e., copy down 'bay' and 'CPUE_run' values)
CPUE_sub3 <- CPUE_sub2 %>%
  group_by(USID) %>%
  fill(set_start, set_end, bay, CPUE_run)

    # Remove other extra columns
    CPUE_sub3 <- CPUE_sub3 %>% 
      select(-set_start, -set_end, -key) %>% 
      arrange(value) %>%
      print()

    # Reformat to be wide format 
    CPUE_spr <- CPUE_sub3 %>% 
      spread(bay, CPUE_run) %>%                                                       # Wide form
      print()

####
# 5. Creating new time series to fill in un-sampled windows
####
# New empty time series for length of whole season, 1 minute intervals
ts.min <- data.frame(value=seq(ymd_hm("2017-04-03 06:07", tz=""), ymd_hm("2017-06-14 23:59", tz=""), by="1 min"))

# Merge CPUE and empty time series dataframes
CPUE_spr.merge <- left_join(ts.min, CPUE_spr, by="value")                             # left_join will preserve sampling intervals and just infill minute intervals between the end of a run and the beginning of another

    # Re-order columns for easy reading
    CPUE_spr.merge <- CPUE_spr.merge %>% 
      select(USID, value, bay2, bay6, bay11) %>% 
      arrange(value) %>% 
      print()

####
# 6. Ensure there are no duplicated date-time intervals
####
# Identify the row numbers of duplicates (used to index below)
which(duplicated(CPUE_spr.merge$value) | duplicated(CPUE_spr.merge$value, fromLast = TRUE))                   

# Pull out the metadata for each row number 
dupl_CPUE.merge <- CPUE_spr.merge %>% 
  mutate(dup_val = duplicated(value)) %>% 
  filter(dup_val) %>% 
  print()

# Change sampling date-time intervals for duplicated events manually 
CPUE_spr.merge[1509,2] <- as.POSIXct("2017-04-04 07:14:59")                                                    # dataframe[row, column]
CPUE_spr.merge[1510,2] <- as.POSIXct("2017-04-04 07:15:01")                                                       

CPUE_spr.merge[4642,2] <- as.POSIXct("2017-04-06 11:26:59")                                                        
CPUE_spr.merge[4643,2] <- as.POSIXct("2017-04-06 11:27:01")                                                        

CPUE_spr.merge[20207,2] <- as.POSIXct("2017-04-17 06:50:59")                                                       
CPUE_spr.merge[20208,2] <- as.POSIXct("2017-04-17 06:51:00")                                                    # This one actually had a catch associated so wanted to make sure it was maintained in that designation


# Re-check duplicates
which(duplicated(CPUE_spr.merge$value) | duplicated(CPUE_spr.merge$value, fromLast = TRUE))

dupl_CPUE.merge <- CPUE_spr.merge %>% 
  mutate(dup_val = duplicated(value)) %>% 
  filter(dup_val) %>% 
  print()

# The remaining duplicates appear to be data transcription errors where end and start times overlap (confirmed on hard copy).
  # I will change the second entry for each duplicate to move it ahead to allow for no overlap. 

####
# 7. Deal with two events with overlapping set end/start times
####
# APRIL 18 SERIES
CPUE_spr.merge[21806,2] <- as.POSIXct("2017-04-18 09:26:59")           # Changed from 09:27 to 09:26:59
CPUE_spr.merge[21803,2] <- as.POSIXct("2017-04-18 09:27:01")           # Changed from 09:27 to 09:27:01             
CPUE_spr.merge[21805,2] <- as.POSIXct("2017-04-18 09:41:00")           # Changed from 09:25 to 09:41 (just put to end rather than bump every entry as it doesn't actually matter)
CPUE_spr.merge[21807,2] <- as.POSIXct("2017-04-18 09:42:00")           # Changed from 09:26 to 09:42 (just put to end rather than bump every entry as it doesn't actually matter)

  # Remove duplicates at the end of the event
  CPUE_spr.merge <- CPUE_spr.merge[-c(21821, 21822), ]                   # Remove the old empty rows from 09:41 and 09:42

  # Export as csv otherwise row numbers get fucked up 
  write.csv(CPUE_spr.merge, "CPUE_spr_merge.csv", row.names = F)
  
  
# MAY 19 SERIES
# Re-load data 
CPUE_spr.merge2 <- read.csv("CPUE_spr_merge.csv")
CPUE_spr.merge2$value <- as.POSIXct(CPUE_spr.merge2$value)
  
# Re-check duplicates
which(duplicated(CPUE_spr.merge2$value) | duplicated(CPUE_spr.merge2$value, fromLast = TRUE))

dupl_CPUE.merge <- CPUE_spr.merge2 %>% 
  mutate(dup_val = duplicated(value)) %>% 
  filter(dup_val) %>% 
  print()

# May 19 overlap series
CPUE_spr.merge2[66322,2] <- as.POSIXct("2017-05-19 07:21:59")           # Changed from 07:22 to 07:21:59
CPUE_spr.merge2[66319,2] <- as.POSIXct("2017-05-19 07:36:00")           # Changed from 07:20 to 07:36 (just put to end rather than bump every entry as it doesn't actually matter)             
CPUE_spr.merge2[66321,2] <- as.POSIXct("2017-05-19 07:37:00")           # Changed from 07:21 to 07:37 (just put to end rather than bump every entry as it doesn't actually matter)
CPUE_spr.merge2[66323,2] <- as.POSIXct("2017-05-19 07:22:01")           # Changed from 07:22 to 07:22:01 

  # Remove duplicates at the end of the event
  CPUE_spr.merge2 <- CPUE_spr.merge2[-c(66337, 66338), ]                   # Remove the old empty rows from 07:36 and 07:37

  
# SUMMARY OF DUPLICATE WORK: 
  # For runs where the end and start times were the same (e.g., one run ended at 07:15 and the next started at 07:15), I separated
  # them by 1 second - new end time would become 07:14:59 and new start time would become 07:15:01. 
  # For the two cases (Apr 18 and May 19) where run end and start times overlapped by 2-3 minutes, the later run of the two was bumped
    # ahead by the time interval: 
      # Apr 18: Run 10 start and end times remained the same but bumped by 1 second (09:12 - 09:26:59)
              # Run 11 start and end times bumped 2 minutes forward. Old times: 09:25 - 09:40. New times: 09:27:10 - 09:42
      # May 19: Run 3 start and end times remained the same but bumped by 1 second (07:07 - 07:21:59)
              # Run 4 start and end times bumped 2 minutes forward. Old times: 07:20 - 07:35. New times: 07:22:01 - 07:37
# These times were given to Matt (Shane/Tod) so that the same corrections can be made in the raw Excel file, but I did not make any
# changes to the file myself! 
  
  
####
# 7. Export
####

write.csv(CPUE_spr.merge2, "mission_SO_CPUE_matrix.csv", row.names=F)




#-------------------------------------------------------------------------------------------------------------------------------------


#                                                 CORRESPONDS TO SECTIONS IN WORD DOC: 
#                                             KD_202109_MissionSockeyeSmoltCPUECalibrations

                                                #_________________________________________#
                                                #    METHOD 3.1: Infilling with CPUE      #
                                                #_________________________________________#


#######################
# 1. Create dataframe #
#######################

data <- read.csv("TEB_leftjoin.csv")      

    # Create run time column
    data <- data %>%
      select(everything()) %>%
      mutate_at(vars(c(17)), funs(as.character)) %>%
      mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
      mutate_at(vars(c(17)), funs(as.numeric)) %>%
      mutate(run_time_s = run_time*60)

# Calculate CPUE per run 
data2 <- data %>%
  filter(trap_type =="RST", sockeye_fry_total != "NR") %>%
  group_by(USID, date) %>%
  summarize(unq_SO = unique(sockeye_smolt_total), run_time = unique(run_time_s)) %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = unq_SO/fished_vol) %>%
  print()

data2<-data2 %>% 
  select(-fished_vol, -run_time, -USID) %>% 
  print()


# Average CPUE per day 
data3 <- data2 %>% 
  group_by(date) %>% 
  summarize(mean_CPUE = mean(CPUE)) %>% 
  print()

##########################
# 2. Simple linear model #
##########################

lm1 <- lm(data3$mean_CPUE ~ data3$date)
r1 <- resid(lm1)
plot(r1)
hist(r1)
qqnorm(r1)
qqline(r1)
plot(lm1)

  # I would not say this is normally distributed... but that is fair because you can't really use time series data to predict linear
    # trends under the traditional y=mx+b formula. 
  # It is also important to assess whether autocorrelation exists within the data, which I will test below 


######################
# 3. Autocorrelation #
######################

# There seem to be a couple of ways to potentially asses this, so I will highlight what I have learned... 

##
# 1. CREATING TIME SERIES USING DAILY AVERAGE KNOWN CPUE (NOT USING MATRIX) 
# (From: http://rpubs.com/Mentors_Ubiqum/tslm)
##

# Make date as.POSIXct
data3$date <- strptime(data3$date, "%Y-%m-%d" )
data3$date <- as.POSIXct(data3$date)

# Create different columns for different time configurations (more important with multi-year dataset)
data3 <- data3 %>% 
  mutate(MonthYear = paste(year(date),formatC(month(date), width = 2, flag = "0"))) %>%                       # Month of year
  mutate(YearDay = paste(year(date), formatC(month(date), width = 2, flag = "0"),                             # Day of week of Year
                                                     formatC(day(date), width = 2, flag = "0"))) %>% 
  mutate(Week = week(date)) %>%                                                                               # Week of year
  mutate(Year = year(date)) %>%
  mutate_at(vars(c(6)), funs(as.character)) %>% 
  mutate_at(vars(c(6)), funs(as.factor))

# Use the YearDay column to aggregate
CPUE_month <- aggregate(data3$mean_CPUE, by = list(data3$YearDay), FUN = function(x) mean(x, na.rm=T))

# Now we create the time series. Note: It's better to define the end of the time series if it's not at the end of one year
myts <- ts(CPUE_month$x, frequency=1)                                      # Frequency = number of obs per unit of time
plot(myts)

# Now that we have several periods we can decompose (again, this is more for multi-year data). Note: can only decompose if in above 
  # ts() call, frequency > 1
myds_month <- decompose(myts)
plot(myds_month)

# Create a data frame to use tslm()
  # First column: time series
  # Second column: Numerical value of time
CPUE_ts <- data.frame(cpue = myts, as.numeric(time(myts)))
names(CPUE_ts) <- c("cpue", "date")

##
# 2. CREATE & FIT MODELS 
##

# Create a model using tslm - We can model using trend, season and random
tslm.1 <- tslm(cpue ~ date, CPUE_ts)
tslm.2 <- tslm(cpue ~ date + trend, CPUE_ts)               # No real effect of trend, can't run seasonal

    # Forecasting using tslm() model - We are going to predict the next 10 years (h)
    CPUE_fc <- forecast(tslm.1,h=120)
    autoplot(tslm.1)

# Examine residuals and model summary results 
acf(resid(tslm.1))                             # The cyclical/oscillating bars indicate autocorrelation 
summary(tslm.1)

    # We can compare the model output results of lm() and tslm() to see which one fits better. 
        # lm(): icpt est: 1.50e-01, date est: -9.95e-11
        # tslm(): icpt est: 1.30e-03, date est: -8.22e-06 

# Fit auto.arima() which will inform the 'order' call
a.ar.1 <- auto.arima(CPUE_ts$cpue, stepwise=F, approximation=F)
acf(resid(a.ar.1))
summary(a.ar.1)            #ar1=0.5023, order: ARIMA(1,0,0)

# Confirm by fitting arima() with order (1,0,0)
ar1 <- arima(myts, order = c(1,0,0))

##
# 3. COMPETE MODELS 
##

# List of models
ts_models <- list()
ts_models$lm1 <- lm(data3$mean_CPUE ~ data3$date)
ts_models$tslm.1 <- tslm(cpue ~ date, CPUE_ts)
ts_models$ar1 <- arima(myts, order = c(1,0,0))

      # Set up function to make table 
      show_estimates <- function(model) {
        model %>%
          broom::tidy() %>% 
          dplyr::select(term, estimate) %>%
          modify_if(is.numeric, round, 8) %>% 
          # + some preparation to combine lm and arima output later
          modify_at("term", as.character) %>% 
          modify_at("term", stringr::str_replace,
                    pattern = "\\(Intercept\\)",
                    replacement = "intercept") %>% 
          modify_at("term", stringr::str_replace,
                    pattern = "(trend|drift)", ## improve pattern
                    replacement = "trend/drift")
      }
      
      #  customize html-tables
      hux_table <- function(df, caption) {
        library(huxtable)
        content_rows <- 1:nrow(df) + 1  # +1 for the header
        content_cols <- 2:ncol(df)
        df %>% 
          hux(add_colnames = TRUE) %>% 
          set_bold(row = 1, col = everywhere, value = TRUE) %>%
          set_bottom_border(row = 1, col = everywhere, value = TRUE) %>%
          set_pad_decimal(content_rows, everywhere, ".") %>%
          set_align(c(1, content_rows), content_cols, "right") %>% 
          set_number_format(content_rows, everywhere, "%5.4g") %>% 
          set_caption(caption)
      }

# Print tables with parameter estimates -- note, they say 0 due to decimal truncation 
ts_models %>% 
  map(show_estimates) %>% 
  reduce(full_join, by = "term") %>% 
  set_names(c("term", names(ts_models))) %>% 
  filter(!str_detect(term, "season")) %>% 
  hux_table("Coefficients including Autocorrelated Models")

# Compare models
ts_models %>% 
  map_df(AIC) %>% 
  gather("model", "fit") %>% 
  arrange(fit) %>% 
  hux_table("AIC")


    # ARIMA models fit better which indicates autocorrelation in dataset 



#########################################
# 3. ASSESS MISSING DATA IN CATCH TABLE #
#########################################

# Load catch table matrix
matrix <- read.csv("mission_SO_CPUE_matrix.csv")

    # Bay 2 matrix
    matrix2 <- matrix %>% 
      select(value, bay2) %>% 
      rename(date=value)
    # Bay 6 matrix
    matrix6 <- matrix %>% 
      select(value, bay6) %>% 
      rename(date=value)
    # Bay 11 matrix
    matrix11 <- matrix %>% 
      select(value, bay11) %>% 
      rename(date=value)

# Create a time series for each Bay
#z.bay2 <- read.zoo(file = matrix2, header = TRUE, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
#bay2.ts <- as.ts(z.bay2)                 # For some reason, while zoo() preserves the matrix fine, this call makes the ts wildy innacurrate - 6mil+ entries vs 104k which is what it is supposed to have. use ts() as below
ts.b2 <- ts(matrix2$bay2)

#z.bay6 <- read.zoo(file = matrix6, header = TRUE, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
#bay6.ts <- as.ts(z.bay6)
ts.b6 <- ts(matrix6$bay6)

#z.bay11 <- read.zoo(file = matrix11, header = TRUE, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
#bay11.ts <- as.ts(z.bay11)
ts.b11 <- ts(matrix11$bay11)

##
# imputeTS()
##
# Summary stats on NAs using imputeTS() package
statsNA(ts.b2)
statsNA(ts.b6)
statsNA(ts.b11)

# Plot NAs using imputTS() package
plotNA.distribution(ts.b2)                             # returns time series plot with pink background for NAs
plotNA.distributionBar(ts.b2, breaks = 20)             # returns stacked bar graph with % NAs for each breaks bin
plotNA.gapsize(ts.b2)                                  # returns side-by-sde bar graph for NA gap size

plotNA.distribution(ts.b6)
plotNA.distributionBar(ts.b6, breaks = 20)
plotNA.gapsize(ts.b6)

plotNA.distribution(ts.b11)
plotNA.distributionBar(ts.b11, breaks = 20)
plotNA.gapsize(ts.b11)


#################################
# 4. INTERPOLATE NAs - imputeTS #
#################################

# Calculate imputations - Bay 2
mean.bay2 <- na.mean(ts.b2)
  med.bay2 <- na.mean(ts.b2, option = "median")
int.a.bay2 <- na.interpolation(ts.b2)
  int.sp.bay2 <- na.interpolation(ts.b2)
  int.st.bay2 <- na.interpolation(ts.b2)
kal.bay2 <- na.kalman(ts.b2)                             # returns warning
sea.bay2 <- na.seadec(ts.b2)                             # no seasonality (but would be in bigger dataset)
# Calculate imputations - Bay 6
mean.bay6 <- na.mean(ts.b6)
  med.bay6 <- na.mean(ts.b6, option = "median")
int.a.bay6 <- na.interpolation(ts.b6)
  int.sp.bay6 <- na.interpolation(ts.b6)
  int.st.bay6 <- na.interpolation(ts.b6)       
kal.bay6 <- na.kalman(ts.b6)                             # returns warning          
# Calculate imputations - Bay 11
mean.bay11 <- na.mean(ts.b11)
  med.bay11 <- na.mean(ts.b11, option = "median")
int.a.bay11 <- na.interpolation(ts.b11)
  int.sp.bay11 <- na.interpolation(ts.b11)
  int.st.bay11 <- na.interpolation(ts.b11)
kal.bay11 <- na.kalman(ts.b11)                           # no warning!


# Plot imputations
plotNA.imputations(ts.b2, mean.bay2)
plotNA.imputations(ts.b2, int.a.bay2)
plotNA.imputations(ts.b2, int.sp.bay2)
plotNA.imputations(ts.b2, int.st.bay2)

plotNA.imputations(ts.b6, mean.bay6)
plotNA.imputations(ts.b6, int.bay6)
plotNA.imputations(ts.b6, kal.bay6)

plotNA.imputations(ts.b11, mean.bay11)
plotNA.imputations(ts.b11, int.bay11)
plotNA.imputations(ts.b11, kal.bay11)



########################
# 5. Export back to df #
########################

##
# Interp series 1 - approx
##

# Make as.df
bay2.aint.df <- as.data.frame(int.a.bay2)
bay6.aint.df <- as.data.frame(int.a.bay6)
bay11.aint.df <- as.data.frame(int.a.bay11)

# Row names from original matrix correspond to time series 
date <- matrix$value
USID <- matrix$USID

# Bind
a.int.df <- cbind(USID, date, bay2.aint.df, bay6.aint.df, bay11.aint.df)
names(a.int.df) <- c("USID", "date", "B2", "B6", "B11")


##
# Interp series 2 - spline
##

# Make as.df
bay2.spint.df <- as.data.frame(int.sp.bay2)
bay6.spint.df <- as.data.frame(int.sp.bay6)
bay11.spint.df <- as.data.frame(int.sp.bay11)

# Row names from original matrix correspond to time series 
date <- matrix$value
USID <- matrix$USID

# Bind
sp.int.df <- cbind(USID, date, bay2.spint.df, bay6.spint.df, bay11.spint.df)
names(sp.int.df) <- c("USID", "date", "B2", "B6", "B11")


##
# Interp series 2 - stine
##

# Make as.df
bay2.stint.df <- as.data.frame(int.st.bay2)
bay6.stint.df <- as.data.frame(int.st.bay6)
bay11.stint.df <- as.data.frame(int.st.bay11)

# Row names from original matrix correspond to time series 
date <- matrix$value
USID <- matrix$USID

# Bind
st.int.df <- cbind(USID, date, bay2.stint.df, bay6.stint.df, bay11.stint.df)
names(st.int.df) <- c("USID", "date", "B2", "B6", "B11")


##########################
# 6. Summarize & compare #
##########################

##
# Interp series 1 - approx
##

# Split dat-time column so that catch can be averaged for each bay, each day 
a.bay.df <- a.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  group_by(day) %>% 
  summarize(B2 = mean(B2), B6 = mean(B6), B11 = mean(B11)) %>%
  gather(bay, CPUE, "B2", "B6", "B6", 2:4)


# Same again but straight to daily average 
a.day.df <- a.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  gather(bay, CPUE, "B2", "B6", "B11", 3:5) %>%  
  group_by(day) %>% 
  summarize(mean_CPUE = mean(CPUE))


##
# Interp series 2 - spline
##

# Split dat-time column so that catch can be averaged for each bay, each day 
sp.bay.df <- sp.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  group_by(day) %>% 
  summarize(B2 = mean(B2), B6 = mean(B6), B11 = mean(B11)) %>%
  gather(bay, CPUE, "B2", "B6", "B11", 2:4)


# Same again but straight to daily average 
sp.day.df <- sp.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  gather(bay, CPUE, "B2", "B6", "B11", 3:5) %>%  
  group_by(day) %>% 
  summarize(mean_CPUE = mean(CPUE))


##
# Interp series 3 - stine
##

# Split dat-time column so that catch can be averaged for each bay, each day 
st.bay.df <- st.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  group_by(day) %>% 
  summarize(B2 = mean(B2), B6 = mean(B6), B11 = mean(B11)) %>%
  gather(bay, CPUE, "B2", "B6", "B11", 2:4)


# Same again but straight to daily average 
st.day.df <- st.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  gather(bay, CPUE, "B2", "B6", "B11", 3:5) %>%  
  group_by(day) %>% 
  summarize(mean_CPUE = mean(CPUE))



##
# Raw series
##

data <- read.csv("TEB_leftjoin.csv")      
    # Create run time column
    data <- data %>%
      select(everything()) %>%
      mutate_at(vars(c(17)), funs(as.character)) %>%
      mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
      mutate_at(vars(c(17)), funs(as.numeric)) %>%
      mutate(run_time_s = run_time*60)

# Calculate CPUE per run 
data2 <- data %>%
  filter(trap_type =="RST", sockeye_fry_total != "NR") %>%
  group_by(USID, date, bay) %>%
  summarize(unq_SO = unique(sockeye_smolt_total), run_time = unique(run_time_s)) %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = unq_SO/fished_vol) %>%
  print()

# Average CPUE per day 
day.dat <- data2 %>% 
  select(-fished_vol, -run_time, -USID) %>% 
  group_by(date) %>% 
  summarize(mean_CPUE = mean(CPUE)) %>% 
  print()

# Average CPUE by bay 
bay.dat <- data2 %>% 
  group_by(date, bay) %>% 
  summarize(mean_CPUE = mean(CPUE))


##
# PLOT: Normal temporal  
##

# Plot by daily average CPUE 
day.dat$date <- as.Date(day.dat$date)
a.day.df$day <- as.Date(a.day.df$day)
ggplot() + 
  geom_line(data=day.dat, aes(x=date, y=mean_CPUE, colour="Raw CPUE", linetype="Raw CPUE"), 
            size=2, alpha=0.7) +
  geom_line(data=a.day.df, aes(x=day, y=mean_CPUE, colour="Interpolated CPUE", linetype="Interpolated CPUE"), 
            size=2, alpha=0.7) +
  #geom_line(data=sp.day.df, aes(x=day, y=mean_CPUE), colour="green", size=2, alpha=0.6) +      # all interp results are the same
  #geom_line(data=st.day.df, aes(x=day, y=mean_CPUE), colour="blue", size=2, alpha=0.6) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual("", values=c("Raw CPUE" = "black", 
                                   "Interpolated CPUE" = "red")) + 
  scale_linetype_manual("", values=c("Raw CPUE" = 1,
                                     "Interpolated CPUE" = 1)) +
  theme(text = element_text(colour="black", size=45),
        plot.margin=margin(t=15,r=15,b=0,l=10),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.80,0.90),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab(expression(bold(paste("CPUE (fish/",m^3,")")))) +   
  xlab("")


# Plot by daily average by BAY CPUE 
bay.dat$date <- as.Date(bay.dat$date)
a.bay.df$day <- as.Date(a.bay.df$day)
ggplot() + 
  geom_line(data=bay.dat, aes(x=date, y=mean_CPUE, group=bay, colour=bay), 
            linetype="dashed", size=2, alpha=0.6) +
  geom_line(data=a.bay.df, aes(x=day, y=CPUE, group=bay, colour=bay), 
            size=2) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"),  
                      breaks=c("B2", "B6", "B11"), 
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
  theme(text = element_text(colour="black", size=45),
        plot.margin=margin(t=15,r=15,b=0,l=0),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.86,0.85),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab(expression(bold(paste("CPUE (fish/",m^3,")")))) +   
  xlab("Date")

##
# PLOT: Cumulative temporal  
##

# Daily raw CPUE
day.cuml.dat <- day.dat %>% 
  mutate(cuml_CPUE = cumsum(mean_CPUE)) %>% 
  mutate(cuml_propn = cuml_CPUE/sum(mean_CPUE))

# Daily raw CPUE by BAY 
bay.cuml.dat <- bay.dat %>% 
  group_by(bay) %>% 
  mutate(cuml_CPUE = cumsum(mean_CPUE)) %>% 
  mutate(cuml_propn = cuml_CPUE/sum(mean_CPUE))

# Daily interpolated CPUE 
a.day.cuml.df <- a.day.df %>% 
  mutate(cuml_CPUE = cumsum(mean_CPUE)) %>% 
  mutate(cuml_propn = cuml_CPUE/sum(mean_CPUE))

# Daily interpolated CPUE by BAY 
a.bay.cuml.df <- a.bay.df %>% 
  group_by(bay) %>%
  mutate(cuml_CPUE = cumsum(CPUE)) %>% 
  mutate(cuml_propn = cuml_CPUE/sum(CPUE))


# Plot by daily cumulative CPUE 
day.cuml.dat$date <- as.Date(day.cuml.dat$date)
a.day.cuml.df$day <- as.Date(a.day.cuml.df$day)

ggplot() + 
  geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
  geom_line(data=day.cuml.dat, aes(x=date, y=cuml_propn, colour="Raw CPUE"), size=2, alpha=0.7) +
  geom_point(data=day.cuml.dat, aes(x=date, y=cuml_propn, fill="Raw CPUE"), pch=21, size=4, alpha=0.7) +
  geom_line(data=a.day.cuml.df, aes(x=day, y=cuml_propn, colour="Interpolated CPUE"), size=2, alpha=0.7) +
  geom_point(data=a.day.cuml.df, aes(x=day, y=cuml_propn, fill="Interpolated CPUE"), pch=21, size=4, alpha=0.7) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual("", values=c("Raw CPUE" = "black", 
                                   "Interpolated CPUE" = "red")) + 
  scale_fill_manual("", values=c("Raw CPUE" = "black",
                                     "Interpolated CPUE" = "red")) +
  theme(text = element_text(colour="black", size=45),
        plot.margin=margin(t=15,r=15,b=0,l=10),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.75,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("") +   
  xlab("")


# Plot by daily cumulative CPUE BY BAY!
bay.cuml.dat$date <- as.Date(bay.cuml.dat$date)
a.bay.cuml.df$day <- as.Date(a.bay.cuml.df$day)

ggplot() + 
  geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
  geom_line(data=bay.cuml.dat, aes(x=date, y=cuml_propn, colour=bay), linetype="dashed", size=2, alpha=0.6) +
  geom_point(data=bay.cuml.dat, aes(x=date, y=cuml_propn, fill=bay), pch=21, size=4, alpha=0.6) +
  geom_line(data=a.bay.cuml.df, aes(x=day, y=cuml_propn, colour=bay), size=2) +
  geom_point(data=a.bay.cuml.df, aes(x=day, y=cuml_propn, fill=bay), pch=21, size=4) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual(values=c("#0059d1", "#f0992d", "#81a926"),  
                      breaks=c("B2", "B6", "B11"), 
                      labels=c("Bay 2", "Bay 6", "Bay 11")) + 
  scale_fill_manual(values=c("#0059d1", "#f0992d", "#81a926"),  
                      breaks=c("B2", "B6", "B11"), 
                      labels=c("Bay 2", "Bay 6", "Bay 11")) +
  theme(text = element_text(colour="black", size=45),
        plot.margin=margin(t=15,r=15,b=0,l=10),
        panel.background = element_rect(fill = "white", colour = "black", size=2),
        panel.grid.minor = element_line(colour = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(size=1.2),
        axis.ticks.length = unit(0.5, "line"),
        axis.title.y.left = element_text(margin=margin(t=0,r=15,b=0,l=0), face="bold", size=30),
        axis.text.y = element_text(colour="black", size=25),
        axis.title.x = element_text(margin=margin(t=10,r=0,b=0,l=0), face="bold", size=30),
        axis.text.x = element_text(colour="black", angle=45, hjust=1, size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.85,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("") +   
  xlab("Date")








#------------------------------------------------------------------------------------------------------------------------------------





                                                #__________________________________________#
                                                #    METHOD 3.2: Infilling with IA (M2)    #
                                                #__________________________________________#


#######################
# 1. Create dataframe #
#######################

data <- read.csv("TEB_leftjoin.csv")      

    # Create run time column
    data <- data %>%
      select(everything()) %>%
      mutate_at(vars(c(17)), funs(as.character)) %>%
      mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
      mutate_at(vars(c(17)), funs(as.numeric)) %>%
      mutate(run_time_s = run_time*60)

# Calculate CPUE per run 
data2 <- data %>%
  filter(trap_type =="RST", sockeye_fry_total != "NR") %>%
  group_by(USID, date) %>%
  summarize(unq_SO = unique(sockeye_smolt_total), run_time = unique(run_time_s)) %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = unq_SO/fished_vol) %>%
  print()

data2<-data2 %>% 
  select(-fished_vol, -run_time, -USID) %>% 
  print()


# Average CPUE per day 
data3 <- data2 %>% 
  group_by(date) %>% 
  summarize(mean_CPUE = mean(CPUE)) %>% 
  print()


































