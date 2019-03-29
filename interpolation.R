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
library(magrittr)
library(grid)
library(gridExtra)
library(chron)
library(mice)

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



                                                #_________________________________________#
                                                #    METHOD 3.1: Infilling with CPUE      #
                                                #_________________________________________#


#++++++++++++++++++++++++++++++++++++++++++++++ RAW DATA 

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


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ INTERPOLATED DATA


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



                                                #______________________________________________________#
                                                #        METHOD 3.2: Infilling with IA (M2)            #
                                                #  (ignoring missing values due to missing velocity)   #
                                                #______________________________________________________#


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++ RAw DATA

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
data3.2.2 <- data %>%
  filter(trap_type =="RST", sockeye_fry_total != "NR") %>%
  group_by(USID, date) %>%
  summarize(unq_SO = unique(sockeye_smolt_total), run_time = unique(run_time_s), velocity=unique(current_speed_mps)) %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = unq_SO/fished_vol) %>%
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE*bay_volume_m3) 

data3.2.2<-data3.2.2 %>% 
  ungroup() %>%
  select(-unq_SO, -fished_vol, -run_time, -USID, -velocity, -CPUE, -bay_width, -bay_depth, -bay_volume_m3) 

# Average IA per day 
data3.2.3 <- data3.2.2 %>% 
  group_by(date) %>% 
  summarize(mean_IA = mean(IA)) %>% 
  print()


#################################################################################
#  I am going to skip sections on autocorrelation and model testing.            #
#   Have already established the time series is autocorrelated, re-visiting     #
#   with IA data would not change it. Also, the NAs introduced in the IA        #
#   calculation due to missing velocity measurements require subsetting         #
#   the data further, and I don't really feel like taking the time to do that!  #
#################################################################################



#########################################
# 2. ASSESS MISSING DATA IN CATCH TABLE #
#########################################

# Load catch table matrix
matrix <- read.csv("mission_SO_IA_matrix.csv")

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

# Calculate imputations 
int.a.bay2 <- na.interpolation(ts.b2)
int.a.bay6 <- na.interpolation(ts.b6)
int.a.bay11 <- na.interpolation(ts.b11)


# Plot imputations
plotNA.imputations(ts.b2, int.a.bay2)
plotNA.imputations(ts.b6, int.a.bay6)
plotNA.imputations(ts.b11, int.a.bay11)



########################
# 5. Export back to df #
########################

##
# Interp series - approx
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
  gather(bay, IA, "B2", "B6", "B6", 2:4)


# Daily average 
a.day.df <- a.int.df %>% 
  mutate(day = paste(date)) %>%
  mutate(day = as.POSIXct(day, format="%Y-%m-%d")) %>%
  gather(bay, IA, "B2", "B6", "B11", 3:5) %>%  
  group_by(day) %>% 
  summarize(mean_IA = mean(IA))



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
data3.2.2 <- data %>%
  filter(trap_type =="RST", sockeye_fry_total != "NR") %>%
  group_by(USID, date, bay) %>%
  summarize(unq_SO = unique(sockeye_smolt_total), run_time = unique(run_time_s), velocity=unique(current_speed_mps)) %>%
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  mutate(CPUE = unq_SO/fished_vol) %>%
  mutate(bay_width = 440/3) %>%                                                                                                                           # New column for Bay width (total river width = 440m/3 = Bay width)                                                                                                                                               
  mutate(bay_depth = 1.13) %>%                                                                                                                            # Bay depth for now is considered the RST fishing depth (1.13m)
  mutate(bay_volume_m3 = bay_width*bay_depth*velocity*run_time) %>%                                                                                   # Step 2: Volume of water in Bay during the whole run (Bay area*current velocity*run length)
  mutate(IA = CPUE*bay_volume_m3) 


# Average IA per day 
day.dat <- data3.2.2 %>% 
  select(-fished_vol, -run_time, -USID, -unq_SO, -run_time, -velocity, -CPUE, -bay_width, -bay_depth, -bay_volume_m3) %>% 
  group_by(date) %>% 
  summarize(mean_IA = mean(IA)) %>% 
  print()

# Average CPUE by bay 
bay.dat <- data2 %>% 
  group_by(date, bay) %>% 
  summarize(mean_IA = mean(IA))


##
# PLOT: Normal temporal  
##

# Plot by daily average IA 
day.dat$date <- as.Date(day.dat$date)
a.day.df$day <- as.Date(a.day.df$day)
ggplot() + 
  geom_line(data=day.dat, aes(x=date, y=mean_IA, colour="Raw IA", linetype="Raw IA"), 
            size=2, alpha=0.7) +
  geom_line(data=a.day.df, aes(x=day, y=mean_IA, colour="Interpolated IA", linetype="Interpolated IA"), 
            size=2, alpha=0.7) +
  #geom_line(data=sp.day.df, aes(x=day, y=mean_CPUE), colour="green", size=2, alpha=0.6) +      # all interp results are the same
  #geom_line(data=st.day.df, aes(x=day, y=mean_IA), colour="blue", size=2, alpha=0.6) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual("", values=c("Raw IA" = "black", 
                                   "Interpolated IA" = "blue")) + 
  scale_linetype_manual("", values=c("Raw IA" = 1,
                                     "Interpolated IA" = 1)) +
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
  ylab("Index of abundance") +   
  xlab("")


# Plot by daily average by BAY IA 
bay.dat$date <- as.Date(bay.dat$date)
a.bay.df$day <- as.Date(a.bay.df$day)
ggplot() + 
  geom_line(data=bay.dat, aes(x=date, y=mean_IA, group=bay, colour=bay), 
            linetype="dashed", size=2, alpha=0.6) +
  geom_line(data=a.bay.df, aes(x=day, y=IA, group=bay, colour=bay), 
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
  ylab("Index of abundance") +   
  xlab("Date")

##
# PLOT: Cumulative temporal  
##

# Daily raw IA
day.cuml.dat <- day.dat %>% 
  na.omit() %>%
  mutate(cuml_IA = cumsum(mean_IA)) %>% 
  mutate(cuml_propn = cuml_IA/sum(mean_IA))

# Daily raw IA by BAY 
bay.cuml.dat <- bay.dat %>% 
  group_by(bay) %>% 
  na.omit() %>%
  mutate(cuml_IA = cumsum(mean_IA)) %>% 
  mutate(cuml_propn = cuml_IA/sum(mean_IA))

# Daily interpolated IA 
a.day.cuml.df <- a.day.df %>% 
  mutate(cuml_IA = cumsum(mean_IA)) %>% 
  mutate(cuml_propn = cuml_IA/sum(mean_IA))

# Daily interpolated IA by BAY 
a.bay.cuml.df <- a.bay.df %>% 
  group_by(bay) %>%
  mutate(cuml_IA = cumsum(IA)) %>% 
  mutate(cuml_propn = cuml_IA/sum(IA))


# Plot by daily cumulative CPUE 
day.cuml.dat$date <- as.Date(day.cuml.dat$date)
a.day.cuml.df$day <- as.Date(a.day.cuml.df$day)

ggplot() + 
  geom_hline(aes(yintercept=0.5), colour="gray30", linetype="dotted", size=1.1) +
  geom_line(data=day.cuml.dat, aes(x=date, y=cuml_propn, colour="Raw IA"), size=2, alpha=0.7) +
  geom_point(data=day.cuml.dat, aes(x=date, y=cuml_propn, fill="Raw IA"), pch=21, size=4, alpha=0.7) +
  geom_line(data=a.day.cuml.df, aes(x=day, y=cuml_propn, colour="Interpolated IA"), size=2, alpha=0.7) +
  geom_point(data=a.day.cuml.df, aes(x=day, y=cuml_propn, fill="Interpolated IA"), pch=21, size=4, alpha=0.7) +
  scale_x_date(date_breaks = "5 day", date_labels = ("%h %d")) +
  scale_colour_manual("", values=c("Raw IA" = "black", 
                                   "Interpolated IA" = "blue")) + 
  scale_fill_manual("", values=c("Raw IA" = "black",
                                     "Interpolated IA" = "blue")) +
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






#####################################################################################################################################

#                                                 __________________________________________________
#                                                 |                                                |
#                                                 |  Infilling mising current values - based on TS |
#                                                 |________________________________________________|

                                                              ####################
                                                              # DETECTING BREAKS #
                                                              ####################
data <- read.csv("TEB_leftjoin.csv")

current <- data %>% 
  filter(trap_type=="RST") %>%             
  group_by(USID, date, set_start, bay) %>% 
  summarize(current=unique(current_speed_mps)) %>% 
  print()

####
# Fix start time 
####
current$set_start <- as.character(current$set_start)

# Add zero to start of 3 digit numbers (i.e., 945 becomes 0945)
current$set_start <- with_options(c(scipen = 999), str_pad(current$set_start, 4, pad = "0"))

# Add colon after ever 2 digits
current <- current %>% 
  ungroup() %>%
  mutate(set_start = gsub('(.{2})', '\\1:', set_start)) 

    # Remove trailing colon
    current$set_start <- str_sub(current$set_start, 1, str_length(current$set_start)-1)

# Make as date-time (POSIXct) for minute intervals
current$time <- paste(current$date, current$set_start, sep=" ")
current$time <- as.POSIXct(current$time)

# Omit the negative value 
current[1108,5] <- NA

  # Plot
  ggplot(data=current, aes(x=time,y=current, fill=bay, group=bay, colour=bay)) +
    geom_point(aes(fill=bay), colour="black", pch=21) +
    geom_line(aes(colour=bay)) +
    scale_x_datetime(breaks = "10 day", date_labels = "%h %d") 


# Remove NAs - Must do this first for known values to detect breakpoints, or else NAs cause error
current2 <- current %>% 
  filter(current > 0) %>%
  print()


####
# TS by bay
####

bay2 <- current2 %>% 
  filter(bay=="B2") %>% 
  mutate_at(vars(c(3)), funs(as.factor)) %>%
  mutate(datetime = paste(date, set_start, sep=" ")) %>% 
  mutate_at(vars(c(7)), funs(as.factor))

bay6 <- current2 %>% 
  filter(bay=="B6") %>% 
  select(-bay)

bay11 <- current2 %>% 
  filter(bay=="B11") %>% 
  select(-bay)

# As time series 
b2.current.ts <- ts(bay2$current)
b6.current.ts <- ts(bay6$current)
b11.current.ts <- ts(bay11$current)

####
# Detect breaks
####

# Detect breaks 
b2.bp <- breakpoints(b2.current.ts ~ 1)
b2.ci <- confint(b2.bp)
summary(b2.bp)
summary(b2.ci)
      # Bay 2 breakpoints are at entries: 203 and 359

b6.bp <- breakpoints(b6.current.ts ~ 1)
b6.ci <- confint(b6.bp)
summary(b6.bp)
      # Bay 6 breakpoints are at entries: 143,210,278,346

b11.bp <- breakpoints(b11.current.ts ~ 1)
b11.ci <- confint(b11.bp)
summary(b11.bp) 
      # Bay 11 breakpoints are at entries: 209, 317


#####
# Plot
#####
# BAY 2
plot(b2.current.ts, ylab="Current velocity (m/s)")
lines(b2.ci, col="red")
lines(b2.bp, col="red")

date.labels=c("Apr 03","Apr 20", "May 04", "May 19", "Jun 08")

b2<-autoplot(b2.current.ts, ts.size=2, ts.colour="#f0992d")  + 
  geom_vline(xintercept=c(203,359), size=1.5, col="red", linetype="dashed") +
  geom_errorbarh(aes(xmax = b2.ci$confint[1,2] + 2, xmin = b2.ci$confint[1,2] - 3, y=0, 
                     height=0.2), size=2, col="red") +
  geom_errorbarh(aes(xmax = b2.ci$confint[2,2] + 1, xmin = b2.ci$confint[2,2] - 26, y=0, 
                     height=0.2), size=2, col="red") +
  scale_x_continuous(breaks=seq(0,400,by=100), labels=date.labels) +
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
        axis.text.x = element_text(colour="black", size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.85,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("") +   
  xlab("")

# BAY 6
plot(b6.current.ts)
lines(b6.bp, col="red")
lines(b6.ci, col="red")

date.labels=c("Apr 03","Apr 20", "May 04", "May 19", "Jun 08")

b6<-autoplot(b6.current.ts, ts.size=2, ts.colour="#81a926")  + 
  geom_vline(xintercept=c(143,210,278,346), size=1.5, col="red", linetype="dashed") +
  geom_errorbarh(aes(xmax = b6.ci$confint[1,2] + 6, xmin = b6.ci$confint[1,2] - 4, y=0, 
                     height=0.2), size=2, col="red") +
  geom_errorbarh(aes(xmax = b6.ci$confint[2,2] + 1, xmin = b6.ci$confint[2,2] - 1, y=0, 
                     height=0.2), size=2, col="red") +
  geom_errorbarh(aes(xmax = b6.ci$confint[3,2] + 10, xmin = b6.ci$confint[3,2] - 27, y=0, 
                     height=0.2), size=2, col="red") +
  geom_errorbarh(aes(xmax = b6.ci$confint[4,2] + 7, xmin = b6.ci$confint[4,2] - 28, y=0, 
                     height=0.2), size=2, col="red") +
  scale_x_continuous(breaks=seq(0,400,by=100), labels=date.labels) +
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
        axis.text.x = element_text(colour="black", size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.85,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("Current velocity (m/s)") +   
  xlab("")

# BAY 11
plot(b11.current.ts)
lines(b11.bp, col="red")
lines(b11.ci)

date.labels=c("Apr 03","Apr 20", "May 04", "May 19", "Jun 08")

b11<-autoplot(b2.current.ts, ts.size=2, ts.colour="#0059d1")  + 
  geom_vline(xintercept=c(209,317), size=1.5, col="red", linetype="dashed") +
  geom_errorbarh(aes(xmax = b11.ci$confint[1,2] + 2, xmin = b11.ci$confint[1,2] - 1, y=0, 
                     height=0.2), size=2, col="red") +
  geom_errorbarh(aes(xmax = b11.ci$confint[2,2] + 6, xmin = b11.ci$confint[2,2] - 8, y=0, 
                     height=0.2), size=2, col="red") +
  scale_x_continuous(breaks=seq(0,400,by=100), labels=date.labels) +
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
        axis.text.x = element_text(colour="black", size=25),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="black"),
        legend.position = c(0.85,0.15),
        legend.key.width = unit(2.5, "line")) +                                                               # Position order is: horizontal adjustment, vertical adjustment   
  ylab("") +   
  xlab("Date")

grid.newpage()
grid.draw(rbind(ggplotGrob(b2), ggplotGrob(b6), ggplotGrob(b11),size="last"))


####
# as.data.frame
####

# Create dataframes from time series so that can x-ref breakpoints with actual dates/times 
bay2.date <- bay2$time
bay2.df <- as.data.frame(b2.current.ts)
bay2.df <- cbind(bay2.df, bay2.date)
      # Entry 203 corresponds to: 2017-05-05 09:58:00
      # Entry 359 corresponds to: 2017-05-28 09:14:00     # bigger conf int

bay6.date <- bay6$time
bay6.df <- as.data.frame(b6.current.ts)
bay6.df <- cbind(bay6.df, bay6.date)
      # Entry 143 corresponds to: 2017-04-26 06:13:00
      # Entry 210 corresponds to: 2017-05-05 09:08:00
      # Entry 278 corresponds to: 2017-05-16 10:50:00     # bigger conf int
      # Entry 346 corresponds to: 2017-05-26 13:23:00     # bigger conf int

bay11.date <- bay11$time
bay11.df <- as.data.frame(b11.current.ts)
bay11.df <- cbind(bay11.df, bay11.date)
      # Entry 209 corresponds to: 2017-05-05 08:22:00
      # Entry 317 corresponds to: 2017-05-22 09:55:00     # slightly bigger conf int


#----------------------------------------------------------------------------------------------------------------------

                                      ###################################################
                                      # INFILL: Mean/median based on breaks + time slot #
                                      ###################################################


######################################
# BAY 2 SUBSETS BASED ON BREAKPOINTS #
######################################

 # Entry 203 corresponds to: 2017-05-05 09:58:00
 # Entry 359 corresponds to: 2017-05-28 09:14:00     # bigger conf int

###
# Break 1
###

bay2.br1 <- current %>%
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-04-03 09:29:00") & time <= as.POSIXct("2017-05-05 09:58:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay2.br1$time_round <- format(as.POSIXct(bay2.br1$time_round), format = "%H:%M:%S")
bay2.br1$jdat <- julian(bay2.br1$time, origin = as.POSIXct('2017-04-03 09:29:00', tz = ''))

# Calculate mean/median current based on rough time slots to nearest half hour
rtime2.1 <- bay2.br1 %>% 
  group_by(time_round) %>% 
  summarize(mean_curr = mean(current, na.rm=T), UL = mean_curr+sd(current, na.rm=T), LL = mean_curr-sd(current, na.rm=T),
            median_curr = median(current, na.rm=T), n = n()) %>% 
  print()
    
    # Plot histograms to see 
    ggplot(data=bay2.br1, aes(x=current)) + 
      geom_histogram(colour="black", fill="orange") +
      facet_grid(time_round ~ .) +
      scale_x_continuous(breaks=seq(0.1,0.9, by=0.05)) +
      theme(panel.grid.major = element_line(colour="gray60"))

# Apply median current estimates based on rounded time 
bay2.br1 <- bay2.br1 %>% 
  mutate(current_der = ifelse(is.na(current) & time_round=="06:30:00", "0.418", 
                       ifelse(is.na(current) & time_round=="07:00:00", "0.529", 
                       ifelse(is.na(current) & time_round=="07:30:00", "0.475",
                       ifelse(is.na(current) & time_round=="08:00:00", "0.504",
                       ifelse(is.na(current) & time_round=="08:30:00", "0.507",
                       ifelse(is.na(current) & time_round=="09:00:00", "0.495",
                       ifelse(is.na(current) & time_round=="09:30:00", "0.526",
                       ifelse(is.na(current) & time_round=="10:00:00", "0.544",
                       ifelse(is.na(current) & time_round=="10:30:00", "0.534",
                       ifelse(is.na(current) & time_round=="11:00:00", "0.526",
                       ifelse(is.na(current) & time_round=="11:30:00", "0.552",
                       ifelse(is.na(current) & time_round=="12:30:00", "0.528",
                       ifelse(is.na(current) & time_round=="13:00:00", "0.511", 
                       ifelse(is.na(current) & time_round=="13:30:00", "0.570", current))))))))))))))) 

# Create lm 
summary(lm(bay2.br1$current_der ~ bay2.br1$jdat))
AICc(lm(bay2.br1$current_der ~ bay2.br1$jdat))

###
# Break 2 - No missing values  
###
bay2.br2 <- current %>%
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-05-05 09:58:00") & time <= as.POSIXct("2017-05-28 09:14:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) %>% 
  mutate(current_der = current)
bay2.br2$time_round <- format(as.POSIXct(bay2.br2$time_round), format = "%H:%M:%S") 

###
# Break 3
###
bay2.br3 <- current %>% 
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-05-28 09:14:00") & time <= as.POSIXct("2017-06-14 12:25:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay2.br3$time_round <- format(as.POSIXct(bay2.br3$time_round), format = "%H:%M:%S") 
bay2.br3[1,5] <- NA

# Calculate mean/median current based on rough time slots to nearest half hour
rtime2.3 <- bay2.br3 %>% 
  group_by(time_round) %>% 
  summarize(mean_curr = mean(current, na.rm=T), UL = mean_curr+sd(current, na.rm=T), LL = mean_curr-sd(current, na.rm=T),
            median_curr = median(current, na.rm=T), n = n()) %>% 
  print()

# Apply median current estimates based on rounded time 
bay2.br3 <- bay2.br3 %>% 
  mutate(current_der = ifelse(is.na(current) & time_round=="09:00:00", "0.818", 
                       ifelse(is.na(current) & time_round=="10:30:00", "0.843", current)))




######################################
# BAY 6 SUBSETS BASED ON BREAKPOINTS #
######################################

# Entry 143 corresponds to: 2017-04-26 06:13:00
# Entry 210 corresponds to: 2017-05-05 09:08:00
# Entry 278 corresponds to: 2017-05-16 10:50:00     # bigger conf int
# Entry 346 corresponds to: 2017-05-26 13:23:00     # bigger conf int

###
# Break 1
###
bay6.br1 <- current %>%
  filter(bay=="B6") %>% 
  filter(time >= as.POSIXct("2017-04-03 09:29:00") & time <= as.POSIXct("2017-04-26 06:13:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay6.br1$time_round <- format(as.POSIXct(bay6.br1$time_round), format = "%H:%M:%S") 

# Calculate mean/median current based on rough time slots to nearest half hour
rtime6.1 <- bay6.br1 %>% 
  group_by(time_round) %>% 
  summarize(mean_curr = mean(current, na.rm=T), UL = mean_curr+sd(current, na.rm=T), LL = mean_curr-sd(current, na.rm=T),
            median_curr = median(current, na.rm=T), n = n()) %>% 
  print()
    
    # Plot histograms to see 
    ggplot(data=bay6.br1, aes(x=current)) + 
      geom_histogram(colour="black", fill="hot pink") +
      facet_grid(time_round ~ .) +
      scale_x_continuous(breaks=seq(0.1,0.9, by=0.05)) +
      theme(panel.grid.major = element_line(colour="gray60"))

# Apply median current estimates based on rounded time 
bay6.br1 <- bay6.br1 %>% 
  mutate(current_der = ifelse(is.na(current) & time_round=="06:30:00", "0.667", 
                       ifelse(is.na(current) & time_round=="07:30:00", "0.697",
                       ifelse(is.na(current) & time_round=="08:30:00", "0.758",
                       ifelse(is.na(current) & time_round=="09:30:00", "0.737",
                       ifelse(is.na(current) & time_round=="10:30:00", "0.746",
                       ifelse(is.na(current) & time_round=="11:00:00", "0.826",
                       ifelse(is.na(current) & time_round=="11:30:00", "0.815",
                       ifelse(is.na(current) & time_round=="12:00:00", "0.833",   
                       ifelse(is.na(current) & time_round=="12:30:00", "0.794",
                       ifelse(is.na(current) & time_round=="13:30:00", "0.828", current)))))))))))

###
# Break 2 - No missing values 
###
bay6.br2 <- current %>%
  filter(bay=="B6") %>% 
  filter(time >= as.POSIXct("2017-04-26 06:13:00") & time <= as.POSIXct("2017-05-05 09:08:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay6.br2$time_round <- format(as.POSIXct(bay6.br2$time_round), format = "%H:%M:%S") 

###
# Break 3 - No missing values 
###
bay6.br3 <- current %>%
  filter(bay=="B6") %>% 
  filter(time >= as.POSIXct("2017-05-05 09:08:00") & time <= as.POSIXct("2017-05-16 10:50:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay6.br3$time_round <- format(as.POSIXct(bay6.br3$time_round), format = "%H:%M:%S")

###
# Break 4 - No missing values
###
bay6.br4 <- current %>%
  filter(bay=="B6") %>% 
  filter(time >= as.POSIXct("2017-05-16 10:50:00") & time <= as.POSIXct("2017-05-26 13:23:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay6.br4$time_round <- format(as.POSIXct(bay6.br4$time_round), format = "%H:%M:%S")

###
# Break 5 - No missing values
###
bay6.br5 <- current %>%
  filter(bay=="B6") %>% 
  filter(time >= as.POSIXct("2017-05-26 13:23:00") & time <= as.POSIXct("2017-06-14 11:40:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay6.br5$time_round <- format(as.POSIXct(bay6.br5$time_round), format = "%H:%M:%S")





#######################################
# BAY 11 SUBSETS BASED ON BREAKPOINTS #
#######################################

# Entry 209 corresponds to: 2017-05-05 08:22:00
# Entry 317 corresponds to: 2017-05-22 09:55:00     # slightly bigger conf int

###
# Break 1
###
bay11.br1 <- current %>%
  filter(bay=="B11") %>% 
  filter(time >= as.POSIXct("2017-04-03 09:29:00") & time <= as.POSIXct("2017-05-05 08:22:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay11.br1$time_round <- format(as.POSIXct(bay11.br1$time_round), format = "%H:%M:%S") 

# Calculate mean/median current based on rough time slots to nearest half hour
rtime11.1 <- bay11.br1 %>% 
  group_by(time_round) %>% 
  summarize(mean_curr = mean(current, na.rm=T), UL = mean_curr+sd(current, na.rm=T), LL = mean_curr-sd(current, na.rm=T),
            median_curr = median(current, na.rm=T), n = n()) %>% 
  print()
    
    # Plot histograms to see 
    ggplot(data=bay11.br1, aes(x=current)) + 
      geom_histogram(colour="black", fill="turquoise") +
      facet_grid(time_round ~ .) +
      scale_x_continuous(breaks=seq(0.1,1.9, by=0.05)) +
      theme(panel.grid.major = element_line(colour="gray60"))

# Apply median current estimates based on rounded time 
bay11.br1 <- bay11.br1 %>% 
  mutate(current_der = ifelse(is.na(current) & time_round=="07:00:00", "0.368",
                       ifelse(is.na(current) & time_round=="07:30:00", "0.429",
                       ifelse(is.na(current) & time_round=="08:00:00", "0.537",
                       ifelse(is.na(current) & time_round=="08:30:00", "0.477",
                       ifelse(is.na(current) & time_round=="09:00:00", "0.416",
                       ifelse(is.na(current) & time_round=="09:30:00", "0.481",
                       ifelse(is.na(current) & time_round=="10:00:00", "0.498",
                       ifelse(is.na(current) & time_round=="11:00:00", "0.458",
                       ifelse(is.na(current) & time_round=="11:30:00", "0.465",
                       ifelse(is.na(current) & time_round=="12:00:00", "0.541",
                       ifelse(is.na(current) & time_round=="12:30:00", "0.536",
                       ifelse(is.na(current) & time_round=="13:00:00", "0.492", current)))))))))))))

#####
# Break 2 - No missing values 
#####
bay11.br2 <- current %>%
  filter(bay=="B11") %>% 
  filter(time >= as.POSIXct("2017-05-05 08:22:00") & time <= as.POSIXct("2017-05-22 09:55:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) %>% 
  mutate(current_der = current)
bay11.br2$time_round <- format(as.POSIXct(bay11.br2$time_round), format = "%H:%M:%S") 

#####
# Break 3
#####
bay11.br3 <- current %>%
  filter(bay=="B11") %>% 
  filter(time >= as.POSIXct("2017-05-22 09:55:00") & time <= as.POSIXct("2017-06-14 12:02:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay11.br3$time_round <- format(as.POSIXct(bay11.br3$time_round), format = "%H:%M:%S") 

# Calculate mean/median current based on rough time slots to nearest half hour
rtime11.3 <- bay11.br3 %>% 
  group_by(time_round) %>% 
  summarize(mean_curr = mean(current, na.rm=T), UL = mean_curr+sd(current, na.rm=T), LL = mean_curr-sd(current, na.rm=T),
            median_curr = median(current, na.rm=T), n = n()) %>% 
  print()
    
    # Plot histograms to see 
    ggplot(data=bay11.br3, aes(x=current)) + 
      geom_histogram(colour="black", fill="turquoise") +
      facet_grid(time_round ~ .) +
      scale_x_continuous(breaks=seq(0.1,1.9, by=0.05)) +
      theme(panel.grid.major = element_line(colour="gray60"))

# Apply median current estimates based on rounded time 
bay11.br3 <- bay11.br3 %>% 
  mutate(current_der = ifelse(is.na(current) & time_round=="13:00:00", "0.890", 
                       ifelse(is.na(current) & time_round=="13:30:00", "0.932", current)))
 


####################
# STACK DATAFRAMES #
####################
names <- list(names(bay2.br1))

b2.new.df <- Reduce(function(...) merge(..., all=TRUE), list(bay2.br1, bay2.br2, bay2.br3))    
b6.new.df <- Reduce(function(...) merge(..., all=TRUE), list(bay6.br1, bay6.br2, bay6.br3, bay6.br4, bay6.br5))
b11.new.df <- Reduce(function(...) merge(..., all=TRUE), list(bay11.br1, bay11.br2, bay11.br3))

new.df <- Reduce(function(...) merge(..., all=TRUE), list(b2.new.df, b6.new.df, b11.new.df))




#----------------------------------------------------------------------------------------------------------------------

                                            #########################################
                                            # INFILL: mice(methods) based on breaks #
                                            #########################################

######################################
# BAY 2 SUBSETS BASED ON BREAKPOINTS #
######################################

 # Entry 203 corresponds to: 2017-05-05 09:58:00
 # Entry 359 corresponds to: 2017-05-28 09:14:00     # bigger conf int

####
# Break 1: 2017-04-03 09:29:00 - 2017-05-05 09:58:00
####

bay2.br1 <- current %>%
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-04-03 09:29:00") & time <= as.POSIXct("2017-05-05 09:58:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay2.br1$time_round <- format(as.POSIXct(bay2.br1$time_round), format = "%H:%M:%S") 
bay2.br1$jdat <- julian(bay2.br1$time, origin = as.POSIXct('2017-04-03 09:29:00', tz = ''))

# Linear model without NAs
lm2.1 <- lm(bay2.br1$current~bay2.br1$jdat)
r1<-resid(lm2.1)
plot(r1)
hist(r1)

# subset data to remove variables not needed for prediction 
bay2.br1 <- bay2.br1 %>% 
  select(-c(USID,date,set_start,bay,time, time_round))

# MICE
init <- mice(bay2.br1, maxit=0) 
predM <- init$predictorMatrix     # Set predictor matrix
predM[c("current")]=0             # signify variables not to be considered as predictors but will be imputed
meth[c("ftime")]=""               # signify variables that don't need to be imputed but will be used as predictors 

    # Impute: pmm
    imp.pmm = mice(bay2.br1, method="pmm", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.pmm, current ~ jdat, scales="free")       # red = imputed
    densityplot(imp.pmm, ~current)                       
        # Model results for imputed values only 
        model.pmm <- with(imp.pmm, lm(current ~ jdat))
        summary(model.pmm)
        summary(pool(model.pmm))
        # Model results with completed dataset 
        comp.pmm <- complete(imp.pmm)
        summary(lm(comp.pmm$current ~ comp.pmm$jdat))
        AICc(lm(comp.pmm$current ~ comp.pmm$jdat))              

    
    #Impute: norm.predict
    imp.pnorm = mice(bay2.br1, method="norm.predict", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.pnorm, current ~ jdat, scales="free")       # red = imputed
    densityplot(imp.pnorm, ~current)                       
        # Model results for imputed values 
        model.pnorm <- with(imp.pnorm, lm(current ~ jdat))
        summary(model.pnorm)
        summary(pool(model.pnorm))
        # Model results with completed dataset 
        comp.pnorm <- complete(imp.pnorm)
        summary(lm(comp.pnorm$current ~ comp.pnorm$jdat))
        AICc(lm(comp.pnorm$current ~ comp.pnorm$jdat))              


    # Impute: Bayesian linear regression
    imp.bays = mice(bay2.br1, method="norm", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.bays, current ~ jdat, scales="free")
    densityplot(imp.bays, ~current)
        # Model results for imputed values 
        model.bayes <- with(imp.bays, lm(current ~ jdat))
        summary(model.bayes)
        summary(pool(model.bayes))
        # Model results with completed dataset
        comp.pbayes <- complete(imp.bays)
        summary(lm(comp.pbayes$current ~ comp.pbayes$jdat))
        AICc(lm(comp.pbayes$current ~ comp.pbayes$jdat))              




###
# Break 2: No missing values  
###
bay2.br2 <- current %>%
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-05-05 09:58:00") & time <= as.POSIXct("2017-05-28 09:14:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) %>% 
  mutate(current_der = current)
bay2.br2$time_round <- format(as.POSIXct(bay2.br2$time_round), format = "%H:%M:%S") 


###
# Break 3: 2017-05-28 09:14:00 - 2017-06-14 12:25:00
###
bay2.br3 <- current %>% 
  filter(bay=="B2") %>% 
  filter(time >= as.POSIXct("2017-05-28 09:14:00") & time <= as.POSIXct("2017-06-14 12:25:00")) %>%
  mutate(time_round = lubridate::round_date(time, "30 minutes")) 
bay2.br3$time_round <- format(as.POSIXct(bay2.br3$time_round), format = "%H:%M:%S") 
bay2.br3[1,5] <- NA
bay2.br3$jdat <- julian(bay2.br3$time, origin = as.POSIXct('2017-04-03 09:29:00', tz = ''))

# Linear model without NAs
lm2.3 <- lm(bay2.br3$current~bay2.br3$jdat)
r1<-resid(lm2.3)
plot(r1)
hist(r1)
qqnorm(r1)
qqline(r1)

# subset data to remove variables not needed for prediction 
bay2.br3 <- bay2.br3 %>% 
  select(-c(USID,date,set_start,bay,time, time_round))

# MICE
init <- mice(bay2.br3, maxit=0) 
predM <- init$predictorMatrix     # Set predictor matrix
predM[c("current")]=0             # signify variables not to be considered as predictors but will be imputed
meth[c("ftime")]=""               # signify variables that don't need to be imputed but will be used as predictors 

    # Impute: pmm
    imp.pmm = mice(bay2.br3, method="pmm", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.pmm, current ~ jdat, scales="free")       # red = imputed
    densityplot(imp.pmm, ~current)                       
        # Model results for imputed values only 
        model.pmm <- with(imp.pmm, lm(current ~ jdat))
        summary(model.pmm)
        summary(pool(model.pmm))
        # Model results with completed dataset 
        comp.pmm <- complete(imp.pmm)
        summary(lm(comp.pmm$current ~ comp.pmm$jdat))
        AICc(lm(comp.pmm$current ~ comp.pmm$jdat))            

    
    #Impute: norm.predict
    imp.pnorm = mice(bay2.br3, method="norm.predict", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.pnorm, current ~ jdat, scales="free")       # red = imputed
    densityplot(imp.pnorm, ~current)                       
        # Model results for imputed values 
        model.pnorm <- with(imp.pnorm, lm(current ~ jdat))
        summary(model.pnorm)
        summary(pool(model.pnorm))
        # Model results with completed dataset 
        comp.pnorm <- complete(imp.pnorm)
        summary(lm(comp.pnorm$current ~ comp.pnorm$jdat))
        AICc(lm(comp.pnorm$current ~ comp.pnorm$jdat))            


    # Impute: Bayesian linear regression
    imp.bays = mice(bay2.br3, method="norm", predictorMatrix=predM, m=5, seed=500)
    xyplot(imp.bays, current ~ jdat, scales="free")
    densityplot(imp.bays, ~current)
        # Model results for imputed values 
        model.bayes <- with(imp.bays, lm(current ~ jdat))
        summary(model.bayes)
        summary(pool(model.bayes))
        # Model results with completed dataset
        comp.pbayes <- complete(imp.bays)
        summary(lm(comp.pbayes$current ~ comp.pbayes$jdat))
        AICc(lm(comp.pbayes$current ~ comp.pbayes$jdat))              







# I need completed datasets for 
  # Bay 2 Break 1 (pnorm) and Break 3 (pnorm)
  # Bay 6 Break 1 (pnorm)
  # Bay 11 Break 1 (pnorm) and Break 3 (median time block)

































