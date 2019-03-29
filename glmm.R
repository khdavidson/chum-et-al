
# Standardized CPUE based on work of 
    # Campbell 2015
    # Campbell 2004
    # Carruthers et al 2011 
# Using resources
    # Gelman and Hill 2006
    # Bartolino et al 2010 - DRAFT - R code documentation to standardize, estimate trends and perform age slicing with MEDITS survey data

# To start, will just use raw total data (i.e., not specific to a CU corrected for sub-sampling)


####################################################################################################################################

setwd("~/`Stock assessment/Analysis/Data files")

# Libraries
library(dplyr)
library(ggplot2)
library(withr)    # with_options
library(stringr)  # str_pad
library(lme4)
library(vcdExtra)  # zero.test
library(AER)       # dispersiontest
library(tidyr)
library(lubridate)  # lubridate::

# Read data
data <- read.csv("TEB_leftjoin.csv")

#####################################################################################################################################

# Variables: 
  # Response: CPUE (fish/m3)
  # Predictors: current velocity (interpolated some values, see "interpolation.R"), TOD, month, bay
  #   (Future predictors may also include trap type, year, depth)





# Set up database to work with
df <- data %>% 
  select(USID, date, run, bay, trap_type, current_speed_mps, set_start, set_end, run_time, sockeye_smolt_total) %>% 
  filter(trap_type=="RST", sockeye_smolt_total != "NR") %>% 
  mutate_at(vars(c(9)), funs(as.character)) %>%
  mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
  mutate_at(vars(c(9)), funs(as.numeric)) %>%
  mutate(run_time = run_time*60) %>% 
  mutate_at(vars(c(7)), funs(as.character)) %>% 
  mutate(set_start = with_options(c(scipen = 999), str_pad(set_start, 4, pad = "0"))) %>% 
  mutate_at(vars(c(8)), funs(as.character)) %>% 
  mutate(set_end = with_options(c(scipen = 999), str_pad(set_end, 4, pad = "0"))) %>% 
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  select(USID, date, run, bay, trap_type, fished_vol, set_start, set_end, run_time, current_speed_mps, sockeye_smolt_total) %>% 
  group_by(USID, date) %>%
  summarize(run=unique(run), bay=unique(bay), trap_type=unique(trap_type), current_vel=unique(current_speed_mps),
            start=unique(set_start), end=unique(set_end), run_time=unique(run_time), fished_vol=unique(fished_vol), n = unique(sockeye_smolt_total)) %>% 
  mutate(CPUE=n/fished_vol) %>%
  #mutate_at(vars(c(7:8)), funs(as.numeric)) %>% 
  mutate(month = paste(date)) %>%
  separate(month, c("y","m","d")) %>%
  mutate(m = ifelse(m=="04", "April", 
                 ifelse(m=="05", "May", "June"))) %>% 
  rename(month=m) %>%
  select(-c(y, d))

# Clean up & merge with interpolated current df (from 'interpolation.R' script)
cur.int <- read.csv("interpolated_current.csv")
cur.int <- cur.int %>% 
  mutate_at(vars(c(2)), funs(as.character)) %>%
  mutate(date = lubridate::dmy(date))

df <- df %>% 
  mutate_at(vars(c(2)), funs(as.Date)) %>% 
  rename(current = current_vel)

db <- left_join(df, cur.int, by=c("USID", "date", "bay", "current"))

db <- db %>% 
  select(USID, time, date, jdat, month, start, end, time_round, run, bay, trap_type, current, current_imp, run_time, fished_vol, n, CPUE) 

db$date <- as.Date(db$date)
db$month <- as.factor(db$month)


#-----------------------------------------------------------------------------------------------------------------------------------

# Merge expanded table and db *******




#-----------------------------------------------------------------------------------------------------------------------------------

####################################
# DATA EXPLORATION AND ASSUMPTIONS #
####################################


# Create global model to assess diagnostic plots 
m.global <- glm(CPUE ~ current_imp + date + bay + TOD, data=db, family=gaussian(link="identity"))
plot(m.global)
    # Extreme pattern in residuals 
    # Clear right-skew of normal QQ
    # Leverage apparent

# Assess collinearity 
z <- cbind(db$CPUE, db$current_imp, db$date, db$bay, db$TOD)
colnames(z) <- c("CPUE", "current velocity", "date", "bay", "TOD")
cor(z)
pairs(z)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr=par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = cor(x, y, use="na.or.complete")
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=2.5)
}
pairs(z,
      upper.panel = panel.cor,
      cex=3,
      pch=16)

    # NOTE: date and velocity are pretty collinear, may need an interaction term. Also with bay x velocity.
      # Pairs plots also indicate strong zero inflation but will test below anyway. 


# Zero-inflation
zero.test(db$CPUE)
  # p < 2.22e-16 therefore reject H0, zero inflation 


#-------------------------------------------------------------------------------------------------------------------------------------

###################
# BINOMIAL MODELS #
###################

db <- db %>% 
  mutate(binary_catch = ifelse(n=="0", "0", "1")) %>% 
  mutate_at(vars(c(17)), funs(as.numeric))

# Fit binomial GLMs 
m.binom <- glm(binary_catch ~ month + current_imp + bay + TOD, data=db, family=binomial(link="logit"))


#-------------------------------------------------------------------------------------------------------------------------------------

##################
# POISSON MODELS #
##################

pois.db <- db %>% 
  filter(n != "0") %>% 
  select(-binary_catch)

m.pois <- glm(CPUE ~ month + current_imp + bay + TOD, data=pois.db, family=gaussian(link="identity"))
plot(m.pois)












