# Standardized CPUE based on work of 
    # Campbell 2015
    # Campbell 2004
    # Carruthers et al 2011 


# To start, will just use raw total data (i.e., not specific to a CU corrected for sub-sampling)

setwd("~/`Stock assessment/Analysis/Data files")
# Read data
data <- read.csv("TEB_leftjoin.csv")

# Libraries
library(dplyr)
library(ggplot2)
library(withr)    # with_options
library(stringr)  # str_pad
library(lme4)
library(vcdExtra)  # zero.test
library(AER)       # dispersiontest
library(tidyr)


# Variables: 
  # Response: CPUE (fish/m3)
  # Predictors: current velocity (interpolated some values, see "interpolation.R"), TOD, month, bay
  #   (Future predictors may also include trap type, year, depth)





# Set up database to work with
df <- data %>% 
  select(USID, date, run, bay, trap_type, current_speed_mps, set_start, run_time, sockeye_smolt_total) %>% 
  filter(trap_type=="RST") %>% 
  mutate_at(vars(c(8)), funs(as.character)) %>%
  mutate(run_time = paste(gsub("0:", "", run_time))) %>% 
  mutate_at(vars(c(8)), funs(as.numeric)) %>%
  mutate(run_time = run_time*60) %>% 
  mutate_at(vars(c(7)), funs(as.character)) %>% 
  mutate(set_start = with_options(c(scipen = 999), str_pad(set_start, 4, pad = "0"))) %>% 
  mutate(fished_vol = as.numeric(ifelse(run_time=="600", "1243.836",                                                                                  # Nested ifelse() command to apply volume of water fished for each run length that isn't 900 s
                                 ifelse(run_time=="1020", "2114.521",                                                                          # Syntax is "run seconds", "fished volume"
                                 ifelse(run_time=="1080", "2238.905",
                                 ifelse(run_time=="1140", "2363.288",
                                 ifelse(run_time=="1200", "2487.672",
                                 ifelse(run_time=="1260", "2612.056",
                                 ifelse(run_time=="1320", "2736.439", "1865.71"))))))))) %>%
  select(USID, date, run, bay, trap_type, fished_vol, set_start, run_time, current_speed_mps, sockeye_smolt_total) %>% 
  group_by(USID, date) %>%
  summarize(run=unique(run), bay=unique(bay), trap_type=unique(trap_type), current_vel=unique(current_speed_mps),
            TOD=unique(set_start), run_time=unique(run_time), fished_vol=unique(fished_vol), n = unique(sockeye_smolt_total)) %>% 
  mutate(CPUE=n/fished_vol) %>%
  mutate_at(vars(c(7)), funs(as.numeric)) %>% 
  mutate(month = paste(date)) %>%
  separate(month, c("y","m","d")) %>%
  mutate(m = ifelse(m=="04", "April", 
                 ifelse(m=="05", "May", "June"))) %>% 
  rename(month=m) %>%
  select(-c(y, d))

# Replace the one negative current value with an NA 
df[1108,6] <- NA


#####

# Create model structure

m.global <- glm(CPUE ~ current_vel + date + bay + TOD, data=df, family=gaussian(link="identity"))
plot(m.global)

ass1 <- glm(CPUE ~ current_vel, data=df, family=gaussian(link="identity"))
plot(ass1)
r1<-resid(ass1)
plot(r1)
hist(r1)
qqnorm(r1)

ass2 <- glm(CPUE ~ TOD, data=df, family=gaussian(link="identity"))
plot(ass2)
r2<-resid(ass2)
plot(r2)
hist(r2)
qqnorm(r2)

# Assess collinearity 
z <- cbind(df$CPUE, df$current_vel, df$date, df$bay, df$TOD)
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

    # Can see high degree of zero inflation just based on pairs plot but will assess anyway

# zero-inflation - requires NA removed
df.nona <- df %>% 
  na.omit()
  
zero.test(df.nona$CPUE)
  # p < 2.22e-16 therefore zero inflated 


###########################################

# Create binary dataset 

df <- df %>% 
  mutate(binary_catch = ifelse(n=="0", "0", "1")) %>% 
  mutate_at(vars(c(13)), funs(as.numeric))

# Fit GLM 
m.binom <- glm(binary_catch ~ month + current_vel + bay + TOD, data=df, family=binomial(link="logit"))


############################################

# Create non-zero dataset

pois.df <- df %>% 
  filter(n != "0") %>% 
  select(-binary_catch)

m.pois <- glm(CPUE ~ month + current_vel + bay + TOD, data=pois.df, family=quasipoisson(link="log"))

plot(m.pois)

## 









