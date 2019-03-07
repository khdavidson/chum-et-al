
    # Decay curve fit tries 

# Total number of fish caught, sampled and released daily 
catch_dat <- data %>% 
  select(USID, date, trap_type, depth_ft, sockeye_smolt_total, sockeye_smolt_release, CU_final, run_time_s, current_speed_mps) %>%
  filter(trap_type != "IPT", sockeye_smolt_total != "NR") %>% 
  group_by(date, USID, trap_type, depth_ft) %>%                                                                                                              
  summarize(unq_catch = unique(sockeye_smolt_total)) %>% 
  group_by(date, trap_type, depth_ft) %>%                                                                                                              
  summarize(sum_SO = sum(unq_catch)) %>%
  ungroup() %>%
  print()

unq <- data %>% 
  group_by(USID, trap_type, depth_ft) %>% 
  filter(trap_type != "IPT") %>%
  group_by(depth_ft) %>% 
  summarize(n = n_distinct(USID)) %>% 
  group_by(depth_ft) %>% 
  summarize(sum=sum(n)) %>%
  print()

depth <- data %>% 
  group_by(USID,depth_ft) %>% 
  summarize(unq_catch = unique(sockeye_smolt_total)) %>% 
  group_by(depth_ft) %>% 
  summarize(unq = sum(unq_catch, na.rm=T)) %>%
  print()

# Fit exponential decay curve 
  # Edit dataframe 
  decay <- catch_dat %>% 
    select(-c(date, trap_type)) %>% 
    mutate(depth_m=ifelse(depth_ft=="0", "1",
                   ifelse(depth_ft=="6", "1.8", "3.7")))%>%
    mutate_at(vars(c(3)), funs(as.numeric)) %>%
    group_by(depth_ft) %>% 
    summarize(mean=mean(sum_SO)) %>%
    print()
  
# Exponential decay function: y = a(1-b)x
  # y: Final amount remaining after the decay over a period of time
  # a: The original amount
  # x: Time
  # The decay factor is (1-b).
  # The variable, b, is percent decrease in decimal form.

# vector
l = length(unique(decay$x))

#loop
x <- decay$x
y <- decay$y
unique_interval <- unique(interval_depths)
num_interval <- length(unique_interval)
interval_mean = vector()

for(i in 1:num_interval){
  interval_mean[i] = mean(decay[decay$x==unique_interval[i],]$y)
}
  
accum_rate <- decay$sum_SO/decay$depth_m
plot(decay$depth_ft, decay$mean)

x.dummy=c(1:15)
edar=800*(x.dummy)^-77
lines(edar)
plot.new()

n1 <- nls(decay$mean~a*decay$depth_ft^b, start=list(a=44.5,b=1.3))
  
fit <- nls(mean ~ SSasymp(depth_ft, yf, y0, log_alpha), data = decay)

ggplot(data=decay, aes(x=x, y=y)) +
  #geom_bar(data=unq, aes(x=depth_ft, y=sum, group=depth_ft),stat="identity", size=3, fill="black", colour="black", alpha=0.5) +
  geom_point(pch = 21, size=3, fill="red", colour="black", alpha=0.5) +
  stat_function(fun = function(x) exp(fit2$coefficients[1] + x*fit2$coefficients[2]))
