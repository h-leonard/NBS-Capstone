# Add quarters to the dataframe 

dd_copy <- dd
dd_copy$QUARTER <- as.yearqtr(dd_copy$BIRTHDATE, format="%Y%m")

as.yearqtr(Data$date, format = "%Y-%m-%d")

# Group by submitter and quarter
hospital_metrics2 <- dd_copy %>%
  group_by(SUBMITTERNAME, QUARTER) %>%
  select(SUBMITTERNAME, TRANSIT_TIME, UNSATCODE, QUARTER) %>%
  summarise(
    total_samples=n(),   
    avg_transit_time = ifelse(!is.nan(mean(TRANSIT_TIME, na.rm=TRUE)), mean(TRANSIT_TIME, na.rm=TRUE), NA),
    unsat_count = sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples
  )

# Group by quarter for state totals - one approach (unweighted mean)
state_metrics <- dd_copy %>%
  group_by(QUARTER) %>%
  select(QUARTER, TRANSIT_TIME, UNSATCODE) %>%
  summarise(
    total_samples=n(),   
    avg_transit_time = mean(TRANSIT_TIME, na.rm=TRUE),
    unsat_count =  sum(!is.na(UNSATCODE)),
    unsat_percent = unsat_count/total_samples
  )

# Filter hospital and state dfs
hospital_filt <- hospital_metrics2[2:5,]
hospital_filt <- ungroup(hospital_filt)
h <- as.character(hospital_filt$SUBMITTERNAME[1])
hospital_filt$SUBMITTERNAME <- 'Hospital'
state_filt <- state_metrics[7:10,]
state_filt$SUBMITTERNAME <- 'State'
state_filt <- state_filt[,c(6,1:5)]

# Bind hospital and state dfs together 
bound <- rbind(hospital_filt, state_filt)

# Create line graph of transit time over the past 4 quarters for the submitter
p1 <- ggplot(bound, aes(x=QUARTER, y=avg_transit_time, colour=SUBMITTERNAME, group=rev(SUBMITTERNAME))) +
  scale_x_yearqtr(format="%Y Q%q", n=4) +
  labs(x="Quarter", y="Days") +
  ggtitle(bquote(atop("Average Transit Time", atop(italic(.(h)), "")))) + 
  geom_line(size=2) +
  scale_color_manual(values=c("blue","grey75")) +
  geom_hline(color="green4", aes(yintercept=2)) +
  geom_text(color="green4", aes(x=2015.6, y=2, label="Goal: under 2 days", vjust=1.25)) +
  theme(legend.title=element_blank())
p1

# Create a line graph of unsatisfactory percentage over the past 4 quarters for the submitter
p2 <- ggplot(bound, aes(x=QUARTER, y=unsat_percent, colour=SUBMITTERNAME, group=rev(SUBMITTERNAME))) +
  scale_x_yearqtr(format="%Y Q%q", n=4) +
  labs(x="Quarter", y="Percentage") +
  ggtitle(bquote(atop("Percentage of Unsatisfactory Samples", atop(italic(.(h)), "")))) +
  geom_line(size=2) +
  scale_color_manual(values=c("blue","grey75")) +
  theme(legend.title=element_blank())
p2
