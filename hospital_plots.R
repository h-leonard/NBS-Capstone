# Add quarters to the dataframe
library(zoo)
dd_copy$QUARTER <- as.yearqtr(dd_copy$BIRTHDATE, format="Q%q")

# Group by submitter and quarter
hospital_metrics <- dd_copy %>%
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

# Create line graph of transit time over the past 4 quarters for the submitter
install.packages("reshape")

library(ggplot2)
library(reshape)

p1 <- ggplot(hospital_metrics[2:5,], aes(x=QUARTER, y=avg_transit_time)) +
  scale_x_yearqtr(format="%Y Q%q", n=4) +
  labs(title=hospital_metrics$SUBMITTERNAME[1], x="", y="Average Transit Time") +
  geom_line(color="blue") +
  geom_line(data=tail(state_metrics, 4), color="red") 
p1

h <- hospital_metrics[2:5, c("QUARTER", "avg_transit_time")]
s <- tail(state_metrics, 4)[c("QUARTER", "avg_transit_time")]

zz <- melt(list(h = h, s = s), id.vars = "QUARTER")

ggplot(zz, aes('QUARTER', value)) + 
  scale_x_yearqtr(format="%Y Q%q", n=4) +
  geom_line() + 
  scale_colour_manual("Dataset", values = c("h" = "blue", "s" = "red"))

ggplot(zz, aes(x, value, colour = L1)) + geom_point() +
  scale_colour_manual("Dataset", values = c("p1" = "blue", "p2" = "red", "p3" = "yellow"))





# Create a line graph of unsatisfactory percentage over the past 4 quarters for the submitter
p2 <- ggplot(hospital_metrics[1:5,], aes(x=QUARTER, y=unsat_percent)) +
  labs(title=hospital_metrics$SUBMITTERNAME[1], x="Quarter", y="Unsatisfactory Percentage")
p2 + geom_line()