date_rand <- function(N, st="2015/04/01", et="2016/03/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}

keep_cols <- c('SUBMITTERNAME', 'TRANSIT_TIME', 'COLLECTIONDATE', 'COLLECTIONTIME', 'BIRTHDATE', 'BIRTHTIME', 
               'UNSATCODE', 'RECALL_FLAG', 'TRANSFUSED')

dd_copy <- dd[keep_cols]

set.seed(84)

# Generate 240 random birthdates from up to 1 year back
birthdates = date_rand(240)

# Pull birthdates and times
BIRTHDATE <- as.vector(format(birthdates, "%d/%m/%Y"))
BIRTHTIME <- as.vector(format(birthdates, "%H%m"))

# Find how long after the birthdate the collection dates were
times_diff <- difftime(dd_copy$COLLECTIONDATE, dd_copy$BIRTHDATE)

# Remove amounts greater than 200
times_diff <- times_diff[times_diff < 200]

# Sample from the times_diff to get a collection date for randomly created observations
samps1 <- sample(times_diff, size=240, replace=TRUE)

COLLECTIONDATE <- as.Date(BIRTHDATE, "%d/%m/%Y") + samps1

# Sample from the collection times to get a collection time for randomly created observations
COLLECTIONTIME <- sample(dd$COLLECTIONTIME, size=240, replace=TRUE)

# Sample from the transit times to get a collection time for randomly created observations
TRANSIT_TIME <- sample(dd$TRANSIT_TIME, size=240, replace=TRUE)

# Sample from the submitters to get a submitter for randomly created observations
SUBMITTERNAME <- sample(dd$SUBMITTERNAME, size=240, replace=TRUE)

# Create random transit times
TRANSIT_TIME <- round(runif(240, 0.06, 5.99), digits=2)

# Sample from 0:13 to create vector of unsat codes, with likelihood of 
# being 0 = 40%. Then change the 0's to NAs.
UNSATCODE <- sample(0:13, size=240, replace=TRUE, prob=c(.81, rep(.19/13, 13)))
UNSATCODE[UNSATCODE == 0] <- NA

# Sample from RECALL_FLAG
RECALL_FLAG <- sample(dd$RECALL_FLAG, size=240, replace=TRUE)

# Sample from TRANSFUSED
TRANSFUSED <- sample(dd$TRANSFUSED, size=240, replace=TRUE)

# Create dataframe from random variables
dd_random <- as.data.frame(cbind(SUBMITTERNAME, TRANSIT_TIME, COLLECTIONDATE, COLLECTIONTIME, BIRTHDATE, BIRTHTIME, UNSATCODE,
                   RECALL_FLAG, TRANSFUSED))

dd_random$SUBMITTERNAME <- SUBMITTERNAME
dd_random$TRANSIT_TIME <- TRANSIT_TIME
dd_random$COLLECTIONDATE <- as.Date(COLLECTIONDATE, format="%d/%m/%Y")
dd_random$COLLECTIONTIME <- COLLECTIONTIME
dd_random$BIRTHDATE <- as.Date(BIRTHDATE, format="%d/%m/%Y")
dd_random$BIRTHTIME <- as.integer(BIRTHTIME)
dd_random$UNSATCODE <- as.integer(UNSATCODE)
dd_random$RECALL_FLAG <- as.factor(RECALL_FLAG)
dd_random$TRANSFUSED <- as.factor(TRANSFUSED)

# Bind random dataframe to copy of data frame
dd_copy <- rbind(dd_copy, dd_random)

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

library(data.table)
.simpleCap <- function(x) {
  s <- tolower(x)
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
.simpleCap(hospital_metrics$SUBMITTERNAME[1])

# Create line graph of transit time over the past 4 quarters for the submitter
library(ggplot2)
p <- ggplot(hospital_metrics[1:5,], aes(x=QUARTER, y=avg_transit_time)) +
  labs(title=hospital_metrics$SUBMITTERNAME[1], x="Quarter", y="Average Transit Time")
p + geom_line()

# Create a line graph of unsatisfactory percentage over the past 4 quarters for the submitter
p2 <- ggplot(hospital_metrics[1:5,], aes(x=QUARTER, y=unsat_percent)) +
  labs(title=hospital_metrics$SUBMITTERNAME[1], x="Quarter", y="Unsatisfactory Percentage")
p2 + geom_line()


