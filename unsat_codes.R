# Codes for unsatisfactory results
unsat_codes <- seq(1,13)
unsat_names <- c("Improperly Collected",
                 "Scratched or Abraded",
                 "Wet",
                 "Oversaturated",
                 "Contaminated",
                 "No Blood",
                 "Insufficient Information",
                 "Old Sample > 10 days in transit",
                 "Infant > 6 months old",
                 "Outdated filter paper card",
                 "Insufficient Quantity",
                 "Interfering Substances Present",
                 "Other")
unsats <- as.data.frame(cbind(unsat_codes, 
                              unsat_names))
names(unsats) <- c('code','description')
