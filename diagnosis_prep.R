# PREPARE DIAGNOSIS DATA

# read in narratives for diagnoses
diag_narr <- read.csv(paste(codes_path, slash, "diagnosis_narratives.csv", sep=""), 
                      stringsAsFactors = FALSE)

#### Explode the VariantNames so these can be checked against the values in the diagnosis data ####

# split up the values
var_split <- strsplit(diag_narr$VariantNames, ";", fixed = TRUE)

## determine length of each element
num_col <- vapply(var_split, length, 1L)

## create an empty character matrix to store the results
exp_var <- matrix(NA_character_, nrow = nrow(diag_narr),
                  ncol = max(num_col), 
                  dimnames = list(NULL, paste0("V", sequence(max(num_col)))))

## use matrix indexing to figure out where to put the results
exp_var[cbind(rep(1:nrow(diag_narr), num_col), 
              sequence(num_col))] <- unlist(var_split, use.names = FALSE)

## Bind the values back together
diag_extend <- cbind(diag_narr, exp_var)

# Get length and width of diag_extend
diag_extend_len <- nrow(diag_extend)
diag_extend_wid <- ncol(diag_extend)

####

# read in diagnosis data
initial_dd_diag <- read_data(diag_data_path, "DIAGNOSISDATE")

# filter diagnosis data based on start/end date
temp_dd_diag <- create_filt_dfs(initial_dd_diag, type="diagnosis")
dd_diag <- as.data.frame(temp_dd_diag[1])

# find the disorder name for each diagnosis in the data
for (i in 1:nrow(dd_diag)) {
  loc = which(diag_extend[,c(6:diag_extend_wid)]==dd_diag$DIAGNOSIS[i])
  if (length(loc) == 0) {
    loc = NA
    dd_diag$DISORDER[i] <- NA
  } else {
    num_row = ifelse(loc %% diag_extend_len != 0, loc %% diag_extend_len, diag_extend_len)
    dd_diag$DISORDER[i] <- diag_extend[num_row, 1]
  }
}

# join diagnoses and narratives
dd_diag_narr <- full_join(dd_diag, diag_narr, by=c("DISORDER"="Disorder"))
