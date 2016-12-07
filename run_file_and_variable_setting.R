##### Change the settings below as needed #####

# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored

wd <- "~/Documents/Classes/DS 6001/Newborn Screening/Data/NBS-Capstone"

#########

# DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the data stored

data_path <- "~/Documents/Classes/DS 6001/Newborn Screening/Data"

#########

# DATA FILE NAME
# Enter the file name of the csv file that will be used
# to generate the data for the reports

data_file <- "Dummy data with multiple quarters.csv"
separator <- ","

#########

# REPORT OUTPUT DIRECTORY
# Enter the location on your computer where you want the hospital
# reports to be saved

output_path <- "/Users/chrispatrick/Documents/Classes/DS 6001/Newborn Screening/Data/reports"

########

# DATES FOR REPORT
# Enter the dates you want to use for generating the hospital reports
# Use "mm/dd/yyyy" format (e.g., "04/01/2016")

start_date <- "04/01/2016"
end_date <- "06/30/2016"

########

# QUARTERLY OR MONTHLY
# Enter "quarterly" or "monthly" to define the parameters 
# for the line chart for transit time and unsat count 

line_chart <- "quarterly"

#########

# FUNCTIONALITY TEST
# Do you want to run a report for a single submitter to test functionality,
# or do you want reports for all submitters? Enter "Y" if you wish to run
# only a single report, "N" otherwise

test_report <- "N"

#########

# RUN REPORT - do not change this code
load_packages <- paste(wd, "/load_packages.r", sep="")
run_file <- paste(wd, "/r_script_pdf.r", sep="")
source(load_packages)
source(run_file)
