##### Change the settings below as needed #####

# SET WORKING DIRECTORY
# This should be where you have all of your R code files stored

wd <- "~/Documents/Classes/DS 6001/Newborn Screening/Data"

#########

# DATA FILE PATH
# This should be your directory (e.g., the location on your 
# computer) where you have the data stored

data_path <- "~/Documents/Classes/DS 6001/Newborn Screening/Data"

#########

# DATA FILE NAME
# Enter the file name of the csv file that will be used
# to generate the data for the reports

data_file <- "Dummy data.csv"

#########

# REPORT OUTPUT DIRECTORY
# Enter the location on your computer where you want the hospital
# reports to be saved

output_path <- "/Users/chrispatrick/Documents/Classes/DS 6001/Newborn Screening/Data/reports"

########

# DATES FOR REPORT
# Enter the dates you want to use for generating the hospital reports

start_date <- "04/01/2016"
end_date <- "06/30/2016"

#########

# RUN REPORT
source("r_script_pdf.r")

