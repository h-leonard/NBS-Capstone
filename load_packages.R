# Load packages for Newborn Screening Hospital Reporting
# Do not access this file directly; "run_file_and_variable_setting.R"
# will automatically run this file.
 
libs <- c('XLConnect', 
          'xtable',
          'data.table',
          'stringr',
          'reshape2',
          'knitr',
          'markdown',
          'zoo',
          'ggplot2',
          'grid',
          'reshape',
          'lubridate',
          'dplyr',
          'pander',
          'shiny',
          'toOrdinal')
 
for (l in libs) {
  if(!is.element(l, .packages(all.available = TRUE)) ) {
    install.packages(l)
  }
  suppressPackageStartupMessages(library(l, character.only=TRUE))
}
