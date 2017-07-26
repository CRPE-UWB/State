#### Center on Reinventing Public Education #### 
# Description: Quick cleaning of existing data we had on Missouri from the database
# Title: Cleaning Missouri 
# Created by: Kevin Cha on 07-17-17
# Updated by: Kevin Cha on 07-25-17
# Data from: AWS (S3)
# Link to Github: https://github.com/CRPE-UWB/State

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_mo_clean")

library(plyr)
library(dplyr)
library(tidyr)

# Function --------------------------------------------------------------------------------------------------------
no_more_special_characters <- function(df_col) {
  # need to replace the chracters
  require(gsubfn)
  # list of special characters to replace with
  unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  # replaces the characters
  df_col <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,df_col)
  
  return(df_col)
}


# Performance --------------------------------------------------------------------------------------------------------
# read in csv file
data_perf <- read.csv("data/mo_school_perf.csv", header = TRUE, stringsAsFactors = FALSE)

# Turn all PCT into hundredths
data_perf$LEVEL_NOT_DETERMINED_PCT.Eng..Language.Arts <- data_perf$LEVEL_NOT_DETERMINED_PCT.Eng..Language.Arts / 100
data_perf$BELOW_BASIC_PCT.Eng..Language.Arts <- data_perf$BELOW_BASIC_PCT.Eng..Language.Arts / 100
data_perf$BASIC_PCT.Eng..Language.Arts <- data_perf$BASIC_PCT.Eng..Language.Arts / 100
data_perf$PROFICIENT_PCT.Eng..Language.Arts <- data_perf$PROFICIENT_PCT.Eng..Language.Arts / 100
data_perf$ADVANCED_PCT.Eng..Language.Arts <- data_perf$ADVANCED_PCT.Eng..Language.Arts / 100
data_perf$BOTTOM_TWO_LEVELS_PCT.Eng..Language.Arts <- data_perf$BOTTOM_TWO_LEVELS_PCT.Eng..Language.Arts / 100
data_perf$TOP_TWO_LEVELS_PCT.Eng..Language.Arts <- data_perf$TOP_TWO_LEVELS_PCT.Eng..Language.Arts / 100
data_perf$BELOW_BASIC_PCT.Mathematics <- data_perf$BELOW_BASIC_PCT.Mathematics / 100
data_perf$BASIC_PCT.Mathematics <- data_perf$BASIC_PCT.Mathematics / 100
data_perf$PROFICIENT_PCT.Mathematics <- data_perf$PROFICIENT_PCT.Mathematics / 100
data_perf$ADVANCED_PCT.Mathematics <- data_perf$ADVANCED_PCT.Mathematics / 100
data_perf$BOTTOM_TWO_LEVELS_PCT.Mathematics <- data_perf$BOTTOM_TWO_LEVELS_PCT.Mathematics / 100
data_perf$TOP_TWO_LEVELS_PCT.Mathematics <- data_perf$TOP_TWO_LEVELS_PCT.Mathematics / 100
data_perf$BELOW_BASIC_PCT.Science <- data_perf$BELOW_BASIC_PCT.Science / 100
data_perf$BASIC_PCT.Science <- data_perf$BASIC_PCT.Science / 100
data_perf$PROFICIENT_PCT.Science <- data_perf$PROFICIENT_PCT.Science / 100
data_perf$ADVANCED_PCT.Science <- data_perf$ADVANCED_PCT.Science / 100
data_perf$BOTTOM_TWO_LEVELS_PCT.Science <- data_perf$BOTTOM_TWO_LEVELS_PCT.Science / 100
data_perf$TOP_TWO_LEVELS_PCT.Science <- data_perf$TOP_TWO_LEVELS_PCT.Science / 100

# Turn NAs into -99
data_perf[is.na(data_perf)] <- -99

# Get rid of special characters
data_perf[data_perf == ','] <- " "
data_perf[data_perf == '~'] <- "-"

# Save as .csv file
write.csv(data_perf,"cleaned_data/mo_perf.csv", row.names = FALSE)

# Demographics --------------------------------------------------------------------------------------------------------
# Required to read .xls data
library(gdata)
# read in xls file
data_demo <- gdata::read.xls("data/mo_school_demo_1991-2014.xls", header = TRUE)

# Turn all PCT into hundredths
data_demo$LUNCH_COUNT_FREE_REDUCED_PCT <- data_demo$LUNCH_COUNT_FREE_REDUCED_PCT / 100
data_demo$ENROLLMENT_ASIAN_PCT <- data_demo$ENROLLMENT_ASIAN_PCT / 100
data_demo$ENROLLMENT_BLACK_PCT <- data_demo$ENROLLMENT_BLACK_PCT / 100
data_demo$ENROLLMENT_WHITE_PCT <- data_demo$ENROLLMENT_WHITE_PCT / 100
data_demo$ENROLLMENT_INDIAN_PCT <- data_demo$ENROLLMENT_INDIAN_PCT / 100
data_demo$ENROLLMENT_HISPANIC_PCT <- data_demo$ENROLLMENT_HISPANIC_PCT / 100

# Turn NAs into -99
data_demo[is.na(data_demo)] <- -99

# Get rid of special characters
data_demo[data_demo == ','] <- " "
data_demo[data_demo == '~'] <- "-"

# Save as .csv file
write.csv(data_demo,"cleaned_data/mo_enroll_1991_2014.csv", row.names = FALSE)



# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/mo_perf_demo_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/mo_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/mo_perf_clean.rds")




