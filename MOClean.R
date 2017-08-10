#### Center on Reinventing Public Education #### 
# Description: Quick cleaning of existing data we had on Missouri from the database
# Title: Cleaning Missouri 
# Created by: Kevin Cha on 07-17-17
# Updated by: Kevin Cha on 08-09-17
# Data from: AWS (S3)
#     Demographics: https://mcds.dese.mo.gov/quickfacts/Pages/District-and-School-Information.aspx
#     Performance: https://mcds.dese.mo.gov/quickfacts/Pages/State-Assessment.aspx      
# Link to Github: https://github.com/CRPE-UWB/State
# Notes: When they use YEAR, I can't tell if its the beginning or end year (ex 2014-2015, beginning = 2014 & end = 2015)

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_mo_clean")

library(plyr)
library(dplyr)
library(tidyr)
library(readxl)

# Function --------------------------------------------------------------------------------------------------------
no_more_special_characters <- function(df_col) {
  # need to replace the chracters
  require(gsubfn)
  # make sure the col is character
  df_col <- as.character(df_col)
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

# Add Code to Front of Col Names
add_code <- function(df, code) {
  for (i in 7:ncol(df)) {
    colnames(df)[i] <- paste(code, colnames(df)[i], sep="_")
    next
  }
  return(df)
}

# last clean
last_clean <- function(df) {
  # last cleaning
  df <- as.data.frame(sapply(df, gsub, pattern=",", replacement=""))
  df <- as.data.frame(sapply(df, gsub, pattern="~", replacement="-"))
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df[is.na(df)] <- -99
  
  return(df)
}

# Read in Each Dataset --------------------------------------------------------------------------------------------------------
# dataset: performance
data_perf_1 <- read.csv("data/mo_school_perf.csv", header = TRUE, stringsAsFactors = FALSE)
# dataset: demographic
data_demo_1 <- read_xls("data/mo_school_demo_1991-2014.xls")
# dataset: performance
data_perf <- read.csv("data/MAP_School_Final.csv", header = TRUE, stringsAsFactors = FALSE)
# dataset: demographic
data_demo <- read_xls("data/Building Demographic Data.xls")

# # Performance V1 (up to 2014) --------------------------------------------------------------------------------------------------------
# # Turn all PCT into hundredths
# data_perf_1$LEVEL_NOT_DETERMINED_PCT.Eng..Language.Arts <- data_perf_1_1$LEVEL_NOT_DETERMINED_PCT.Eng..Language.Arts / 100
# data_perf_1$BELOW_BASIC_PCT.Eng..Language.Arts <- data_perf_1$BELOW_BASIC_PCT.Eng..Language.Arts / 100
# data_perf_1$BASIC_PCT.Eng..Language.Arts <- data_perf_1$BASIC_PCT.Eng..Language.Arts / 100
# data_perf_1$PROFICIENT_PCT.Eng..Language.Arts <- data_perf_1$PROFICIENT_PCT.Eng..Language.Arts / 100
# data_perf_1$ADVANCED_PCT.Eng..Language.Arts <- data_perf_1$ADVANCED_PCT.Eng..Language.Arts / 100
# data_perf_1$BOTTOM_TWO_LEVELS_PCT.Eng..Language.Arts <- data_perf_1$BOTTOM_TWO_LEVELS_PCT.Eng..Language.Arts / 100
# data_perf_1$TOP_TWO_LEVELS_PCT.Eng..Language.Arts <- data_perf_1$TOP_TWO_LEVELS_PCT.Eng..Language.Arts / 100
# data_perf_1$BELOW_BASIC_PCT.Mathematics <- data_perf_1$BELOW_BASIC_PCT.Mathematics / 100
# data_perf_1$BASIC_PCT.Mathematics <- data_perf_1$BASIC_PCT.Mathematics / 100
# data_perf_1$PROFICIENT_PCT.Mathematics <- data_perf_1$PROFICIENT_PCT.Mathematics / 100
# data_perf_1$ADVANCED_PCT.Mathematics <- data_perf_1$ADVANCED_PCT.Mathematics / 100
# data_perf_1$BOTTOM_TWO_LEVELS_PCT.Mathematics <- data_perf_1$BOTTOM_TWO_LEVELS_PCT.Mathematics / 100
# data_perf_1$TOP_TWO_LEVELS_PCT.Mathematics <- data_perf_1$TOP_TWO_LEVELS_PCT.Mathematics / 100
# data_perf_1$BELOW_BASIC_PCT.Science <- data_perf_1$BELOW_BASIC_PCT.Science / 100
# data_perf_1$BASIC_PCT.Science <- data_perf_1$BASIC_PCT.Science / 100
# data_perf_1$PROFICIENT_PCT.Science <- data_perf_1$PROFICIENT_PCT.Science / 100
# data_perf_1$ADVANCED_PCT.Science <- data_perf_1$ADVANCED_PCT.Science / 100
# data_perf_1$BOTTOM_TWO_LEVELS_PCT.Science <- data_perf_1$BOTTOM_TWO_LEVELS_PCT.Science / 100
# data_perf_1$TOP_TWO_LEVELS_PCT.Science <- data_perf_1$TOP_TWO_LEVELS_PCT.Science / 100
# 
# # Turn NAs into -99
# data_perf_1[is.na(data_perf_1)] <- -99
# 
# # Get rid of special characters
# data_perf_1[data_perf_1 == ','] <- " "
# data_perf_1[data_perf_1 == '~'] <- "-"
# 
# # Save as .csv file
# write.csv(data_perf_1,"cleaned_data/mo_perf.csv", row.names = FALSE)
# 
# # Demographics V1 (up to 2014) --------------------------------------------------------------------------------------------------------
# # Turn all PCT into numeric
# data_demo_1$LUNCH_COUNT_FREE_REDUCED_PCT <- as.numeric(data_demo_1$LUNCH_COUNT_FREE_REDUCED_PCT)
# data_demo_1$ENROLLMENT_ASIAN_PCT <- as.numeric(data_demo_1$ENROLLMENT_ASIAN_PCT)
# data_demo_1$ENROLLMENT_BLACK_PCT <- as.numeric(data_demo_1$ENROLLMENT_BLACK_PCT)
# data_demo_1$ENROLLMENT_WHITE_PCT <- as.numeric(data_demo_1$ENROLLMENT_WHITE_PCT)
# data_demo_1$ENROLLMENT_INDIAN_PCT <- as.numeric(data_demo_1$ENROLLMENT_INDIAN_PCT)
# data_demo_1$ENROLLMENT_HISPANIC_PCT <- as.numeric(data_demo_1$ENROLLMENT_HISPANIC_PCT)
#   
# # Turn all PCT into hundredths
# data_demo_1$LUNCH_COUNT_FREE_REDUCED_PCT <- data_demo_1$LUNCH_COUNT_FREE_REDUCED_PCT / 100
# data_demo_1$ENROLLMENT_ASIAN_PCT <- data_demo_1$ENROLLMENT_ASIAN_PCT / 100
# data_demo_1$ENROLLMENT_BLACK_PCT <- data_demo_1$ENROLLMENT_BLACK_PCT / 100
# data_demo_1$ENROLLMENT_WHITE_PCT <- data_demo_1$ENROLLMENT_WHITE_PCT / 100
# data_demo_1$ENROLLMENT_INDIAN_PCT <- data_demo_1$ENROLLMENT_INDIAN_PCT / 100
# data_demo_1$ENROLLMENT_HISPANIC_PCT <- data_demo_1$ENROLLMENT_HISPANIC_PCT / 100
# 
# # Turn NAs into -99
# data_demo_1[is.na(data_demo_1)] <- -99
# 
# # Get rid of special characters
# data_demo_1[data_demo_1 == ','] <- " "
# data_demo_1[data_demo_1 == '~'] <- "-"
# 
# # Save as .csv file
# write.csv(data_demo_1,"cleaned_data/mo_enroll_1991_2014.csv", row.names = FALSE)
# 
# 
# 

# Performance V2 --------------------------------------------------------------------------------------------------------
# get only the columns we need
data_perf <- data_perf %>% select(c("COUNTY_DISTRICT_SCHOOL_CODE", "COUNTY_DISTRICT", "DISTRICT_NAME", "SCHOOL_CODE_0001", "SCHOOL_NAME", 
                                    "YEAR", "CONTENT_AREA", 
                                   "LEVEL_NOT_DETERMINED", "LEVEL_NOT_DETERMINED_PCT", "BELOW_BASIC", "BELOW_BASIC_PCT", "BASIC", "BASIC_PCT",
                                   "PROFICIENT", "PROFICIENT_PCT", "ADVANCED", "ADVANCED_PCT", "BOTTOM_TWO_LEVELS", "BOTTOM_TWO_LEVELS_PCT",
                                   "TOP_TWO_LEVELS", "TOP_TWO_LEVELS_PCT"))
backup_perf <- data_perf
data_perf <- backup_perf

# apparently make sure each non-pct column in numeric instead of integer
data_perf$LEVEL_NOT_DETERMINED <- as.numeric(data_perf$LEVEL_NOT_DETERMINED)
data_perf$BELOW_BASIC <- as.numeric(data_perf$BELOW_BASIC)
data_perf$BASIC <- as.numeric(data_perf$BASIC)
data_perf$PROFICIENT <- as.numeric(data_perf$PROFICIENT)
data_perf$ADVANCED <- as.numeric(data_perf$ADVANCED)
data_perf$BOTTOM_TWO_LEVELS <- as.numeric(data_perf$BOTTOM_TWO_LEVELS)
data_perf$TOP_TWO_LEVELS <- as.numeric(data_perf$TOP_TWO_LEVELS)

# sum the rows into one for each school
data_perf <- data_perf %>% 
  group_by_("COUNTY_DISTRICT_SCHOOL_CODE","COUNTY_DISTRICT","DISTRICT_NAME","SCHOOL_CODE_0001","SCHOOL_NAME", "YEAR", "CONTENT_AREA") %>% 
  summarise(sum(LEVEL_NOT_DETERMINED, na.rm = TRUE), mean(LEVEL_NOT_DETERMINED_PCT, na.rm = TRUE), sum(BELOW_BASIC, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            sum(BASIC, na.rm = TRUE), mean(BASIC_PCT, na.rm = TRUE), sum(PROFICIENT, na.rm = TRUE), mean(PROFICIENT_PCT, na.rm = TRUE),
            sum(ADVANCED, na.rm = TRUE), mean(ADVANCED_PCT, na.rm = TRUE), sum(BOTTOM_TWO_LEVELS, na.rm = TRUE), mean(BOTTOM_TWO_LEVELS_PCT, na.rm = TRUE),
            sum(TOP_TWO_LEVELS, na.rm = TRUE), mean(TOP_TWO_LEVELS_PCT, na.rm = TRUE))
# rename the columns
colnames(data_perf) <- c("COUNTY_DISTRICT_SCHOOL_CODE", "COUNTY_DISTRICT", "DISTRICT_NAME", "SCHOOL_CODE_0001", "SCHOOL_NAME", 
                         "YEAR", "CONTENT_AREA", 
                         "LEVEL_NOT_DETERMINED", "LEVEL_NOT_DETERMINED_PCT", "BELOW_BASIC", "BELOW_BASIC_PCT", "BASIC", "BASIC_PCT",
                         "PROFICIENT", "PROFICIENT_PCT", "ADVANCED", "ADVANCED_PCT", "BOTTOM_TWO_LEVELS", "BOTTOM_TWO_LEVELS_PCT",
                         "TOP_TWO_LEVELS", "TOP_TWO_LEVELS_PCT")

# round the pct columns
data_perf$LEVEL_NOT_DETERMINED_PCT <- round(data_perf$LEVEL_NOT_DETERMINED_PCT, digits = 1)
data_perf$BELOW_BASIC_PCT <- round(data_perf$BELOW_BASIC_PCT, digits = 1)
data_perf$BASIC_PCT <- round(data_perf$BASIC_PCT, digits = 1)
data_perf$PROFICIENT_PCT <- round(data_perf$PROFICIENT_PCT, digits = 1)
data_perf$ADVANCED_PCT <- round(data_perf$ADVANCED_PCT, digits = 1)
data_perf$BOTTOM_TWO_LEVELS_PCT <- round(data_perf$BOTTOM_TWO_LEVELS_PCT, digits = 1)
data_perf$TOP_TWO_LEVELS_PCT <- round(data_perf$TOP_TWO_LEVELS_PCT, digits = 1)

# separate into 2 datasets (1 for ELA, 1 for Math), sum the rows into one for each school, then recombine
data_perf_ela <- data_perf %>% filter(CONTENT_AREA == "Eng. Language Arts")
data_perf_ela$CONTENT_AREA <- NULL
data_perf_ela <- add_code(data_perf_ela, "ELA")

data_perf_math <- data_perf %>% filter(CONTENT_AREA == "Mathematics")
data_perf_math$CONTENT_AREA <- NULL
data_perf_math <- add_code(data_perf_math, "MATH")

data_perf <- full_join (data_perf_ela, data_perf_math)

# last clean
data_perf <- last_clean(data_perf)

# separate by year
data_perf_10 <- data_perf %>% filter(YEAR == 2010)
data_perf_11 <- data_perf %>% filter(YEAR == 2011)
data_perf_12 <- data_perf %>% filter(YEAR == 2012)
data_perf_13 <- data_perf %>% filter(YEAR == 2013)
data_perf_14 <- data_perf %>% filter(YEAR == 2014)

# write .csv file
write.csv(data_perf, "cleaned_data/mo_perf_2010_14.csv", row.names = FALSE)
write.csv(data_perf_10, "cleaned_data/mo_perf_2010.csv", row.names = FALSE)
write.csv(data_perf_11, "cleaned_data/mo_perf_2011.csv", row.names = FALSE)
write.csv(data_perf_12, "cleaned_data/mo_perf_2012.csv", row.names = FALSE)
write.csv(data_perf_13, "cleaned_data/mo_perf_2013.csv", row.names = FALSE)
write.csv(data_perf_14, "cleaned_data/mo_perf_2014.csv", row.names = FALSE)

# Demographics V2 --------------------------------------------------------------------------------------------------------
# turn * and NULL into NA
data_demo[data_demo == "*"] <- NA
data_demo[data_demo == "NULL"] <- NA

# get rid of unwanted columns
data_demo$JANUARY_MEMBERSHIP <- NULL
data_demo$ENROLLMENT_GRADE_PK_ELL_LEP <- NULL
data_demo$ENROLLMENT_GRADE_PK_ELL_LEP_PCT <- NULL
data_demo$IEP_SCHOOLAGE_CHILDCOUNT <- NULL
data_demo$IEP_INCIDENCE_RATE <- NULL

# last clean
data_demo <- last_clean(data_demo)

# separate by year
data_demo_01 <- data_demo %>% filter(YEAR == 2001)
data_demo_02 <- data_demo %>% filter(YEAR == 2002)
data_demo_03 <- data_demo %>% filter(YEAR == 2003)
data_demo_04 <- data_demo %>% filter(YEAR == 2004)
data_demo_05 <- data_demo %>% filter(YEAR == 2005)
data_demo_06 <- data_demo %>% filter(YEAR == 2006)
data_demo_07 <- data_demo %>% filter(YEAR == 2007)
data_demo_08 <- data_demo %>% filter(YEAR == 2008)
data_demo_09 <- data_demo %>% filter(YEAR == 2009)
data_demo_10 <- data_demo %>% filter(YEAR == 2010)
data_demo_11 <- data_demo %>% filter(YEAR == 2011)
data_demo_12 <- data_demo %>% filter(YEAR == 2012)
data_demo_13 <- data_demo %>% filter(YEAR == 2013)
data_demo_14 <- data_demo %>% filter(YEAR == 2014)
data_demo_15 <- data_demo %>% filter(YEAR == 2015)
data_demo_16 <- data_demo %>% filter(YEAR == 2016)

# write .csv file
write.csv(data_demo, "cleaned_data/mo_enroll_2001_16.csv", row.names = FALSE)
write.csv(data_demo_01, "cleaned_data/mo_enroll_2001.csv", row.names = FALSE)
write.csv(data_demo_02, "cleaned_data/mo_enroll_2002.csv", row.names = FALSE)
write.csv(data_demo_03, "cleaned_data/mo_enroll_2003.csv", row.names = FALSE)
write.csv(data_demo_04, "cleaned_data/mo_enroll_2004.csv", row.names = FALSE)
write.csv(data_demo_05, "cleaned_data/mo_enroll_2005.csv", row.names = FALSE)
write.csv(data_demo_06, "cleaned_data/mo_enroll_2006.csv", row.names = FALSE)
write.csv(data_demo_07, "cleaned_data/mo_enroll_2007.csv", row.names = FALSE)
write.csv(data_demo_08, "cleaned_data/mo_enroll_2008.csv", row.names = FALSE)
write.csv(data_demo_09, "cleaned_data/mo_enroll_2009.csv", row.names = FALSE)
write.csv(data_demo_01, "cleaned_data/mo_enroll_2010.csv", row.names = FALSE)
write.csv(data_demo_11, "cleaned_data/mo_enroll_2011.csv", row.names = FALSE)
write.csv(data_demo_13, "cleaned_data/mo_enroll_2013.csv", row.names = FALSE)
write.csv(data_demo_14, "cleaned_data/mo_enroll_2014.csv", row.names = FALSE)
write.csv(data_demo_15, "cleaned_data/mo_enroll_2015.csv", row.names = FALSE)
write.csv(data_demo_16, "cleaned_data/mo_enroll_2016.csv", row.names = FALSE)

# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/mo_perf_perf_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/mo_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/mo_perf_clean.rds")




