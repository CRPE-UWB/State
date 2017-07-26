#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Oklahoma's Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Oklahoma 
# Created by: Kevin Cha on 07-24-17
# Updated by: Kevin Cha on 07-25-17
# Data from: 
#       Demographic: http://sde.ok.gov/sde/documents/2014-02-13/state-student-public-enrollment-2013
#           -Year: 2007-2008 => 2016-2017     File: School Site Totals w/ Ethnicity and Gender
#       Performance: http://sde.ok.gov/sde/accountability-resources
#           -Used: 2016 OSTP Assessment Participation and Performance Data - Schools + 2015 OSTP Assessment Participation and Performance Data - Schools
#                    http://sde.ok.gov/sde/accountability-archive
#           -Used: 2014 OSTP Assessment Participation and Performance Data - Schools (zip file)
# Codebook: 
#       Performance: In the Raw Data 
# Link to Github: https://github.com/CRPE-UWB/State

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_ok_clean")

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(gdata)
library(readr)
library(readxl)

# List-Colnames --------------------------------------------------------------------------------------------------------
demo_list_0607 <- c("COUNTY", "DISTRICT", "SCHOOL_NAME", "COUNTY_DISTRICT_CODE", "SITE_CODE",
               "BLACK_M", "BLACK_F", "AMINDIAN_M", "AMINDIAN_F", "HISPANIC_M", "HISPANIC_F", 
               "ASIAN_M", "ASIAN_F", "WHITE_M", "WHITE_F", "TOTAL_ENROLL")
demo_list_08 <- c("COUNTY", "DISTRICT", "SCHOOL_NAME", "COUNTY_DISTRICT_CODE", "SITE_CODE",
                    "BLACK_M", "BLACK_F", "AMINDIAN_M", "AMINDIAN_F", "HISPANIC_M", "HISPANIC_F", 
                    "ASIAN_M", "ASIAN_F","PACISLAND_M", "PACISLAND_F", "WHITE_M", "WHITE_F", "TOTAL_ENROLL")
demo_list_0910 <- c("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME",  
                  "BLACK_M", "BLACK_F", "AMINDIAN_M", "AMINDIAN_F", "HISPANIC_M", "HISPANIC_F", 
                  "ASIAN_M", "ASIAN_F","PACISLAND_M", "PACISLAND_F", "WHITE_M", "WHITE_F", "TOTAL_ENROLL")
demo_list_1117 <- c("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME",  
                  "HISPANIC_M", "HISPANIC_F", "AMINDIAN_M", "AMINDIAN_F", "ASIAN_M", "ASIAN_F", 
                  "BLACK_M", "BLACK_F", "PACISLAND_M", "PACISLAND_F", "WHITE_M", "WHITE_F", "TWOORMORE_M", "TWOORMORE_F",
                  "TOTAL_ENROLL")

# List-Order --------------------------------------------------------------------------------------------------------
demo_order_0607 <- c("SCHOOL_NAME", "COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "YEAR", "TOTAL_ENROLL",
                     "MALE", "MALE_PCT",
                     "FEMALE", "FEMALE_PCT",
                     "BLACK", "BLACK_PCT", "BLACK_M", "BLACK_M_PCT", "BLACK_F", "BLACK_F_PCT",
                     "AMINDIAN", "AMINDIAN_PCT", "AMINDIAN_M", "AMINDIAN_M_PCT", "AMINDIAN_F", "AMINDIAN_F_PCT",
                     "HISPANIC", "HISPANIC_PCT", "HISPANIC_M", "HISPANIC_M_PCT", "HISPANIC_F", "HISPANIC_F_PCT",
                     "ASIAN", "ASIAN_PCT", "ASIAN_M", "ASIAN_M_PCT", "ASIAN_F", "ASIAN_F_PCT",
                     "WHITE", "WHITE_PCT", "WHITE_M", "WHITE_M_PCT", "WHITE_F", "WHITE_F_PCT"
                    )
demo_order_080910 <- c("SCHOOL_NAME", "COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "YEAR", "TOTAL_ENROLL",
                     "MALE", "MALE_PCT",
                     "FEMALE", "FEMALE_PCT",
                     "BLACK", "BLACK_PCT", "BLACK_M", "BLACK_M_PCT", "BLACK_F", "BLACK_F_PCT",
                     "AMINDIAN", "AMINDIAN_PCT", "AMINDIAN_M", "AMINDIAN_M_PCT", "AMINDIAN_F", "AMINDIAN_F_PCT",
                     "HISPANIC", "HISPANIC_PCT", "HISPANIC_M", "HISPANIC_M_PCT", "HISPANIC_F", "HISPANIC_F_PCT",
                     "ASIAN", "ASIAN_PCT", "ASIAN_M", "ASIAN_M_PCT", "ASIAN_F", "ASIAN_F_PCT",
                     "PACISLAND", "PACISLAND_PCT", "PACISLAND_M", "PACISLAND_M_PCT", "PACISLAND_F", "PACISLAND_F_PCT",
                     "WHITE", "WHITE_PCT", "WHITE_M", "WHITE_M_PCT", "WHITE_F", "WHITE_F_PCT"
                    )
demo_order_1117 <- c("SCHOOL_NAME", "COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "YEAR", "TOTAL_ENROLL",
                       "MALE", "MALE_PCT",
                       "FEMALE", "FEMALE_PCT",
                       "BLACK", "BLACK_PCT", "BLACK_M", "BLACK_M_PCT", "BLACK_F", "BLACK_F_PCT",
                       "AMINDIAN", "AMINDIAN_PCT", "AMINDIAN_M", "AMINDIAN_M_PCT", "AMINDIAN_F", "AMINDIAN_F_PCT",
                       "HISPANIC", "HISPANIC_PCT", "HISPANIC_M", "HISPANIC_M_PCT", "HISPANIC_F", "HISPANIC_F_PCT",
                       "ASIAN", "ASIAN_PCT", "ASIAN_M", "ASIAN_M_PCT", "ASIAN_F", "ASIAN_F_PCT",
                       "PACISLAND", "PACISLAND_PCT", "PACISLAND_M", "PACISLAND_M_PCT", "PACISLAND_F", "PACISLAND_F_PCT",
                       "WHITE", "WHITE_PCT", "WHITE_M", "WHITE_M_PCT", "WHITE_F", "WHITE_F_PCT", 
                       "TWOORMORE", "TWOORMORE_PCT", "TWOORMORE_M", "TWOORMORE_M_PCT", "TWOORMORE_F", "TWOORMORE_F_PCT"
                    )


perf_list <- c("YEAR", "COUNTY_CODE", "DISTRICT_CODE", "SITE_CODE", "FULL_CODE", "COUNTY_NAME", 
               "DISTRICT_NAME", "SCHOOL_NAME", "GRADE_SPAN", "TITLE1", "SUBJECT", "GRADE",
               "TOTAL_STUDENTS", "PARTICIPATION_RATE", "ELL_1STYR_EXEMPTIONS", 
               "UNSATISF_RATE", "LIMITED_KNOWLEDGE_RATE", "PROF_RATE", "ADV_RATE")

perf_order <- c("YEAR", "COUNTY_CODE", "DISTRICT_CODE", "SITE_CODE", "FULL_CODE", "COUNTY_NAME", 
               "DISTRICT_NAME", "SCHOOL_NAME", "GRADE_SPAN", "TITLE1", "GRADE", 
               "MATH_TOTAL_STUDENTS", "READING_TOTAL_STUDENTS", "MATH_PARTICIPATION_RATE", "READING_PARTICIPATION_RATE", 
               "MATH_ELL_1STYR_EXEMPTIONS", "READING_ELL_1STYR_EXEMPTIONS", "MATH_UNSATISF_RATE", "READING_UNSATISF_RATE",
               "MATH_LIMITED_KNOWLEDGE_RATE", "READING_LIMITED_KNOWLEDGE_RATE", "MATH_PROF_RATE", "READING_PROF_RATE",
               "MATH_ADV_RATE", "READING_ADV_RATE"
              )
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
dec_num_only <- function(x) {
  require(stringr)
  
  # sets to look for decimal number
  regexp <- "[[:digit:]]+\\.*[[:digit:]]*"
  x <- str_extract(x, regexp)
  return(x)
}
# Add Code to Front of Col Names
add_code <- function(df, code) {
  for (i in 12:ncol(df)) {
    colnames(df)[i] <- paste(code, colnames(df)[i], sep="_")
    next
  }
  return(df)
}

last_clean <- function(df) {
  # turn -1 and . into NAs
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  
  # make sure no special characters
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  return(df)
}

add_gender_race_col_0607 <- function(df) {
  # add the races together
  df$BLACK <- df$BLACK_M + df$BLACK_F
  df$AMINDIAN <- df$AMINDIAN_M + df$AMINDIAN_F 
  df$HISPANIC <- df$HISPANIC_M + df$HISPANIC_F 
  df$ASIAN <- df$ASIAN_M + df$ASIAN_F 
  df$WHITE <- df$WHITE_M + df$WHITE_F 
  
  # add the gender together
  df$MALE <- df$BLACK_M + df$AMINDIAN_M + df$HISPANIC_M + df$ASIAN_M + df$WHITE_M
  df$FEMALE <- df$BLACK_F + df$AMINDIAN_F + df$HISPANIC_F + df$ASIAN_F + df$WHITE_F
  
  # create pct columns
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_M_PCT <- round(df$BLACK_M / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_F_PCT <- round(df$BLACK_F / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_M_PCT <- round(df$AMINDIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_F_PCT <- round(df$AMINDIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_M_PCT <- round(df$HISPANIC_M / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_F_PCT <- round(df$HISPANIC_F / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_M_PCT <- round(df$ASIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_F_PCT <- round(df$ASIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_M_PCT <- round(df$WHITE_M / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_F_PCT <- round(df$WHITE_F / df$TOTAL_ENROLL, digits = 3)
  df$MALE_PCT <- round(df$MALE / df$TOTAL_ENROLL, digits = 3)
  df$FEMALE_PCT <- round(df$FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  return(df)
}
add_gender_race_col_080910 <- function(df) {
  # add the races together
  df$BLACK <- df$BLACK_M + df$BLACK_F
  df$AMINDIAN <- df$AMINDIAN_M + df$AMINDIAN_F 
  df$HISPANIC <- df$HISPANIC_M + df$HISPANIC_F 
  df$ASIAN <- df$ASIAN_M + df$ASIAN_F 
  df$PACISLAND <- df$PACISLAND_M + df$PACISLAND_F
  df$WHITE <- df$WHITE_M + df$WHITE_F 
  
  # add the gender together
  df$MALE <- df$BLACK_M + df$AMINDIAN_M + df$HISPANIC_M + df$ASIAN_M + df$PACISLAND_M + df$WHITE_M
  df$FEMALE <- df$BLACK_F + df$AMINDIAN_F + df$HISPANIC_F + df$ASIAN_F + df$PACISLAND_F + df$WHITE_F
  
  # create pct columns
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_M_PCT <- round(df$BLACK_M / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_F_PCT <- round(df$BLACK_F / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_M_PCT <- round(df$AMINDIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_F_PCT <- round(df$AMINDIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_M_PCT <- round(df$HISPANIC_M / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_F_PCT <- round(df$HISPANIC_F / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_M_PCT <- round(df$ASIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_F_PCT <- round(df$ASIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_M_PCT <- round(df$PACISLAND_M / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_F_PCT <- round(df$PACISLAND_F / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_M_PCT <- round(df$WHITE_M / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_F_PCT <- round(df$WHITE_F / df$TOTAL_ENROLL, digits = 3)
  df$MALE_PCT <- round(df$MALE / df$TOTAL_ENROLL, digits = 3)
  df$FEMALE_PCT <- round(df$FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  return(df)
}
add_gender_race_col_1117 <- function(df) {
  # add the races together
  df$BLACK <- df$BLACK_M + df$BLACK_F
  df$AMINDIAN <- df$AMINDIAN_M + df$AMINDIAN_F 
  df$HISPANIC <- df$HISPANIC_M + df$HISPANIC_F 
  df$ASIAN <- df$ASIAN_M + df$ASIAN_F 
  df$PACISLAND <- df$PACISLAND_M + df$PACISLAND_F
  df$WHITE <- df$WHITE_M + df$WHITE_F 
  df$TWOORMORE <- df$TWOORMORE_M + df$TWOORMORE_F
  
  # add the gender together
  df$MALE <- df$BLACK_M + df$AMINDIAN_M + df$HISPANIC_M + df$ASIAN_M + df$PACISLAND_M + df$WHITE_M + df$TWOORMORE_M
  df$FEMALE <- df$BLACK_F + df$AMINDIAN_F + df$HISPANIC_F + df$ASIAN_F + df$PACISLAND_F + df$WHITE_F + df$TWOORMORE_F
  
  # create pct columns
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_M_PCT <- round(df$BLACK_M / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_F_PCT <- round(df$BLACK_F / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_M_PCT <- round(df$AMINDIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$AMINDIAN_F_PCT <- round(df$AMINDIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_M_PCT <- round(df$HISPANIC_M / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_F_PCT <- round(df$HISPANIC_F / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_M_PCT <- round(df$ASIAN_M / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_F_PCT <- round(df$ASIAN_F / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_M_PCT <- round(df$PACISLAND_M / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_F_PCT <- round(df$PACISLAND_F / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_M_PCT <- round(df$WHITE_M / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_F_PCT <- round(df$WHITE_F / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_PCT <- round(df$TWOORMORE / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_M_PCT <- round(df$TWOORMORE_M / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_F_PCT <- round(df$TWOORMORE_F / df$TOTAL_ENROLL, digits = 3)
  df$MALE_PCT <- round(df$MALE / df$TOTAL_ENROLL, digits = 3)
  df$FEMALE_PCT <- round(df$FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  return(df)
}

perf_clean <- function (df) {
  # get rid of unwanted columns
  df$EducationAgencyType <- NULL
  df$GradeLevelLow <- NULL
  df$GradeLevelHigh <- NULL
  df$ReportSubgroup <- NULL
  
  # take only the columns that deal with ALL grades
  df <- df %>% 
    filter(GradeLevel == "All")
  
  # turn YES and NO to 1 and 0
  df[df == 'YES'] <- 1
  df[df == 'NO'] <- 0
  
  # change column names
  colnames(df) <- perf_list
  
  # make sure COUNTY_CODE is 2 digits
  df$COUNTY_CODE <- sprintf("%02d", df$COUNTY_CODE)
  
  # get rid of any non-numeric numbers
  df$TOTAL_STUDENTS <- dec_num_only(df$TOTAL_STUDENTS)
  df$PARTICIPATION_RATE <- dec_num_only(df$PARTICIPATION_RATE)
  df$ELL_1STYR_EXEMPTIONS <- dec_num_only(df$ELL_1STYR_EXEMPTIONS)
  df$UNSATISF_RATE <- dec_num_only(df$UNSATISF_RATE)
  df$LIMITED_KNOWLEDGE_RATE <- dec_num_only(df$LIMITED_KNOWLEDGE_RATE)
  df$PROF_RATE <- dec_num_only(df$PROF_RATE)
  df$ADV_RATE <- dec_num_only(df$ADV_RATE)
  
  # change all 95 to .95
  df$PARTICIPATION_RATE <- gsub("95","0.95",df$PARTICIPATION_RATE) 
  df$PARTICIPATION_RATE <- gsub("0.0.95","0.95",df$PARTICIPATION_RATE) 
  
  # make sure all decimals are to hundredths
  df$PARTICIPATION_RATE <- sprintf("%0.4s", df$PARTICIPATION_RATE)
  df$UNSATISF_RATE <- sprintf("%0.4s", df$UNSATISF_RATE)
  df$LIMITED_KNOWLEDGE_RATE <- sprintf("%0.4s", df$LIMITED_KNOWLEDGE_RATE)
  df$PROF_RATE <- sprintf("%0.4s", df$PROF_RATE)
  df$ADV_RATE <- sprintf("%0.4s", df$ADV_RATE)
  
  # change all 7.00 to 0.70
  df$PARTICIPATION_RATE <- gsub("7.00","0.70",df$PARTICIPATION_RATE) 
  df$UNSATISF_RATE <- gsub("7.00","0.70", df$UNSATISF_RATE)
  df$LIMITED_KNOWLEDGE_RATE <- gsub("7.00","0.70", df$LIMITED_KNOWLEDGE_RATE)
  df$PROF_RATE <- gsub("7.00","0.70", df$PROF_RATE)
  df$ADV_RATE <- gsub("7.00","0.70", df$ADV_RATE)
  
  # turn N/A and *** and NA to NA
  df[df == 'N/A'] <- NA
  df[df == 'NA'] <- NA
  df[df == '***'] <- NA
  
  # make all numeric
  df$TOTAL_STUDENTS <- as.numeric(df$TOTAL_STUDENTS)
  df$PARTICIPATION_RATE <- as.numeric(df$PARTICIPATION_RATE)
  df$ELL_1STYR_EXEMPTIONS <- as.numeric(df$ELL_1STYR_EXEMPTIONS)
  df$UNSATISF_RATE <- as.numeric(df$UNSATISF_RATE)
  df$LIMITED_KNOWLEDGE_RATE <- as.numeric(df$LIMITED_KNOWLEDGE_RATE)
  df$PROF_RATE <- as.numeric(df$PROF_RATE)
  df$ADV_RATE <- as.numeric(df$ADV_RATE)
  
  # combine the schools together
  df <- df %>% 
    group_by_("YEAR", "COUNTY_CODE", "DISTRICT_CODE", "SITE_CODE", "FULL_CODE", "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", "GRADE_SPAN",
              "TITLE1", "SUBJECT", "GRADE") %>% 
    summarise(sum(TOTAL_STUDENTS, na.rm = TRUE), mean(PARTICIPATION_RATE, na.rm = TRUE), sum(ELL_1STYR_EXEMPTIONS, na.rm = TRUE), 
              mean(UNSATISF_RATE, na.rm = TRUE), mean(LIMITED_KNOWLEDGE_RATE, na.rm = TRUE), mean(PROF_RATE, na.rm = TRUE), mean(ADV_RATE, na.rm = TRUE))
  
  # change column names (again)
  colnames(df) <- perf_list
  # turn NaN to NA
  df[df == 'NaN'] <- NA
  
  # rond them to hundredths
  df$PARTICIPATION_RATE <- round(df$PARTICIPATION_RATE, digits = 2) 
  df$UNSATISF_RATE <- round(df$UNSATISF_RATE, digits = 2)
  df$LIMITED_KNOWLEDGE_RATE <- round(df$LIMITED_KNOWLEDGE_RATE, digits = 2)
  df$PROF_RATE <- round(df$PROF_RATE, digits = 2)
  df$ADV_RATE <- round(df$ADV_RATE, digits = 2)
  
  # separate into Mathematics table and Reading/Language Arts table
  df_math <- df %>% filter(SUBJECT == 'Mathematics')
  # get rid of SUBJECT column
  df_math$SUBJECT <- NULL
  # add MATH to front of colnames
  df_math <- add_code(df_math, "MATH")
  df_reading<- df %>% filter(SUBJECT == 'Reading/Language Arts')
  # get rid of SUBJECT column
  df_reading$SUBJECT <- NULL
  # add MATH to front of colnames
  df_reading <- add_code(df_reading, "READING")
  
  # combine them back together
  df <- merge(df_math, df_reading)
  
  # reorder the columns
  df <- setcolorder(df, perf_order)
  
  # last part of cleaning
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}

# Performance --------------------------------------------------------------------------------------------------------

# 2013-14 -----
# read in xls file
data_perf_14 <- read_excel("data/2014 OSTP Assessment Participation and Performance - Schools.xlsx", sheet = 1)

# clean everything
data_perf_14 <- perf_clean(data_perf_14)

# Save as .csv file
write.csv(data_perf_14,"cleaned_data/data_perf_2014.csv, row.names = FALSE")

# 2014-15 -----
# read in xls file
data_perf_15 <- read_excel("data/2015 OSTP Assessment Participation and Performance - Schools.xlsx", sheet = 1)

# clean everything
data_perf_15 <- perf_clean(data_perf_15)

# Merge them
data_perf <- full_join(data_perf_14, data_perf_15)

# Save as .csv file
write.csv(data_perf_15,"cleaned_data/data_perf_2015.csv, row.names = FALSE")

# 2015-16 -----
# read in xls file
data_perf_16 <- read_excel("data/OSTP Participation and Performance - Schools 2016.xlsx", sheet = 1)

# clean everything
data_perf_16 <- perf_clean(data_perf_16)

# Merge them
data_perf <- full_join(data_perf, data_perf_16)

# Save as .csv file
write.csv(data_perf_16,"cleaned_data/data_perf_2016.csv, row.names = FALSE")

# Finish -----
# Save as .csv file
write.csv(data_perf,"cleaned_data/data_perf_2014_16.csv, row.names = FALSE")

# Demographics --------------------------------------------------------------------------------------------------------

# 2005-06 -----
# read in xls file
data_demo_06 <- gdata::read.xls("data/GRADE_byGradeEthGen_BySite_2A_0506_Public.xls", sheet = 1, header = TRUE)

# get rid of unwanted columns
data_demo_06$GRADE <- NULL

# change column names
colnames(data_demo_06) <- demo_list_0607

# get rid of last row (total)
data_demo_06 <- data_demo_06[-nrow(data_demo_06),] 

# change column to right type
data_demo_06$COUNTY <- as.character(data_demo_06$COUNTY)
data_demo_06$DISTRICT <- as.character(data_demo_06$DISTRICT)
data_demo_06$SCHOOL_NAME <- as.character(data_demo_06$SCHOOL_NAME)
data_demo_06$COUNTY_DISTRICT_CODE <- as.character(data_demo_06$COUNTY_DISTRICT_CODE)
data_demo_06$SITE_CODE <- as.numeric(data_demo_06$SITE_CODE)
data_demo_06$BLACK_M <- as.numeric(levels(data_demo_06$BLACK_M))[as.integer(data_demo_06$BLACK_M)]
data_demo_06$BLACK_F <- as.numeric(levels(data_demo_06$BLACK_F))[as.integer(data_demo_06$BLACK_F)]
data_demo_06$AMINDIAN_M <- as.numeric(levels(data_demo_06$AMINDIAN_M))[as.integer(data_demo_06$AMINDIAN_M)]
data_demo_06$AMINDIAN_F <- as.numeric(levels(data_demo_06$AMINDIAN_F))[as.integer(data_demo_06$AMINDIAN_F)]
data_demo_06$HISPANIC_M <- as.numeric(levels(data_demo_06$HISPANIC_M))[as.integer(data_demo_06$HISPANIC_M)]
data_demo_06$HISPANIC_F <- as.numeric(levels(data_demo_06$HISPANIC_F))[as.integer(data_demo_06$HISPANIC_F)]
data_demo_06$ASIAN_M <- as.numeric(levels(data_demo_06$ASIAN_M))[as.integer(data_demo_06$ASIAN_M)]
data_demo_06$ASIAN_F <- as.numeric(levels(data_demo_06$ASIAN_F))[as.integer(data_demo_06$ASIAN_F)]
data_demo_06$WHITE_M <- as.numeric(levels(data_demo_06$WHITE_M))[as.integer(data_demo_06$WHITE_M)]
data_demo_06$WHITE_F <- as.numeric(levels(data_demo_06$WHITE_F))[as.integer(data_demo_06$WHITE_F)]
data_demo_06$TOTAL_ENROLL <- as.numeric(levels(data_demo_06$TOTAL_ENROLL))[as.integer(data_demo_06$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_06 <- data_demo_06 %>% 
  group_by_("COUNTY", "DISTRICT", "SCHOOL_NAME", "COUNTY_DISTRICT_CODE", "SITE_CODE") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(WHITE_M), sum(WHITE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_06) <- demo_list_0607

# add race and gender and pct columns
data_demo_06 <- add_gender_race_col_0607(data_demo_06)

# add year 
data_demo_06$YEAR <- 2006

# change order of columns
data_demo_06 <- setcolorder(data_demo_06, demo_order_0607)

# final clean
data_demo_06 <- last_clean(data_demo_06)

# Save as .csv file
write.csv(data_demo_06,"cleaned_data/data_enroll_2006.csv", row.names = FALSE)


# 2006-07 -----
# read in xls file
data_demo_07 <- gdata::read.xls("data/GRADE_byGradeEthGen_BySite_2A_0607_Public.xls", header = TRUE)

# get rid of unwanted columns
data_demo_07$Grade <- NULL

# change column names
colnames(data_demo_07) <- demo_list_0607

# get rid of last row (total)
data_demo_07 <- data_demo_07[-nrow(data_demo_07),] 

# change column to right type
data_demo_07$COUNTY <- as.character(data_demo_07$COUNTY)
data_demo_07$DISTRICT <- as.character(data_demo_07$DISTRICT)
data_demo_07$SCHOOL_NAME <- as.character(data_demo_07$SCHOOL_NAME)
data_demo_07$COUNTY_DISTRICT_CODE <- as.character(data_demo_07$COUNTY_DISTRICT_CODE)
data_demo_07$SITE_CODE <- as.numeric(data_demo_07$SITE_CODE)
data_demo_07$BLACK_M <- as.numeric(levels(data_demo_07$BLACK_M))[as.integer(data_demo_07$BLACK_M)]
data_demo_07$BLACK_F <- as.numeric(levels(data_demo_07$BLACK_F))[as.integer(data_demo_07$BLACK_F)]
data_demo_07$AMINDIAN_M <- as.numeric(levels(data_demo_07$AMINDIAN_M))[as.integer(data_demo_07$AMINDIAN_M)]
data_demo_07$AMINDIAN_F <- as.numeric(levels(data_demo_07$AMINDIAN_F))[as.integer(data_demo_07$AMINDIAN_F)]
data_demo_07$HISPANIC_M <- as.numeric(levels(data_demo_07$HISPANIC_M))[as.integer(data_demo_07$HISPANIC_M)]
data_demo_07$HISPANIC_F <- as.numeric(levels(data_demo_07$HISPANIC_F))[as.integer(data_demo_07$HISPANIC_F)]
data_demo_07$ASIAN_M <- as.numeric(levels(data_demo_07$ASIAN_M))[as.integer(data_demo_07$ASIAN_M)]
data_demo_07$ASIAN_F <- as.numeric(levels(data_demo_07$ASIAN_F))[as.integer(data_demo_07$ASIAN_F)]
data_demo_07$WHITE_M <- as.numeric(levels(data_demo_07$WHITE_M))[as.integer(data_demo_07$WHITE_M)]
data_demo_07$WHITE_F <- as.numeric(levels(data_demo_07$WHITE_F))[as.integer(data_demo_07$WHITE_F)]
data_demo_07$TOTAL_ENROLL <- as.numeric(levels(data_demo_07$TOTAL_ENROLL))[as.integer(data_demo_07$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_07 <- data_demo_07 %>% 
  group_by_("COUNTY", "DISTRICT", "SCHOOL_NAME", "COUNTY_DISTRICT_CODE", "SITE_CODE") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(WHITE_M), sum(WHITE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_07) <- demo_list_0607

# add race and gender and pct columns
data_demo_07 <- add_gender_race_col_0607(data_demo_07)

# add year 
data_demo_07$YEAR <- 2007

# change order of columns
data_demo_07 <- setcolorder(data_demo_07, demo_order_0607)

# final clean
data_demo_07 <- last_clean(data_demo_07)

# Merge them
data_demo <- full_join(data_demo_06, data_demo_07)

# Save as .csv file
write.csv(data_demo_07,"cleaned_data/data_enroll_2007.csv", row.names = FALSE)


# 2007-08 -----
# read in xls file
data_demo_08 <- gdata::read.xls("data/GRADE_byGradeEthGen6-2A_June-17-2008-FY0708_Public.xls", header = TRUE)

# get rid of unwanted columns
data_demo_08$FY <- NULL
data_demo_08$Grades <- NULL
data_demo_08$Site.Level <- NULL
data_demo_08$Grade <- NULL

# change column names
colnames(data_demo_08) <- demo_list_08

# get rid of last row (total)
data_demo_08 <- data_demo_08[-nrow(data_demo_08),] 

# change column to right type
data_demo_08$COUNTY <- as.character(data_demo_08$COUNTY)
data_demo_08$DISTRICT <- as.character(data_demo_08$DISTRICT)
data_demo_08$SCHOOL_NAME <- as.character(data_demo_08$SCHOOL_NAME)
data_demo_08$COUNTY_DISTRICT_CODE <- as.character(data_demo_08$COUNTY_DISTRICT_CODE)
data_demo_08$SITE_CODE <- as.numeric(data_demo_08$SITE_CODE)
data_demo_08$BLACK_M <- as.numeric(levels(data_demo_08$BLACK_M))[as.integer(data_demo_08$BLACK_M)]
data_demo_08$BLACK_F <- as.numeric(levels(data_demo_08$BLACK_F))[as.integer(data_demo_08$BLACK_F)]
data_demo_08$AMINDIAN_M <- as.numeric(levels(data_demo_08$AMINDIAN_M))[as.integer(data_demo_08$AMINDIAN_M)]
data_demo_08$AMINDIAN_F <- as.numeric(levels(data_demo_08$AMINDIAN_F))[as.integer(data_demo_08$AMINDIAN_F)]
data_demo_08$HISPANIC_M <- as.numeric(levels(data_demo_08$HISPANIC_M))[as.integer(data_demo_08$HISPANIC_M)]
data_demo_08$HISPANIC_F <- as.numeric(levels(data_demo_08$HISPANIC_F))[as.integer(data_demo_08$HISPANIC_F)]
data_demo_08$ASIAN_M <- as.numeric(levels(data_demo_08$ASIAN_M))[as.integer(data_demo_08$ASIAN_M)]
data_demo_08$ASIAN_F <- as.numeric(levels(data_demo_08$ASIAN_F))[as.integer(data_demo_08$ASIAN_F)]
data_demo_08$PACISLAND_M <- as.numeric(data_demo_08$PACISLAND_M)
data_demo_08$PACISLAND_F <- as.numeric(data_demo_08$PACISLAND_F)
data_demo_08$WHITE_M <- as.numeric(levels(data_demo_08$WHITE_M))[as.integer(data_demo_08$WHITE_M)]
data_demo_08$WHITE_F <- as.numeric(levels(data_demo_08$WHITE_F))[as.integer(data_demo_08$WHITE_F)]
data_demo_08$TOTAL_ENROLL <- as.numeric(levels(data_demo_08$TOTAL_ENROLL))[as.integer(data_demo_08$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_08 <- data_demo_08 %>% 
  group_by_("COUNTY", "DISTRICT", "SCHOOL_NAME", "COUNTY_DISTRICT_CODE", "SITE_CODE") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_08) <- demo_list_08

# add race and gender and pct columns
data_demo_08 <- add_gender_race_col_080910(data_demo_08)

# add year 
data_demo_08$YEAR <- 2008

# change order of columns
data_demo_08 <- setcolorder(data_demo_08, demo_order_080910)

# final clean
data_demo_08 <- last_clean(data_demo_08)

# Merge them
data_demo <- full_join(data_demo, data_demo_08)

# Save as .csv file
write.csv(data_demo_08,"cleaned_data/data_enroll_2008.csv", row.names = FALSE)


# 2008-09 -----
# read in xls file
data_demo_09 <- gdata::read.xls("data/GG_byGradeBySITE_2A_EthGen6-06-03-09-FY0809_Public.xls", header = TRUE)

# get rid of unwanted columns
data_demo_09$Elementary.District <- NULL
data_demo_09$Grades..Low.High. <- NULL
data_demo_09$Site.Level <- NULL
data_demo_09$Grade <- NULL

# change column names
colnames(data_demo_09) <- demo_list_0910

# get rid of last row (total)
data_demo_09 <- data_demo_09[-nrow(data_demo_09),] 

# change column to right type
data_demo_09$COUNTY <- as.character(data_demo_09$COUNTY)
data_demo_09$DISTRICT <- as.character(data_demo_09$DISTRICT)
data_demo_09$SCHOOL_NAME <- as.character(data_demo_09$SCHOOL_NAME)
data_demo_09$COUNTY_DISTRICT_CODE <- as.character(data_demo_09$COUNTY_DISTRICT_CODE)
data_demo_09$SITE_CODE <- as.numeric(data_demo_09$SITE_CODE)
data_demo_09$BLACK_M <- as.numeric(levels(data_demo_09$BLACK_M))[as.integer(data_demo_09$BLACK_M)]
data_demo_09$BLACK_F <- as.numeric(levels(data_demo_09$BLACK_F))[as.integer(data_demo_09$BLACK_F)]
data_demo_09$AMINDIAN_M <- as.numeric(levels(data_demo_09$AMINDIAN_M))[as.integer(data_demo_09$AMINDIAN_M)]
data_demo_09$AMINDIAN_F <- as.numeric(levels(data_demo_09$AMINDIAN_F))[as.integer(data_demo_09$AMINDIAN_F)]
data_demo_09$HISPANIC_M <- as.numeric(levels(data_demo_09$HISPANIC_M))[as.integer(data_demo_09$HISPANIC_M)]
data_demo_09$HISPANIC_F <- as.numeric(levels(data_demo_09$HISPANIC_F))[as.integer(data_demo_09$HISPANIC_F)]
data_demo_09$ASIAN_M <- as.numeric(levels(data_demo_09$ASIAN_M))[as.integer(data_demo_09$ASIAN_M)]
data_demo_09$ASIAN_F <- as.numeric(levels(data_demo_09$ASIAN_F))[as.integer(data_demo_09$ASIAN_F)]
data_demo_09$PACISLAND_M <- as.numeric(data_demo_09$PACISLAND_M)
data_demo_09$PACISLAND_F <- as.numeric(data_demo_09$PACISLAND_F)
data_demo_09$WHITE_M <- as.numeric(levels(data_demo_09$WHITE_M))[as.integer(data_demo_09$WHITE_M)]
data_demo_09$WHITE_F <- as.numeric(levels(data_demo_09$WHITE_F))[as.integer(data_demo_09$WHITE_F)]
data_demo_09$TOTAL_ENROLL <- as.numeric(levels(data_demo_09$TOTAL_ENROLL))[as.integer(data_demo_09$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_09 <- data_demo_09 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_09) <- demo_list_0910

# add race and gender and pct columns
data_demo_09 <- add_gender_race_col_080910(data_demo_09)

# add year 
data_demo_09$YEAR <- 2009

# change order of columns
data_demo_09 <- setcolorder(data_demo_09, demo_order_080910)

# final clean
data_demo_09 <- last_clean(data_demo_09)

# Merge them
data_demo <- full_join(data_demo, data_demo_09)

# Save as .csv file
write.csv(data_demo_09,"cleaned_data/data_enroll_2009.csv", row.names = FALSE)


# 2009-10 -----
# read in xls file
data_demo_10 <- gdata::read.xls("data/GG_byGradeBySITE_2A_EthGen_2010-07-12-FY0910_Public.xls", header = TRUE)

# get rid of unwanted columns
data_demo_10$Elementary.District <- NULL
data_demo_10$Grades..Low.High. <- NULL
data_demo_10$Site.Level <- NULL
data_demo_10$Grade <- NULL

# change column names
colnames(data_demo_10) <- demo_list_0910

# get rid of last row (total)
data_demo_10 <- data_demo_10[-nrow(data_demo_10),] 

# change column to right type
data_demo_10$COUNTY <- as.character(data_demo_10$COUNTY)
data_demo_10$DISTRICT <- as.character(data_demo_10$DISTRICT)
data_demo_10$SCHOOL_NAME <- as.character(data_demo_10$SCHOOL_NAME)
data_demo_10$COUNTY_DISTRICT_CODE <- as.character(data_demo_10$COUNTY_DISTRICT_CODE)
data_demo_10$SITE_CODE <- as.numeric(data_demo_10$SITE_CODE)
data_demo_10$BLACK_M <- as.numeric(levels(data_demo_10$BLACK_M))[as.integer(data_demo_10$BLACK_M)]
data_demo_10$BLACK_F <- as.numeric(levels(data_demo_10$BLACK_F))[as.integer(data_demo_10$BLACK_F)]
data_demo_10$AMINDIAN_M <- as.numeric(levels(data_demo_10$AMINDIAN_M))[as.integer(data_demo_10$AMINDIAN_M)]
data_demo_10$AMINDIAN_F <- as.numeric(levels(data_demo_10$AMINDIAN_F))[as.integer(data_demo_10$AMINDIAN_F)]
data_demo_10$HISPANIC_M <- as.numeric(levels(data_demo_10$HISPANIC_M))[as.integer(data_demo_10$HISPANIC_M)]
data_demo_10$HISPANIC_F <- as.numeric(levels(data_demo_10$HISPANIC_F))[as.integer(data_demo_10$HISPANIC_F)]
data_demo_10$ASIAN_M <- as.numeric(levels(data_demo_10$ASIAN_M))[as.integer(data_demo_10$ASIAN_M)]
data_demo_10$ASIAN_F <- as.numeric(levels(data_demo_10$ASIAN_F))[as.integer(data_demo_10$ASIAN_F)]
data_demo_10$PACISLAND_M <- as.numeric(data_demo_10$PACISLAND_M)
data_demo_10$PACISLAND_F <- as.numeric(data_demo_10$PACISLAND_F)
data_demo_10$WHITE_M <- as.numeric(levels(data_demo_10$WHITE_M))[as.integer(data_demo_10$WHITE_M)]
data_demo_10$WHITE_F <- as.numeric(levels(data_demo_10$WHITE_F))[as.integer(data_demo_10$WHITE_F)]
data_demo_10$TOTAL_ENROLL <- as.numeric(levels(data_demo_10$TOTAL_ENROLL))[as.integer(data_demo_10$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_10 <- data_demo_10 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_10) <- demo_list_0910

# add race and gender and pct columns
data_demo_10 <- add_gender_race_col_080910(data_demo_10)

# add year 
data_demo_10$YEAR <- 2010

# change order of columns
data_demo_10 <- setcolorder(data_demo_10, demo_order_080910)

# final clean
data_demo_10 <- last_clean(data_demo_10)

# Merge them
data_demo <- full_join(data_demo, data_demo_10)

# Save as .csv file
write.csv(data_demo_10,"cleaned_data/data_enroll_2010.csv", row.names = FALSE)


# 2010-11 -----
# read in xls file
data_demo_11 <- gdata::read.xls("data/StudEnroll_bySITE_2A_FY10-11_EthGen.xls", header = TRUE)

# get rid of unwanted columns
data_demo_11$Elementary.District <- NULL
data_demo_11$Grades..Low.High. <- NULL
data_demo_11$Site.Level <- NULL
data_demo_11$Grade <- NULL

# change column names
colnames(data_demo_11) <- demo_list_1117

# get rid of last row (total)
data_demo_11 <- data_demo_11[-nrow(data_demo_11),] 

# change column to right type
data_demo_11$COUNTY <- as.character(data_demo_11$COUNTY)
data_demo_11$DISTRICT <- as.character(data_demo_11$DISTRICT)
data_demo_11$SCHOOL_NAME <- as.character(data_demo_11$SCHOOL_NAME)
data_demo_11$COUNTY_DISTRICT_CODE <- as.character(data_demo_11$COUNTY_DISTRICT_CODE)
data_demo_11$SITE_CODE <- as.numeric(data_demo_11$SITE_CODE)
data_demo_11$BLACK_M <- as.numeric(levels(data_demo_11$BLACK_M))[as.integer(data_demo_11$BLACK_M)]
data_demo_11$BLACK_F <- as.numeric(levels(data_demo_11$BLACK_F))[as.integer(data_demo_11$BLACK_F)]
data_demo_11$AMINDIAN_M <- as.numeric(levels(data_demo_11$AMINDIAN_M))[as.integer(data_demo_11$AMINDIAN_M)]
data_demo_11$AMINDIAN_F <- as.numeric(levels(data_demo_11$AMINDIAN_F))[as.integer(data_demo_11$AMINDIAN_F)]
data_demo_11$HISPANIC_M <- as.numeric(levels(data_demo_11$HISPANIC_M))[as.integer(data_demo_11$HISPANIC_M)]
data_demo_11$HISPANIC_F <- as.numeric(levels(data_demo_11$HISPANIC_F))[as.integer(data_demo_11$HISPANIC_F)]
data_demo_11$ASIAN_M <- as.numeric(levels(data_demo_11$ASIAN_M))[as.integer(data_demo_11$ASIAN_M)]
data_demo_11$ASIAN_F <- as.numeric(levels(data_demo_11$ASIAN_F))[as.integer(data_demo_11$ASIAN_F)]
data_demo_11$PACISLAND_M <- as.numeric(data_demo_11$PACISLAND_M)
data_demo_11$PACISLAND_F <- as.numeric(data_demo_11$PACISLAND_F)
data_demo_11$WHITE_M <- as.numeric(levels(data_demo_11$WHITE_M))[as.integer(data_demo_11$WHITE_M)]
data_demo_11$WHITE_F <- as.numeric(levels(data_demo_11$WHITE_F))[as.integer(data_demo_11$WHITE_F)]
data_demo_11$TWOORMORE_M <- as.numeric(levels(data_demo_11$TWOORMORE_M))[as.integer(data_demo_11$TWOORMORE_M)]
data_demo_11$TWOORMORE_F <- as.numeric(levels(data_demo_11$TWOORMORE_F))[as.integer(data_demo_11$TWOORMORE_F)]
data_demo_11$TOTAL_ENROLL <- as.numeric(levels(data_demo_11$TOTAL_ENROLL))[as.integer(data_demo_11$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_11 <- data_demo_11 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_11) <- demo_list_1117

# add race and gender and pct columns
data_demo_11 <- add_gender_race_col_1117(data_demo_11)

# add year 
data_demo_11$YEAR <- 2011

# change order of columns
data_demo_11 <- setcolorder(data_demo_11, demo_order_1117)

# final clean
data_demo_11 <- last_clean(data_demo_11)

# Merge them
data_demo <- full_join(data_demo, data_demo_11)

# Save as .csv file
write.csv(data_demo_11,"cleaned_data/data_enroll_2011.csv", row.names = FALSE)


# 2011-12 -----
# read in xls file
data_demo_12 <- gdata::read.xls("data/StudEnroll_bySITE_2A_FY11-12_wEthGen.xls", header = TRUE)

# get rid of unwanted columns
data_demo_12$Elementary.District <- NULL
data_demo_12$Grades..Low.High. <- NULL
data_demo_12$Site.Level <- NULL
data_demo_12$Grade <- NULL

# change column names
colnames(data_demo_12) <- demo_list_1117

# get rid of last row (total)
data_demo_12 <- data_demo_12[-nrow(data_demo_12),] 

# change column to right type
data_demo_12$COUNTY <- as.character(data_demo_12$COUNTY)
data_demo_12$DISTRICT <- as.character(data_demo_12$DISTRICT)
data_demo_12$SCHOOL_NAME <- as.character(data_demo_12$SCHOOL_NAME)
data_demo_12$COUNTY_DISTRICT_CODE <- as.character(data_demo_12$COUNTY_DISTRICT_CODE)
data_demo_12$SITE_CODE <- as.numeric(data_demo_12$SITE_CODE)
data_demo_12$BLACK_M <- as.numeric(levels(data_demo_12$BLACK_M))[as.integer(data_demo_12$BLACK_M)]
data_demo_12$BLACK_F <- as.numeric(levels(data_demo_12$BLACK_F))[as.integer(data_demo_12$BLACK_F)]
data_demo_12$AMINDIAN_M <- as.numeric(levels(data_demo_12$AMINDIAN_M))[as.integer(data_demo_12$AMINDIAN_M)]
data_demo_12$AMINDIAN_F <- as.numeric(levels(data_demo_12$AMINDIAN_F))[as.integer(data_demo_12$AMINDIAN_F)]
data_demo_12$HISPANIC_M <- as.numeric(levels(data_demo_12$HISPANIC_M))[as.integer(data_demo_12$HISPANIC_M)]
data_demo_12$HISPANIC_F <- as.numeric(levels(data_demo_12$HISPANIC_F))[as.integer(data_demo_12$HISPANIC_F)]
data_demo_12$ASIAN_M <- as.numeric(levels(data_demo_12$ASIAN_M))[as.integer(data_demo_12$ASIAN_M)]
data_demo_12$ASIAN_F <- as.numeric(levels(data_demo_12$ASIAN_F))[as.integer(data_demo_12$ASIAN_F)]
data_demo_12$PACISLAND_M <- as.numeric(data_demo_12$PACISLAND_M)
data_demo_12$PACISLAND_F <- as.numeric(data_demo_12$PACISLAND_F)
data_demo_12$WHITE_M <- as.numeric(levels(data_demo_12$WHITE_M))[as.integer(data_demo_12$WHITE_M)]
data_demo_12$WHITE_F <- as.numeric(levels(data_demo_12$WHITE_F))[as.integer(data_demo_12$WHITE_F)]
data_demo_12$TWOORMORE_M <- as.numeric(levels(data_demo_12$TWOORMORE_M))[as.integer(data_demo_12$TWOORMORE_M)]
data_demo_12$TWOORMORE_F <- as.numeric(levels(data_demo_12$TWOORMORE_F))[as.integer(data_demo_12$TWOORMORE_F)]
data_demo_12$TOTAL_ENROLL <- as.numeric(levels(data_demo_12$TOTAL_ENROLL))[as.integer(data_demo_12$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_12 <- data_demo_12 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_12) <- demo_list_1117

# add race and gender and pct columns
data_demo_12 <- add_gender_race_col_1117(data_demo_12)

# add year 
data_demo_12$YEAR <- 2012

# change order of columns
data_demo_12 <- setcolorder(data_demo_12, demo_order_1117)

# final clean
data_demo_12 <- last_clean(data_demo_12)

# Merge them
data_demo <- full_join(data_demo, data_demo_12)

# Save as .csv file
write.csv(data_demo_12,"cleaned_data/data_enroll_2012.csv", row.names = FALSE)


# 2012-13 -----
# read in xls file
data_demo_13 <- gdata::read.xls("data/GG_BySITE-FY12-13_Public_2013_01_25.xls", header = TRUE)

# get rid of unwanted columns
data_demo_13$Elementary.District <- NULL
data_demo_13$Grades..Low.High. <- NULL
data_demo_13$Site.Level <- NULL
data_demo_13$Grade <- NULL

# change column names
colnames(data_demo_13) <- demo_list_1117

# get rid of last row (total)
data_demo_13 <- data_demo_13[-nrow(data_demo_13),] 

# change column to right type
data_demo_13$COUNTY <- as.character(data_demo_13$COUNTY)
data_demo_13$DISTRICT <- as.character(data_demo_13$DISTRICT)
data_demo_13$SCHOOL_NAME <- as.character(data_demo_13$SCHOOL_NAME)
data_demo_13$COUNTY_DISTRICT_CODE <- as.character(data_demo_13$COUNTY_DISTRICT_CODE)
data_demo_13$SITE_CODE <- as.numeric(data_demo_13$SITE_CODE)
data_demo_13$BLACK_M <- as.numeric(levels(data_demo_13$BLACK_M))[as.integer(data_demo_13$BLACK_M)]
data_demo_13$BLACK_F <- as.numeric(levels(data_demo_13$BLACK_F))[as.integer(data_demo_13$BLACK_F)]
data_demo_13$AMINDIAN_M <- as.numeric(levels(data_demo_13$AMINDIAN_M))[as.integer(data_demo_13$AMINDIAN_M)]
data_demo_13$AMINDIAN_F <- as.numeric(levels(data_demo_13$AMINDIAN_F))[as.integer(data_demo_13$AMINDIAN_F)]
data_demo_13$HISPANIC_M <- as.numeric(levels(data_demo_13$HISPANIC_M))[as.integer(data_demo_13$HISPANIC_M)]
data_demo_13$HISPANIC_F <- as.numeric(levels(data_demo_13$HISPANIC_F))[as.integer(data_demo_13$HISPANIC_F)]
data_demo_13$ASIAN_M <- as.numeric(levels(data_demo_13$ASIAN_M))[as.integer(data_demo_13$ASIAN_M)]
data_demo_13$ASIAN_F <- as.numeric(levels(data_demo_13$ASIAN_F))[as.integer(data_demo_13$ASIAN_F)]
data_demo_13$PACISLAND_M <- as.numeric(data_demo_13$PACISLAND_M)
data_demo_13$PACISLAND_F <- as.numeric(data_demo_13$PACISLAND_F)
data_demo_13$WHITE_M <- as.numeric(levels(data_demo_13$WHITE_M))[as.integer(data_demo_13$WHITE_M)]
data_demo_13$WHITE_F <- as.numeric(levels(data_demo_13$WHITE_F))[as.integer(data_demo_13$WHITE_F)]
data_demo_13$TWOORMORE_M <- as.numeric(levels(data_demo_13$TWOORMORE_M))[as.integer(data_demo_13$TWOORMORE_M)]
data_demo_13$TWOORMORE_F <- as.numeric(levels(data_demo_13$TWOORMORE_F))[as.integer(data_demo_13$TWOORMORE_F)]
data_demo_13$TOTAL_ENROLL <- as.numeric(levels(data_demo_13$TOTAL_ENROLL))[as.integer(data_demo_13$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_13 <- data_demo_13 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_13) <- demo_list_1117

# add race and gender and pct columns
data_demo_13 <- add_gender_race_col_1117(data_demo_13)

# add year 
data_demo_13$YEAR <- 2013

# change order of columns
data_demo_13 <- setcolorder(data_demo_13, demo_order_1117)

# final clean
data_demo_13 <- last_clean(data_demo_13)

# Merge them
data_demo <- full_join(data_demo, data_demo_13)

# Save as .csv file
write.csv(data_demo_13,"cleaned_data/data_enroll_2013.csv", row.names = FALSE)


# 2013-14 -----
# read in xls file
data_demo_14 <- gdata::read.xls("data/StudEnroll_bySITE_2A_EthGen-FY13-14_wEthGen.xls", header = TRUE)

# get rid of unwanted columns
data_demo_14$Elementary.District <- NULL
data_demo_14$Grades..Low.High. <- NULL
data_demo_14$Site.Level <- NULL
data_demo_14$Grade <- NULL
data_demo_14$Corresponding..E..CDS.code <- NULL

# change column names
colnames(data_demo_14) <- demo_list_1117

# get rid of last row (total)
data_demo_14 <- data_demo_14[-nrow(data_demo_14),] 

# change column to right type
data_demo_14$COUNTY <- as.character(data_demo_14$COUNTY)
data_demo_14$DISTRICT <- as.character(data_demo_14$DISTRICT)
data_demo_14$SCHOOL_NAME <- as.character(data_demo_14$SCHOOL_NAME)
data_demo_14$COUNTY_DISTRICT_CODE <- as.character(data_demo_14$COUNTY_DISTRICT_CODE)
data_demo_14$SITE_CODE <- as.numeric(data_demo_14$SITE_CODE)
data_demo_14$BLACK_M <- as.numeric(levels(data_demo_14$BLACK_M))[as.integer(data_demo_14$BLACK_M)]
data_demo_14$BLACK_F <- as.numeric(levels(data_demo_14$BLACK_F))[as.integer(data_demo_14$BLACK_F)]
data_demo_14$AMINDIAN_M <- as.numeric(levels(data_demo_14$AMINDIAN_M))[as.integer(data_demo_14$AMINDIAN_M)]
data_demo_14$AMINDIAN_F <- as.numeric(levels(data_demo_14$AMINDIAN_F))[as.integer(data_demo_14$AMINDIAN_F)]
data_demo_14$HISPANIC_M <- as.numeric(levels(data_demo_14$HISPANIC_M))[as.integer(data_demo_14$HISPANIC_M)]
data_demo_14$HISPANIC_F <- as.numeric(levels(data_demo_14$HISPANIC_F))[as.integer(data_demo_14$HISPANIC_F)]
data_demo_14$ASIAN_M <- as.numeric(levels(data_demo_14$ASIAN_M))[as.integer(data_demo_14$ASIAN_M)]
data_demo_14$ASIAN_F <- as.numeric(levels(data_demo_14$ASIAN_F))[as.integer(data_demo_14$ASIAN_F)]
data_demo_14$PACISLAND_M <- as.numeric(data_demo_14$PACISLAND_M)
data_demo_14$PACISLAND_F <- as.numeric(data_demo_14$PACISLAND_F)
data_demo_14$WHITE_M <- as.numeric(levels(data_demo_14$WHITE_M))[as.integer(data_demo_14$WHITE_M)]
data_demo_14$WHITE_F <- as.numeric(levels(data_demo_14$WHITE_F))[as.integer(data_demo_14$WHITE_F)]
data_demo_14$TWOORMORE_M <- as.numeric(levels(data_demo_14$TWOORMORE_M))[as.integer(data_demo_14$TWOORMORE_M)]
data_demo_14$TWOORMORE_F <- as.numeric(levels(data_demo_14$TWOORMORE_F))[as.integer(data_demo_14$TWOORMORE_F)]
data_demo_14$TOTAL_ENROLL <- as.numeric(levels(data_demo_14$TOTAL_ENROLL))[as.integer(data_demo_14$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_14 <- data_demo_14 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_14) <- demo_list_1117

# add race and gender and pct columns
data_demo_14 <- add_gender_race_col_1117(data_demo_14)

# add year 
data_demo_14$YEAR <- 2014

# change order of columns
data_demo_14 <- setcolorder(data_demo_14, demo_order_1117)

# final clean
data_demo_14 <- last_clean(data_demo_14)

# Merge them
data_demo <- full_join(data_demo, data_demo_14)

# Save as .csv file
write.csv(data_demo_14,"cleaned_data/data_enroll_2014.csv", row.names = FALSE)


# 2014-15 -----
# read in xls file
data_demo_15 <- gdata::read.xls("data/2014-2015_Oklahoma_Public_School_Enrollment-w-Ethinicity_Totals.xls", header = TRUE)

# get rid of unwanted columns
data_demo_15$Elementary.District <- NULL
data_demo_15$Grades..Low.High. <- NULL
data_demo_15$Site.Level <- NULL
data_demo_15$Grade <- NULL
data_demo_15$Corresponding..E..CDS.code <- NULL

# change column names
colnames(data_demo_15) <- demo_list_1117

# get rid of last row (total)
data_demo_15 <- data_demo_15[-nrow(data_demo_15),] 

# change column to right type
data_demo_15$COUNTY <- as.character(data_demo_15$COUNTY)
data_demo_15$DISTRICT <- as.character(data_demo_15$DISTRICT)
data_demo_15$SCHOOL_NAME <- as.character(data_demo_15$SCHOOL_NAME)
data_demo_15$COUNTY_DISTRICT_CODE <- as.character(data_demo_15$COUNTY_DISTRICT_CODE)
data_demo_15$SITE_CODE <- as.numeric(data_demo_15$SITE_CODE)
data_demo_15$BLACK_M <- as.numeric(levels(data_demo_15$BLACK_M))[as.integer(data_demo_15$BLACK_M)]
data_demo_15$BLACK_F <- as.numeric(levels(data_demo_15$BLACK_F))[as.integer(data_demo_15$BLACK_F)]
data_demo_15$AMINDIAN_M <- as.numeric(levels(data_demo_15$AMINDIAN_M))[as.integer(data_demo_15$AMINDIAN_M)]
data_demo_15$AMINDIAN_F <- as.numeric(levels(data_demo_15$AMINDIAN_F))[as.integer(data_demo_15$AMINDIAN_F)]
data_demo_15$HISPANIC_M <- as.numeric(levels(data_demo_15$HISPANIC_M))[as.integer(data_demo_15$HISPANIC_M)]
data_demo_15$HISPANIC_F <- as.numeric(levels(data_demo_15$HISPANIC_F))[as.integer(data_demo_15$HISPANIC_F)]
data_demo_15$ASIAN_M <- as.numeric(levels(data_demo_15$ASIAN_M))[as.integer(data_demo_15$ASIAN_M)]
data_demo_15$ASIAN_F <- as.numeric(levels(data_demo_15$ASIAN_F))[as.integer(data_demo_15$ASIAN_F)]
data_demo_15$PACISLAND_M <- as.numeric(data_demo_15$PACISLAND_M)
data_demo_15$PACISLAND_F <- as.numeric(data_demo_15$PACISLAND_F)
data_demo_15$WHITE_M <- as.numeric(levels(data_demo_15$WHITE_M))[as.integer(data_demo_15$WHITE_M)]
data_demo_15$WHITE_F <- as.numeric(levels(data_demo_15$WHITE_F))[as.integer(data_demo_15$WHITE_F)]
data_demo_15$TWOORMORE_M <- as.numeric(levels(data_demo_15$TWOORMORE_M))[as.integer(data_demo_15$TWOORMORE_M)]
data_demo_15$TWOORMORE_F <- as.numeric(levels(data_demo_15$TWOORMORE_F))[as.integer(data_demo_15$TWOORMORE_F)]
data_demo_15$TOTAL_ENROLL <- as.numeric(levels(data_demo_15$TOTAL_ENROLL))[as.integer(data_demo_15$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_15 <- data_demo_15 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_15) <- demo_list_1117

# add race and gender and pct columns
data_demo_15 <- add_gender_race_col_1117(data_demo_14)

# add year 
data_demo_15$YEAR <- 2015

# change order of columns
data_demo_15 <- setcolorder(data_demo_15, demo_order_1117)

# final clean
data_demo_15 <- last_clean(data_demo_15)

# Merge them
data_demo <- full_join(data_demo, data_demo_15)

# Save as .csv file
write.csv(data_demo_15,"cleaned_data/data_enroll_2015.csv", row.names = FALSE)


# 2015-16 -----
# read in xls file
data_demo_16 <- gdata::read.xls("data/FY15-16_ENROLLMENT_Oklahoma_Public_Schools_by_SCHOOL_SITE__wEthnicity&Gender-_v2015-12-18.xls", header = TRUE)

# get rid of unwanted columns
data_demo_16$X..Grades..Low.High. <- NULL
data_demo_16$X..Site.Level <- NULL
data_demo_16$X..Grade <- NULL

# change column names
colnames(data_demo_16) <- demo_list_1117

# get rid of last row (total)
data_demo_16 <- data_demo_16[-nrow(data_demo_16),] 

# change column to right type
data_demo_16$COUNTY <- as.character(data_demo_16$COUNTY)
data_demo_16$DISTRICT <- as.character(data_demo_16$DISTRICT)
data_demo_16$SCHOOL_NAME <- as.character(data_demo_16$SCHOOL_NAME)
data_demo_16$COUNTY_DISTRICT_CODE <- as.character(data_demo_16$COUNTY_DISTRICT_CODE)
data_demo_16$SITE_CODE <- as.numeric(data_demo_16$SITE_CODE)
data_demo_16$BLACK_M <- as.numeric(levels(data_demo_16$BLACK_M))[as.integer(data_demo_16$BLACK_M)]
data_demo_16$BLACK_F <- as.numeric(levels(data_demo_16$BLACK_F))[as.integer(data_demo_16$BLACK_F)]
data_demo_16$AMINDIAN_M <- as.numeric(levels(data_demo_16$AMINDIAN_M))[as.integer(data_demo_16$AMINDIAN_M)]
data_demo_16$AMINDIAN_F <- as.numeric(levels(data_demo_16$AMINDIAN_F))[as.integer(data_demo_16$AMINDIAN_F)]
data_demo_16$HISPANIC_M <- as.numeric(levels(data_demo_16$HISPANIC_M))[as.integer(data_demo_16$HISPANIC_M)]
data_demo_16$HISPANIC_F <- as.numeric(levels(data_demo_16$HISPANIC_F))[as.integer(data_demo_16$HISPANIC_F)]
data_demo_16$ASIAN_M <- as.numeric(levels(data_demo_16$ASIAN_M))[as.integer(data_demo_16$ASIAN_M)]
data_demo_16$ASIAN_F <- as.numeric(levels(data_demo_16$ASIAN_F))[as.integer(data_demo_16$ASIAN_F)]
data_demo_16$PACISLAND_M <- as.numeric(data_demo_16$PACISLAND_M)
data_demo_16$PACISLAND_F <- as.numeric(data_demo_16$PACISLAND_F)
data_demo_16$WHITE_M <- as.numeric(levels(data_demo_16$WHITE_M))[as.integer(data_demo_16$WHITE_M)]
data_demo_16$WHITE_F <- as.numeric(levels(data_demo_16$WHITE_F))[as.integer(data_demo_16$WHITE_F)]
data_demo_16$TWOORMORE_M <- as.numeric(levels(data_demo_16$TWOORMORE_M))[as.integer(data_demo_16$TWOORMORE_M)]
data_demo_16$TWOORMORE_F <- as.numeric(levels(data_demo_16$TWOORMORE_F))[as.integer(data_demo_16$TWOORMORE_F)]
data_demo_16$TOTAL_ENROLL <- as.numeric(levels(data_demo_16$TOTAL_ENROLL))[as.integer(data_demo_16$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_16 <- data_demo_16 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_16) <- demo_list_1117

# add race and gender and pct columns
data_demo_16 <- add_gender_race_col_1117(data_demo_16)

# add year 
data_demo_16$YEAR <- 2016

# change order of columns
data_demo_16 <- setcolorder(data_demo_16, demo_order_1117)

# final clean
data_demo_16 <- last_clean(data_demo_16)

# Merge them
data_demo <- full_join(data_demo, data_demo_16)

# Save as .csv file
write.csv(data_demo_16,"cleaned_data/data_enroll_2016.csv", row.names = FALSE)


# 2016-17 -----
# read in xls file
data_demo_17 <- gdata::read.xls("data/GG_bySITE_2A_EthGen-FY16-17_Public_2016-12-02_0.xls", header = TRUE, skip = 1)

# get rid of unwanted columns
data_demo_17$Elementary.District <- NULL
data_demo_17$Low.Grade <- NULL
data_demo_17$High.Grade <- NULL
data_demo_17$Corresponding..E..CDS.code <- NULL
data_demo_17$Grade <- NULL

# change column names
colnames(data_demo_17) <- demo_list_1117

# get rid of last row (total)
data_demo_17 <- data_demo_17[-nrow(data_demo_17),] 

# change column to right type
data_demo_17$COUNTY <- as.character(data_demo_17$COUNTY)
data_demo_17$DISTRICT <- as.character(data_demo_17$DISTRICT)
data_demo_17$SCHOOL_NAME <- as.character(data_demo_17$SCHOOL_NAME)
data_demo_17$COUNTY_DISTRICT_CODE <- as.character(data_demo_17$COUNTY_DISTRICT_CODE)
data_demo_17$SITE_CODE <- as.numeric(data_demo_17$SITE_CODE)
data_demo_17$BLACK_M <- as.numeric(levels(data_demo_17$BLACK_M))[as.integer(data_demo_17$BLACK_M)]
data_demo_17$BLACK_F <- as.numeric(levels(data_demo_17$BLACK_F))[as.integer(data_demo_17$BLACK_F)]
data_demo_17$AMINDIAN_M <- as.numeric(levels(data_demo_17$AMINDIAN_M))[as.integer(data_demo_17$AMINDIAN_M)]
data_demo_17$AMINDIAN_F <- as.numeric(levels(data_demo_17$AMINDIAN_F))[as.integer(data_demo_17$AMINDIAN_F)]
data_demo_17$HISPANIC_M <- as.numeric(levels(data_demo_17$HISPANIC_M))[as.integer(data_demo_17$HISPANIC_M)]
data_demo_17$HISPANIC_F <- as.numeric(levels(data_demo_17$HISPANIC_F))[as.integer(data_demo_17$HISPANIC_F)]
data_demo_17$ASIAN_M <- as.numeric(levels(data_demo_17$ASIAN_M))[as.integer(data_demo_17$ASIAN_M)]
data_demo_17$ASIAN_F <- as.numeric(levels(data_demo_17$ASIAN_F))[as.integer(data_demo_17$ASIAN_F)]
data_demo_17$PACISLAND_M <- as.numeric(data_demo_17$PACISLAND_M)
data_demo_17$PACISLAND_F <- as.numeric(data_demo_17$PACISLAND_F)
data_demo_17$WHITE_M <- as.numeric(levels(data_demo_17$WHITE_M))[as.integer(data_demo_17$WHITE_M)]
data_demo_17$WHITE_F <- as.numeric(levels(data_demo_17$WHITE_F))[as.integer(data_demo_17$WHITE_F)]
data_demo_17$TWOORMORE_M <- as.numeric(levels(data_demo_17$TWOORMORE_M))[as.integer(data_demo_17$TWOORMORE_M)]
data_demo_17$TWOORMORE_F <- as.numeric(levels(data_demo_17$TWOORMORE_F))[as.integer(data_demo_17$TWOORMORE_F)]
data_demo_17$TOTAL_ENROLL <- as.numeric(levels(data_demo_17$TOTAL_ENROLL))[as.integer(data_demo_17$TOTAL_ENROLL)]

# combines the rows of schools together
data_demo_17 <- data_demo_17 %>% 
  group_by_("COUNTY", "DISTRICT", "COUNTY_DISTRICT_CODE", "SITE_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(BLACK_M), sum(BLACK_F), sum(AMINDIAN_M), sum(AMINDIAN_F),
            sum(HISPANIC_M), sum(HISPANIC_F), sum(ASIAN_M), sum(ASIAN_F),
            sum(PACISLAND_M), sum(PACISLAND_F), sum(WHITE_M), sum(WHITE_F),
            sum(TWOORMORE_M), sum(TWOORMORE_F), sum(TOTAL_ENROLL))
# change column names(again)
colnames(data_demo_17) <- demo_list_1117

# add race and gender and pct columns
data_demo_17 <- add_gender_race_col_1117(data_demo_17)

# add year 
data_demo_17$YEAR <- 2017

# change order of columns
data_demo_17 <- setcolorder(data_demo_17, demo_order_1117)

# final clean
data_demo_17 <- last_clean(data_demo_17)

# Merge them
data_demo <- full_join(data_demo, data_demo_17)

# Save as .csv file
write.csv(data_demo_17,"cleaned_data/data_enroll_2017.csv", row.names = FALSE)

# Finish -----
# final clean
data_demo <- last_clean(data_demo)

# Save as .csv file
write.csv(data_demo,"cleaned_data/data_enroll_2006_17.csv", row.names = FALSE)


# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/ok_perf_demo_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/ok_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/ok_perf_clean.rds")




