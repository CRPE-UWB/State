#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of California's Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning California 
# Created by: Kevin Cha on 07-26-17
# Updated by: Kevin Cha on 08-08-17
# Data from: 
#     Performance: STAR: http://star.cde.ca.gov/starresearchfiles.asp    CAASPP: http://caaspp.cde.ca.gov/ 
#       -Click on 20XX CAASPP Test Results=>Research Files)
#     Demographics: http://www.cde.ca.gov/ds/sd/sd/filesenr.asp
#       -Click on "File Name"
# Codebook:
#     Demographics: http://www.cde.ca.gov/ds/sd/sd/filesenr.asp
#       -Click on "File Structure"
#     Performance:
#       -2016: http://caaspp.cde.ca.gov/sb2016/research_fixfileformat       2015: http://caaspp.cde.ca.gov/caaspp2015/research_fixfileformat.aspx   
#       -2014: http://caaspp.cde.ca.gov/caaspp2014/research_fixfileformat.aspx    2013: http://star.cde.ca.gov/star2013/research_fixfileformat.aspx
#       -2012: http://star.cde.ca.gov/star2012/research_fixfileformat.aspx      2011: http://star.cde.ca.gov/star2011/research_fixfileformat.aspx
#       -2010: http://star.cde.ca.gov/star2010/research_fixfileformat.asp       2009: http://star.cde.ca.gov/star2009/research_fixfileformat.asp
#       -2008: http://star.cde.ca.gov/star2008/research_fixfileformat.asp       2007: http://star.cde.ca.gov/star2007/research_fixfileformat.asp
# Link to Github: https://github.com/CRPE-UWB/State
# Notes: -idk what's with the warning messages: "Unknown or uninitialised column: 'ETHNIC'." but it can be ignored
#        -For Performance: 2014+2015 will have problems due to lack of info in the dataset 

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_ca_clean") #MAC

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(readxl)
library(readr)


# Demo List --------------------------------------------------------------------------------------------------------
demo_order <- c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL", "YEAR", "TOTAL_ENROLL", "TOTAL_MALE", "TOTAL_MALE_PCT", 
                "TOTAL_FEMALE", "TOTAL_FEMALE_PCT", "AMINDIAN", "AMINDIAN_PCT", "ASIAN", "ASIAN_PCT",
                "BLACK", "BLACK_PCT", "FILIPINO", "FILIPINO_PCT", "HISPANIC", "HISPANIC_PCT", 
                "PACISLAND", "PACISLAND_PCT", "TWOORMORE", "TWOORMORE_PCT", "WHITE", "WHITE_PCT", "NOTREPORTED", "NOTREPORTED_PCT")
demo_order_2 <- c("CDS_CODE", "COUNTY", "DISTRICT", "SCHOOL", "YEAR", "TOTAL_ENROLL", "TOTAL_MALE", "TOTAL_MALE_PCT", 
                "TOTAL_FEMALE", "TOTAL_FEMALE_PCT", "AMINDIAN", "AMINDIAN_PCT", "ASIAN", "ASIAN_PCT",
                "BLACK", "BLACK_PCT", "FILIPINO", "FILIPINO_PCT", "HISPANIC", "HISPANIC_PCT", 
                "PACISLAND", "PACISLAND_PCT", "TWOORMORE", "TWOORMORE_PCT", "WHITE", "WHITE_PCT")
demo_order_3 <- c("CDS_CODE", "YEAR", "TOTAL_ENROLL", "TOTAL_MALE", "TOTAL_MALE_PCT", 
                  "TOTAL_FEMALE", "TOTAL_FEMALE_PCT", "AMINDIAN", "AMINDIAN_PCT", "ASIAN", "ASIAN_PCT",
                  "BLACK", "BLACK_PCT", "FILIPINO", "FILIPINO_PCT", "HISPANIC", "HISPANIC_PCT", 
                  "PACISLAND", "PACISLAND_PCT", "TWOORMORE", "TWOORMORE_PCT", "WHITE", "WHITE_PCT")
demo_order_4 <- c("CDS_CODE", "YEAR", "TOTAL_ENROLL", "TOTAL_MALE", "TOTAL_MALE_PCT", 
                  "TOTAL_FEMALE", "TOTAL_FEMALE_PCT", "AMINDIAN", "AMINDIAN_PCT", "ASIAN", "ASIAN_PCT",
                  "BLACK", "BLACK_PCT", "FILIPINO", "FILIPINO_PCT", "HISPANIC", "HISPANIC_PCT", 
                  "PACISLAND", "PACISLAND_PCT", "WHITE", "WHITE_PCT")
# Perf List --------------------------------------------------------------------------------------------------------
perf_list_16 <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                  "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TEST_ID", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                  "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "STUDENTS_SCORED")
perf_list_16b <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                  "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                  "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "STUDENTS_SCORED")

perf_list_1415 <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                  "TOTAL_TESTED_ENTITY", "TEST_ID", "NUM_STUDENTS_TESTED",
                  "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")
perf_list_1415b <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                    "TOTAL_TESTED_ENTITY", "NUM_STUDENTS_TESTED",
                    "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")

perf_list_0913 <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                  "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TEST_ID", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                  "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")
perf_list_0913b <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                    "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                    "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")

perf_list_0708 <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                    "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TEST_ID", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                    "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")
perf_list_0708b <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
                    "TOTAL_ENROLL", "TOTAL_TESTED_ENTITY", "TOTAL_REPORTED", "NUM_STUDENTS_TESTED",
                    "ADV_PCT", "PROF_PCT", "BASIC_PCT", "BELOW_BASIC_PCT", "FAR_BELOW_BASIC_PCT", "STUDENTS_SCORED")

info_list <- c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR", 
               "TYPE_ID", "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", "ZIP_CODE")

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

clean_demo_1017 <- function(df) {
  # turn all numbers into what they actually are for demographic
  df$ETHNIC[df$ETHNIC == 0] <- "NOTREPORTED"
  df$ETHNIC[df$ETHNIC == 1] <- "AMINDIAN"
  df$ETHNIC[df$ETHNIC == 2] <- "ASIAN"
  df$ETHNIC[df$ETHNIC == 3] <- "PACISLAND"
  df$ETHNIC[df$ETHNIC == 4] <- "FILIPINO"
  df$ETHNIC[df$ETHNIC == 5] <- "HISPANIC"
  df$ETHNIC[df$ETHNIC == 6] <- "BLACK"
  df$ETHNIC[df$ETHNIC == 7] <- "WHITE"
  df$ETHNIC[df$ETHNIC == 9] <- "TWOORMORE"
  
  # combine the ethnic and gender columns
  df$ETHNIC <- paste(df$ETHNIC, df$GENDER, sep = "_")
  
  # get rid of not needed columns
  df$GENDER <- NULL
  df$KDGN <- NULL
  df$GR_1 <- NULL
  df$GR_2 <- NULL
  df$GR_3 <- NULL
  df$GR_4 <- NULL
  df$GR_5 <- NULL
  df$GR_6 <- NULL
  df$GR_7 <- NULL
  df$GR_8 <- NULL
  df$GR_9 <- NULL
  df$GR_10 <- NULL
  df$GR_11 <- NULL
  df$GR_12 <- NULL
  df$UNGR_ELM <- NULL
  df$UNGR_SEC <- NULL
  df$ADULT <- NULL
  
  # Separate by ethnicity
  df <-df %>% 
    spread(ETHNIC, ENR_TOTAL)
  
  # change the NA into 0
  df[is.na(df)] <- 0
  
  # create a MALE column
  df$TOTAL_MALE <- rowSums(df[,c("AMINDIAN_M", "ASIAN_M", "BLACK_M", "FILIPINO_M", "HISPANIC_M", "NOTREPORTED_M", "PACISLAND_M", "TWOORMORE_M", "WHITE_M")], na.rm = TRUE) 
  # create a MALE column
  df$TOTAL_FEMALE <- rowSums(df[,c("AMINDIAN_F", "ASIAN_F", "BLACK_F", "FILIPINO_F", "HISPANIC_F", "NOTREPORTED_F", "PACISLAND_F", "TWOORMORE_F", "WHITE_F")], na.rm = TRUE)
  # create a TOTAL_ENROLL column
  df$TOTAL_ENROLL <- rowSums(df[,c("TOTAL_MALE", "TOTAL_FEMALE")], na.rm = TRUE)
  
  # create columns that contain total ethnicity
  df$AMINDIAN <- rowSums(df[,c("AMINDIAN_M", "AMINDIAN_F")], na.rm = TRUE)
  df$ASIAN <- rowSums(df[,c("ASIAN_M", "ASIAN_F")], na.rm = TRUE)
  df$BLACK <- rowSums(df[,c("BLACK_M", "BLACK_F")], na.rm = TRUE)
  df$FILIPINO <- rowSums(df[,c("FILIPINO_M", "FILIPINO_F")], na.rm = TRUE)
  df$HISPANIC <- rowSums(df[,c("HISPANIC_M", "HISPANIC_F")], na.rm = TRUE)
  df$NOTREPORTED <- rowSums(df[,c("NOTREPORTED_M", "NOTREPORTED_F")], na.rm = TRUE)
  df$PACISLAND <- rowSums(df[,c("PACISLAND_M", "PACISLAND_F")], na.rm = TRUE)
  df$TWOORMORE <- rowSums(df[,c("TWOORMORE_M", "TWOORMORE_F")], na.rm = TRUE)
  df$WHITE <- rowSums(df[,c("WHITE_M", "WHITE_F")], na.rm = TRUE)
  
  # get rid of the separated gender columns
  df$AMINDIAN_F <- NULL
  df$AMINDIAN_M <- NULL
  df$ASIAN_F <- NULL
  df$ASIAN_M <- NULL
  df$BLACK_F <- NULL
  df$BLACK_M <- NULL
  df$FILIPINO_F <- NULL
  df$FILIPINO_M <- NULL
  df$HISPANIC_F <- NULL
  df$HISPANIC_M <- NULL
  df$NOTREPORTED_F <- NULL
  df$NOTREPORTED_M <- NULL
  df$PACISLAND_F <- NULL
  df$PACISLAND_M <- NULL
  df$TWOORMORE_F <- NULL
  df$TWOORMORE_M <- NULL
  df$WHITE_F <- NULL
  df$WHITE_M <- NULL
  
  # create a PCT column
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$FILIPINO_PCT <- round(df$FILIPINO / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$NOTREPORTED_PCT <- round(df$NOTREPORTED / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_PCT <- round(df$TWOORMORE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_MALE_PCT <- round(df$TOTAL_MALE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_FEMALE_PCT <- round(df$TOTAL_FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  # gets rid of multiple warnings
  df$ETHNIC <- NA
  df$ETHNIC <- NULL
  
  # last part of cleaning
  df$COUNTY <- no_more_special_characters(df$COUNTY)
  df$DISTRICT <- no_more_special_characters(df$DISTRICT)
  df$SCHOOL <- no_more_special_characters(df$SCHOOL)
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}
clean_demo_0809 <- function(df) {
  # turn all numbers into what they actually are for demographic
  df$ETHNIC[df$ETHNIC == 1] <- "AMINDIAN"
  df$ETHNIC[df$ETHNIC == 2] <- "ASIAN"
  df$ETHNIC[df$ETHNIC == 3] <- "PACISLAND"
  df$ETHNIC[df$ETHNIC == 4] <- "FILIPINO"
  df$ETHNIC[df$ETHNIC == 5] <- "HISPANIC"
  df$ETHNIC[df$ETHNIC == 6] <- "BLACK"
  df$ETHNIC[df$ETHNIC == 7] <- "WHITE"
  df$ETHNIC[df$ETHNIC == 8] <- "TWOORMORE"
  
  # combine the ethnic and gender columns
  df$ETHNIC <- paste(df$ETHNIC, df$GENDER, sep = "_")
  
  # get rid of not needed columns
  df$GENDER <- NULL
  df$KDGN <- NULL
  df$GR_1 <- NULL
  df$GR_2 <- NULL
  df$GR_3 <- NULL
  df$GR_4 <- NULL
  df$GR_5 <- NULL
  df$GR_6 <- NULL
  df$GR_7 <- NULL
  df$GR_8 <- NULL
  df$GR_9 <- NULL
  df$GR_10 <- NULL
  df$GR_11 <- NULL
  df$GR_12 <- NULL
  df$UNGR_ELM <- NULL
  df$UNGR_SEC <- NULL
  df$ADULT <- NULL
  
  # Separate by ethnicity
  df <-df %>% 
    spread(ETHNIC, ENR_TOTAL)
  
  # change the NA into 0
  df[is.na(df)] <- 0
  
  # create a MALE column
  df$TOTAL_MALE <- rowSums(df[,c("AMINDIAN_M", "ASIAN_M", "BLACK_M", "FILIPINO_M", "HISPANIC_M", "PACISLAND_M", "TWOORMORE_M", "WHITE_M")], na.rm = TRUE) 
  # create a MALE column
  df$TOTAL_FEMALE <- rowSums(df[,c("AMINDIAN_F", "ASIAN_F", "BLACK_F", "FILIPINO_F", "HISPANIC_F", "NOTREPORTED_F", "PACISLAND_F", "TWOORMORE_F", "WHITE_F")], na.rm = TRUE)
  # create a TOTAL_ENROLL column
  df$TOTAL_ENROLL <- rowSums(df[,c("TOTAL_MALE", "TOTAL_FEMALE")], na.rm = TRUE)
  
  # create columns that contain total ethnicity
  df$AMINDIAN <- rowSums(df[,c("AMINDIAN_M", "AMINDIAN_F")], na.rm = TRUE)
  df$ASIAN <- rowSums(df[,c("ASIAN_M", "ASIAN_F")], na.rm = TRUE)
  df$BLACK <- rowSums(df[,c("BLACK_M", "BLACK_F")], na.rm = TRUE)
  df$FILIPINO <- rowSums(df[,c("FILIPINO_M", "FILIPINO_F")], na.rm = TRUE)
  df$HISPANIC <- rowSums(df[,c("HISPANIC_M", "HISPANIC_F")], na.rm = TRUE)
  df$PACISLAND <- rowSums(df[,c("PACISLAND_M", "PACISLAND_F")], na.rm = TRUE)
  df$TWOORMORE <- rowSums(df[,c("TWOORMORE_M", "TWOORMORE_F")], na.rm = TRUE)
  df$WHITE <- rowSums(df[,c("WHITE_M", "WHITE_F")], na.rm = TRUE)
  
  # get rid of the separated gender columns
  df$AMINDIAN_F <- NULL
  df$AMINDIAN_M <- NULL
  df$ASIAN_F <- NULL
  df$ASIAN_M <- NULL
  df$BLACK_F <- NULL
  df$BLACK_M <- NULL
  df$FILIPINO_F <- NULL
  df$FILIPINO_M <- NULL
  df$HISPANIC_F <- NULL
  df$HISPANIC_M <- NULL
  df$PACISLAND_F <- NULL
  df$PACISLAND_M <- NULL
  df$TWOORMORE_F <- NULL
  df$TWOORMORE_M <- NULL
  df$WHITE_F <- NULL
  df$WHITE_M <- NULL
  
  # create a PCT column
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$FILIPINO_PCT <- round(df$FILIPINO / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_PCT <- round(df$TWOORMORE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_MALE_PCT <- round(df$TOTAL_MALE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_FEMALE_PCT <- round(df$TOTAL_FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  # gets rid of multiple warnings
  df$ETHNIC <- NA
  df$ETHNIC <- NULL
  
  # last part of cleaning
  df$COUNTY <- no_more_special_characters(df$COUNTY)
  df$DISTRICT <- no_more_special_characters(df$DISTRICT)
  df$SCHOOL <- no_more_special_characters(df$SCHOOL)
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}
clean_demo_9907 <- function(df) {
  # turn all numbers into what they actually are for demographic
  df$ETHNIC[df$ETHNIC == 1] <- "AMINDIAN"
  df$ETHNIC[df$ETHNIC == 2] <- "ASIAN"
  df$ETHNIC[df$ETHNIC == 3] <- "PACISLAND"
  df$ETHNIC[df$ETHNIC == 4] <- "FILIPINO"
  df$ETHNIC[df$ETHNIC == 5] <- "HISPANIC"
  df$ETHNIC[df$ETHNIC == 6] <- "BLACK"
  df$ETHNIC[df$ETHNIC == 7] <- "WHITE"
  df$ETHNIC[df$ETHNIC == 8] <- "TWOORMORE"
  
  # combine the ethnic and gender columns
  df$ETHNIC <- paste(df$ETHNIC, df$GENDER, sep = "_")
  
  # get rid of not needed columns
  df$GENDER <- NULL
  df$KDGN <- NULL
  df$GR_1 <- NULL
  df$GR_2 <- NULL
  df$GR_3 <- NULL
  df$GR_4 <- NULL
  df$GR_5 <- NULL
  df$GR_6 <- NULL
  df$GR_7 <- NULL
  df$GR_8 <- NULL
  df$GR_9 <- NULL
  df$GR_10 <- NULL
  df$GR_11 <- NULL
  df$GR_12 <- NULL
  df$UNGR_ELM <- NULL
  df$UNGR_SEC <- NULL
  df$ADULT <- NULL
  
  # Separate by ethnicity
  df <-df %>% 
    spread(ETHNIC, ENR_TOTAL)
  
  # change the NA into 0
  df[is.na(df)] <- 0
  
  # create a MALE column
  df$TOTAL_MALE <- rowSums(df[,c("AMINDIAN_M", "ASIAN_M", "BLACK_M", "FILIPINO_M", "HISPANIC_M", "PACISLAND_M", "TWOORMORE_M", "WHITE_M")], na.rm = TRUE) 
  # create a MALE column
  df$TOTAL_FEMALE <- rowSums(df[,c("AMINDIAN_F", "ASIAN_F", "BLACK_F", "FILIPINO_F", "HISPANIC_F", "PACISLAND_F", "TWOORMORE_F", "WHITE_F")], na.rm = TRUE)
  # create a TOTAL_ENROLL column
  df$TOTAL_ENROLL <- rowSums(df[,c("TOTAL_MALE", "TOTAL_FEMALE")], na.rm = TRUE)
  
  # create columns that contain total ethnicity
  df$AMINDIAN <- rowSums(df[,c("AMINDIAN_M", "AMINDIAN_F")], na.rm = TRUE)
  df$ASIAN <- rowSums(df[,c("ASIAN_M", "ASIAN_F")], na.rm = TRUE)
  df$BLACK <- rowSums(df[,c("BLACK_M", "BLACK_F")], na.rm = TRUE)
  df$FILIPINO <- rowSums(df[,c("FILIPINO_M", "FILIPINO_F")], na.rm = TRUE)
  df$HISPANIC <- rowSums(df[,c("HISPANIC_M", "HISPANIC_F")], na.rm = TRUE)
  df$PACISLAND <- rowSums(df[,c("PACISLAND_M", "PACISLAND_F")], na.rm = TRUE)
  df$TWOORMORE <- rowSums(df[,c("TWOORMORE_M", "TWOORMORE_F")], na.rm = TRUE)
  df$WHITE <- rowSums(df[,c("WHITE_M", "WHITE_F")], na.rm = TRUE)
  
  # get rid of the separated gender columns
  df$AMINDIAN_F <- NULL
  df$AMINDIAN_M <- NULL
  df$ASIAN_F <- NULL
  df$ASIAN_M <- NULL
  df$BLACK_F <- NULL
  df$BLACK_M <- NULL
  df$FILIPINO_F <- NULL
  df$FILIPINO_M <- NULL
  df$HISPANIC_F <- NULL
  df$HISPANIC_M <- NULL
  df$PACISLAND_F <- NULL
  df$PACISLAND_M <- NULL
  df$TWOORMORE_F <- NULL
  df$TWOORMORE_M <- NULL
  df$WHITE_F <- NULL
  df$WHITE_M <- NULL
  
  # create a PCT column
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$FILIPINO_PCT <- round(df$FILIPINO / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$TWOORMORE_PCT <- round(df$TWOORMORE / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_MALE_PCT <- round(df$TOTAL_MALE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_FEMALE_PCT <- round(df$TOTAL_FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  # gets rid of multiple warnings
  df$ETHNIC <- NA
  df$ETHNIC <- NULL
  
  # last part of cleaning
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}
clean_demo_9498 <- function(df) {
  # turn all numbers into what they actually are for demographic
  df$ETHNIC[df$ETHNIC == 1] <- "AMINDIAN"
  df$ETHNIC[df$ETHNIC == 2] <- "ASIAN"
  df$ETHNIC[df$ETHNIC == 3] <- "PACISLAND"
  df$ETHNIC[df$ETHNIC == 4] <- "FILIPINO"
  df$ETHNIC[df$ETHNIC == 5] <- "HISPANIC"
  df$ETHNIC[df$ETHNIC == 6] <- "BLACK"
  df$ETHNIC[df$ETHNIC == 7] <- "WHITE"
  
  # combine the ethnic and gender columns
  df$ETHNIC <- paste(df$ETHNIC, df$GENDER, sep = "_")
  
  # get rid of not needed columns
  df$GENDER <- NULL
  df$KDGN <- NULL
  df$GR_1 <- NULL
  df$GR_2 <- NULL
  df$GR_3 <- NULL
  df$GR_4 <- NULL
  df$GR_5 <- NULL
  df$GR_6 <- NULL
  df$GR_7 <- NULL
  df$GR_8 <- NULL
  df$GR_9 <- NULL
  df$GR_10 <- NULL
  df$GR_11 <- NULL
  df$GR_12 <- NULL
  df$UNGR_ELM <- NULL
  df$UNGR_SEC <- NULL
  df$ADULT <- NULL
  
  # Separate by ethnicity
  df <-df %>% 
    spread(ETHNIC, ENR_TOTAL)
  
  # change the NA into 0
  df[is.na(df)] <- 0
  
  # create a MALE column
  df$TOTAL_MALE <- rowSums(df[,c("AMINDIAN_M", "ASIAN_M", "BLACK_M", "FILIPINO_M", "HISPANIC_M", "PACISLAND_M", "WHITE_M")], na.rm = TRUE) 
  # create a MALE column
  df$TOTAL_FEMALE <- rowSums(df[,c("AMINDIAN_F", "ASIAN_F", "BLACK_F", "FILIPINO_F", "HISPANIC_F", "PACISLAND_F", "WHITE_F")], na.rm = TRUE)
  # create a TOTAL_ENROLL column
  df$TOTAL_ENROLL <- rowSums(df[,c("TOTAL_MALE", "TOTAL_FEMALE")], na.rm = TRUE)
  
  # create columns that contain total ethnicity
  df$AMINDIAN <- rowSums(df[,c("AMINDIAN_M", "AMINDIAN_F")], na.rm = TRUE)
  df$ASIAN <- rowSums(df[,c("ASIAN_M", "ASIAN_F")], na.rm = TRUE)
  df$BLACK <- rowSums(df[,c("BLACK_M", "BLACK_F")], na.rm = TRUE)
  df$FILIPINO <- rowSums(df[,c("FILIPINO_M", "FILIPINO_F")], na.rm = TRUE)
  df$HISPANIC <- rowSums(df[,c("HISPANIC_M", "HISPANIC_F")], na.rm = TRUE)
  df$PACISLAND <- rowSums(df[,c("PACISLAND_M", "PACISLAND_F")], na.rm = TRUE)
  df$WHITE <- rowSums(df[,c("WHITE_M", "WHITE_F")], na.rm = TRUE)
  
  # get rid of the separated gender columns
  df$AMINDIAN_F <- NULL
  df$AMINDIAN_M <- NULL
  df$ASIAN_F <- NULL
  df$ASIAN_M <- NULL
  df$BLACK_F <- NULL
  df$BLACK_M <- NULL
  df$FILIPINO_F <- NULL
  df$FILIPINO_M <- NULL
  df$HISPANIC_F <- NULL
  df$HISPANIC_M <- NULL
  df$PACISLAND_F <- NULL
  df$PACISLAND_M <- NULL
  df$WHITE_F <- NULL
  df$WHITE_M <- NULL
  
  # create a PCT column
  df$AMINDIAN_PCT <- round(df$AMINDIAN / df$TOTAL_ENROLL, digits = 3)
  df$ASIAN_PCT <- round(df$ASIAN / df$TOTAL_ENROLL, digits = 3)
  df$BLACK_PCT <- round(df$BLACK / df$TOTAL_ENROLL, digits = 3)
  df$FILIPINO_PCT <- round(df$FILIPINO / df$TOTAL_ENROLL, digits = 3)
  df$HISPANIC_PCT <- round(df$HISPANIC / df$TOTAL_ENROLL, digits = 3)
  df$PACISLAND_PCT <- round(df$PACISLAND / df$TOTAL_ENROLL, digits = 3)
  df$WHITE_PCT <- round(df$WHITE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_MALE_PCT <- round(df$TOTAL_MALE / df$TOTAL_ENROLL, digits = 3)
  df$TOTAL_FEMALE_PCT <- round(df$TOTAL_FEMALE / df$TOTAL_ENROLL, digits = 3)
  
  # gets rid of multiple warnings
  df$ETHNIC <- NA
  df$ETHNIC <- NULL
  
  # last part of cleaning
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}

add_info <- function(df_info, df_perf) {
  #change column names
  colnames(df_info) <- info_list
  # don't include statewide
  df_info <- df_info %>% 
    filter(COUNTY_CODE != '00')
  # include only schools and charters
  # don't include statewide
  df_info <- df_info %>% 
    filter(TYPE_ID == c(7,9,10))
  # make sure columns are character
  df_info$COUNTY_NAME <- as.character(df_info$COUNTY_NAME)
  df_info$DISTRICT_NAME <- as.character(df_info$DISTRICT_NAME)
  df_info$SCHOOL_NAME <- as.character(df_info$SCHOOL_NAME)
  # change TYPE_ID for df_info
  df_info$TYPE_ID[df_info$TYPE_ID == 7] <- 'SCHOOL'
  df_info$TYPE_ID[df_info$TYPE_ID == 9] <- 'DIRECT FUNDED CHARTER'
  df_info$TYPE_ID[df_info$TYPE_ID == 10] <- 'LOCALLY FUNDED CHARTER'
  df_info[df_info == ','] <- ""
  df_info[df_info == '~'] <- "-"
  # merge it with data_perf
  df_perf <- inner_join(df_info, df_perf, by=c("COUNTY_CODE", "DISTRICT_CODE", "SCHOOL_CODE", "CHARTER_NUM", "YEAR"))
  return(df_perf)
}

# Read in Each Dataset --------------------------------------------------------------------------------------------------------
# dataset: performance
data_perf_16 <- read_csv("data/sb_ca2016_1_csv_v3.txt")
# dataset: info
data_info_16 <- read_csv("data/sb_ca2016entities_csv.txt")
# dataset: performance
data_perf_15 <- read_csv("data/ca2015_1_csv_v3.txt")
# dataset: info
data_info_15 <- read_csv("data/ca2015entities_csv.txt")
# dataset: performance
data_perf_14 <- read_csv("data/ca2014_1_csv_v2.txt")
# dataset: info
data_info_14 <- read_csv("data/ca2014entities_csv.txt")
# dataset: performance
data_perf_13 <- read_csv("data/ca2013_1_csv_v3.txt")
# dataset: info
data_info_13 <- read_csv("data/ca2013entities_csv.txt")
# dataset: performance
data_perf_12 <- read_csv("data/ca2012_1_csv_v3.txt")
# dataset: info
data_info_12 <- read_csv("data/ca2012entities_csv.txt")
# dataset: performance
data_perf_11 <- read_csv("data/ca2011_1_csv_v3.txt")
# dataset: info
data_info_11 <- read_csv("data/ca2011entities_csv.txt")
# dataset: performance
data_perf_10 <- read_csv("data/ca2010_1_csv_v3.txt")
# dataset: info
data_info_10 <- read_csv("data/ca2010entities_csv.txt")
# dataset: performance
data_perf_09 <- read_csv("data/ca2009_1_csv_v3.txt")
# dataset: info
data_info_09 <- read_csv("data/ca2009entities_csv.txt")
# dataset: performance
data_perf_08 <- read_csv("data/ca2008_1_csv_v3.txt")
# dataset: info
data_info_08 <- read_csv("data/ca2008entities_csv.txt")
# dataset: performance
data_perf_07 <- read_csv("data/CA2007_1_CSV_v3.txt")
# dataset: info
data_info_07 <- read_csv("data/CA2007Entities_CSV.txt")
# dataset: demographics
data_demo_17 <- read_tsv("data/enroll_2017.txt")
# dataset: demographics
data_demo_16 <- read_tsv("data/enroll_2016.txt")
# dataset: demographics
data_demo_15 <- read_tsv("data/enroll_2015.txt")
# dataset: demographics
data_demo_14 <- read_tsv("data/enroll_2014.txt")
# dataset: demographics
data_demo_13 <- read_tsv("data/enroll_2013.txt")
# dataset: demographics
data_demo_12 <- read_tsv("data/enroll_2012.txt")
# dataset: demographics
data_demo_11 <- read_tsv("data/enroll_2011.txt")
# dataset: demographics
data_demo_10 <- read_tsv("data/enroll_2010.txt")
# dataset: demographics
data_demo_09 <- read_tsv("data/enroll_2009.txt")
# dataset: demographics
data_demo_08 <- read_tsv("data/enroll_2008.txt")
# dataset: demographics
data_demo_07 <- read_tsv("data/enroll_2007.txt")
# dataset: demographics
data_demo_06 <- read_tsv("data/enroll_2006.txt")
# dataset: demographics
data_demo_05 <- read_tsv("data/enroll_2005.txt")
# dataset: demographics
data_demo_04 <- read_tsv("data/enroll_2004.txt")
# dataset: demographics
data_demo_03 <- read_tsv("data/enroll_2003.txt")
# dataset: demographics
data_demo_02 <- read_tsv("data/enroll_2002.txt")
# dataset: demographics
data_demo_01 <- read_tsv("data/enroll_2001.txt")
# dataset: demographics
data_demo_00 <- read_tsv("data/enroll_2000.txt")
# dataset: demographics
data_demo_99 <- read_tsv("data/enroll_1999.txt")
# dataset: demographics
data_demo_98 <- read_tsv("data/enroll_1998.txt")
# dataset: demographics
data_demo_97 <- read_tsv("data/enroll_1997.txt")
# dataset: demographics
data_demo_96 <- read_tsv("data/enroll_1996.txt")
# dataset: demographics
data_demo_95 <- read_tsv("data/enroll_1995.txt")
# dataset: demographics
data_demo_94 <- read_tsv("data/enroll_1994.txt")

# Performance --------------------------------------------------------------------------------------------------------
# 2015-2016 -----
# keep wanted columns
data_perf_16 <- data_perf_16 %>% 
                  select(c("County Code", "District Code", "School Code", "Filler", "Test Year", "Total CAASPP Enrollment",
                           "Total Tested At Entity Level", "Test Id", "CAASPP Reported Enrollment", "Students Tested", "Percentage Standard Exceeded",
                           "Percentage Standard Met", "Percentage Standard Nearly Met", "Percentage Standard Not Met",
                           "Students with Scores"))

# change column names
colnames(data_perf_16) <- perf_list_16

# turn * into NA
data_perf_16[data_perf_16 == "*"] <- NA
data_perf_16[data_perf_16 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_16 <- data_perf_16 %>% 
                  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_16$TOTAL_ENROLL <- as.numeric(data_perf_16$TOTAL_ENROLL)
data_perf_16$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_16$TOTAL_TESTED_ENTITY)
data_perf_16$TOTAL_REPORTED <- as.numeric(data_perf_16$TOTAL_REPORTED)
data_perf_16$NUM_STUDENTS_TESTED <- as.numeric(data_perf_16$NUM_STUDENTS_TESTED)
data_perf_16$ADV_PCT <- as.numeric(data_perf_16$ADV_PCT)
data_perf_16$PROF_PCT <- as.numeric(data_perf_16$PROF_PCT)
data_perf_16$BASIC_PCT <- as.numeric(data_perf_16$BASIC_PCT)
data_perf_16$BELOW_BASIC_PCT <- as.numeric(data_perf_16$BELOW_BASIC_PCT)
data_perf_16$STUDENTS_SCORED <- as.numeric(data_perf_16$STUDENTS_SCORED)

# create the math columns
data_perf_16_m <- data_perf_16 %>% 
  filter(TEST_ID == 2)
# get rid of TEST_ID
data_perf_16_m$TEST_ID <- NULL
# combine rows of school together
data_perf_16_m <- data_perf_16_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_16_m) <- perf_list_16b
# turn NaN to NAs
data_perf_16_m[data_perf_16_m == 'NaN'] <- NA
# round the PCT columns
data_perf_16_m$ADV_PCT <- round(data_perf_16_m$ADV_PCT, digits = 3)
data_perf_16_m$PROF_PCT <- round(data_perf_16_m$PROF_PCT, digits = 3)
data_perf_16_m$BASIC_PCT <- round(data_perf_16_m$BASIC_PCT, digits = 3)
data_perf_16_m$BELOW_BASIC_PCT <- round(data_perf_16_m$BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_16_m)) {
  colnames(data_perf_16_m)[i] <- paste('MATH', colnames(data_perf_16_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_16_e <- data_perf_16 %>% 
  filter(TEST_ID == 1)
# get rid of TEST_ID
data_perf_16_e$TEST_ID <- NULL
# combine rows of school together
data_perf_16_e <- data_perf_16_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_16_e) <- perf_list_16b
# turn NaN to NAs
data_perf_16_e[data_perf_16_e == 'NaN'] <- NA
# round the PCT columns
data_perf_16_e$ADV_PCT <- round(data_perf_16_e$ADV_PCT, digits = 3)
data_perf_16_e$PROF_PCT <- round(data_perf_16_e$PROF_PCT, digits = 3)
data_perf_16_e$BASIC_PCT <- round(data_perf_16_e$BASIC_PCT, digits = 3)
data_perf_16_e$BELOW_BASIC_PCT <- round(data_perf_16_e$BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_16_e)) {
  colnames(data_perf_16_e)[i] <- paste('ELA', colnames(data_perf_16_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_16 <- full_join(data_perf_16_m, data_perf_16_e)

# add info
data_perf_16 <- add_info(data_info_16, data_perf_16)

# last clean
data_perf_16[data_perf_16 == ","] <- ""
data_perf_16[data_perf_16 == "~"] <- "-"
data_perf_16[data_perf_16 == "ñ"] <- "n"
data_perf_16[data_perf_16 == "é"] <- "e"
data_perf_16[is.na(data_perf_16)] <- -99

# write .csv file
write.csv(data_perf_16, "cleaned_data/ca_perf_2016.csv", row.names = FALSE)


# 2014-2015 -----
# keep wanted columns
data_perf_15 <- data_perf_15 %>% 
  select(c("County Code", "District Code", "School Code", "filler", "Test Year", 
           "Total Tested At Entity Level", "Test Id", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_15) <- perf_list_1415

# turn * into NA
data_perf_15[data_perf_15 == "*"] <- NA
data_perf_15[data_perf_15 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_15 <- data_perf_15 %>% 
  filter(COUNTY_CODE != '00')

# create the ela columns
data_perf_15 <- data_perf_15 %>% 
  filter(TEST_ID == 38)
# get rid of TEST_ID
data_perf_15$TEST_ID <- NULL
# combine rows of school together
data_perf_15 <- data_perf_15 %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_TESTED_ENTITY, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_15) <- perf_list_1415b
# turn NaN to NAs
data_perf_15[data_perf_15 == 'NaN'] <- NA
# round the PCT columns
data_perf_15$ADV_PCT <- round(data_perf_15$ADV_PCT, digits = 3)
data_perf_15$PROF_PCT <- round(data_perf_15$PROF_PCT, digits = 3)
data_perf_15$BASIC_PCT <- round(data_perf_15$BASIC_PCT, digits = 3)
data_perf_15$BELOW_BASIC_PCT <- round(data_perf_15$BELOW_BASIC_PCT, digits = 3)
data_perf_15$FAR_BELOW_BASIC_PCT <- round(data_perf_15$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_15)) {
  colnames(data_perf_15)[i] <- paste('ELA', colnames(data_perf_15)[i], sep="_")
  next
}

# add info
data_perf_15 <- add_info(data_info_15, data_perf_15)

# last clean
data_perf_15[data_perf_15 == ","] <- ""
data_perf_15[data_perf_15 == "~"] <- "-"
data_perf_15[data_perf_15 == "ñ"] <- "n"
data_perf_15[data_perf_15 == "é"] <- "e"
data_perf_15[is.na(data_perf_15)] <- -99

# merge them
data_perf <- full_join(data_perf_16, data_perf_15)

# write .csv file
write.csv(data_perf_15, "cleaned_data/ca_perf_2015.csv", row.names = FALSE)


# 2013-2014 -----
# keep wanted columns
data_perf_14 <- data_perf_14 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", 
           "Total Tested At Entity Level", "Test Id", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_14) <- perf_list_1415

# turn * into NA
data_perf_14[data_perf_14 == "*"] <- NA
data_perf_14[data_perf_14 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_14 <- data_perf_14 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_14$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_14$TOTAL_TESTED_ENTITY)
data_perf_14$NUM_STUDENTS_TESTED <- as.numeric(data_perf_14$NUM_STUDENTS_TESTED)
data_perf_14$ADV_PCT <- as.numeric(data_perf_14$ADV_PCT)
data_perf_14$PROF_PCT <- as.numeric(data_perf_14$PROF_PCT)
data_perf_14$BASIC_PCT <- as.numeric(data_perf_14$BASIC_PCT)
data_perf_14$BELOW_BASIC_PCT <- as.numeric(data_perf_14$BELOW_BASIC_PCT)
data_perf_14$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_14$FAR_BELOW_BASIC_PCT)
data_perf_14$STUDENTS_SCORED <- as.numeric(data_perf_14$STUDENTS_SCORED)

# create the math columns
data_perf_14_m <- data_perf_14 %>% 
  filter(TEST_ID == 31)
# get rid of TEST_ID
data_perf_14_m$TEST_ID <- NULL
# combine rows of school together
data_perf_14_m <- data_perf_14_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_TESTED_ENTITY, na.rm = TRUE), 
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_14_m) <- perf_list_1415b
# turn NaN to NAs
data_perf_14_m[data_perf_14_m == 'NaN'] <- NA
# round the PCT columns
data_perf_14_m$ADV_PCT <- round(data_perf_14_m$ADV_PCT, digits = 3)
data_perf_14_m$PROF_PCT <- round(data_perf_14_m$PROF_PCT, digits = 3)
data_perf_14_m$BASIC_PCT <- round(data_perf_14_m$BASIC_PCT, digits = 3)
data_perf_14_m$BELOW_BASIC_PCT <- round(data_perf_14_m$BELOW_BASIC_PCT, digits = 3)
data_perf_14_m$FAR_BELOW_BASIC_PCT <- round(data_perf_14_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_14_m)) {
  colnames(data_perf_14_m)[i] <- paste('MATH', colnames(data_perf_14_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_14_e <- data_perf_14 %>% 
  filter(TEST_ID == c(30,38))
# get rid of TEST_ID
data_perf_14_e$TEST_ID <- NULL
# combine rows of school together
data_perf_14_e <- data_perf_14_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_TESTED_ENTITY, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_14_e) <- perf_list_1415b
# turn NaN to NAs
data_perf_14_e[data_perf_14_e == 'NaN'] <- NA
# round the PCT columns
data_perf_14_e$ADV_PCT <- round(data_perf_14_e$ADV_PCT, digits = 3)
data_perf_14_e$PROF_PCT <- round(data_perf_14_e$PROF_PCT, digits = 3)
data_perf_14_e$BASIC_PCT <- round(data_perf_14_e$BASIC_PCT, digits = 3)
data_perf_14_e$BELOW_BASIC_PCT <- round(data_perf_14_e$BELOW_BASIC_PCT, digits = 3)
data_perf_14_e$FAR_BELOW_BASIC_PCT <- round(data_perf_14_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_14_e)) {
  colnames(data_perf_14_e)[i] <- paste('ELA', colnames(data_perf_14_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_14 <- full_join(data_perf_14_m, data_perf_14_e)

# add info
data_perf_14 <- add_info(data_info_14, data_perf_14)

# last clean
data_perf_14[data_perf_14 == ","] <- ""
data_perf_14[data_perf_14 == "~"] <- "-"
data_perf_14[data_perf_14 == "ñ"] <- "n"
data_perf_14[data_perf_14 == "é"] <- "e"
data_perf_14[is.na(data_perf_14)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_14)

# write .csv file
write.csv(data_perf_14, "cleaned_data/ca_perf_2014.csv", row.names = FALSE)


# 2012-2013 -----
# keep wanted columns
data_perf_13 <- data_perf_13 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_13) <- perf_list_0913

# turn * into NA
data_perf_13[data_perf_13 == "*"] <- NA
data_perf_13[data_perf_13 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_13 <- data_perf_13 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_13$TOTAL_ENROLL <- as.numeric(data_perf_13$TOTAL_ENROLL)
data_perf_13$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_13$TOTAL_TESTED_ENTITY)
data_perf_13$TOTAL_REPORTED <- as.numeric(data_perf_13$TOTAL_REPORTED)
data_perf_13$NUM_STUDENTS_TESTED <- as.numeric(data_perf_13$NUM_STUDENTS_TESTED)
data_perf_13$ADV_PCT <- as.numeric(data_perf_13$ADV_PCT)
data_perf_13$PROF_PCT <- as.numeric(data_perf_13$PROF_PCT)
data_perf_13$BASIC_PCT <- as.numeric(data_perf_13$BASIC_PCT)
data_perf_13$BELOW_BASIC_PCT <- as.numeric(data_perf_13$BELOW_BASIC_PCT)
data_perf_13$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_13$FAR_BELOW_BASIC_PCT)
data_perf_13$STUDENTS_SCORED <- as.numeric(data_perf_13$STUDENTS_SCORED)

# create the math columns
data_perf_13_m <- data_perf_13 %>% 
  filter(TEST_ID == c(8,9,10,11,12,13,14,15,28,31,39,45,47,48,49,50))
# get rid of TEST_ID
data_perf_13_m$TEST_ID <- NULL
# combine rows of school together
data_perf_13_m <- data_perf_13_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_13_m) <- perf_list_0913b
# turn NaN to NAs
data_perf_13_m[data_perf_13_m == 'NaN'] <- NA
# round the PCT columns
data_perf_13_m$ADV_PCT <- round(data_perf_13_m$ADV_PCT, digits = 3)
data_perf_13_m$PROF_PCT <- round(data_perf_13_m$PROF_PCT, digits = 3)
data_perf_13_m$BASIC_PCT <- round(data_perf_13_m$BASIC_PCT, digits = 3)
data_perf_13_m$BELOW_BASIC_PCT <- round(data_perf_13_m$BELOW_BASIC_PCT, digits = 3)
data_perf_13_m$FAR_BELOW_BASIC_PCT <- round(data_perf_13_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_13_m)) {
  colnames(data_perf_13_m)[i] <- paste('MATH', colnames(data_perf_13_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_13_e <- data_perf_13 %>% 
  filter(TEST_ID == c(7,30,38,44))
# get rid of TEST_ID
data_perf_13_e$TEST_ID <- NULL
# combine rows of school together
data_perf_13_e <- data_perf_13_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_13_e) <- perf_list_0913b
# turn NaN to NAs
data_perf_13_e[data_perf_13_e == 'NaN'] <- NA
# round the PCT columns
data_perf_13_e$ADV_PCT <- round(data_perf_13_e$ADV_PCT, digits = 3)
data_perf_13_e$PROF_PCT <- round(data_perf_13_e$PROF_PCT, digits = 3)
data_perf_13_e$BASIC_PCT <- round(data_perf_13_e$BASIC_PCT, digits = 3)
data_perf_13_e$BELOW_BASIC_PCT <- round(data_perf_13_e$BELOW_BASIC_PCT, digits = 3)
data_perf_13_e$FAR_BELOW_BASIC_PCT <- round(data_perf_13_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_13_e)) {
  colnames(data_perf_13_e)[i] <- paste('ELA', colnames(data_perf_13_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_13 <- full_join(data_perf_13_m, data_perf_13_e)

# add info
data_perf_13 <- add_info(data_info_13, data_perf_13)

# last clean
data_perf_13[data_perf_13 == ","] <- ""
data_perf_13[data_perf_13 == "~"] <- "-"
data_perf_13[data_perf_13 == "ñ"] <- "n"
data_perf_13[data_perf_13 == "é"] <- "e"
data_perf_13[is.na(data_perf_13)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_13)

# write .csv file
write.csv(data_perf_13, "cleaned_data/ca_perf_2013.csv", row.names = FALSE)


# 2011-2012 -----
# keep wanted columns
data_perf_12 <- data_perf_12 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_12) <- perf_list_0913

# turn * into NA
data_perf_12[data_perf_12 == "*"] <- NA
data_perf_12[data_perf_12 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_12 <- data_perf_12 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_12$TOTAL_ENROLL <- as.numeric(data_perf_12$TOTAL_ENROLL)
data_perf_12$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_12$TOTAL_TESTED_ENTITY)
data_perf_12$TOTAL_REPORTED <- as.numeric(data_perf_12$TOTAL_REPORTED)
data_perf_12$NUM_STUDENTS_TESTED <- as.numeric(data_perf_12$NUM_STUDENTS_TESTED)
data_perf_12$ADV_PCT <- as.numeric(data_perf_12$ADV_PCT)
data_perf_12$PROF_PCT <- as.numeric(data_perf_12$PROF_PCT)
data_perf_12$BASIC_PCT <- as.numeric(data_perf_12$BASIC_PCT)
data_perf_12$BELOW_BASIC_PCT <- as.numeric(data_perf_12$BELOW_BASIC_PCT)
data_perf_12$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_12$FAR_BELOW_BASIC_PCT)
data_perf_12$STUDENTS_SCORED <- as.numeric(data_perf_12$STUDENTS_SCORED)

# create the math columns
data_perf_12_m <- data_perf_12 %>% 
  filter(TEST_ID == c(8,9,10,11,12,13,14,15,28,31,39,45,47,48,49,50))
# get rid of TEST_ID
data_perf_12_m$TEST_ID <- NULL
# combine rows of school together
data_perf_12_m <- data_perf_12_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_12_m) <- perf_list_0913b
# turn NaN to NAs
data_perf_12_m[data_perf_12_m == 'NaN'] <- NA
# round the PCT columns
data_perf_12_m$ADV_PCT <- round(data_perf_12_m$ADV_PCT, digits = 3)
data_perf_12_m$PROF_PCT <- round(data_perf_12_m$PROF_PCT, digits = 3)
data_perf_12_m$BASIC_PCT <- round(data_perf_12_m$BASIC_PCT, digits = 3)
data_perf_12_m$BELOW_BASIC_PCT <- round(data_perf_12_m$BELOW_BASIC_PCT, digits = 3)
data_perf_12_m$FAR_BELOW_BASIC_PCT <- round(data_perf_12_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_12_m)) {
  colnames(data_perf_12_m)[i] <- paste('MATH', colnames(data_perf_12_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_12_e <- data_perf_12 %>% 
  filter(TEST_ID == c(7,30,38,44))
# get rid of TEST_ID
data_perf_12_e$TEST_ID <- NULL
# combine rows of school together
data_perf_12_e <- data_perf_12_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_12_e) <- perf_list_0913b
# turn NaN to NAs
data_perf_12_e[data_perf_12_e == 'NaN'] <- NA
# round the PCT columns
data_perf_12_e$ADV_PCT <- round(data_perf_12_e$ADV_PCT, digits = 3)
data_perf_12_e$PROF_PCT <- round(data_perf_12_e$PROF_PCT, digits = 3)
data_perf_12_e$BASIC_PCT <- round(data_perf_12_e$BASIC_PCT, digits = 3)
data_perf_12_e$BELOW_BASIC_PCT <- round(data_perf_12_e$BELOW_BASIC_PCT, digits = 3)
data_perf_12_e$FAR_BELOW_BASIC_PCT <- round(data_perf_12_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_12_e)) {
  colnames(data_perf_12_e)[i] <- paste('ELA', colnames(data_perf_12_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_12 <- full_join(data_perf_12_m, data_perf_12_e)

# add info
data_perf_12 <- add_info(data_info_12, data_perf_12)

# last clean
data_perf_12[data_perf_12 == ","] <- ""
data_perf_12[data_perf_12 == "~"] <- "-"
data_perf_12[data_perf_12 == "ñ"] <- "n"
data_perf_12[data_perf_12 == "é"] <- "e"
data_perf_12[is.na(data_perf_12)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_12)

# write .csv file
write.csv(data_perf_12, "cleaned_data/ca_perf_2012.csv", row.names = FALSE)


# 2010-2011 -----
# keep wanted columns
data_perf_11 <- data_perf_11 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_11) <- perf_list_0913

# turn * into NA
data_perf_11[data_perf_11 == "*"] <- NA
data_perf_11[data_perf_11 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_11 <- data_perf_11 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_11$TOTAL_ENROLL <- as.numeric(data_perf_11$TOTAL_ENROLL)
data_perf_11$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_11$TOTAL_TESTED_ENTITY)
data_perf_11$TOTAL_REPORTED <- as.numeric(data_perf_11$TOTAL_REPORTED)
data_perf_11$NUM_STUDENTS_TESTED <- as.numeric(data_perf_11$NUM_STUDENTS_TESTED)
data_perf_11$ADV_PCT <- as.numeric(data_perf_11$ADV_PCT)
data_perf_11$PROF_PCT <- as.numeric(data_perf_11$PROF_PCT)
data_perf_11$BASIC_PCT <- as.numeric(data_perf_11$BASIC_PCT)
data_perf_11$BELOW_BASIC_PCT <- as.numeric(data_perf_11$BELOW_BASIC_PCT)
data_perf_11$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_11$FAR_BELOW_BASIC_PCT)
data_perf_11$STUDENTS_SCORED <- as.numeric(data_perf_11$STUDENTS_SCORED)

# create the math columns
data_perf_11_m <- data_perf_11 %>% 
  filter(TEST_ID == c(8,9,10,11,12,13,14,15,28,31,39))
# get rid of TEST_ID
data_perf_11_m$TEST_ID <- NULL
# combine rows of school together
data_perf_11_m <- data_perf_11_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_11_m) <- perf_list_0913b
# turn NaN to NAs
data_perf_11_m[data_perf_11_m == 'NaN'] <- NA
# round the PCT columns
data_perf_09_m$ADV_PCT <- round(data_perf_09_m$ADV_PCT, digits = 3)
data_perf_09_m$PROF_PCT <- round(data_perf_09_m$PROF_PCT, digits = 3)
data_perf_09_m$BASIC_PCT <- round(data_perf_09_m$BASIC_PCT, digits = 3)
data_perf_09_m$BELOW_BASIC_PCT <- round(data_perf_09_m$BELOW_BASIC_PCT, digits = 3)
data_perf_09_m$FAR_BELOW_BASIC_PCT <- round(data_perf_09_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_11_m)) {
  colnames(data_perf_11_m)[i] <- paste('MATH', colnames(data_perf_11_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_11_e <- data_perf_11 %>% 
  filter(TEST_ID == c(7,30,38,44))
# get rid of TEST_ID
data_perf_11_e$TEST_ID <- NULL
# combine rows of school together
data_perf_11_e <- data_perf_11_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_11_e) <- perf_list_0913b
# turn NaN to NAs
data_perf_11_e[data_perf_11_e == 'NaN'] <- NA
# round the PCT columns
data_perf_11_e$ADV_PCT <- round(data_perf_11_e$ADV_PCT, digits = 3)
data_perf_11_e$PROF_PCT <- round(data_perf_11_e$PROF_PCT, digits = 3)
data_perf_11_e$BASIC_PCT <- round(data_perf_11_e$BASIC_PCT, digits = 3)
data_perf_11_e$BELOW_BASIC_PCT <- round(data_perf_11_e$BELOW_BASIC_PCT, digits = 3)
data_perf_11_e$FAR_BELOW_BASIC_PCT <- round(data_perf_11_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_11_e)) {
  colnames(data_perf_11_e)[i] <- paste('ELA', colnames(data_perf_11_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_11 <- full_join(data_perf_11_m, data_perf_11_e)

# add info
data_perf_11 <- add_info(data_info_11, data_perf_11)

# last clean
data_perf_11[data_perf_11 == ","] <- ""
data_perf_11[data_perf_11 == "~"] <- "-"
data_perf_11[data_perf_11 == "ñ"] <- "n"
data_perf_11[data_perf_11 == "é"] <- "e"
data_perf_11[is.na(data_perf_11)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_11)

# write .csv file
write.csv(data_perf_11, "cleaned_data/ca_perf_2011.csv", row.names = FALSE)


# 2009-2010 -----
# keep wanted columns
data_perf_10 <- data_perf_10 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_10) <- perf_list_0913

# turn * into NA
data_perf_10[data_perf_10 == "*"] <- NA
data_perf_10[data_perf_10 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_10 <- data_perf_10 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_10$TOTAL_ENROLL <- as.numeric(data_perf_10$TOTAL_ENROLL)
data_perf_10$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_10$TOTAL_TESTED_ENTITY)
data_perf_10$TOTAL_REPORTED <- as.numeric(data_perf_10$TOTAL_REPORTED)
data_perf_10$NUM_STUDENTS_TESTED <- as.numeric(data_perf_10$NUM_STUDENTS_TESTED)
data_perf_10$ADV_PCT <- as.numeric(data_perf_10$ADV_PCT)
data_perf_10$PROF_PCT <- as.numeric(data_perf_10$PROF_PCT)
data_perf_10$BASIC_PCT <- as.numeric(data_perf_10$BASIC_PCT)
data_perf_10$BELOW_BASIC_PCT <- as.numeric(data_perf_10$BELOW_BASIC_PCT)
data_perf_10$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_10$FAR_BELOW_BASIC_PCT)
data_perf_10$STUDENTS_SCORED <- as.numeric(data_perf_10$STUDENTS_SCORED)

# create the math columns
data_perf_10_m <- data_perf_10 %>% 
  filter(TEST_ID == c(8,9,10,11,12,13,14,15,28,31,39,45,47,48,49))
# get rid of TEST_ID
data_perf_10_m$TEST_ID <- NULL
# combine rows of school together
data_perf_10_m <- data_perf_10_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_10_m) <- perf_list_0913b
# turn NaN to NAs
data_perf_10_m[data_perf_10_m == 'NaN'] <- NA
# round the PCT columns
data_perf_10_m$ADV_PCT <- round(data_perf_10_m$ADV_PCT, digits = 3)
data_perf_10_m$PROF_PCT <- round(data_perf_10_m$PROF_PCT, digits = 3)
data_perf_10_m$BASIC_PCT <- round(data_perf_10_m$BASIC_PCT, digits = 3)
data_perf_10_m$BELOW_BASIC_PCT <- round(data_perf_10_m$BELOW_BASIC_PCT, digits = 3)
data_perf_10_m$FAR_BELOW_BASIC_PCT <- round(data_perf_10_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_10_m)) {
  colnames(data_perf_10_m)[i] <- paste('MATH', colnames(data_perf_10_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_10_e <- data_perf_10 %>% 
  filter(TEST_ID == c(7,30,38,44))
# get rid of TEST_ID
data_perf_10_e$TEST_ID <- NULL
# combine rows of school together
data_perf_10_e <- data_perf_10_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_10_e) <- perf_list_0913b
# turn NaN to NAs
data_perf_10_e[data_perf_10_e == 'NaN'] <- NA
# round the PCT columns
data_perf_10_e$ADV_PCT <- round(data_perf_10_e$ADV_PCT, digits = 3)
data_perf_10_e$PROF_PCT <- round(data_perf_10_e$PROF_PCT, digits = 3)
data_perf_10_e$BASIC_PCT <- round(data_perf_10_e$BASIC_PCT, digits = 3)
data_perf_10_e$BELOW_BASIC_PCT <- round(data_perf_10_e$BELOW_BASIC_PCT, digits = 3)
data_perf_10_e$FAR_BELOW_BASIC_PCT <- round(data_perf_10_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_10_e)) {
  colnames(data_perf_10_e)[i] <- paste('ELA', colnames(data_perf_10_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_10 <- full_join(data_perf_10_m, data_perf_10_e)

# add info
data_perf_10 <- add_info(data_info_10, data_perf_10)

# last clean
data_perf_10[data_perf_10 == ","] <- ""
data_perf_10[data_perf_10 == "~"] <- "-"
data_perf_10[data_perf_10 == "ñ"] <- "n"
data_perf_10[data_perf_10 == "é"] <- "e"
data_perf_10[is.na(data_perf_10)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_10)

# write .csv file
write.csv(data_perf_10, "cleaned_data/ca_perf_2010.csv", row.names = FALSE)


# 2008-2009 -----
# keep wanted columns
data_perf_09 <- data_perf_09 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Test Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "Percentage Advanced",
           "Percentage Proficient", "Percentage Basic", "Percentage Below Basic", "Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_09) <- perf_list_0913

# turn * into NA
data_perf_09[data_perf_09 == "*"] <- NA
data_perf_09[data_perf_09 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_09 <- data_perf_09 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_09$TOTAL_ENROLL <- as.numeric(data_perf_09$TOTAL_ENROLL)
data_perf_09$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_09$TOTAL_TESTED_ENTITY)
data_perf_09$TOTAL_REPORTED <- as.numeric(data_perf_09$TOTAL_REPORTED)
data_perf_09$NUM_STUDENTS_TESTED <- as.numeric(data_perf_09$NUM_STUDENTS_TESTED)
data_perf_09$ADV_PCT <- as.numeric(data_perf_09$ADV_PCT)
data_perf_09$PROF_PCT <- as.numeric(data_perf_09$PROF_PCT)
data_perf_09$BASIC_PCT <- as.numeric(data_perf_09$BASIC_PCT)
data_perf_09$BELOW_BASIC_PCT <- as.numeric(data_perf_09$BELOW_BASIC_PCT)
data_perf_09$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_09$FAR_BELOW_BASIC_PCT)
data_perf_09$STUDENTS_SCORED <- as.numeric(data_perf_09$STUDENTS_SCORED)

# create the math columns
data_perf_09_m <- data_perf_09 %>% 
  filter(TEST_ID == c(2,8,10,11,12,13,14,15,28,31,39))
# get rid of TEST_ID
data_perf_09_m$TEST_ID <- NULL
# combine rows of school together
data_perf_09_m <- data_perf_09_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_09_m) <- perf_list_0913b
# turn NaN to NAs
data_perf_09_m[data_perf_09_m == 'NaN'] <- NA
# round the PCT columns
data_perf_09_m$ADV_PCT <- round(data_perf_09_m$ADV_PCT, digits = 3)
data_perf_09_m$PROF_PCT <- round(data_perf_09_m$PROF_PCT, digits = 3)
data_perf_09_m$BASIC_PCT <- round(data_perf_09_m$BASIC_PCT, digits = 3)
data_perf_09_m$BELOW_BASIC_PCT <- round(data_perf_09_m$BELOW_BASIC_PCT, digits = 3)
data_perf_09_m$FAR_BELOW_BASIC_PCT <- round(data_perf_09_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_09_m)) {
  colnames(data_perf_09_m)[i] <- paste('MATH', colnames(data_perf_09_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_09_e <- data_perf_09 %>% 
  filter(TEST_ID == c(1,3,7,30,38))
# get rid of TEST_ID
data_perf_09_e$TEST_ID <- NULL
# combine rows of school together
data_perf_09_e <- data_perf_09_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_09_e) <- perf_list_0913b
# turn NaN to NAs
data_perf_09_e[data_perf_09_e == 'NaN'] <- NA
# round the PCT columns
data_perf_09_e$ADV_PCT <- round(data_perf_09_e$ADV_PCT, digits = 3)
data_perf_09_e$PROF_PCT <- round(data_perf_09_e$PROF_PCT, digits = 3)
data_perf_09_e$BASIC_PCT <- round(data_perf_09_e$BASIC_PCT, digits = 3)
data_perf_09_e$BELOW_BASIC_PCT <- round(data_perf_09_e$BELOW_BASIC_PCT, digits = 3)
data_perf_09_e$FAR_BELOW_BASIC_PCT <- round(data_perf_09_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_09_e)) {
  colnames(data_perf_09_e)[i] <- paste('ELA', colnames(data_perf_09_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_09 <- full_join(data_perf_09_m, data_perf_09_e)

# add info
data_perf_09 <- add_info(data_info_09, data_perf_09)

# last clean
data_perf_09[data_perf_09 == ","] <- ""
data_perf_09[data_perf_09 == "~"] <- "-"
data_perf_09[data_perf_09 == "ñ"] <- "n"
data_perf_09[data_perf_09 == "é"] <- "e"
data_perf_09[is.na(data_perf_09)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_09)

# write .csv file
write.csv(data_perf_09, "cleaned_data/ca_perf_2009.csv", row.names = FALSE)


# 2007-2008 -----
# keep wanted columns
data_perf_08 <- data_perf_08 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "CST/CAPA Percentage Advanced",
           "CST/CAPA Percentage Proficient", "CST/CAPA Percentage Basic", "CST/CAPA Percentage Below Basic", "CST/CAPA Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_08) <- perf_list_0708

# turn * into NA
data_perf_08[data_perf_08 == "*"] <- NA
data_perf_08[data_perf_08 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_08 <- data_perf_08 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_08$TOTAL_ENROLL <- as.numeric(data_perf_08$TOTAL_ENROLL)
data_perf_08$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_08$TOTAL_TESTED_ENTITY)
data_perf_08$TOTAL_REPORTED <- as.numeric(data_perf_08$TOTAL_REPORTED)
data_perf_08$NUM_STUDENTS_TESTED <- as.numeric(data_perf_08$NUM_STUDENTS_TESTED)
data_perf_08$ADV_PCT <- as.numeric(data_perf_08$ADV_PCT)
data_perf_08$PROF_PCT <- as.numeric(data_perf_08$PROF_PCT)
data_perf_08$BASIC_PCT <- as.numeric(data_perf_08$BASIC_PCT)
data_perf_08$BELOW_BASIC_PCT <- as.numeric(data_perf_08$BELOW_BASIC_PCT)
data_perf_08$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_08$FAR_BELOW_BASIC_PCT)
data_perf_08$STUDENTS_SCORED <- as.numeric(data_perf_08$STUDENTS_SCORED)

# create the math columns
data_perf_08_m <- data_perf_08 %>% 
  filter(TEST_ID == c(2,8,10,11,12,13,14,15,28,31,39,45))
# get rid of TEST_ID
data_perf_08_m$TEST_ID <- NULL
# combine rows of school together
data_perf_08_m <- data_perf_08_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_08_m) <- perf_list_0708b
# turn NaN to NAs
data_perf_08_m[data_perf_08_m == 'NaN'] <- NA
# round the PCT columns
data_perf_08_m$ADV_PCT <- round(data_perf_08_m$ADV_PCT, digits = 3)
data_perf_08_m$PROF_PCT <- round(data_perf_08_m$PROF_PCT, digits = 3)
data_perf_08_m$BASIC_PCT <- round(data_perf_08_m$BASIC_PCT, digits = 3)
data_perf_08_m$BELOW_BASIC_PCT <- round(data_perf_08_m$BELOW_BASIC_PCT, digits = 3)
data_perf_08_m$FAR_BELOW_BASIC_PCT <- round(data_perf_08_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_08_m)) {
  colnames(data_perf_08_m)[i] <- paste('MATH', colnames(data_perf_08_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_08_e <- data_perf_08 %>% 
  filter(TEST_ID == c(1,3,7,30,38,44))
# get rid of TEST_ID
data_perf_08_e$TEST_ID <- NULL
# combine rows of school together
data_perf_08_e <- data_perf_08_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_08_e) <- perf_list_0708b
# turn NaN to NAs
data_perf_08_e[data_perf_08_e == 'NaN'] <- NA
# round the PCT columns
data_perf_08_e$ADV_PCT <- round(data_perf_08_e$ADV_PCT, digits = 3)
data_perf_08_e$PROF_PCT <- round(data_perf_08_e$PROF_PCT, digits = 3)
data_perf_08_e$BASIC_PCT <- round(data_perf_08_e$BASIC_PCT, digits = 3)
data_perf_08_e$BELOW_BASIC_PCT <- round(data_perf_08_e$BELOW_BASIC_PCT, digits = 3)
data_perf_08_e$FAR_BELOW_BASIC_PCT <- round(data_perf_08_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_08_e)) {
  colnames(data_perf_08_e)[i] <- paste('ELA', colnames(data_perf_08_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_08 <- full_join(data_perf_08_m, data_perf_08_e)

# add info
data_perf_08 <- add_info(data_info_08, data_perf_08)

# last clean
data_perf_08[data_perf_08 == ","] <- ""
data_perf_08[data_perf_08 == "~"] <- "-"
data_perf_08[data_perf_08 == "ñ"] <- "n"
data_perf_08[data_perf_08 == "é"] <- "e"
data_perf_08[is.na(data_perf_08)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_08)

# write .csv file
write.csv(data_perf_08, "cleaned_data/ca_perf_2008.csv", row.names = FALSE)


# 2006-2007 -----
# keep wanted columns
data_perf_07 <- data_perf_07 %>% 
  select(c("County Code", "District Code", "School Code", "Charter Number", "Year", "Total STAR Enrollment",
           "Total Tested At Entity Level", "Test Id", "STAR Reported Enrollment/CAPA Eligible", "Students Tested", "CST/CAPA Percentage Advanced",
           "CST/CAPA Percentage Proficient", "CST/CAPA Percentage Basic", "CST/CAPA Percentage Below Basic", "CST/CAPA Percentage Far Below Basic",
           "Students with Scores"))

# change column names
colnames(data_perf_07) <- perf_list_0708

# turn * into NA
data_perf_07[data_perf_07 == "*"] <- NA
data_perf_07[data_perf_07 == "-Inf"] <- NA

# get rid of statewide rows
data_perf_07 <- data_perf_07 %>% 
  filter(COUNTY_CODE != '00')

# make sure its numeric
data_perf_07$TOTAL_ENROLL <- as.numeric(data_perf_07$TOTAL_ENROLL)
data_perf_07$TOTAL_TESTED_ENTITY <- as.numeric(data_perf_07$TOTAL_TESTED_ENTITY)
data_perf_07$TOTAL_REPORTED <- as.numeric(data_perf_07$TOTAL_REPORTED)
data_perf_07$NUM_STUDENTS_TESTED <- as.numeric(data_perf_07$NUM_STUDENTS_TESTED)
data_perf_07$ADV_PCT <- as.numeric(data_perf_07$ADV_PCT)
data_perf_07$PROF_PCT <- as.numeric(data_perf_07$PROF_PCT)
data_perf_07$BASIC_PCT <- as.numeric(data_perf_07$BASIC_PCT)
data_perf_07$BELOW_BASIC_PCT <- as.numeric(data_perf_07$BELOW_BASIC_PCT)
data_perf_07$FAR_BELOW_BASIC_PCT <- as.numeric(data_perf_07$FAR_BELOW_BASIC_PCT)
data_perf_07$STUDENTS_SCORED <- as.numeric(data_perf_07$STUDENTS_SCORED)

# create the math columns
data_perf_07_m <- data_perf_07 %>% 
  filter(TEST_ID == c(2,8,10,11,12,13,14,15,28,31,39))
# get rid of TEST_ID
data_perf_07_m$TEST_ID <- NULL
# combine rows of school together
data_perf_07_m <- data_perf_07_m %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_07_m) <- perf_list_0708b
# turn NaN to NAs
data_perf_07_m[data_perf_07_m == 'NaN'] <- NA
# round the PCT columns
data_perf_07_m$ADV_PCT <- round(data_perf_07_m$ADV_PCT, digits = 3)
data_perf_07_m$PROF_PCT <- round(data_perf_07_m$PROF_PCT, digits = 3)
data_perf_07_m$BASIC_PCT <- round(data_perf_07_m$BASIC_PCT, digits = 3)
data_perf_07_m$BELOW_BASIC_PCT <- round(data_perf_07_m$BELOW_BASIC_PCT, digits = 3)
data_perf_07_m$FAR_BELOW_BASIC_PCT <- round(data_perf_07_m$FAR_BELOW_BASIC_PCT, digits = 3)
# add MATH_ in columns' names
for (i in 6:ncol(data_perf_07_m)) {
  colnames(data_perf_07_m)[i] <- paste('MATH', colnames(data_perf_07_m)[i], sep="_")
  next
}

# create the ela columns
data_perf_07_e <- data_perf_07 %>% 
  filter(TEST_ID == c(1,3,7,30,38))
# get rid of TEST_ID
data_perf_07_e$TEST_ID <- NULL
# combine rows of school together
data_perf_07_e <- data_perf_07_e %>% 
  group_by_("COUNTY_CODE","DISTRICT_CODE","SCHOOL_CODE","CHARTER_NUM","YEAR") %>% 
  summarise(max(TOTAL_ENROLL, na.rm = TRUE),max(TOTAL_TESTED_ENTITY, na.rm = TRUE), max(TOTAL_REPORTED, na.rm = TRUE),
            max(NUM_STUDENTS_TESTED, na.rm = TRUE), mean(ADV_PCT, na.rm = TRUE), mean(PROF_PCT, na.rm = TRUE),
            mean(BASIC_PCT, na.rm = TRUE), mean(BELOW_BASIC_PCT, na.rm = TRUE),
            mean(FAR_BELOW_BASIC_PCT, na.rm = TRUE), max(STUDENTS_SCORED, na.rm = TRUE))
# change column names (again)
colnames(data_perf_07_e) <- perf_list_0708b
# turn NaN to NAs
data_perf_07_e[data_perf_07_e == 'NaN'] <- NA
# round the PCT columns
data_perf_07_e$ADV_PCT <- round(data_perf_07_e$ADV_PCT, digits = 3)
data_perf_07_e$PROF_PCT <- round(data_perf_07_e$PROF_PCT, digits = 3)
data_perf_07_e$BASIC_PCT <- round(data_perf_07_e$BASIC_PCT, digits = 3)
data_perf_07_e$BELOW_BASIC_PCT <- round(data_perf_07_e$BELOW_BASIC_PCT, digits = 3)
data_perf_07_e$FAR_BELOW_BASIC_PCT <- round(data_perf_07_e$FAR_BELOW_BASIC_PCT, digits = 3)
# add ELA_ in columns' names
for (i in 6:ncol(data_perf_07_e)) {
  colnames(data_perf_07_e)[i] <- paste('ELA', colnames(data_perf_07_e)[i], sep="_")
  next
}

# combine the math and ela datasets
data_perf_07 <- full_join(data_perf_07_m, data_perf_07_e)

# add info
data_perf_07 <- add_info(data_info_07, data_perf_07)

# last clean
data_perf_07[data_perf_07 == ","] <- ""
data_perf_07[data_perf_07 == "~"] <- "-"
data_perf_07[data_perf_07 == "ñ"] <- "n"
data_perf_07[data_perf_07 == "é"] <- "e"
data_perf_07[is.na(data_perf_07)] <- -99

# merge them
data_perf <- full_join(data_perf, data_perf_07)

# write .csv file
write.csv(data_perf_07, "cleaned_data/ca_perf_2007.csv", row.names = FALSE)


# Finish -----
# change NAs into -99
data_perf[is.na(data_perf)] <- -99

# Save as .csv file
write.csv(data_perf, "cleaned_data/ca_perf_2006_16.csv", row.names = FALSE)
# Demographics --------------------------------------------------------------------------------------------------------
# Notes:  -----
# For 2010-17:
# Code 0 = Not reported
# Code 1 = American Indian or Alaska Native, Not Hispanic
# Code 2 = Asian, Not Hispanic
# Code 3 = Pacific Islander, Not Hispanic
# Code 4 = Filipino, Not Hispanic
# Code 5 = Hispanic or Latino
# Code 6 = African American, not Hispanic
# Code 7 = White, not Hispanic
# Code 9 = Two or More Races, Not Hispanic
#
# For 1998-08:
# Code 1 = American Indian or Alaska Native
# Code 2 = Asian
# Code 3 = Pacific Islander
# Code 4 = Filipino
# Code 5 = Hispanic or Latino
# Code 6 = African American, not Hispanic
# Code 7 = White, not Hispanic
# Code 8 = Multiple or No Response

# 2016-2017 -----
# clean everything
data_demo_17 <- clean_demo_1017(data_demo_17)

# Add YEAR COLUMN
data_demo_17$YEAR <- 2017

# change column order
data_demo_17 <- setcolorder(data_demo_17, demo_order)

# make sure CDS_CODE is numeric
data_demo_17$CDS_CODE <- as.character(data_demo_17$CDS_CODE)

# write .csv file
write.csv(data_demo_17, "cleaned_data/ca_enroll_2017.csv", row.names = FALSE)


# 2015-2016 -----
# clean everything
data_demo_16 <- clean_demo_1017(data_demo_16)

# Add YEAR COLUMN
data_demo_16$YEAR <- 2016

# change column order
data_demo_16 <- setcolorder(data_demo_16, demo_order)

# make sure CDS_CODE is numeric
data_demo_16$CDS_CODE <- as.character(data_demo_16$CDS_CODE)

# merge them
data_demo <- full_join(data_demo_17, data_demo_16)
data_demo$CDS_CODE <- as.character(data_demo$CDS_CODE)

# write .csv file
write.csv(data_demo_16, "cleaned_data/ca_enroll_2016.csv", row.names = FALSE)


# 2014-2015 -----
# clean everything
data_demo_15 <- clean_demo_1017(data_demo_15)

# Add YEAR COLUMN
data_demo_15$YEAR <- 2015

# change column order
data_demo_15 <- setcolorder(data_demo_15, demo_order)

# make sure CDS_CODE is numeric
data_demo_15$CDS_CODE <- as.character(data_demo_15$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_15)

# write .csv file
write.csv(data_demo_15, "cleaned_data/ca_enroll_2015.csv", row.names = FALSE)


# 2013-2014 -----
# clean everything
data_demo_14 <- clean_demo_1017(data_demo_14)

# Add YEAR COLUMN
data_demo_14$YEAR <- 2014

# change column order
data_demo_14 <- setcolorder(data_demo_14, demo_order)

# make sure CDS_CODE is numeric
data_demo_14$CDS_CODE <- as.character(data_demo_14$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_14)

# write .csv file
write.csv(data_demo_14, "cleaned_data/ca_enroll_2014.csv", row.names = FALSE)


# 2012-2013 -----
# read in tab-separated .txt file
data_demo_13 <- read_tsv("data/enroll_2013.txt")

# clean everything
data_demo_13 <- clean_demo_1017(data_demo_13)

# Add YEAR COLUMN
data_demo_13$YEAR <- 2013

# change column order
data_demo_13 <- setcolorder(data_demo_13, demo_order)

# make sure CDS_CODE is numeric
data_demo_13$CDS_CODE <- as.character(data_demo_13$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_13)

# write .csv file
write.csv(data_demo_13, "cleaned_data/ca_enroll_2013.csv", row.names = FALSE)


# 2011-2012 -----
# clean everything
data_demo_12 <- clean_demo_1017(data_demo_12)

# Add YEAR COLUMN
data_demo_12$YEAR <- 2012

# change column order
data_demo_12 <- setcolorder(data_demo_12, demo_order)

# make sure CDS_CODE is numeric
data_demo_12$CDS_CODE <- as.character(data_demo_12$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_12)

# write .csv file
write.csv(data_demo_12, "cleaned_data/ca_enroll_2012.csv", row.names = FALSE)


# 2010-2011 -----
# clean everything
data_demo_11 <- clean_demo_1017(data_demo_11)

# Add YEAR COLUMN
data_demo_11$YEAR <- 2011

# change column order
data_demo_11 <- setcolorder(data_demo_11, demo_order)

# make sure CDS_CODE is numeric
data_demo_11$CDS_CODE <- as.character(data_demo_11$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_11)

# write .csv file
write.csv(data_demo_11, "cleaned_data/ca_enroll_2011.csv", row.names = FALSE)


# 2009-2010 -----
# clean everything
data_demo_10 <- clean_demo_1017(data_demo_10)

# Add YEAR COLUMN
data_demo_10$YEAR <- 2010

# change column order
data_demo_10 <- setcolorder(data_demo_10, demo_order)

# make sure CDS_CODE is numeric
data_demo_10$CDS_CODE <- as.character(data_demo_10$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_10)

# write .csv file
write.csv(data_demo_10, "cleaned_data/ca_enroll_2010.csv", row.names = FALSE)


# 2008-2009 -----
# clean everything
data_demo_09 <- clean_demo_0809(data_demo_09)

# Add YEAR COLUMN
data_demo_09$YEAR <- 2009

# change column order
data_demo_09 <- setcolorder(data_demo_09, demo_order_2)

# make sure CDS_CODE is numeric
data_demo_09$CDS_CODE <- as.character(data_demo_09$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_09)

# write .csv file
write.csv(data_demo_09, "cleaned_data/ca_enroll_2009.csv", row.names = FALSE)


# 2007-2008 -----
# clean everything
data_demo_08 <- clean_demo_0809(data_demo_08)

# Add YEAR COLUMN
data_demo_08$YEAR <- 2008

# change column order
data_demo_08 <- setcolorder(data_demo_08, demo_order_2)

# make sure CDS_CODE is numeric
data_demo_08$CDS_CODE <- as.character(data_demo_08$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_08)

# write .csv file
write.csv(data_demo_08, "cleaned_data/ca_enroll_2008.csv", row.names = FALSE)


# 2006-2007 -----
# clean everything
data_demo_07 <- clean_demo_9907(data_demo_07)

# Add YEAR COLUMN
data_demo_07$YEAR <- 2007

# change column order
data_demo_07 <- setcolorder(data_demo_07, demo_order_3)

# make sure CDS_CODE is character
data_demo_07$CDS_CODE <- as.character(data_demo_07$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_07)

# write .csv file
write.csv(data_demo_07, "cleaned_data/ca_enroll_2007.csv", row.names = FALSE)


# 2005-2006 -----
# clean everything
data_demo_06 <- clean_demo_9907(data_demo_06)

# Add YEAR COLUMN
data_demo_06$YEAR <- 2006

# change column order
data_demo_06 <- setcolorder(data_demo_06, demo_order_3)

# make sure CDS_CODE is character
data_demo_06$CDS_CODE <- as.character(data_demo_06$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_06)

# write .csv file
write.csv(data_demo_06, "cleaned_data/ca_enroll_2006.csv", row.names = FALSE)


# 2004-2005 -----
# clean everything
data_demo_05 <- clean_demo_9907(data_demo_05)

# Add YEAR COLUMN
data_demo_05$YEAR <- 2005

# change column order
data_demo_05 <- setcolorder(data_demo_05, demo_order_3)

# make sure CDS_CODE is character
data_demo_05$CDS_CODE <- as.character(data_demo_05$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_05)

# write .csv file
write.csv(data_demo_05, "cleaned_data/ca_enroll_2005.csv", row.names = FALSE)


# 2003-2004 -----
# clean everything
data_demo_04 <- clean_demo_9907(data_demo_04)

# Add YEAR COLUMN
data_demo_04$YEAR <- 2004

# change column order
data_demo_04 <- setcolorder(data_demo_04, demo_order_3)

# make sure CDS_CODE is character
data_demo_04$CDS_CODE <- as.character(data_demo_04$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_04)

# write .csv file
write.csv(data_demo_04, "cleaned_data/ca_enroll_2004.csv", row.names = FALSE)


# 2002-2003 -----
# clean everything
data_demo_03 <- clean_demo_9907(data_demo_03)

# Add YEAR COLUMN
data_demo_03$YEAR <- 2003

# change column order
data_demo_03 <- setcolorder(data_demo_03, demo_order_3)

# make sure CDS_CODE is character
data_demo_03$CDS_CODE <- as.character(data_demo_03$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_03)

# write .csv file
write.csv(data_demo_03, "cleaned_data/ca_enroll_2003.csv", row.names = FALSE)


# 2001-2002 -----
# clean everything
data_demo_02 <- clean_demo_9907(data_demo_02)

# Add YEAR COLUMN
data_demo_02$YEAR <- 2002

# change column order
data_demo_02 <- setcolorder(data_demo_02, demo_order_3)

# make sure CDS_CODE is character
data_demo_02$CDS_CODE <- as.character(data_demo_02$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_02)

# write .csv file
write.csv(data_demo_02, "cleaned_data/ca_enroll_2002.csv", row.names = FALSE)


# 2000-2001 -----
# clean everything
data_demo_01 <- clean_demo_9907(data_demo_01)

# Add YEAR COLUMN
data_demo_01$YEAR <- 2001

# change column order
data_demo_01 <- setcolorder(data_demo_01, demo_order_3)

# make sure CDS_CODE is character
data_demo_01$CDS_CODE <- as.character(data_demo_01$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_01)

# write .csv file
write.csv(data_demo_01, "cleaned_data/ca_enroll_2001.csv", row.names = FALSE)


# 1999-2000 -----
# clean everything
data_demo_00 <- clean_demo_9907(data_demo_00)

# Add YEAR COLUMN
data_demo_00$YEAR <- 2000

# change column order
data_demo_00 <- setcolorder(data_demo_00, demo_order_3)

# merge them
data_demo <- full_join(data_demo, data_demo_00)

# write .csv file
write.csv(data_demo_00, "cleaned_data/ca_enroll_2000.csv", row.names = FALSE)


# 1998-1999 -----
# clean everything
data_demo_99 <- clean_demo_9907(data_demo_99)

# Add YEAR COLUMN
data_demo_99$YEAR <- 1999

# change column order
data_demo_99 <- setcolorder(data_demo_99, demo_order_3)

# make sure CDS_CODE is character
data_demo_99$CDS_CODE <- as.character(data_demo_99$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_99)

# write .csv file
write.csv(data_demo_99, "cleaned_data/ca_enroll_1999.csv", row.names = FALSE)


# 1997-1998 -----
# clean everything
data_demo_98 <- clean_demo_9498(data_demo_98)

# Add YEAR COLUMN
data_demo_98$YEAR <- 1998

# change column order
data_demo_98 <- setcolorder(data_demo_98, demo_order_4)

# make sure CDS_CODE is character
data_demo_98$CDS_CODE <- as.character(data_demo_98$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_98)

# write .csv file
write.csv(data_demo_98, "cleaned_data/ca_enroll_1998.csv", row.names = FALSE)


# 1996-1997 -----
# clean everything
data_demo_97 <- clean_demo_9498(data_demo_97)

# Add YEAR COLUMN
data_demo_97$YEAR <- 1997

# change column order
data_demo_97 <- setcolorder(data_demo_97, demo_order_4)

# make sure CDS_CODE is character
data_demo_97$CDS_CODE <- as.character(data_demo_97$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_97)

# write .csv file
write.csv(data_demo_97, "cleaned_data/ca_enroll_1997.csv", row.names = FALSE)


# 1995-1996 -----
# clean everything
data_demo_96 <- clean_demo_9498(data_demo_96)

# Add YEAR COLUMN
data_demo_96$YEAR <- 1996

# change column order
data_demo_96 <- setcolorder(data_demo_96, demo_order_4)

# make sure CDS_CODE is character
data_demo_96$CDS_CODE <- as.character(data_demo_96$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_96)

# write .csv file
write.csv(data_demo_96, "cleaned_data/ca_enroll_1996.csv", row.names = FALSE)


# 1994-1995 -----
# clean everything
data_demo_95 <- clean_demo_9498(data_demo_95)

# Add YEAR COLUMN
data_demo_95$YEAR <- 1995

# change column order
data_demo_95 <- setcolorder(data_demo_95, demo_order_4)

# make sure CDS_CODE is character
data_demo_95$CDS_CODE <- as.character(data_demo_95$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_95)

# write .csv file
write.csv(data_demo_95, "cleaned_data/ca_enroll_1995.csv", row.names = FALSE)


# 1993-1994 -----
# clean everything
data_demo_94 <- clean_demo_9498(data_demo_94)

# Add YEAR COLUMN
data_demo_94$YEAR <- 1994

# change column order
data_demo_94 <- setcolorder(data_demo_94, demo_order_4)

# make sure CDS_CODE is character
data_demo_97$CDS_CODE <- as.character(data_demo_97$CDS_CODE)

# merge them
data_demo <- full_join(data_demo, data_demo_94)

# write .csv file
write.csv(data_demo_94, "cleaned_data/ca_enroll_1994.csv", row.names = FALSE)


# Finish -----
# change NAs into -99
data_demo[is.na(data_demo)] <- -99

# Save as .csv file
write.csv(data_demo, "cleaned_data/ca_enroll_1994_17.csv", row.names = FALSE)

# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/ca_perf_enroll_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/ca_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/ca_perf_clean.rds")



