#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Colorado's Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Colorado 
# Created by: Kevin Cha on 08-07-17
# Updated by: Kevin Cha on 08-08-17
# Data from: 
#     Performance: https://www.cde.state.co.us/accountability/performanceframeworkresults
#     Demographics: https://www.cde.state.co.us/cdereval/rvprioryearpmdata
# Codebook:
#     Demographics: 
#     Performance:
# Link to Github: https://github.com/CRPE-UWB/State
# Notes: Performance Frameworks were not produced during 2014-2015 due to a legislative hold on accountability.

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_co_clean") #MAC

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(readr)
library(readxl)

# List --------------------------------------------------------------------------------------------------------
demo_list_10 <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "GRADE", 
                  "AMINDIAN_F", "AMINDIAN_M", "ASIAN_F", "ASIAN_M", "BLACK_F", "BLACK_M",
                  "HISPANIC_F", "HISPANIC_M", "WHITE_F", "WHITE_M", "TOTAL")
demo_list_1116 <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "GRADE",
                    "AMINDIAN_F", "AMINDIAN_M", "ASIAN_F", "ASIAN_M", "BLACK_F", "BLACK_M", 
                    "HISPANIC_F", "HISPANIC_M", "WHITE_F", "WHITE_M", "PACISLAND_F", "PACISLAND_M", 
                    "TWOORMORE_F", "TWOORMORE_M", "TOTAL")

# Functions --------------------------------------------------------------------------------------------------------
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

clean_demo <- function(df, year) {
  # get rid of GRADE column
  df$GRADE <- NULL
  
  # make sure the columns are their right class
  df$DISTRICT_CODE <- as.numeric(df$DISTRICT_CODE)
  df$DISTRICT_NAME <- as.character(df$DISTRICT_NAME)
  df$SCHOOL_CODE <- as.numeric(df$SCHOOL_CODE)
  df$SCHOOL_NAME <- as.character(df$SCHOOL_NAME)
  df$AMINDIAN_F <- as.numeric(df$AMINDIAN_F)
  df$AMINDIAN_M <- as.numeric(df$AMINDIAN_M)
  df$ASIAN_F <- as.numeric(df$ASIAN_F)
  df$ASIAN_M <- as.numeric(df$ASIAN_M)
  df$BLACK_F <- as.numeric(df$BLACK_F)
  df$BLACK_M <- as.numeric(df$BLACK_M)
  df$HISPANIC_F <- as.numeric(df$HISPANIC_F)
  df$HISPANIC_M <- as.numeric(df$HISPANIC_M)
  df$WHITE_F <- as.numeric(df$WHITE_F)
  df$WHITE_M <- as.numeric(df$WHITE_M)
  df$PACISLAND_F <- as.numeric(df$PACISLAND_F)
  df$PACISLAND_M <- as.numeric(df$PACISLAND_M)
  df$TWOORMORE_F <- as.numeric(df$TWOORMORE_F)
  df$TWOORMORE_M <- as.numeric(df$TWOORMORE_M)
  df$TOTAL <- as.numeric(df$TOTAL)
  
  # add a year column
  df$YEAR <- year
  
  # create a total and pct columns for each gender+ethnicity
  df$TOTAL_F <- df$AMINDIAN_F + df$ASIAN_F + df$BLACK_F + df$HISPANIC_F + df$WHITE_F + df$PACISLAND_F + df$TWOORMORE_F
  df$PCT_F <- round(df$TOTAL_F/df$TOTAL, digits = 3)
  df$TOTAL_M <- df$AMINDIAN_M + df$ASIAN_M + df$BLACK_M + df$HISPANIC_M + df$WHITE_M + df$PACISLAND_M + df$TWOORMORE_M
  df$PCT_M <- round(df$TOTAL_M/df$TOTAL, digits = 3)
  df$AMINDIAN <- df$AMINDIAN_F + df$AMINDIAN_M
  df$AMINDIAN_PCT <- round(df$AMINDIAN/df$TOTAL, digits = 3)
  df$ASIAN <- df$ASIAN_F + df$ASIAN_M
  df$ASIAN_PCT <- round(df$ASIAN/df$TOTAL, digits = 3)
  df$BLACK <- df$BLACK_F + df$BLACK_M
  df$BLACK_PCT <- round(df$BLACK/df$TOTAL, digits = 3)
  df$HISPANIC <- df$HISPANIC_F + df$HISPANIC_M
  df$HISPANIC_PCT <- round(df$HISPANIC/df$TOTAL, digits = 3)
  df$WHITE <- df$AMINDIAN_F + df$AMINDIAN_M
  df$WHITE_PCT <- round(df$WHITE/df$TOTAL, digits = 3)
  df$PACISLAND <- df$PACISLAND_F + df$PACISLAND_M
  df$PACISLAND_PCT <- round(df$PACISLAND/df$TOTAL, digits = 3)
  df$TWOORMORE <- df$TWOORMORE_F + df$TWOORMORE_M
  df$TWOORMORE_PCT <- round(df$TWOORMORE/df$TOTAL, digits = 3)
  
  # get rid of the columns that separate
  df$AMINDIAN_F <- NULL
  df$AMINDIAN_M <- NULL
  df$ASIAN_F <- NULL
  df$ASIAN_M <- NULL
  df$BLACK_F <- NULL
  df$BLACK_M <- NULL 
  df$HISPANIC_F <- NULL
  df$HISPANIC_M <- NULL
  df$WHITE_F <- NULL
  df$WHITE_M <- NULL
  df$PACISLAND_F <- NULL
  df$PACISLAND_M <- NULL
  df$TWOORMORE_F <- NULL
  df$TWOORMORE_M <- NULL
  
  # last cleaning
  df <- as.data.frame(sapply(df, gsub, pattern=",", replacement=""))
  df <- as.data.frame(sapply(df, gsub, pattern="~", replacement="-"))
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df[is.na(df)] <- -99
  
  return(df)
}



# Read in Each Dataset --------------------------------------------------------------------------------------------------------



# Demographic --------------------------------------------------------------------------------------------------------
# 2015-16 -----
# read in .xlsx
demo_16 <- read_xlsx("data/2015_16_sch_membershipbysch_race_ethnicity_gender_grade.xlsx", skip = 2)

# change column names
colnames(demo_16) <- demo_list_1116

# get rid of the first row
demo_16 <- demo_16[-1,]

# get rid of "Not a school" rows
demo_16 <- demo_16[!grepl("Not a school",demo_16$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_16$GRADE[is.na(demo_16$GRADE)] <- "TOTAL"

# keep only "TOTAL" rows
demo_16 <- demo_16[grepl("TOTAL",demo_16$GRADE),]

# get rid of district total rows
demo_16 <- demo_16[!grepl("TOTAL",demo_16$DISTRICT_NAME),]

# clean it
demo_16 <- clean_demo(demo_16, 2016)

# write as a .csv file
write.csv(demo_16, "cleaned_data/co_enroll_2016.csv")


# 2014-15 -----
# read in .xlsx
demo_15 <- read_xlsx("data/2014_15_sch_membershipbysch_race_ethnicity_gender_grade.xlsx", skip = 2)

# change column names
colnames(demo_15) <- demo_list_1116

# get rid of the first row
demo_15 <- demo_15[-1,]

# get rid of "Not a school" rows
demo_15 <- demo_15[!grepl("Not a school",demo_15$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_15$GRADE[demo_15$GRADE=="Sch Total"] <- "TOTAL"

# keep only "TOTAL" rows
demo_15 <- demo_15[grepl("TOTAL",demo_15$GRADE),]

# clean it
demo_15 <- clean_demo(demo_15, 2015)

# merge it
data_demo <- full_join(demo_16, demo_15)

# write as a .csv file
write.csv(demo_15, "cleaned_data/co_enroll_2015.csv")


# 2013-14 -----
# read in .xls
demo_14 <- read_xls("data/2013_14_sch_membershipbysch_race_ethnicity_gender_grade_revised.xls", skip = 2)

# change column names
colnames(demo_14) <- demo_list_1116

# get rid of the first row
demo_14 <- demo_14[-1,]

# get rid of "Not a school" rows
demo_14 <- demo_14[!grepl("Not a school",demo_14$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_14$GRADE[demo_14$GRADE=="Sch Total"] <- "TOTAL"

# keep only "TOTAL" rows
demo_14 <- demo_14[grepl("TOTAL",demo_14$GRADE),]

# clean it
demo_14 <- clean_demo(demo_14, 2014)

# merge it
data_demo <- full_join(data_demo, demo_14)

# write as a .csv file
write.csv(demo_14, "cleaned_data/co_enroll_2014.csv")

# 2012-13 -----
# read in .xls
demo_13 <- read_xls("data/2012_13_sch_membershipbysch_race_ethnicity_gender_grade.xls", skip = 2)

# change column names
colnames(demo_13) <- demo_list_1116

# get rid of the first row
demo_13 <- demo_13[-1,]

# get rid of "Not a school" rows
demo_13 <- demo_13[!grepl("Not a school",demo_13$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_13$GRADE[demo_13$GRADE=="SCHOOL TOTALS"]  <- "TOTAL"

# keep only "TOTAL" rows
demo_13 <- demo_13[grepl("TOTAL",demo_13$GRADE),]

# clean it
demo_13 <- clean_demo(demo_13, 2013)

# merge it
data_demo <- full_join(data_demo, demo_13)

# write as a .csv file
write.csv(demo_13, "cleaned_data/co_enroll_2013.csv")


# 2011-12 -----
# read in .xls
demo_12 <- read_xls("data/2011_12_sch_membershipbysch_race_ethnicity_gender_grade.xls", skip = 2)

# change column names
colnames(demo_12) <- demo_list_1116

# get rid of the first row
demo_12 <- demo_12[-1,]

# get rid of "Not a school" rows
demo_12 <- demo_12[!grepl("Not a school",demo_12$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_12$GRADE[demo_12$GRADE=="SCHOOL TOTALS"] <- "TOTAL"

# keep only "TOTAL" rows
demo_12 <- demo_12[grepl("TOTAL",demo_12$GRADE),]

# clean it
demo_12 <- clean_demo(demo_12, 2012)

# merge it
data_demo <- full_join(data_demo, demo_12)

# write as a .csv file
write.csv(demo_12, "cleaned_data/co_enroll_2012.csv")

# 2010-11 -----
# read in .xlsx
demo_11 <- read_xls("data/2010_11_sch_membershipbysch_race_ethnicity_gender_grade.xls")

# change column names
colnames(demo_11) <- demo_list_1116

# get rid of the first row
demo_11 <- demo_11[-1,]

# get rid of "Not a school" rows
demo_11 <- demo_11[!grepl("Not a school",demo_11$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_11$GRADE[demo_11$GRADE=="SCHOOL TOTAL"] <- "TOTAL"

# keep only "TOTAL" rows
demo_11 <- demo_11[grepl("TOTAL",demo_11$GRADE),]

# clean it
demo_11 <- clean_demo(demo_11, 2011)

# merge it
data_demo <- full_join(data_demo, demo_11)

# write as a .csv file
write.csv(demo_11, "cleaned_data/co_enroll_2011.csv")


# 2009-10 -----
# read in .xlsx
demo_10 <- read_xls("data/2009_10_sch_membershipbysch_race_ethnicity_gender_grade.xls", skip = 2)

# change in column names
colnames(demo_10) <- demo_list_10

# get rid of the first row
demo_10 <- demo_10[-1,]

# get rid of "Not a school" rows
demo_10 <- demo_10[!grepl("Not a school",demo_10$SCHOOL_NAME),]

# turn NA into TOTAL in GRADE column
demo_10$GRADE[demo_10$GRADE=="SCHOOL TOTAL"] <- "TOTAL"

# keep only "TOTAL" rows
demo_10 <- demo_10[grepl("TOTAL",demo_10$GRADE),]

# get rid of GRADE column to prepare to aggregate rows together
demo_10$GRADE <- NULL

# clean it
demo_10 <- clean_demo(demo_10, 2010)

# merge it
data_demo <- full_join(data_demo, demo_10)

# write as a .csv file
write.csv(demo_10, "cleaned_data/co_enroll_2010.csv")


# Finish -----
# Save as .csv file
write.csv(data_demo, "cleaned_data/co_enroll_2010_16.csv", row.names = FALSE)


# Performance --------------------------------------------------------------------------------------------------------
# 2015-16 -----
# read in .xlsx
perf_16 <- read_xlsx("data/CDE_perf_2016.xlsx", sheet = 2)


# 2013-14 -----
# read in .xlsx
perf_14 <- read_xlsx("data/CDE_perf_2014.xlsx")


# 2012-13 -----
# read in .xlsx
perf_13 <- read_xlsx("data/CDE_perf_2013.xlsx")


# 2011-12 -----
# read in .xlsx
perf_12 <- read_xlsx("data/CDE_perf_2012.xlsx")


# 2010-11 -----
# read in .xlsx
perf_11 <- read_xlsx("data/CDE_perf_2011.xlsx")


# 2009-10 -----
# read in .xlsx
perf_10 <- read_xlsx("data/CDE_perf_2010.xlsx", sheet = 1)
temp_10 <- perf_10
perf_10 <- temp_10

# select the columns
info_10 <- perf_10 %>% select(c("District Number", "District Name", "School Number", "School Name",
                          "CharterorOnline", "INITIAL_PlanType", "FINAL_PlanType"))
math_10 <- perf_10 %>% select(c("SPF_ACH_IND_PTS_EARN_MATH", "SPF_ACH_IND_PTS_ELGBL_MATH", "SPF_ACH_IND_RATING_MATH",
                                "SPF_ACH_IND_N_COUNT_MATH", "SPF_ACH_IND_PA_PCT_MATH", "SPF_ACH_IND_PCTILE_MATH",
                                "SRPS_SCH_PART_NM_MATH", "SRPS_SCH_PART_DN_MATH", "SPF_TEST_PART_PCT_TEST_MATH", "SPF_TEST_PART_RATING_MATH"))
read_10 <- perf_10 %>% select(c("SPF_ACH_IND_PTS_EARN_READ", "SPF_ACH_IND_PTS_ELGBL_READ", "SPF_ACH_IND_RATING_READ",
                                "SPF_ACH_IND_N_COUNT_READ", "SPF_ACH_IND_PA_PCT_READ", "SPF_ACH_IND_PCTILE_READ",
                                "SRPS_SCH_PART_NM_READ", "SRPS_SCH_PART_DN_READ", "SPF_TEST_PART_PCT_TEST_READ", "SPF_TEST_PART_RATING_READ"))

# rename the columns
colnames(info_10) <- c("DISTRICT_NUM", "DISTRICT_NAME", "SCHOOL_NUM", "SCHOOL_NAME", "CHARTERORONLINE", "INITIAL_PLANTYPE", "FINAL_PLANTYPE")
colnames(read_10) <- c("READ_PTS_EARNED", "READ_PTS_ELIGIBLE", "READ_RATING", "READ_NUMOFSTUD_USED", "READ_PCT_PROFORADV", "READ_PCTLERANKING",
                       "READ_PARTIC_NUMERATOR", "READ_PARTIC_DENOMINATOR", "READ_PCTOFSTUD_TESTED", "READ_MET_95PCT_PARTICRATE")
colnames(math_10) <- c("MATH_PTS_EARNED", "MATH_PTS_ELIGIBLE", "MATH_RATING", "MATH_NUMOFSTUD_USED", "MATH_PCT_PROFORADV", "MATH_PCTLERANKING",
                       "MATH_PARTIC_NUMERATOR", "MATH_PARTIC_DENOMINATOR", "MATH_PCTOFSTUD_TESTED", "MATH_MET_95PCT_PARTICRATE")

# combine them



# Finish -----
# Save as .csv file
write.csv(data_perf, "cleaned_data/co_perf_2010_16", row.names = FALSE)

# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/co_perf_enroll_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/co_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/co_perf_clean.rds")


