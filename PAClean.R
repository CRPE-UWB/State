#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Philadelphia's Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Philadelphia 
# Created by: Kevin Cha on 08-03-17
# Updated by: Kevin Cha on 08-08-17
# Data from: 
#     Performance: http://www.education.pa.gov/Data-and-Statistics/Pages/Keystone-Exams-Results.aspx#tab-1
#                 http://www.education.pa.gov/Data-and-Statistics/PSSA/Pages/default.aspx#tab-1                    
#     Demographics: http://www.education.pa.gov/Data-and-Statistics/Pages/Enrollment%20Reports%20and%20Projections.aspx#tab-1
# Codebook:
#     Demographics: inside the raw data files
#     Performance:
# Link to Github: https://github.com/CRPE-UWB/State
# Notes: For Demographics, will not use anything before 2011-12 because those datasets are missing both gender and race
#        For Performance, unable to obtain 2013-14+2014-15 because everytime it redirects me to this site: http://eseafedreport.com/

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_pa_clean") #MAC

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(readr)
library(readxl)

# List --------------------------------------------------------------------------------------------------------



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

last_clean_demo <- function(df,year) {
  # add YEAR column
  df$YEAR <- year
  
  # last cleaning
  df <- as.data.frame(sapply(df, gsub, pattern=",", replacement=""))
  df <- as.data.frame(sapply(df, gsub, pattern="~", replacement="-"))
  df$LEA_NAME <- no_more_special_characters(df$LEA_NAME)
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df$COUNTY <- no_more_special_characters(df$COUNTY)
  df[is.na(df)] <- -99
  
  return(df)
}

# Read in Each Dataset --------------------------------------------------------------------------------------------------------



# Demographic --------------------------------------------------------------------------------------------------------
# 2016-17 -----
# datset: gender
demo_g_17 <- read_xlsx("data/Enrollment Public Schools 2016-17.xlsx", sheet = 4, skip = 4)
# select columns we only need
demo_g_17 <- demo_g_17 %>% select(c("AUN", "LEA Name", "LEA Type", "County", "School Number", "School Name", "Student Gender Code", "Total"))
# turn the * into NA
demo_g_17[demo_g_17 == "*"] <- NA
# create a total column
demo_g_17$TOTAL_ENROLL <- demo_g_17$Total
# spread the Student Gender Code
demo_g_17 <- demo_g_17 %>% spread("Student Gender Code", Total)
# get rid of the <NA> column
demo_g_17$`<NA>` <- NULL
# rename the columns
colnames(demo_g_17) <- c("AUN", "LEA_NAME", "LEA_TYPE", "COUNTY", "SCHOOL_NUM", "SCHOOL_NAME", "TOTAL", "FEMALE", "MALE")
# combine the rows together
demo_g_17 <- demo_g_17 %>% 
  group_by_("AUN", "LEA_NAME", "LEA_TYPE", "COUNTY", "SCHOOL_NUM", "SCHOOL_NAME") %>% 
  summarise(sum(TOTAL, na.rm = TRUE), sum(FEMALE, na.rm = TRUE), sum(MALE, na.rm = TRUE))
# rename the columns(again)
colnames(demo_g_17) <- c("AUN", "LEA_NAME", "LEA_TYPE", "COUNTY", "SCHOOL_NUM", "SCHOOL_NAME", "TOTAL", "FEMALE", "MALE")


# dataset: race
demo_r_17 <- read_xlsx("data/Enrollment Public Schools 2016-17.xlsx", sheet = 5, skip = 4)
# dataset: school
demo_s_17 <- read_xlsx("data/Enrollment Public Schools 2016-17.xlsx", sheet = 3, skip = 4)
# select columns we only need
demo_r_17 <- demo_r_17 %>% select(c("AUN", "LEA Name", "LEA Type", "County", "Race", "Total"))
demo_s_17 <- demo_s_17 %>% select(c("AUN", "LEA Name", "LEA Type", "County", "School Number", "School Name", "Total"))
# turn the * into NA
demo_r_17[demo_r_17 == "*"] <- NA
# spread the Student Gender Code
demo_r_17 <- demo_r_17 %>% spread("Race", Total)
# get rid of the <NA> column
demo_r_17$`<NA>` <- NULL
# combine the two
demo_r_17 <- left_join(demo_s_17, demo_r_17)
# rename the columns
colnames(demo_r_17) <- c("AUN", "LEA_NAME", "LEA_TYPE", "COUNTY", "AMINDIAN", "ASIAN", "BLACK", "HISPANIC", "TWOORMORE", "PACISLAND", "WHITE")
# make the ethnicity columns numeric
demo_r_17$AMINDIAN <- as.numeric(demo_r_17$AMINDIAN)
demo_r_17$ASIAN <- as.numeric(demo_r_17$ASIAN)
demo_r_17$HISPANIC <- as.numeric(demo_r_17$HISPANIC)
demo_r_17$TWOORMORE <- as.numeric(demo_r_17$TWOORMORE)
demo_r_17$PACISLAND <- as.numeric(demo_r_17$PACISLAND)
demo_r_17$WHITE <- as.numeric(demo_r_17$WHITE)
  
# combine them
demo_17 <- full_join(demo_g_17, demo_r_17)

# last cleaning
demo_17 <- last_clean(demo_17)


# 2015-16 -----
# read in .xlsx 
demo_16 <- read_xlsx("data/Enrollment Public Schools 2015-16.xlsx")


# 2014-15 -----
# read in .xlsx 
demo_15 <- read_xlsx("data/Enrollment Public Schools 2014-15.xlsx")


# 2013-14 -----
# read in .xlsx 
demo_14 <- read_xlsx("data/Enrollment Public Schools 2013-14.xlsx")


# 2012-13 -----
# read in .xlsx 
demo_13 <- read_xlsx("data/Enrollment Public Schools 2012-13.xlsx")


# 2011-12 -----
# read in .xlsx 
demo_12 <- read_xlsx("data/Enrollment Public Schools 2011-12.xlsx")


# Finish -----
# Save as .csv file
write.csv(data_demo, "cleaned_data/pa_enroll_2010_17", row.names = FALSE)

# Performance --------------------------------------------------------------------------------------------------------
# 2015-16 -----


# 2014-15 -----


# 2013-14 -----


# 2012-13 -----


# 2011-12 -----


# 2010-11 -----


# Finish -----
# Save as .csv file
write.csv(data_perf, "cleaned_data/pa_perf_20_1", row.names = FALSE)

# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/pa_perf_enroll_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/pa_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/pa_perf_clean.rds")



