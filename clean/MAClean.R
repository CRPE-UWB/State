#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Massachusetts' Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Massachusetts 
# Created by: Kevin Cha on 07-19-17
# Updated by: Kevin Cha on 07-25-17
# Data from: 
#     Performance: http://profiles.doe.mass.edu/state_report/mcas.aspx
#             -Report Type: School    -Year: 2008 => 2016   -Grade: ALL   -Student Group: All Students
#     Demographics: http://www.doe.mass.edu/infoservices/reports/enroll/default.html?yr=1112
#             -Just Use Enrollment Data widget: School Year 2003-04 => 2016-17 and download 'Enrollment by School/Race'
# Notes: 2002-3, 2008-9 were too different to include [for demographics]
# Link to Github: https://github.com/CRPE-UWB/State

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_ma_clean")

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(gdata)

# List --------------------------------------------------------------------------------------------------------
demo_list <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "COUNTY_NAME", "GRADE", "SCHOOL_TOTAL",
               "BLACK_TOTAL", "ASIAN_TOTAL", "HISPANIC_TOTAL", "TWOORMORE_TOTAL", "AMINDIAN_TOTAL",
               "PACISLAND_TOTAL", "WHITE_TOTAL")
demo_list_a <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "COUNTY_NAME", "SCHOOL_TOTAL",
               "BLACK_TOTAL", "ASIAN_TOTAL", "HISPANIC_TOTAL", "TWOORMORE_TOTAL", "AMINDIAN_TOTAL",
               "PACISLAND_TOTAL", "WHITE_TOTAL")
target_demo_list <- c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "COUNTY_NAME", "SCHOOL_TOTAL",
                      "BLACK_TOTAL", "BLACK_ENROLL_PCT", "ASIAN_TOTAL", "ASIAN_ENROLL_PCT","HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                      "TWOORMORE_TOTAL", "TWOORMORE_ENROLL_PCT", "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT",
                      "PACISLAND_TOTAL", "PACISLAND_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT")
demo_list2 <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "GRADE", "SCHOOL_TOTAL",
               "BLACK_TOTAL", "ASIAN_TOTAL", "HISPANIC_TOTAL", "TWOORMORE_TOTAL", "AMINDIAN_TOTAL",
               "PACISLAND_TOTAL", "WHITE_TOTAL")
demo_list2_a <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "SCHOOL_TOTAL",
                "BLACK_TOTAL", "ASIAN_TOTAL", "HISPANIC_TOTAL", "TWOORMORE_TOTAL", "AMINDIAN_TOTAL",
                "PACISLAND_TOTAL", "WHITE_TOTAL")
target_demo_list2 <- c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_TOTAL",
                      "BLACK_TOTAL", "BLACK_ENROLL_PCT", "ASIAN_TOTAL", "ASIAN_ENROLL_PCT","HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                      "TWOORMORE_TOTAL", "TWOORMORE_ENROLL_PCT", "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT",
                      "PACISLAND_TOTAL", "PACISLAND_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT")
demo_order <- c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "YEAR", "COUNTY_NAME", "SCHOOL_TOTAL",
                      "BLACK_TOTAL", "BLACK_ENROLL_PCT", "ASIAN_TOTAL", "ASIAN_ENROLL_PCT","HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                      "TWOORMORE_TOTAL", "TWOORMORE_ENROLL_PCT", "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT",
                      "PACISLAND_TOTAL", "PACISLAND_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT")
demo_order2 <- c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "YEAR", "SCHOOL_TOTAL",
                       "BLACK_TOTAL", "BLACK_ENROLL_PCT", "ASIAN_TOTAL", "ASIAN_ENROLL_PCT","HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                       "TWOORMORE_TOTAL", "TWOORMORE_ENROLL_PCT", "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT",
                       "PACISLAND_TOTAL", "PACISLAND_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT")

perf_list <- c("SCHOOL_NAME", "SCHOOL_CODE", "SUBJECT", "PROF_OR_HIGHER", "PROF_OR_HIGHER_PCT", "ADV", "ADV_PCT", "PROF", "PROF_PCT",
               "NEEDS_IMPROV", "NEEDS_IMPROV_PCT", "WARNING_FAIL", "WARNING_FAIL_PCT", "STUDENT_COUNT", 
               "COMPOSITE_PERF_INDEX", "MEDIAN_STUD_GROWTH_PERCENTILE", "NUM_STUD_GROWTH_PERCENTILE")
perf_order <- c("SCHOOL_NAME", "SCHOOL_CODE", "YEAR", "ELA_PROF_OR_HIGHER", "MTH_PROF_OR_HIGHER", "ELA_PROF_OR_HIGHER_PCT", "MTH_PROF_OR_HIGHER_PCT",
                "ELA_ADV", "MTH_ADV", "ELA_ADV_PCT", "MTH_ADV_PCT", "ELA_PROF", "MTH_PROF", "ELA_PROF_PCT", "MTH_PROF_PCT",
                "ELA_NEEDS_IMPROV", "MTH_NEEDS_IMPROV", "ELA_NEEDS_IMPROV_PCT", "MTH_NEEDS_IMPROV_PCT", 
                "ELA_WARNING_FAIL", "MTH_WARNING_FAIL", "ELA_WARNING_FAIL_PCT", "MTH_WARNING_FAIL_PCT",
                "ELA_STUDENT_COUNT", "MTH_STUDENT_COUNT")

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

# Add Code to Front of Col Names
add_code <- function(df, code) {
  for (i in 4:ncol(df)) {
    colnames(df)[i] <- paste(code, colnames(df)[i], sep="_")
    next
  }
  return(df)
}

clean_demo <- function(df) {
  # change colnames to be consistent
  colnames(df) <- demo_list
  
  # lets get rid of GRADE column
  df$GRADE <- NULL
  
  # change into *** into NAs
  df[df == "***"] <- NA
  
  # change the columns to numeric
  df$SCHOOL_TOTAL <- as.numeric(df$SCHOOL_TOTAL)
  df$BLACK_TOTAL <- as.numeric(df$BLACK_TOTAL)
  df$ASIAN_TOTAL <- as.numeric(df$ASIAN_TOTAL)
  df$HISPANIC_TOTAL <- as.numeric(df$HISPANIC_TOTAL)
  df$TWOORMORE_TOTAL <- as.numeric(df$TWOORMORE_TOTAL)
  df$AMINDIAN_TOTAL <- as.numeric(df$AMINDIAN_TOTAL)
  df$PACISLAND_TOTAL <- as.numeric(df$PACISLAND_TOTAL)
  df$WHITE_TOTAL <- as.numeric(df$WHITE_TOTAL)
  
  # change the columns to character
  df$DISTRICT_NAME <- as.character(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- as.character(df$SCHOOL_NAME)
  df$COUNTY_NAME <- as.character(df$COUNTY_NAME)
  
  # combines the rows of schools together
  df <- df %>% 
    group_by_("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "COUNTY_NAME") %>% 
    summarise(sum(SCHOOL_TOTAL), sum(BLACK_TOTAL), sum(ASIAN_TOTAL), sum(HISPANIC_TOTAL),
              sum(TWOORMORE_TOTAL), sum(AMINDIAN_TOTAL), sum(PACISLAND_TOTAL), sum(WHITE_TOTAL))
  
  # rename the column names
  colnames(df) <- demo_list_a
  
  # create pct enrollment columns
  df$BLACK_ENROLL_PCT <- round(df$BLACK_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$ASIAN_ENROLL_PCT <- round(df$ASIAN_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$HISPANIC_ENROLL_PCT <- round(df$HISPANIC_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$TWOORMORE_ENROLL_PCT <- round(df$TWOORMORE_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$AMINDIAN_ENROLL_PCT <- round(df$AMINDIAN_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$PACISLAND_ENROLL_PCT <- round(df$PACISLAND_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$WHITE_ENROLL_PCT <- round(df$WHITE_TOTAL / df$SCHOOL_TOTAL, digits=3)
  
  # reorder the columns
  df <- setcolorder(df, target_demo_list)
  
  
  # make sure ORG_CODE is 4 digits long
  df$DISTRICT_CODE <- sprintf("%04d", df$DISTRICT_CODE)
  # make sure SCHOOL_CODE is 8 digits long
  df$SCHOOL_CODE <- sprintf("%08d", df$SCHOOL_CODE)
  
  # turn -1 and . into NAs
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  
  # make sure no special characters
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df$COUNTY_NAME <- no_more_special_characters(df$COUNTY_NAME)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  return(df)
}

clean_demo2 <- function(df) {
  # change colnames to be consistent
  colnames(df) <- demo_list2
  
  # lets get rid of GRADE column
  df$GRADE <- NULL
  
  # change into *** into NAs
  df[df == "***"] <- NA
  
  # change the columns to numeric
  df$SCHOOL_TOTAL <- as.numeric(df$SCHOOL_TOTAL)
  df$BLACK_TOTAL <- as.numeric(df$BLACK_TOTAL)
  df$ASIAN_TOTAL <- as.numeric(df$ASIAN_TOTAL)
  df$HISPANIC_TOTAL <- as.numeric(df$HISPANIC_TOTAL)
  df$TWOORMORE_TOTAL <- as.numeric(df$TWOORMORE_TOTAL)
  df$AMINDIAN_TOTAL <- as.numeric(df$AMINDIAN_TOTAL)
  df$PACISLAND_TOTAL <- as.numeric(df$PACISLAND_TOTAL)
  df$WHITE_TOTAL <- as.numeric(df$WHITE_TOTAL)
  
  # change the columns to character
  df$DISTRICT_NAME <- as.character(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- as.character(df$SCHOOL_NAME)
  
  # combines the rows of schools together
  df <- df %>% 
    group_by_("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME") %>% 
    summarise(sum(SCHOOL_TOTAL), sum(BLACK_TOTAL), sum(ASIAN_TOTAL), sum(HISPANIC_TOTAL),
              sum(TWOORMORE_TOTAL), sum(AMINDIAN_TOTAL), sum(PACISLAND_TOTAL), sum(WHITE_TOTAL))
  
  # rename the column names
  colnames(df) <- demo_list2_a
  
  # create pct enrollment columns
  df$BLACK_ENROLL_PCT <- round(df$BLACK_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$ASIAN_ENROLL_PCT <- round(df$ASIAN_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$HISPANIC_ENROLL_PCT <- round(df$HISPANIC_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$TWOORMORE_ENROLL_PCT <- round(df$TWOORMORE_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$AMINDIAN_ENROLL_PCT <- round(df$AMINDIAN_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$PACISLAND_ENROLL_PCT <- round(df$PACISLAND_TOTAL / df$SCHOOL_TOTAL, digits=3)
  df$WHITE_ENROLL_PCT <- round(df$WHITE_TOTAL / df$SCHOOL_TOTAL, digits=3)
  
  # reorder the columns
  df <- setcolorder(df, target_demo_list2)
  
  
  # make sure ORG_CODE is 4 digits long
  df$DISTRICT_CODE <- sprintf("%04d", df$DISTRICT_CODE)
  # make sure SCHOOL_CODE is 8 digits long
  df$SCHOOL_CODE <- sprintf("%08d", df$SCHOOL_CODE)
  
  # turn -1 and . into NAs
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  
  # make sure no special characters
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  return(df)
}

clean_perf <- function(df) {
  # change colnames
  colnames(df) <- perf_list
  
  # change N/A to NA
  df[df == 'N/A'] <- NA
  
  # get rid of some columns
  df$COMPOSITE_PERF_INDEX <- NULL
  df$MEDIAN_STUD_GROWTH_PERCENTILE <- NULL
  df$NUM_STUD_GROWTH_PERCENTILE <- NULL
  
  # change to proper class
  df$SCHOOL_NAME <- as.character(df$SCHOOL_NAME)
  df$SUBJECT <- as.character(df$SUBJECT)
  df$PROF_OR_HIGHER <- as.numeric(df$PROF_OR_HIGHER)
  df$PROF_OR_HIGHER_PCT <- as.numeric(df$PROF_OR_HIGHER_PCT)
  df$PROF <- as.numeric(df$PROF)
  df$PROF_PCT <- as.numeric(df$PROF_PCT)
  df$ADV <- as.numeric(df$ADV)
  df$ADV_PCT <- as.numeric(df$ADV_PCT)
  df$NEEDS_IMPROV <- as.numeric(df$NEEDS_IMPROV)
  df$NEEDS_IMPROV_PCT <- as.numeric(df$NEEDS_IMPROV_PCT)
  df$WARNING_FAIL <- as.numeric(df$WARNING_FAIL)
  df$WARNING_FAIL_PCT <- as.numeric(df$WARNING_FAIL_PCT)
  df$PROF_OR_HIGHER <- as.numeric(df$PROF_OR_HIGHER)
  df$STUDENT_COUNT <- as.numeric(df$STUDENT_COUNT)
  
  # get rid of certain rows
  df <- df[!grepl("State Totals -", df$SCHOOL_NAME),]
  
  # combine subject column with the respective scores to make new columns ie instead of 2 cols: SUBJECT, ADV => SUBJECT_ADV
  # best way to do it (i think anyways): separate into 2 different DT and then spread and then recombine
  df_ELA <- df %>% 
    filter(SUBJECT == 'ELA')    
  df_ELA <- add_code(df_ELA, 'ELA')
  df_ELA$SUBJECT <- NULL
  df_MTH <- df %>% 
    filter(SUBJECT == 'MTH')  
  df_MTH <- add_code(df_MTH, 'MTH')
  df_MTH$SUBJECT <- NULL
  
  df <- full_join(df_ELA, df_MTH, by=c("SCHOOL_NAME", "SCHOOL_CODE"))
  
  # last part of cleaning
  df$SCHOOL_NAME <- no_more_special_characters(df$SCHOOL_NAME)
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  df[is.na(df)] <- -99
  
  return(df)
}

# Performance --------------------------------------------------------------------------------------------------------
# Notes -----
# P+A = Proficient or Higher, A = Advanced, P = Proficient, NI = Needs Improvement, W/F = Warning/Failing, 
# CPI = Composite Performance Index, SGP = Median Student Growth Percentile, 
# Inc SGP = number of students included in student growth percentile calculations	
# Have to manually fix the raw data because R had trouble reading it in (due to the fact that the format wasn't being recognized as an xls file)

# 2007-2008 -----
# read in xls file
data_perf_08 <- gdata::read.xls("data/mcas_08.xlsx", skip = 5)

# clean everything
data_perf_08 <- clean_perf(data_perf_08)

# add year column
data_perf_08$YEAR <- 2008

# rearrange the columns
data_perf_08 <- setcolorder(data_perf_08, perf_order)

# Save as .csv file
write.csv(data_perf_08,"cleaned_data/data_perf_2008.csv", row.names = FALSE)


# 2008-2009 -----
# read in xls file
data_perf_09 <- gdata::read.xls("data/mcas_09.xlsx", skip = 5)

# clean everything
data_perf_09 <- clean_perf(data_perf_09)

# add year column
data_perf_09$YEAR <- 2009

# rearrange the columns
data_perf_09 <- setcolorder(data_perf_09, perf_order)

# Merge them together
data_perf <- full_join(data_perf_08, data_perf_09)

# Save as .csv file
write.csv(data_perf_08,"cleaned_data/data_perf_2009.csv", row.names = FALSE)


# 2009-2010 -----
# read in xls file
data_perf_10 <- gdata::read.xls("data/mcas_10.xlsx", skip = 5)

# clean everything
data_perf_10 <- clean_perf(data_perf_10)

# add year column
data_perf_10$YEAR <- 2010

# rearrange the columns
data_perf_10 <- setcolorder(data_perf_10, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_10)

# Save as .csv file
write.csv(data_perf_10,"cleaned_data/data_perf_2010.csv", row.names = FALSE)


# 2010-2011 -----
# read in xls file
data_perf_11 <- gdata::read.xls("data/mcas_11.xlsx", skip = 5)

# clean everything
data_perf_11 <- clean_perf(data_perf_11)

# add year column
data_perf_11$YEAR <- 2011

# rearrange the columns
data_perf_11 <- setcolorder(data_perf_11, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_11)

# Save as .csv file
write.csv(data_perf_11,"cleaned_data/data_perf_2011.csv", row.names = FALSE)


# 2011-2012 -----
# read in xls file
data_perf_12 <- gdata::read.xls("data/mcas_12.xlsx", skip = 5)

# clean everything
data_perf_12 <- clean_perf(data_perf_12)

# add year column
data_perf_12$YEAR <- 2012

# rearrange the columns
data_perf_12 <- setcolorder(data_perf_12, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_12)

# Save as .csv file
write.csv(data_perf_12,"cleaned_data/data_perf_2012.csv", row.names = FALSE)


# 2012-2013 -----
# read in xls file
data_perf_13 <- gdata::read.xls("data/mcas_13.xlsx", skip = 5)

# clean everything
data_perf_13 <- clean_perf(data_perf_13)

# add year column
data_perf_13$YEAR <- 2013

# rearrange the columns
data_perf_13 <- setcolorder(data_perf_13, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_13)

# Save as .csv file
write.csv(data_perf_13,"cleaned_data/data_perf_2013.csv", row.names = FALSE)


# 2013-2014 -----
# read in xls file
data_perf_14 <- gdata::read.xls("data/mcas_14.xlsx", skip = 5)

# clean everything
data_perf_14 <- clean_perf(data_perf_14)

# add year column
data_perf_14$YEAR <- 2014

# rearrange the columns
data_perf_14 <- setcolorder(data_perf_14, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_14)

# Save as .csv file
write.csv(data_perf_14,"cleaned_data/data_perf_2014.csv", row.names = FALSE)


# 2014-2015 -----
# read in xls file
data_perf_15 <- gdata::read.xls("data/mcas_15.xlsx", skip = 5)

# clean everything
data_perf_15 <- clean_perf(data_perf_15)

# add year column
data_perf_15$YEAR <- 2015

# rearrange the columns
data_perf_15 <- setcolorder(data_perf_15, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_15)

# Save as .csv file
write.csv(data_perf_15,"cleaned_data/data_perf_2015.csv", row.names = FALSE)


# 2015-2016 -----
# read in xls file
data_perf_16 <- gdata::read.xls("data/mcas_16.xlsx", skip = 5)

# clean everything
data_perf_16 <- clean_perf(data_perf_16)

# add year column
data_perf_16$YEAR <- 2016

# rearrange the columns
data_perf_16 <- setcolorder(data_perf_16, perf_order)

# Merge them together
data_perf <- full_join(data_perf, data_perf_16)

# Save as .csv file
write.csv(data_perf_16,"cleaned_data/data_perf_2016.csv", row.names = FALSE)


# Demographics --------------------------------------------------------------------------------------------------------
# read in xls file
# 2003-2004 -----
data_demo_04 <- gdata::read.xls("data/School-GradeRace_2003_04.xls", header = TRUE)

# get rid of unwanted columns
data_demo_04$Total.Minority <- NULL
data_demo_04$FY <- NULL
data_demo_04$Gr... <- NULL
data_demo_04$Grade <- NULL
data_demo_04$District.School.Total <- NULL

# changes order to match others
order3 <- c("District", "District.Name", "School", "School.Name", "Total", "Black", "Asian", 
            "Hispanic",  "Am_Ind.", "White")
data_demo_04 <- setcolorder(data_demo_04, order3)

# change into *** into NAs
data_demo_04[data_demo_04 == "***"] <- NA

# change colnames
colnames(data_demo_04) <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "ASIAN_TOTAL", 
                       "HISPANIC_TOTAL", "AMINDIAN_TOTAL", "WHITE_TOTAL")

# change the columns to numeric
data_demo_04$SCHOOL_TOTAL <- as.numeric(data_demo_04$SCHOOL_TOTAL)
data_demo_04$BLACK_TOTAL <- as.numeric(data_demo_04$BLACK_TOTAL)
data_demo_04$ASIAN_TOTAL <- as.numeric(data_demo_04$ASIAN_TOTAL)
data_demo_04$HISPANIC_TOTAL <- as.numeric(data_demo_04$HISPANIC_TOTAL)
data_demo_04$AMINDIAN_TOTAL <- as.numeric(data_demo_04$AMINDIAN_TOTAL)
data_demo_04$WHITE_TOTAL <- as.numeric(data_demo_04$WHITE_TOTAL)

# change the columns to character
data_demo_04$DISTRICT_NAME <- as.character(data_demo_04$DISTRICT_NAME)
data_demo_04$SCHOOL_NAME <- as.character(data_demo_04$SCHOOL_NAME)

# combines the rows of schools together
data_demo_04 <- data_demo_04 %>% 
  group_by_("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(SCHOOL_TOTAL), sum(BLACK_TOTAL), sum(ASIAN_TOTAL), sum(HISPANIC_TOTAL),
            sum(AMINDIAN_TOTAL), sum(WHITE_TOTAL))

# rename the column names
colnames(data_demo_04) <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "ASIAN_TOTAL", 
                       "HISPANIC_TOTAL", "AMINDIAN_TOTAL", "WHITE_TOTAL")

# create pct enrollment columns
data_demo_04$BLACK_ENROLL_PCT <- round(data_demo_04$BLACK_TOTAL / data_demo_04$SCHOOL_TOTAL, digits=3)
data_demo_04$ASIAN_ENROLL_PCT <- round(data_demo_04$ASIAN_TOTAL / data_demo_04$SCHOOL_TOTAL, digits=3)
data_demo_04$HISPANIC_ENROLL_PCT <- round(data_demo_04$HISPANIC_TOTAL / data_demo_04$SCHOOL_TOTAL, digits=3)
data_demo_04$AMINDIAN_ENROLL_PCT <- round(data_demo_04$AMINDIAN_TOTAL / data_demo_04$SCHOOL_TOTAL, digits=3)
data_demo_04$WHITE_ENROLL_PCT <- round(data_demo_04$WHITE_TOTAL / data_demo_04$SCHOOL_TOTAL, digits=3)

# reorder the columns
data_demo_04 <- setcolorder(data_demo_04, c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "BLACK_ENROLL_PCT",
                                  "ASIAN_TOTAL", "ASIAN_ENROLL_PCT", "HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                                  "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT"))

# make sure ORG_CODE is 4 digits long
data_demo_04$DISTRICT_CODE <- sprintf("%04d", data_demo_04$DISTRICT_CODE)
# make sure SCHOOL_CODE is 8 digits long
data_demo_04$SCHOOL_CODE <- sprintf("%08d", data_demo_04$SCHOOL_CODE)

# add year column
data_demo_04$YEAR <- 2004

# turn -1 and . into NAs
data_demo_04[data_demo_04 == ','] <- ""
data_demo_04[data_demo_04 == '~'] <- "-"

# make sure no special characters
data_demo_04$DISTRICT_NAME <- no_more_special_characters(data_demo_04$DISTRICT_NAME)
data_demo_04$SCHOOL_NAME <- no_more_special_characters(data_demo_04$SCHOOL_NAME)

# turn NAs into -99
data_demo_04[is.na(data_demo_04)] <- -99

# new order
data_demo_04 <- setcolorder(data_demo_04, c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "YEAR", "SCHOOL_TOTAL", "BLACK_TOTAL", "BLACK_ENROLL_PCT",
                                            "ASIAN_TOTAL", "ASIAN_ENROLL_PCT", "HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                                            "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT"))

# write as .csv file
write.csv(data_demo_04, "cleaned_data/ma_enroll_2003_04.csv", row.names = FALSE)


# 2004-2005 -----
data_demo_05 <- gdata::read.xls("data/School-GradeRace_2004_05.xls", header = TRUE, skip=4)

# get rid of unwanted columns
data_demo_05$Minority.Total <- NULL
data_demo_05$County <- NULL
data_demo_05$Gr..Code <- NULL
data_demo_05$Grade <- NULL
data_demo_05$Grade.Total <- NULL
data_demo_05$District.Total <- NULL

# changes order to match others
order2 <- c("District.Code", "District", "School.Code", "School", "School.Total", "African.American", 
           "Asian", "Hispanic", "Native.American", "White")
data_demo_05 <- setcolorder(data_demo_05, order2)

# change into *** into NAs
data_demo_05[data_demo_05 == "***"] <- NA

# change colnames
colnames(data_demo_05) <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "ASIAN_TOTAL", 
                       "HISPANIC_TOTAL", "AMINDIAN_TOTAL", "WHITE_TOTAL")

# change the columns to numeric
data_demo_05$SCHOOL_TOTAL <- as.numeric(data_demo_05$SCHOOL_TOTAL)
data_demo_05$BLACK_TOTAL <- as.numeric(data_demo_05$BLACK_TOTAL)
data_demo_05$ASIAN_TOTAL <- as.numeric(data_demo_05$ASIAN_TOTAL)
data_demo_05$HISPANIC_TOTAL <- as.numeric(data_demo_05$HISPANIC_TOTAL)
data_demo_05$AMINDIAN_TOTAL <- as.numeric(data_demo_05$AMINDIAN_TOTAL)
data_demo_05$WHITE_TOTAL <- as.numeric(data_demo_05$WHITE_TOTAL)

# change the columns to character
data_demo_05$DISTRICT_NAME <- as.character(data_demo_05$DISTRICT_NAME)
data_demo_05$SCHOOL_NAME <- as.character(data_demo_05$SCHOOL_NAME)

# combines the rows of schools together
data_demo_05 <- data_demo_05 %>% 
  group_by_("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME") %>% 
  summarise(sum(SCHOOL_TOTAL), sum(BLACK_TOTAL), sum(ASIAN_TOTAL), sum(HISPANIC_TOTAL),
            sum(AMINDIAN_TOTAL), sum(WHITE_TOTAL))

# rename the column names
colnames(data_demo_05) <- c("DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_CODE", "SCHOOL_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "ASIAN_TOTAL", 
                       "HISPANIC_TOTAL", "AMINDIAN_TOTAL", "WHITE_TOTAL")

# create pct enrollment columns
data_demo_05$BLACK_ENROLL_PCT <- round(data_demo_05$BLACK_TOTAL / data_demo_05$SCHOOL_TOTAL, digits=3)
data_demo_05$ASIAN_ENROLL_PCT <- round(data_demo_05$ASIAN_TOTAL / data_demo_05$SCHOOL_TOTAL, digits=3)
data_demo_05$HISPANIC_ENROLL_PCT <- round(data_demo_05$HISPANIC_TOTAL / data_demo_05$SCHOOL_TOTAL, digits=3)
data_demo_05$AMINDIAN_ENROLL_PCT <- round(data_demo_05$AMINDIAN_TOTAL / data_demo_05$SCHOOL_TOTAL, digits=3)
data_demo_05$WHITE_ENROLL_PCT <- round(data_demo_05$WHITE_TOTAL / data_demo_05$SCHOOL_TOTAL, digits=3)

# reorder the columns
data_demo_05 <- setcolorder(data_demo_05, c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "SCHOOL_TOTAL", "BLACK_TOTAL", "BLACK_ENROLL_PCT",
                                  "ASIAN_TOTAL", "ASIAN_ENROLL_PCT", "HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                                  "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT"))

# make sure ORG_CODE is 4 digits long
data_demo_05$DISTRICT_CODE <- sprintf("%04d", data_demo_05$DISTRICT_CODE)
# make sure SCHOOL_CODE is 8 digits long
data_demo_05$SCHOOL_CODE <- sprintf("%08d", data_demo_05$SCHOOL_CODE)

# add year column
data_demo_05$YEAR <- 2005

# turn -1 and . into NAs
data_demo_05[data_demo_05 == ','] <- ""
data_demo_05[data_demo_05 == '~'] <- "-"

# make sure no special characters
data_demo_05$DISTRICT_NAME <- no_more_special_characters(data_demo_05$DISTRICT_NAME)
data_demo_05$SCHOOL_NAME <- no_more_special_characters(data_demo_05$SCHOOL_NAME)

# turn NAs into -99
data_demo_05[is.na(data_demo_05)] <- -99

# new order
data_demo_05 <- setcolorder(data_demo_05, c("SCHOOL_CODE", "SCHOOL_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "YEAR", "SCHOOL_TOTAL", "BLACK_TOTAL", "BLACK_ENROLL_PCT",
                                            "ASIAN_TOTAL", "ASIAN_ENROLL_PCT", "HISPANIC_TOTAL", "HISPANIC_ENROLL_PCT",
                                            "AMINDIAN_TOTAL", "AMINDIAN_ENROLL_PCT", "WHITE_TOTAL", "WHITE_ENROLL_PCT"))

# merge together
data_demo3 <- full_join(data_demo_04, data_demo_05)

# write as .csv file
write.csv(data_demo_05, "cleaned_data/ma_enroll_2004_05.csv", row.names = FALSE)


# 2005-2006 -----
data_demo_06 <- gdata::read.xls("data/School-GradeRace_2005_06.xls", header = TRUE, skip=4)

# get rid of unwanted columns
data_demo_06$Total_Enrollment <- NULL
data_demo_06$County <- NULL

# changes order to match others
order <- c("District.Code", "District.Name", "SCHOOL", "School.Name.", "Grade", "School.Total", "African.American", 
           "Asian.", "Hispanic", "Multi.Race..Non.Hispanic", "Native.American", "Native.Hawaiian",  "White")
data_demo_06 <- setcolorder(data_demo_06, order)

# clean everything
data_demo_06 <- clean_demo2(data_demo_06)

# add year column
data_demo_06$YEAR <- 2006

# new order
data_demo_06 <- setcolorder(data_demo_06, demo_order2)

# write as .csv file
write.csv(data_demo_06, "cleaned_data/ma_enroll_2005_06.csv", row.names = FALSE)


# 2006-2007 -----
data_demo_07 <- gdata::read.xls("data/School-GradeRace_2006_07.xls", header = TRUE, skip=4)

# clean everything
data_demo_07 <- clean_demo2(data_demo_07)

# add year column
data_demo_07$YEAR <- 2007

# new order
data_demo_07 <- setcolorder(data_demo_07, demo_order2)

# merge together
data_demo1 <- full_join(data_demo_06, data_demo_07)

# write as .csv file
write.csv(data_demo_07, "cleaned_data/ma_enroll_2006_07.csv", row.names = FALSE)


# 2007-2008 -----
data_demo_08 <- gdata::read.xls("data/School-GradeRace_2007_08.xls", header = TRUE)

# clean everything
data_demo_08 <- clean_demo2(data_demo_08)

# add year column
data_demo_08$YEAR <- 2008

# new order
data_demo_08 <- setcolorder(data_demo_08, demo_order2)

# merge together
data_demo1 <- full_join(data_demo1, data_demo_08)

# write as .csv file
write.csv(data_demo_08, "cleaned_data/ma_enroll_2007_08.csv", row.names = FALSE)


# 2009-2010 -----
data_demo_10 <- gdata::read.xls("data/School-GradeRace_2009_10.xls", header = TRUE)

# clean everything
data_demo_10 <- clean_demo(data_demo_10)

# get rid of uncommon column
data_demo_10$COUNTY_NAME <- NULL

# add year column
data_demo_10$YEAR <- 2010

# new order
data_demo_10 <- setcolorder(data_demo_10, demo_order2)

# write as .csv file
write.csv(data_demo_10, "cleaned_data/ma_enroll_2009_10.csv", row.names = FALSE)


# 2010-2011 -----
data_demo_11 <- gdata::read.xls("data/School-GradeRace_2010_11.xls", header = TRUE)

# clean everything
data_demo_11 <- clean_demo(data_demo_11)

# get rid of uncommon column
data_demo_11$COUNTY_NAME <- NULL

# add year column
data_demo_11$YEAR <- 2011

# new order
data_demo_11 <- setcolorder(data_demo_11, demo_order2)

# merge together
data_demo2 <- full_join(data_demo_10, data_demo_11)

# write as .csv file
write.csv(data_demo_11, "cleaned_data/ma_enroll_2010_11.csv", row.names = FALSE)


# 2011-2012 -----
data_demo_12 <- gdata::read.xls("data/School-GradeRace_2011_12.xls", header = TRUE)

# clean everything
data_demo_12 <- clean_demo(data_demo_12)

# add year column
data_demo_12$YEAR <- 2012

# new order
data_demo_12 <- setcolorder(data_demo_12, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_12)

# write as .csv file
write.csv(data_demo_12, "cleaned_data/ma_enroll_2011_12.csv", row.names = FALSE)


# 2012-2013 -----
data_demo_13 <- gdata::read.xls("data/School-GradeRace_2012_13.xlsx", header = TRUE)

# clean everything
data_demo_13 <- clean_demo(data_demo_13)

# add year column
data_demo_13$YEAR <- 2013

# new order
data_demo_13 <- setcolorder(data_demo_13, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_13)

# write as .csv file
write.csv(data_demo_13, "cleaned_data/ma_enroll_2012_13.csv", row.names = FALSE)


# 2013-2014 -----
data_demo_14 <- gdata::read.xls("data/School-GradeRace_2013_14.xlsx", header = TRUE)

# clean everything
data_demo_14 <- clean_demo(data_demo_14)

# add year column
data_demo_14$YEAR <- 2014

# new order
data_demo_14 <- setcolorder(data_demo_14, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_14)

# write as .csv file
write.csv(data_demo_14, "cleaned_data/ma_enroll_2013_14.csv", row.names = FALSE)


# 2014-2015 -----
data_demo_15 <- gdata::read.xls("data/School-GradeRace_2014_15.xlsx", header = TRUE)

# clean everything
data_demo_15 <- clean_demo(data_demo_15)

# get rid of state total [ORG_CODE = 0, SCHOOL_CODE = 0] and other unncessary rows
data_demo_15 <- data_demo_15[-1,]

# add year column
data_demo_15$YEAR <- 2015

# new order
data_demo_15 <- setcolorder(data_demo_15, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_15)

# write as .csv file
write.csv(data_demo_15, "cleaned_data/ma_enroll_2014_15.csv", row.names = FALSE)

  
# 2015-2016 -----
data_demo_16 <- gdata::read.xls("data/School-GradeRace_2015_16.xlsx", header = TRUE)

# clean everything
data_demo_16 <- clean_demo(data_demo_16)

# get rid of state total [ORG_CODE = 0, SCHOOL_CODE = 0] and other unncessary rows
data_demo_16 <- data_demo_16[-1,]

# add year column
data_demo_16$YEAR <- 2016

# new order
data_demo_16 <- setcolorder(data_demo_16, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_16)

# write as .csv file
write.csv(data_demo_16, "cleaned_data/ma_enroll_2015_16.csv", row.names = FALSE)


# 2016-2017 -----
data_demo_17 <- gdata::read.xls("data/School-GradeRace_2016_17.xlsx", header = TRUE, skip = 4)

# clean everything
data_demo_17 <- clean_demo(data_demo_17)

# get rid of state total [ORG_CODE = 0, SCHOOL_CODE = 0] and other unncessary rows
data_demo_17 <- data_demo_17[-c(1,2),]

# add year column
data_demo_17$YEAR <- 2017

# new order
data_demo_17 <- setcolorder(data_demo_17, demo_order)

# merge together
data_demo2 <- full_join(data_demo2, data_demo_17)

# write as .csv file
write.csv(data_demo_17, "cleaned_data/ma_enroll_2016_17.csv", row.names = FALSE)


# Finish -----
# final finish
data_demo <- full_join(data_demo3, data_demo1)
data_demo <- full_join(data_demo, data_demo2)
# change something
data_demo2$COUNTY_NAME <- NULL
data_demo$COUNTY_NAME <- NULL

# Save as .csv file
# 2004-05
write.csv(data_demo3,"cleaned_data/ma_enroll_2004_05.csv", row.names = FALSE)
# 2006-08
write.csv(data_demo1,"cleaned_data/ma_enroll_2006_08.csv", row.names = FALSE)
# 2010-17
write.csv(data_demo2,"cleaned_data/ma_enroll_2010_17.csv", row.names = FALSE)
#2004-2017
write.csv(data_demo,"cleaned_data/ma_enroll_2004_17.csv", row.names = FALSE)


# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/ma_perf_demo_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/ma_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/ma_perf_clean.rds")




