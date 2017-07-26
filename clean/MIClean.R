#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Michigan's Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Michigan 
# Created by: Kevin Cha on 07-24-17
# Updated by: Kevin Cha on 07-25-17
# Data from: 
#       Demographics: https://www.mischooldata.org/DistrictSchoolProfiles/EntitySummary/SchoolDataFile.aspx
#               -ISD: All ISDs in State   -District: All Districts in State    -School: All Schools in State
#               -School Year: 2002-2003 => 2016-2017   -Data Files: Student Count
#       Performance: https://www.mischooldata.org/DistrictSchoolProfiles/EntitySummary/SchoolDataFile.aspx
#               -ISD: All ISDs in State   -District: All Districts in State    -School: All Schools in State
#               -School Year: 2007-2008 => 2011-2012   -Data Files: Accountability Results
# Codebook: https://www.mischooldata.org/DistrictSchoolProfiles/EntitySummary/SchoolDataFile.aspx
#               CLICK: K-12 School Data file table layouts


# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_mi_clean")

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)


# List --------------------------------------------------------------------------------------------------------
demo_list <- c("YEAR", "ISD_CODE", "ISD_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "BUILDING_CODE", "BUILDING_NAME", "ENTITY_TYPE", "LOCALE_NAME",
               "TOTAL_ENROLL", "MALE_ENROLL", "FEMALE_ENROLL", "AMINDIAN_ENROLL", "ASIAN_ENROLL", "BLACK_ENROLL", "HISPANIC_ENROLL", 
               "PACISLAND_ENROLL", "WHITE_ENROLL", "TWOORMORE_ENROLL", "ECONDISADV_ENROLL", "ELL_ENROLL")
demo_order <- c("YEAR", "ISD_CODE", "ISD_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "BUILDING_CODE", "BUILDING_NAME", "ENTITY_TYPE", "LOCALE_NAME",
                "TOTAL_ENROLL", "MALE_ENROLL", "MALE_PCT", "FEMALE_ENROLL", "FEMALE_PCT", "AMINDIAN_ENROLL", "AMINDIAN_PCT", 
                "ASIAN_ENROLL", "ASIAN_PCT", "BLACK_ENROLL", "BLACK_PCT", "HISPANIC_ENROLL", "HISPANIC_PCT",  "PACISLAND_ENROLL", "PACISLAND_PCT", 
                "WHITE_ENROLL", "WHITE_PCT", "TWOORMORE_ENROLL", "TWOORMORE_PCT", "ECONDISADV_ENROLL", "ECONDISADV_PCT", "ELL_ENROLL", "ELL_PCT")

perf_list <- c("YEAR", "ISD_CODE", "ISD_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "BUILDING_CODE", "BUILDING_NAME", "ENTITY_TYPE", 
               "SUBGROUP", "TOTAL_ENROLL", "MATH_ENROLL", "MATH_PARTICIPATION_PCT", "READING_ENROLL", "READING_PARTICIPATION_PCT",
               "MATH_FAY_ENROLL", "MATH_FAY_PROF", "MATH_FAY_PROF_PCT", "READING_FAY_ENROLL", "READING_FAY_PROF", "READING_FAY_PROF_PCT",
               "ATTEND_RATE", "GRAD_RATE", "MET_AYP_MATH", "MET_AYP_READING", "MET_AYP", "MET_PARTICIPATION_MATH", "MET_PARTICIPATION_READING",
               "MET_PROF_MATH", "MET_PROF_READING", "MET_ATTEND", "MET_GRAD_RATE")
perf_list2 <- c("YEAR", "ISD_CODE", "ISD_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "BUILDING_CODE", "BUILDING_NAME", "ENTITY_TYPE", 
               "SUBGROUP", "TOTAL_ENROLL", "MATH_ENROLL", "MATH_PARTICIPATION_PCT", "READING_ENROLL", "READING_PARTICIPATION_PCT",
               "MATH_FAY_ENROLL", "MATH_FAY_PROF", "MATH_FAY_PROF_PCT", "READING_FAY_ENROLL", "READING_FAY_PROF", "READING_FAY_PROF_PCT",
               "ATTEND_RATE", "GRAD_RATE")
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
last_clean <- function(df) {
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  
  df[df == " "] <- NA
  df[df == "NaN"] <- NA
  df[df == "Inf"] <- NA
  
  # make sure no special characters
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$ISD_NAME <- no_more_special_characters(df$ISD_NAME)
  df$LOCALE_NAME <- no_more_special_characters(df$LOCALE_NAME)
  df$BUILDING_NAME <- no_more_special_characters(df$BUILDING_NAME)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  return(df)
}

dec_num_only <- function(x) {
  require(stringr)
  
  # sets to look for decimal number
  regexp <- "[[:digit:]]+\\.*[[:digit:]]*"
  x <- str_extract(x, regexp)
  return(x)
}

clean_demo <- function(df) {
  # get rid of the 'All Districts' rows
  df <- df %>% 
    filter(df$DistrictName != 'All Districts')
  
  # get rid of unwanted columns
  df$KINDERGARTEN_ENROLLMENT <- NULL
  df$GRADE_1_ENROLLMENT <- NULL
  df$GRADE_2_ENROLLMENT <- NULL
  df$GRADE_3_ENROLLMENT <- NULL
  df$GRADE_4_ENROLLMENT <- NULL
  df$GRADE_5_ENROLLMENT <- NULL
  df$GRADE_6_ENROLLMENT <- NULL
  df$GRADE_7_ENROLLMENT <- NULL
  df$GRADE_8_ENROLLMENT <- NULL
  df$GRADE_9_ENROLLMENT <- NULL
  df$GRADE_10_ENROLLMENT <- NULL
  df$GRADE_11_ENROLLMENT <- NULL
  df$GRADE_12_ENROLLMENT <- NULL
  df$SPECIAL_EDUCATION_ENROLLMENT <- NULL
  
  # change column names
  colnames(df) <- demo_list
  
  # change year
  df$YEAR <- 2003
  
  # get rid of <
  df$ELL_ENROLL <- dec_num_only(df$ELL_ENROLL)
  
  # change all enroll to numeric
  df$MALE_ENROLL <- as.numeric(df$MALE_ENROLL)
  df$FEMALE_ENROLL <- as.numeric(df$FEMALE_ENROLL)
  df$AMINDIAN_ENROLL <- as.numeric(df$AMINDIAN_ENROLL)
  df$ASIAN_ENROLL <- as.numeric(df$ASIAN_ENROLL)
  df$BLACK_ENROLL <- as.numeric(df$BLACK_ENROLL)
  df$HISPANIC_ENROLL <- as.numeric(df$HISPANIC_ENROLL)
  df$PACISLAND_ENROLL <- as.numeric(df$PACISLAND_ENROLL)
  df$WHITE_ENROLL <- as.numeric(df$WHITE_ENROLL)
  df$TWOORMORE_ENROLL <- as.numeric(df$TWOORMORE_ENROLL)
  df$ECONDISADV_ENROLL <- as.numeric(df$ECONDISADV_ENROLL)
  df$ELL_ENROLL <- as.numeric(df$ELL_ENROLL)
  
  # create pct columns
  df$MALE_PCT <- round(df$MALE_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$FEMALE_PCT <- round(df$FEMALE_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$AMINDIAN_PCT <- round(df$AMINDIAN_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$ASIAN_PCT <- round(df$ASIAN_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$BLACK_PCT <- round(df$BLACK_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$HISPANIC_PCT <- round(df$HISPANIC_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$PACISLAND_PCT <- round(df$PACISLAND_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$WHITE_PCT <- round(df$WHITE_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$TWOORMORE_PCT <- round(df$TWOORMORE_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$ECONDISADV_PCT <- round(df$ECONDISADV_ENROLL/df$TOTAL_ENROLL, digits=3)
  df$ELL_PCT <- round(df$ELL_ENROLL/df$TOTAL_ENROLL, digits=3)
  
  # rearrange the order of columns
  df <- setcolorder(df, demo_order)
  
  # last clean
  df <- last_clean(df)
  
  return(df)
}

clean_perf <- function(df) {
  # get rid of the 'All Districts' rows
  df <- df %>% 
    filter(df$DistrictName != 'All Districts')
  
  # get only 'All Students' rows
  df <- df %>% 
    filter(df$Subgroup == 'All Students')
  
  # turn YES and NO to 1 and 0
  df[df == 'Yes'] <- 1
  df[df == 'No'] <- 0
  
  # turn empty spaces into NA
  df[df == ''] <- NA
  
  # change column names
  colnames(df) <- perf_list
  
  # change year
  df$YEAR <- 2008
  
  # combine rows with similar school info together
  df <- df %>% 
    group_by_("YEAR", "ISD_CODE", "ISD_NAME", "DISTRICT_CODE", "DISTRICT_NAME", "BUILDING_CODE", "BUILDING_NAME", "ENTITY_TYPE", "SUBGROUP") %>% 
    summarise(sum(TOTAL_ENROLL, na.rm = TRUE), 
              sum(MATH_ENROLL, na.rm = TRUE), mean(MATH_PARTICIPATION_PCT, na.rm = TRUE), 
              sum(READING_ENROLL, na.rm = TRUE), mean(READING_PARTICIPATION_PCT, na.rm = TRUE),
              sum(MATH_FAY_ENROLL, na.rm = TRUE), sum(MATH_FAY_PROF, na.rm = TRUE), mean(MATH_FAY_PROF_PCT, na.rm = TRUE),
              sum(READING_FAY_ENROLL, na.rm = TRUE), sum(READING_FAY_PROF, na.rm = TRUE), mean(READING_FAY_PROF_PCT, na.rm = TRUE),
              mean(ATTEND_RATE, na.rm = TRUE), mean(GRAD_RATE, na.rm = TRUE)
    )
  
  # change column names AGAIN
  colnames(df) <- perf_list2
  
  # last clean
  df[df == ','] <- ""
  df[df == '~'] <- "-"
  
  df[df == " "] <- NA
  df[df == "NaN"] <- NA
  
  # change special characters
  df$ISD_NAME <- no_more_special_characters(df$ISD_NAME)
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$BUILDING_NAME <- no_more_special_characters(df$BUILDING_NAME)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  return(df)
}
  
# Performance --------------------------------------------------------------------------------------------------------

# 2007-2008 -----
# Read in csv file
data_perf_08 <- read.csv("data/accountability_200708.csv", stringsAsFactors = FALSE, header = TRUE)

# clean everything
data_perf_08 <- clean_perf(data_perf_08)

# write .csv file
write.csv(data_perf_08,"cleaned_data/data_perf_2008.csv", row.names = FALSE)


# 2008-2009 -----
# Read in csv file
data_perf_09 <- read.csv("data/accountability_200809.csv", stringsAsFactors = FALSE, header = TRUE)

# clean everything
data_perf_09 <- clean_perf(data_perf_09)

# merge them
data_perf <- full_join(data_perf_08, data_perf_09)

# write as .csv
write.csv(data_perf_09,"cleaned_data/data_perf_2009.csv", row.names = FALSE)

# 2009-2010 -----
# Read in csv file
data_perf_10 <- read.csv("data/accountability_200910.csv", stringsAsFactors = FALSE, header = TRUE)

# clean everything
data_perf_10 <- clean_perf(data_perf_10)

# merge them
data_perf <- full_join(data_perf, data_perf_10)

# write as .csv
write.csv(data_perf_10,"cleaned_data/data_perf_2010.csv", row.names = FALSE)


# 2010-2011 -----
# Read in csv file
data_perf_11 <- read.csv("data/accountability_201011.csv", stringsAsFactors = FALSE, header = TRUE)

# clean everything
data_perf_11 <- clean_perf(data_perf_11)

# merge them
data_perf <- full_join(data_perf, data_perf_11)

# write as .csv
write.csv(data_perf_11,"cleaned_data/data_perf_2011.csv", row.names = FALSE)


# 2011-2012 -----
# Read in csv file
data_perf_12 <- read.csv("data/accountability_201112.csv", stringsAsFactors = FALSE, header = TRUE)

# clean everything
data_perf_12 <- clean_perf(data_perf_12)

# merge them
data_perf <- full_join(data_perf, data_perf_12)

# write as .csv
write.csv(data_perf_12,"cleaned_data/data_perf_2012.csv", row.names = FALSE)


# Finish -----
write.csv(data_perf,"cleaned_data/data_perf_2008_12.csv", row.names = FALSE)


# Demographics --------------------------------------------------------------------------------------------------------

# 2002-2003 -----
# Read in csv file
data_demo_03 <- read.csv("data/studentcount_200203.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_03 <- clean_demo(data_demo_03)

# Save as .csv file
write.csv(data_demo_03,"cleaned_data/data_enroll_2003.csv", row.names = FALSE)


# 2003-2004 -----
# Read in csv file
data_demo_04 <- read.csv("data/studentcount_200304.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_04 <- clean_demo(data_demo_04)

# Merge them together
data_demo <- full_join(data_demo_03, data_demo_04)

# Save as .csv file
write.csv(data_demo_04,"cleaned_data/data_enroll_2004.csv", row.names = FALSE)


# 2004-2005 -----
# Read in csv file
data_demo_05 <- read.csv("data/studentcount_200405.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_05 <- clean_demo(data_demo_05)

# Merge them together
data_demo <- full_join(data_demo, data_demo_05)

# Save as .csv file
write.csv(data_demo_05,"cleaned_data/data_enroll_2005.csv", row.names = FALSE)


# 2005-2006 -----
# Read in csv file
data_demo_06 <- read.csv("data/studentcount_200506.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_06 <- clean_demo(data_demo_06)

# Merge them together
data_demo <- full_join(data_demo, data_demo_06)

# Save as .csv file
write.csv(data_demo_06,"cleaned_data/data_enroll_2006.csv", row.names = FALSE)


# 2006-2007 -----
# Read in csv file
data_demo_07 <- read.csv("data/studentcount_200607.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_07 <- clean_demo(data_demo_07)

# Merge them together
data_demo <- full_join(data_demo, data_demo_06)

# Save as .csv file
write.csv(data_demo_07,"cleaned_data/data_enroll_2007.csv", row.names = FALSE)


# 2007-2008 -----
# Read in csv file
data_demo_08 <- read.csv("data/studentcount_200708.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_08 <- clean_demo(data_demo_08)

# Merge them together
data_demo <- full_join(data_demo, data_demo_08)

# Save as .csv file
write.csv(data_demo_08,"cleaned_data/data_enroll_2008.csv", row.names = FALSE)


# 2008-2009 -----
# Read in csv file
data_demo_09 <- read.csv("data/studentcount_200809.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_09 <- clean_demo(data_demo_09)

# Merge them together
data_demo <- full_join(data_demo, data_demo_09)

# Save as .csv file
write.csv(data_demo_09,"cleaned_data/data_enroll_2009.csv", row.names = FALSE)


# 2009-2010 -----
# Read in csv file
data_demo_10 <- read.csv("data/studentcount_200910.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_10 <- clean_demo(data_demo_10)

# Merge them together
data_demo <- full_join(data_demo, data_demo_10)

# Save as .csv file
write.csv(data_demo_10,"cleaned_data/data_enroll_2010.csv", row.names = FALSE)


# 2010-2011 -----
# Read in csv file
data_demo_11 <- read.csv("data/studentcount_201011.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_11 <- clean_demo(data_demo_11)

# Merge them together
data_demo <- full_join(data_demo, data_demo_11)

# Save as .csv file
write.csv(data_demo_11,"cleaned_data/data_enroll_2011.csv", row.names = FALSE)


# 2011-2012 -----
# Read in csv file
data_demo_12 <- read.csv("data/studentcount_201112.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_12 <- clean_demo(data_demo_12)

# Merge them together
data_demo <- full_join(data_demo, data_demo_12)

# Save as .csv file
write.csv(data_demo_12,"cleaned_data/data_enroll_2012.csv", row.names = FALSE)


# 2012-2013 -----
# Read in csv file
data_demo_13 <- read.csv("data/studentcount_201213.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_13 <- clean_demo(data_demo_13)

# Merge them together
data_demo <- full_join(data_demo, data_demo_13)

# Save as .csv file
write.csv(data_demo_13,"cleaned_data/data_enroll_2013.csv", row.names = FALSE)


# 2013-2014 -----
# Read in csv file
data_demo_14 <- read.csv("data/studentcount_201314.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_14 <- clean_demo(data_demo_14)

# Merge them together
data_demo <- full_join(data_demo, data_demo_14)

# Save as .csv file
write.csv(data_demo_13,"cleaned_data/data_enroll_2014.csv", row.names = FALSE)


# 2014-2015 -----
# Read in csv file
data_demo_15 <- read.csv("data/studentcount_201415.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_15 <- clean_demo(data_demo_15)

# Merge them together
data_demo <- full_join(data_demo, data_demo_15)

# Save as .csv file
write.csv(data_demo_15,"cleaned_data/data_enroll_2015.csv", row.names = FALSE)


# 2015-2016 -----
# Read in csv file
data_demo_16 <- read.csv("data/studentcount_201516.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_16 <- clean_demo(data_demo_16)

# Merge them together
data_demo <- full_join(data_demo, data_demo_16)

# Save as .csv file
write.csv(data_demo_16,"cleaned_data/data_enroll_2016.csv", row.names = FALSE)


# 2016-2017 -----
# Read in csv file
data_demo_17 <- read.csv("data/studentcount_201617.csv", stringsAsFactors = FALSE, header = TRUE)

# clean it
data_demo_17 <- clean_demo(data_demo_17)

# Merge them together
data_demo <- full_join(data_demo, data_demo_17)

# Save as .csv file
write.csv(data_demo_17,"cleaned_data/data_enroll_2017.csv", row.names = FALSE)

# Finish -----
write.csv(data_demo,"cleaned_data/data_enroll_2003_17.csv", row.names = FALSE)

# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/ok_perf_demo_clean.Rdata")
# Save as .RDS 
saveRDS(data_demo, file="cleaned_data/mi_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/mi_perf_clean.rds")





