#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Texas' Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Texas 
# Created by: Kevin Cha on 07-17-17
# Updated by: Kevin Cha on 07-25-17
# Data from: 
#    2015-16: https://rptsvr1.tea.texas.gov/perfreport/tapr/2016/xplore/DownloadSelData.html
#    2014-15: https://rptsvr1.tea.texas.gov/perfreport/tapr/2015/xplore/DownloadSelData.html
#    2013-14: https://rptsvr1.tea.texas.gov/perfreport/tapr/2014/xplore/DownloadSelData.html
#    2012-13: https://rptsvr1.tea.texas.gov/perfreport/tapr/2013/xplore/DownloadSelData.html
# Codebooks:
#    2015-16: https://rptsvr1.tea.texas.gov/perfreport/tapr/2016/xplore/taprref.html
#    2014-15: https://rptsvr1.tea.texas.gov/perfreport/tapr/2015/xplore/taprref.html
#    2013-14: https://rptsvr1.tea.texas.gov/perfreport/tapr/2014/xplore/taprref.html
#    2012-13: https://rptsvr1.tea.texas.gov/perfreport/tapr/2013/xplore/taprref.html  

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_tx_clean") #MAC
setwd("C:/Users/phato_000/Documents/CRPE/state/TX") #PC

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

# Functions --------------------------------------------------------------------------------------------------------
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


clean_demo <- function(df) {
  
  # spreads the ethnicity
  df <-df %>% 
    spread(Ethnicity.Name, Enrollment.by.Ethnicity)
  
  # first colnames change
  colnames(df) <- col_names_demo
  
  # turn -9999999 into NA
  df[df == -9999999] <- NA
  
  # creates a total enrollment column
  df$ALL_ENROLL <- rowSums(na.rm=TRUE, df[c('AMINDIAN_ENROLL', "ASIAN_ENROLL", "BLACK_ENROLL", "HISPANIC_ENROLL",
                                                                "PACIFIC_ISLANDER_ENROLL", "TWO_OR_MORE_ENROLL", "WHITE_ENROLL")])
  
  # create a percent column for each ethnicity
  df$AMINDIAN_ENROLL_PCT <- round(df$AMINDIAN_ENROLL / df$ALL_ENROLL, digits = 3)
  df$ASIAN_ENROLL_PCT <- round(df$ASIAN_ENROLL / df$ALL_ENROLL, digits = 3)
  df$BLACK_ENROLL_PCT <- round(df$BLACK_ENROLL / df$ALL_ENROLL, digits = 3)
  df$HISPANIC_ENROLL_PCT <- round(df$HISPANIC_ENROLL / df$ALL_ENROLL, digits = 3)
  df$PACIFIC_ISLANDER_ENROLL_PCT <- round(df$PACIFIC_ISLANDER_ENROLL / df$ALL_ENROLL, digits = 3)
  df$TWO_OR_MORE_ENROLL_PCT <- round(df$TWO_OR_MORE_ENROLL / df$ALL_ENROLL, digits = 3)
  df$WHITE_ENROLL_PCT <- round(df$WHITE_ENROLL / df$ALL_ENROLL, digits = 3)
  
  # changes the order of the columns 
  df <- setcolorder(df, target_order_demo)
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  # get rid of commas and tildas
  df[df == ','] <- " "
  df[df == '~'] <- "-"
  
  df$REGION <- as.character(df$REGION)
  df$COUNTY_NAME <- as.character(df$COUNTY_NAME)
  df$DISTRICT_NAME <- as.character(df$DISTRICT_NAME)
  df$CAMPUS_NAME <- as.character(df$CAMPUS_NAME)
  
  df$REGION <- no_more_special_characters(df$REGION)
  df$COUNTY_NAME <- no_more_special_characters(df$COUNTY_NAME)
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  df$CAMPUS_NAME <- no_more_special_characters(df$CAMPUS_NAME)
  
  df$CAMPUS_NUMBER <- sprintf("%09d", df$CAMPUS_NUMBER)
  
  return(df)
}



# Lists --------------------------------------------------------------------------------------------------------
col_names_demo <- c('YEAR', 'REGION', 'COUNTY_NAME', 'DISTRICT_NAME', 'DISTRICT_NUMBER',
                     'CAMPUS_NAME', 'CAMPUS_NUMBER', 'AMINDIAN_ENROLL', 'ASIAN_ENROLL', 'BLACK_ENROLL',
                     'HISPANIC_ENROLL', 'PACIFIC_ISLANDER_ENROLL', 'TWO_OR_MORE_ENROLL', 'WHITE_ENROLL')

target_order_demo <- c('YEAR', 'REGION', 'COUNTY_NAME', 'DISTRICT_NAME', 'DISTRICT_NUMBER', 
                       'CAMPUS_NAME', 'CAMPUS_NUMBER', 'ALL_ENROLL', 'AMINDIAN_ENROLL', 'AMINDIAN_ENROLL_PCT',
                       'ASIAN_ENROLL', 'ASIAN_ENROLL_PCT', 'BLACK_ENROLL', 'BLACK_ENROLL_PCT',
                       'HISPANIC_ENROLL', 'HISPANIC_ENROLL_PCT', 'PACIFIC_ISLANDER_ENROLL', 'PACIFIC_ISLANDER_ENROLL_PCT',
                       'TWO_OR_MORE_ENROLL', 'TWO_OR_MORE_ENROLL_PCT', 'WHITE_ENROLL', 'WHITE_ENROLL_PCT')

colnames_satisf_131415 <- 
  c("CAMPUS_NUM", 
    "SATISF_RATE_ALL_BLACK", "SATISF_RATE_ALL_TOTAL", "SATISF_RATE_ALL_AMINDIAN", "SATISF_RATE_ALL_ASIAN", "SATISF_RATE_ALL_ATRISK",
    "SATISF_RATE_ALL_ELL", "SATISF_RATE_ALL_ECONDISADV", "SATISF_RATE_ALL_F", "SATISF_RATE_ALL_HISP", "SATISF_RATE_ALL_M", "SATISF_RATE_ALL_PACISLAND", 
    "SATISF_RATE_ALL_SPED", "SATISF_RATE_ALL_TWOORMORE", "SATISF_RATE_ALL_WHITE",
    "SATISF_RATE_READING_BLACK", "SATISF_RATE_READING_TOTAL", "SATISF_RATE_READING_AMINDIAN", "SATISF_RATE_READING_ASIAN", "SATISF_RATE_READING_ATRISK",
    "SATISF_RATE_READING_ELL", "SATISF_RATE_READING_ECONDISADV", "SATISF_RATE_READING_F", "SATISF_RATE_READING_HISP", "SATISF_RATE_READING_M", "SATISF_RATE_READING_PACISLAND", 
    "SATISF_RATE_READING_SPED", "SATISF_RATE_READING_TWOORMORE", "SATISF_RATE_READING_WHITE",
    "SATISF_RATE_MATH_BLACK", "SATISF_RATE_MATH_TOTAL", "SATISF_RATE_MATH_AMINDIAN", "SATISF_RATE_MATH_ASIAN", "SATISF_RATE_MATH_ATRISK",
    "SATISF_RATE_MATH_ELL", "SATISF_RATE_MATH_ECONDISADV", "SATISF_RATE_MATH_F", "SATISF_RATE_MATH_HISP", "SATISF_RATE_MATH_M", "SATISF_RATE_MATH_PACISLAND", 
    "SATISF_RATE_MATH_SPED", "SATISF_RATE_MATH_TWOORMORE", "SATISF_RATE_MATH_WHITE",
    "SATISF_RATE_WRITING_BLACK", "SATISF_RATE_WRITING_TOTAL", "SATISF_RATE_WRITING_AMINDIAN", "SATISF_RATE_WRITING_ASIAN", "SATISF_RATE_WRITING_ATRISK",
    "SATISF_RATE_WRITING_ELL", "SATISF_RATE_WRITING_ECONDISADV", "SATISF_RATE_WRITING_F", "SATISF_RATE_WRITING_HISP", "SATISF_RATE_WRITING_M", "SATISF_RATE_WRITING_PACISLAND", 
    "SATISF_RATE_WRITING_SPED", "SATISF_RATE_WRITING_TWOORMORE", "SATISF_RATE_WRITING_WHITE",
    "SATISF_RATE_SCIENCE_BLACK", "SATISF_RATE_SCIENCE_TOTAL", "SATISF_RATE_SCIENCE_AMINDIAN", "SATISF_RATE_SCIENCE_ASIAN", "SATISF_RATE_SCIENCE_ATRISK",
    "SATISF_RATE_SCIENCE_ELL", "SATISF_RATE_SCIENCE_ECONDISADV", "SATISF_RATE_SCIENCE_F", "SATISF_RATE_SCIENCE_HISP", "SATISF_RATE_SCIENCE_M", "SATISF_RATE_SCIENCE_PACISLAND", 
    "SATISF_RATE_SCIENCE_SPED", "SATISF_RATE_SCIENCE_TWOORMORE", "SATISF_RATE_SCIENCE_WHITE",
    "SATISF_RATE_SOCIALSCIENCE_BLACK", "SATISF_RATE_SOCIALSCIENCE_TOTAL", "SATISF_RATE_SOCIALSCIENCE_AMINDIAN", "SATISF_RATE_SOCIALSCIENCE_ASIAN", "SATISF_RATE_SOCIALSCIENCE_ATRISK",
    "SATISF_RATE_SOCIALSCIENCE_ELL", "SATISF_RATE_SOCIALSCIENCE_ECONDISADV", "SATISF_RATE_SOCIALSCIENCE_F", "SATISF_RATE_SOCIALSCIENCE_HISP", "SATISF_RATE_SOCIALSCIENCE_M", "SATISF_RATE_SOCIALSCIENCE_PACISLAND", 
    "SATISF_RATE_SOCIALSCIENCE_SPED", "SATISF_RATE_SOCIALSCIENCE_TWOORMORE", "SATISF_RATE_SOCIALSCIENCE_WHITE"
  )
colnames_satisf_16 <- 
  c("CAMPUS_NUM", 
    "SATISF_RATE_ALL_BLACK", "SATISF_RATE_MATH_BLACK", "SATISF_RATE_READING_BLACK", 
    "SATISF_RATE_SCIENCE_BLACK", "SATISF_RATE_SOCIALSCIENCE_BLACK", "SATISF_RATE_WRITING_BLACK", 
    "SATISF_RATE_ALL_TOTAL", "SATISF_RATE_MATH_TOTAL", "SATISF_RATE_READING_TOTAL", 
    "SATISF_RATE_SCIENCE_TOTAL", "SATISF_RATE_SOCIALSCIENCE_TOTAL", "SATISF_RATE_WRITING_TOTAL",
    "SATISF_RATE_ALL_AMINDIAN", "SATISF_RATE_MATH_AMINDIAN", "SATISF_RATE_READING_AMINDIAN", 
    "SATISF_RATE_SCIENCE_AMINDIAN", "SATISF_RATE_SOCIALSCIENCE_AMINDIAN", "SATISF_RATE_WRITING_AMINDIAN",
    "SATISF_RATE_ALL_ASIAN", "SATISF_RATE_MATH_ASIAN", "SATISF_RATE_READING_ASIAN", 
    "SATISF_RATE_SCIENCE_ASIAN", "SATISF_RATE_SOCIALSCIENCE_ASIAN", "SATISF_RATE_WRITING_ASIAN", 
    "SATISF_RATE_ALL_ATRISK", "SATISF_RATE_MATH_ATRISK", "SATISF_RATE_READING_ATRISK", 
    "SATISF_RATE_SCIENCE_ATRISK", "SATISF_RATE_SOCIALSCIENCE_ATRISK", "SATISF_RATE_WRITING_ATRISK", 
    "SATISF_RATE_ALL_ELL", "SATISF_RATE_MATH_ELL", "SATISF_RATE_READING_ELL", 
    "SATISF_RATE_SCIENCE_ELL", "SATISF_RATE_SOCIALSCIENCE_ELL", "SATISF_RATE_WRITING_ELL", 
    "SATISF_RATE_ALL_ECONDISADV", "SATISF_RATE_MATH_ECONDISADV", "SATISF_RATE_READING_ECONDISADV", 
    "SATISF_RATE_SCIENCE_ECONDISADV", "SATISF_RATE_SOCIALSCIENCE_ECONDISADV", "SATISF_RATE_WRITING_ECONDISADV", 
    "SATISF_RATE_ALL_F", "SATISF_RATE_MATH_F", "SATISF_RATE_READING_F", 
    "SATISF_RATE_SCIENCE_F", "SATISF_RATE_SOCIALSCIENCE_F", "SATISF_RATE_WRITING_F",
    "SATISF_RATE_ALL_HISP", "SATISF_RATE_MATH_HISP", "SATISF_RATE_READING_HISP", 
    "SATISF_RATE_SCIENCE_HISP", "SATISF_RATE_SOCIALSCIENCE_HISP", "SATISF_RATE_WRITING_HISP",
    "SATISF_RATE_ALL_M", "SATISF_RATE_MATH_M", "SATISF_RATE_READING_M", 
    "SATISF_RATE_SCIENCE_M", "SATISF_RATE_SOCIALSCIENCE_M", "SATISF_RATE_WRITING_M",
    "SATISF_RATE_ALL_PACISLAND", "SATISF_RATE_MATH_PACISLAND", "SATISF_RATE_READING_PACISLAND", 
    "SATISF_RATE_SCIENCE_PACISLAND", "SATISF_RATE_SOCIALSCIENCE_PACISLAND", "SATISF_RATE_WRITING_PACISLAND",
    "SATISF_RATE_ALL_SPED", "SATISF_RATE_MATH_SPED", "SATISF_RATE_READING_SPED", 
    "SATISF_RATE_SCIENCE_SPED", "SATISF_RATE_SOCIALSCIENCE_SPED", "SATISF_RATE_WRITING_SPED",
    "SATISF_RATE_ALL_TWOORMORE", "SATISF_RATE_MATH_TWOORMORE", "SATISF_RATE_READING_TWOORMORE", 
    "SATISF_RATE_SCIENCE_TWOORMORE", "SATISF_RATE_SOCIALSCIENCE_TWOORMORE", "SATISF_RATE_WRITING_TWOORMORE",
    "SATISF_RATE_ALL_WHITE", "SATISF_RATE_MATH_WHITE", "SATISF_RATE_READING_WHITE", 
    "SATISF_RATE_SCIENCE_WHITE", "SATISF_RATE_SOCIALSCIENCE_WHITE", "SATISF_RATE_WRITING_WHITE"
  )
colnames_adv_131415 <-
  c("CAMPUS_NUM",
    "ADV_RATE_ALL_BLACK", "ADV_RATE_ALL_TOTAL", "ADV_RATE_ALL_AMINDIAN", "ADV_RATE_ALL_ASIAN", "ADV_RATE_ALL_ATRISK",
    "ADV_RATE_ALL_ELL", "ADV_RATE_ALL_ECONDISADV", "ADV_RATE_ALL_F", "ADV_RATE_ALL_HISP", "ADV_RATE_ALL_M", "ADV_RATE_ALL_PACISLAND", 
    "ADV_RATE_ALL_SPED", "ADV_RATE_ALL_TWOORMORE", "ADV_RATE_ALL_WHITE",
    "ADV_RATE_READING_BLACK", "ADV_RATE_READING_TOTAL", "ADV_RATE_READING_AMINDIAN", "ADV_RATE_READING_ASIAN", "ADV_RATE_READING_ATRISK",
    "ADV_RATE_READING_ELL", "ADV_RATE_READING_ECONDISADV", "ADV_RATE_READING_F", "ADV_RATE_READING_HISP", "ADV_RATE_READING_M", "ADV_RATE_READING_PACISLAND", 
    "ADV_RATE_READING_SPED", "ADV_RATE_READING_TWOORMORE", "ADV_RATE_READING_WHITE",
    "ADV_RATE_MATH_BLACK", "ADV_RATE_MATH_TOTAL", "ADV_RATE_MATH_AMINDIAN", "ADV_RATE_MATH_ASIAN", "ADV_RATE_MATH_ATRISK",
    "ADV_RATE_MATH_ELL", "ADV_RATE_MATH_ECONDISADV", "ADV_RATE_MATH_F", "ADV_RATE_MATH_HISP", "ADV_RATE_MATH_M", "ADV_RATE_MATH_PACISLAND", 
    "ADV_RATE_MATH_SPED", "ADV_RATE_MATH_TWOORMORE", "ADV_RATE_MATH_WHITE",
    "ADV_RATE_WRITING_BLACK", "ADV_RATE_WRITING_TOTAL", "ADV_RATE_WRITING_AMINDIAN", "ADV_RATE_WRITING_ASIAN", "ADV_RATE_WRITING_ATRISK",
    "ADV_RATE_WRITING_ELL", "ADV_RATE_WRITING_ECONDISADV", "ADV_RATE_WRITING_F", "ADV_RATE_WRITING_HISP", "ADV_RATE_WRITING_M", "ADV_RATE_WRITING_PACISLAND", 
    "ADV_RATE_WRITING_SPED", "ADV_RATE_WRITING_TWOORMORE", "ADV_RATE_WRITING_WHITE",
    "ADV_RATE_SCIENCE_BLACK", "ADV_RATE_SCIENCE_TOTAL", "ADV_RATE_SCIENCE_AMINDIAN", "ADV_RATE_SCIENCE_ASIAN", "ADV_RATE_SCIENCE_ATRISK",
    "ADV_RATE_SCIENCE_ELL", "ADV_RATE_SCIENCE_ECONDISADV", "ADV_RATE_SCIENCE_F", "ADV_RATE_SCIENCE_HISP", "ADV_RATE_SCIENCE_M", "ADV_RATE_SCIENCE_PACISLAND", 
    "ADV_RATE_SCIENCE_SPED", "ADV_RATE_SCIENCE_TWOORMORE", "ADV_RATE_SCIENCE_WHITE",
    "ADV_RATE_SOCIALSCIENCE_BLACK", "ADV_RATE_SOCIALSCIENCE_TOTAL", "ADV_RATE_SOCIALSCIENCE_AMINDIAN", "ADV_RATE_SOCIALSCIENCE_ASIAN", "ADV_RATE_SOCIALSCIENCE_ATRISK",
    "ADV_RATE_SOCIALSCIENCE_ELL", "ADV_RATE_SOCIALSCIENCE_ECONDISADV", "ADV_RATE_SOCIALSCIENCE_F", "ADV_RATE_SOCIALSCIENCE_HISP", "ADV_RATE_SOCIALSCIENCE_M", "ADV_RATE_SOCIALSCIENCE_PACISLAND", 
    "ADV_RATE_SOCIALSCIENCE_SPED", "ADV_RATE_SOCIALSCIENCE_TWOORMORE", "ADV_RATE_SOCIALSCIENCE_WHITE"
  )
colnames_adv_16 <- 
  c("CAMPUS_NUM", 
    "ADV_RATE_ALL_BLACK", "ADV_RATE_MATH_BLACK", "ADV_RATE_READING_BLACK", 
    "ADV_RATE_SCIENCE_BLACK", "ADV_RATE_SOCIALSCIENCE_BLACK", "ADV_RATE_WRITING_BLACK", 
    "ADV_RATE_ALL_TOTAL", "ADV_RATE_MATH_TOTAL", "ADV_RATE_READING_TOTAL", 
    "ADV_RATE_SCIENCE_TOTAL", "ADV_RATE_SOCIALSCIENCE_TOTAL", "ADV_RATE_WRITING_TOTAL",
    "ADV_RATE_ALL_AMINDIAN", "ADV_RATE_MATH_AMINDIAN", "ADV_RATE_READING_AMINDIAN", 
    "ADV_RATE_SCIENCE_AMINDIAN", "ADV_RATE_SOCIALSCIENCE_AMINDIAN", "ADV_RATE_WRITING_AMINDIAN",
    "ADV_RATE_ALL_ASIAN", "ADV_RATE_MATH_ASIAN", "ADV_RATE_READING_ASIAN", 
    "ADV_RATE_SCIENCE_ASIAN", "ADV_RATE_SOCIALSCIENCE_ASIAN", "ADV_RATE_WRITING_ASIAN", 
    "ADV_RATE_ALL_ATRISK", "ADV_RATE_MATH_ATRISK", "ADV_RATE_READING_ATRISK", 
    "ADV_RATE_SCIENCE_ATRISK", "ADV_RATE_SOCIALSCIENCE_ATRISK", "ADV_RATE_WRITING_ATRISK", 
    "ADV_RATE_ALL_ELL", "ADV_RATE_MATH_ELL", "ADV_RATE_READING_ELL", 
    "ADV_RATE_SCIENCE_ELL", "ADV_RATE_SOCIALSCIENCE_ELL", "ADV_RATE_WRITING_ELL", 
    "ADV_RATE_ALL_ECONDISADV", "ADV_RATE_MATH_ECONDISADV", "ADV_RATE_READING_ECONDISADV", 
    "ADV_RATE_SCIENCE_ECONDISADV", "ADV_RATE_SOCIALSCIENCE_ECONDISADV", "ADV_RATE_WRITING_ECONDISADV", 
    "ADV_RATE_ALL_F", "ADV_RATE_MATH_F", "ADV_RATE_READING_F", 
    "ADV_RATE_SCIENCE_F", "ADV_RATE_SOCIALSCIENCE_F", "ADV_RATE_WRITING_F",
    "ADV_RATE_ALL_HISP", "ADV_RATE_MATH_HISP", "ADV_RATE_READING_HISP", 
    "ADV_RATE_SCIENCE_HISP", "ADV_RATE_SOCIALSCIENCE_HISP", "ADV_RATE_WRITING_HISP",
    "ADV_RATE_ALL_M", "ADV_RATE_MATH_M", "ADV_RATE_READING_M", 
    "ADV_RATE_SCIENCE_M", "ADV_RATE_SOCIALSCIENCE_M", "ADV_RATE_WRITING_M",
    "ADV_RATE_ALL_PACISLAND", "ADV_RATE_MATH_PACISLAND", "ADV_RATE_READING_PACISLAND", 
    "ADV_RATE_SCIENCE_PACISLAND", "ADV_RATE_SOCIALSCIENCE_PACISLAND", "ADV_RATE_WRITING_PACISLAND",
    "ADV_RATE_ALL_SPED", "ADV_RATE_MATH_SPED", "ADV_RATE_READING_SPED", 
    "ADV_RATE_SCIENCE_SPED", "ADV_RATE_SOCIALSCIENCE_SPED", "ADV_RATE_WRITING_SPED",
    "ADV_RATE_ALL_TWOORMORE", "ADV_RATE_MATH_TWOORMORE", "ADV_RATE_READING_TWOORMORE", 
    "ADV_RATE_SCIENCE_TWOORMORE", "ADV_RATE_SOCIALSCIENCE_TWOORMORE", "ADV_RATE_WRITING_TWOORMORE",
    "ADV_RATE_ALL_WHITE", "ADV_RATE_MATH_WHITE", "ADV_RATE_READING_WHITE", 
    "ADV_RATE_SCIENCE_WHITE", "ADV_RATE_SOCIALSCIENCE_WHITE", "ADV_RATE_WRITING_WHITE"
  )


satisf_target_order <- 
  c("CAMPUS_NUM", 
    "SATISF_RATE_ALL_TOTAL", "SATISF_RATE_ALL_BLACK", "SATISF_RATE_ALL_AMINDIAN", "SATISF_RATE_ALL_ASIAN", "SATISF_RATE_ALL_ATRISK",
    "SATISF_RATE_ALL_ELL", "SATISF_RATE_ALL_ECONDISADV", "SATISF_RATE_ALL_F", "SATISF_RATE_ALL_HISP", "SATISF_RATE_ALL_M", "SATISF_RATE_ALL_PACISLAND", 
    "SATISF_RATE_ALL_SPED", "SATISF_RATE_ALL_TWOORMORE", "SATISF_RATE_ALL_WHITE",
    "SATISF_RATE_READING_TOTAL", "SATISF_RATE_READING_BLACK", "SATISF_RATE_READING_AMINDIAN", "SATISF_RATE_READING_ASIAN", "SATISF_RATE_READING_ATRISK",
    "SATISF_RATE_READING_ELL", "SATISF_RATE_READING_ECONDISADV", "SATISF_RATE_READING_F", "SATISF_RATE_READING_HISP", "SATISF_RATE_READING_M", "SATISF_RATE_READING_PACISLAND", 
    "SATISF_RATE_READING_SPED", "SATISF_RATE_READING_TWOORMORE", "SATISF_RATE_READING_WHITE",
    "SATISF_RATE_MATH_TOTAL", "SATISF_RATE_MATH_BLACK", "SATISF_RATE_MATH_AMINDIAN", "SATISF_RATE_MATH_ASIAN", "SATISF_RATE_MATH_ATRISK",
    "SATISF_RATE_MATH_ELL", "SATISF_RATE_MATH_ECONDISADV", "SATISF_RATE_MATH_F", "SATISF_RATE_MATH_HISP", "SATISF_RATE_MATH_M", "SATISF_RATE_MATH_PACISLAND", 
    "SATISF_RATE_MATH_SPED", "SATISF_RATE_MATH_TWOORMORE", "SATISF_RATE_MATH_WHITE",
    "SATISF_RATE_WRITING_TOTAL","SATISF_RATE_WRITING_BLACK", "SATISF_RATE_WRITING_AMINDIAN", "SATISF_RATE_WRITING_ASIAN", "SATISF_RATE_WRITING_ATRISK",
    "SATISF_RATE_WRITING_ELL", "SATISF_RATE_WRITING_ECONDISADV", "SATISF_RATE_WRITING_F", "SATISF_RATE_WRITING_HISP", "SATISF_RATE_WRITING_M", "SATISF_RATE_WRITING_PACISLAND", 
    "SATISF_RATE_WRITING_SPED", "SATISF_RATE_WRITING_TWOORMORE", "SATISF_RATE_WRITING_WHITE",
    "SATISF_RATE_SCIENCE_TOTAL", "SATISF_RATE_SCIENCE_BLACK", "SATISF_RATE_SCIENCE_AMINDIAN", "SATISF_RATE_SCIENCE_ASIAN", "SATISF_RATE_SCIENCE_ATRISK",
    "SATISF_RATE_SCIENCE_ELL", "SATISF_RATE_SCIENCE_ECONDISADV", "SATISF_RATE_SCIENCE_F", "SATISF_RATE_SCIENCE_HISP", "SATISF_RATE_SCIENCE_M", "SATISF_RATE_SCIENCE_PACISLAND", 
    "SATISF_RATE_SCIENCE_SPED", "SATISF_RATE_SCIENCE_TWOORMORE", "SATISF_RATE_SCIENCE_WHITE",
    "SATISF_RATE_SOCIALSCIENCE_TOTAL", "SATISF_RATE_SOCIALSCIENCE_BLACK", "SATISF_RATE_SOCIALSCIENCE_AMINDIAN", "SATISF_RATE_SOCIALSCIENCE_ASIAN", "SATISF_RATE_SOCIALSCIENCE_ATRISK",
    "SATISF_RATE_SOCIALSCIENCE_ELL", "SATISF_RATE_SOCIALSCIENCE_ECONDISADV", "SATISF_RATE_SOCIALSCIENCE_F", "SATISF_RATE_SOCIALSCIENCE_HISP", "SATISF_RATE_SOCIALSCIENCE_M", "SATISF_RATE_SOCIALSCIENCE_PACISLAND", 
    "SATISF_RATE_SOCIALSCIENCE_SPED", "SATISF_RATE_SOCIALSCIENCE_TWOORMORE", "SATISF_RATE_SOCIALSCIENCE_WHITE"
  )

adv_target_order <-
  c("CAMPUS_NUM",
    "ADV_RATE_ALL_TOTAL", "ADV_RATE_ALL_BLACK", "ADV_RATE_ALL_AMINDIAN", "ADV_RATE_ALL_ASIAN", "ADV_RATE_ALL_ATRISK",
    "ADV_RATE_ALL_ELL", "ADV_RATE_ALL_ECONDISADV",  "ADV_RATE_ALL_F", "ADV_RATE_ALL_HISP", "ADV_RATE_ALL_M", "ADV_RATE_ALL_PACISLAND", 
    "ADV_RATE_ALL_SPED", "ADV_RATE_ALL_TWOORMORE", "ADV_RATE_ALL_WHITE",
    "ADV_RATE_READING_TOTAL", "ADV_RATE_READING_BLACK", "ADV_RATE_READING_AMINDIAN", "ADV_RATE_READING_ASIAN", "ADV_RATE_READING_ATRISK",
    "ADV_RATE_READING_ELL", "ADV_RATE_READING_ECONDISADV", "ADV_RATE_READING_F", "ADV_RATE_READING_HISP", "ADV_RATE_READING_M", "ADV_RATE_READING_PACISLAND", 
    "ADV_RATE_READING_SPED", "ADV_RATE_READING_TWOORMORE", "ADV_RATE_READING_WHITE",
    "ADV_RATE_MATH_TOTAL", "ADV_RATE_MATH_BLACK", "ADV_RATE_MATH_AMINDIAN", "ADV_RATE_MATH_ASIAN", "ADV_RATE_MATH_ATRISK",
    "ADV_RATE_MATH_ELL", "ADV_RATE_MATH_ECONDISADV", "ADV_RATE_MATH_F", "ADV_RATE_MATH_HISP", "ADV_RATE_MATH_M", "ADV_RATE_MATH_PACISLAND", 
    "ADV_RATE_MATH_SPED", "ADV_RATE_MATH_TWOORMORE", "ADV_RATE_MATH_WHITE",
    "ADV_RATE_WRITING_TOTAL", "ADV_RATE_WRITING_BLACK", "ADV_RATE_WRITING_AMINDIAN", "ADV_RATE_WRITING_ASIAN", "ADV_RATE_WRITING_ATRISK",
    "ADV_RATE_WRITING_ELL", "ADV_RATE_WRITING_ECONDISADV", "ADV_RATE_WRITING_F",  "ADV_RATE_WRITING_HISP", "ADV_RATE_WRITING_M", "ADV_RATE_WRITING_PACISLAND", 
    "ADV_RATE_WRITING_SPED", "ADV_RATE_WRITING_TWOORMORE", "ADV_RATE_WRITING_WHITE",
    "ADV_RATE_SCIENCE_TOTAL", "ADV_RATE_SCIENCE_BLACK", "ADV_RATE_SCIENCE_AMINDIAN", "ADV_RATE_SCIENCE_ASIAN", "ADV_RATE_SCIENCE_ATRISK",
    "ADV_RATE_SCIENCE_ELL", "ADV_RATE_SCIENCE_ECONDISADV", "ADV_RATE_SCIENCE_F", "ADV_RATE_SCIENCE_HISP", "ADV_RATE_SCIENCE_M", "ADV_RATE_SCIENCE_PACISLAND", 
    "ADV_RATE_SCIENCE_SPED", "ADV_RATE_SCIENCE_TWOORMORE", "ADV_RATE_SCIENCE_WHITE",
    "ADV_RATE_SOCIALSCIENCE_TOTAL", "ADV_RATE_SOCIALSCIENCE_BLACK", "ADV_RATE_SOCIALSCIENCE_AMINDIAN", "ADV_RATE_SOCIALSCIENCE_ASIAN", "ADV_RATE_SOCIALSCIENCE_ATRISK",
    "ADV_RATE_SOCIALSCIENCE_ELL", "ADV_RATE_SOCIALSCIENCE_ECONDISADV", "ADV_RATE_SOCIALSCIENCE_F", "ADV_RATE_SOCIALSCIENCE_HISP", "ADV_RATE_SOCIALSCIENCE_M", "ADV_RATE_SOCIALSCIENCE_PACISLAND", 
    "ADV_RATE_SOCIALSCIENCE_SPED", "ADV_RATE_SOCIALSCIENCE_TWOORMORE", "ADV_RATE_SOCIALSCIENCE_WHITE"
  )

combined_target_order <- 
  c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME", "REGION","GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN", 
    "SATISF_RATE_ALL_TOTAL", "ADV_RATE_ALL_TOTAL", "SATISF_RATE_ALL_BLACK", "ADV_RATE_ALL_BLACK",
    "SATISF_RATE_ALL_AMINDIAN", "ADV_RATE_ALL_AMINDIAN", "SATISF_RATE_ALL_ASIAN", "ADV_RATE_ALL_ASIAN",
    "SATISF_RATE_ALL_ATRISK", "ADV_RATE_ALL_ATRISK", "SATISF_RATE_ALL_ELL", "ADV_RATE_ALL_ELL",
    "SATISF_RATE_ALL_ECONDISADV", "ADV_RATE_ALL_ECONDISADV", "SATISF_RATE_ALL_F", "ADV_RATE_ALL_F",
    "SATISF_RATE_ALL_HISP", "ADV_RATE_ALL_HISP", "SATISF_RATE_ALL_M", "ADV_RATE_ALL_M",
    "SATISF_RATE_ALL_PACISLAND", "ADV_RATE_ALL_PACISLAND", "SATISF_RATE_ALL_SPED", "ADV_RATE_ALL_SPED", 
    "SATISF_RATE_ALL_TWOORMORE", "ADV_RATE_ALL_TWOORMORE", "SATISF_RATE_ALL_WHITE", "ADV_RATE_ALL_WHITE",
    "SATISF_RATE_READING_TOTAL", "ADV_RATE_READING_TOTAL", "SATISF_RATE_READING_BLACK", "ADV_RATE_READING_BLACK",
    "SATISF_RATE_READING_AMINDIAN", "ADV_RATE_READING_AMINDIAN", "SATISF_RATE_READING_ASIAN", "ADV_RATE_READING_ASIAN", 
    "SATISF_RATE_READING_ATRISK", "ADV_RATE_READING_ATRISK", "SATISF_RATE_READING_ELL", "ADV_RATE_READING_ELL", 
    "SATISF_RATE_READING_ECONDISADV", "ADV_RATE_READING_ECONDISADV", "SATISF_RATE_READING_F", "ADV_RATE_READING_F",
    "SATISF_RATE_READING_HISP", "ADV_RATE_READING_HISP", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
    "SATISF_RATE_READING_PACISLAND", "ADV_RATE_READING_PACISLAND", "SATISF_RATE_READING_SPED", "ADV_RATE_READING_SPED",
    "SATISF_RATE_READING_TWOORMORE", "ADV_RATE_READING_TWOORMORE", "SATISF_RATE_READING_WHITE", "ADV_RATE_READING_WHITE",
    "SATISF_RATE_MATH_TOTAL", "ADV_RATE_MATH_TOTAL", "SATISF_RATE_MATH_BLACK", "ADV_RATE_MATH_BLACK",
    "SATISF_RATE_MATH_AMINDIAN", "ADV_RATE_MATH_AMINDIAN", "SATISF_RATE_MATH_ASIAN", "ADV_RATE_MATH_ASIAN",
    "SATISF_RATE_MATH_ATRISK", "ADV_RATE_MATH_ATRISK", "SATISF_RATE_MATH_ELL", "ADV_RATE_MATH_ELL",
    "SATISF_RATE_MATH_ECONDISADV", "ADV_RATE_MATH_ECONDISADV", "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F",
    "SATISF_RATE_MATH_HISP", "ADV_RATE_MATH_HISP", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M",
    "SATISF_RATE_MATH_PACISLAND", "ADV_RATE_MATH_PACISLAND", "SATISF_RATE_MATH_SPED", "ADV_RATE_MATH_SPED",
    "SATISF_RATE_MATH_TWOORMORE", "ADV_RATE_MATH_TWOORMORE", "SATISF_RATE_MATH_WHITE", "ADV_RATE_MATH_WHITE",
    "SATISF_RATE_WRITING_TOTAL", "ADV_RATE_WRITING_TOTAL", "SATISF_RATE_WRITING_BLACK", "ADV_RATE_WRITING_BLACK", 
    "SATISF_RATE_WRITING_AMINDIAN", "ADV_RATE_WRITING_AMINDIAN", "SATISF_RATE_WRITING_ASIAN", "ADV_RATE_WRITING_ASIAN", 
    "SATISF_RATE_WRITING_ATRISK", "ADV_RATE_WRITING_ATRISK", "SATISF_RATE_WRITING_ELL", "ADV_RATE_WRITING_ELL", 
    "SATISF_RATE_WRITING_ECONDISADV", "ADV_RATE_WRITING_ECONDISADV", "SATISF_RATE_WRITING_F", "ADV_RATE_WRITING_F",
    "SATISF_RATE_WRITING_HISP", "ADV_RATE_WRITING_HISP", "SATISF_RATE_WRITING_M", "ADV_RATE_WRITING_M",
    "SATISF_RATE_WRITING_PACISLAND", "ADV_RATE_WRITING_PACISLAND", "SATISF_RATE_WRITING_SPED", "ADV_RATE_WRITING_SPED",
    "SATISF_RATE_WRITING_TWOORMORE", "ADV_RATE_WRITING_TWOORMORE", "SATISF_RATE_WRITING_WHITE", "ADV_RATE_WRITING_WHITE",
    "SATISF_RATE_SCIENCE_TOTAL", "ADV_RATE_SCIENCE_TOTAL", "SATISF_RATE_SCIENCE_BLACK", "ADV_RATE_SCIENCE_BLACK",
    "SATISF_RATE_SCIENCE_AMINDIAN", "ADV_RATE_SCIENCE_AMINDIAN", "SATISF_RATE_SCIENCE_ASIAN", "ADV_RATE_SCIENCE_ASIAN",
    "SATISF_RATE_SCIENCE_ATRISK", "ADV_RATE_SCIENCE_ATRISK", "SATISF_RATE_SCIENCE_ELL", "ADV_RATE_SCIENCE_ELL", 
    "SATISF_RATE_SCIENCE_ECONDISADV", "ADV_RATE_SCIENCE_ECONDISADV", "SATISF_RATE_SCIENCE_F", "ADV_RATE_SCIENCE_F",
    "SATISF_RATE_SCIENCE_HISP", "ADV_RATE_SCIENCE_HISP", "SATISF_RATE_SCIENCE_M", "ADV_RATE_SCIENCE_M",
    "SATISF_RATE_SCIENCE_PACISLAND", "ADV_RATE_SCIENCE_PACISLAND", "SATISF_RATE_SCIENCE_SPED", "ADV_RATE_SCIENCE_SPED", 
    "SATISF_RATE_SCIENCE_TWOORMORE", "ADV_RATE_SCIENCE_TWOORMORE", "SATISF_RATE_SCIENCE_WHITE", "ADV_RATE_SCIENCE_WHITE",
    "SATISF_RATE_SOCIALSCIENCE_TOTAL", "ADV_RATE_SOCIALSCIENCE_TOTAL", "SATISF_RATE_SOCIALSCIENCE_BLACK", "ADV_RATE_SOCIALSCIENCE_BLACK",
    "SATISF_RATE_SOCIALSCIENCE_AMINDIAN", "ADV_RATE_SOCIALSCIENCE_AMINDIAN", "SATISF_RATE_SOCIALSCIENCE_ASIAN", "ADV_RATE_SOCIALSCIENCE_ASIAN",
    "SATISF_RATE_SOCIALSCIENCE_ATRISK", "ADV_RATE_SOCIALSCIENCE_ATRISK", "SATISF_RATE_SOCIALSCIENCE_ELL", "ADV_RATE_SOCIALSCIENCE_ELL",
    "SATISF_RATE_SOCIALSCIENCE_ECONDISADV", "ADV_RATE_SOCIALSCIENCE_ECONDISADV", "SATISF_RATE_SOCIALSCIENCE_F", "ADV_RATE_SOCIALSCIENCE_F",
    "SATISF_RATE_SOCIALSCIENCE_HISP", "ADV_RATE_SOCIALSCIENCE_HISP", "SATISF_RATE_SOCIALSCIENCE_M", "ADV_RATE_SOCIALSCIENCE_M",
    "SATISF_RATE_SOCIALSCIENCE_PACISLAND", "ADV_RATE_SOCIALSCIENCE_PACISLAND", "SATISF_RATE_SOCIALSCIENCE_SPED", "ADV_RATE_SOCIALSCIENCE_SPED",
    "SATISF_RATE_SOCIALSCIENCE_TWOORMORE", "ADV_RATE_SOCIALSCIENCE_TWOORMORE", "SATISF_RATE_SOCIALSCIENCE_WHITE", "ADV_RATE_SOCIALSCIENCE_WHITE"
  )



list_info <- c("CAMPUS_NUM", "CAMPUS_NAME", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM",
               "GRADE_SPAN", "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "CAMPUS_DISTRICT_PAIR_NAME", "REGION")
list_info_order <- c("CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME", "REGION",
                     "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN")

stud_list <- c("CAMPUS_NUM",
               "ALL_COUNT", "ASIAN_COUNT", "ASIAN_PCT", "BLACK_COUNT", "BLACK_PCT", "DAEP_COUNT", "DAEP_PCT",
               "ECONDISADV_COUNT", "ECONDISADV_PCT", "HISPANIC_COUNT", "HISPANIC_PCT", 
               "AMINDIAN_COUNT", "AMINDIAN_PCT", "LEP_COUNT", "LEP_PCT", "NONEDDISADV_COUNT", "NONEDDISADV_PCT",
               "PACISLAND_COUNT", "PACISLAND_PCT", "ATRISK_COUNT", "ATRISK_PCT", "TWOORMORE_COUNT", "TWOORMORE_PCT",
               "WHITE_COUNT", "WHITE_PCT")
### Demographics --------------------------------------------------------------------------------------------------------
# # 2011-12 -----
# # read in csv file
# # skip the first 2 lines because it isn't part of the datatable
# data_demo_12 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2011-2012.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # clean everything
# data_demo_12 <- clean_demo(data_demo_12)
# 
# # clean year
# data_demo_12[data_demo_12 == '2011-2012'] <- '2012'
# 
# # write.csv(data_demo_12, "cleaned_data/tx_enroll_2011_12.csv", row.names = FALSE)
# 
# 
# # 2012-13 -----
# # read in csv file
# # skip the first 2 lines because it isn't part of the datatable
# data_demo_13 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2012-2013.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # clean everything
# data_demo_13 <- clean_demo(data_demo_13)
# 
# # clean year
# data_demo_13[data_demo_13 == '2012-2013'] <- '2013'
# 
# # merge the datasets together
# data_demo <- full_join(data_demo_12, data_demo_13)
# 
# # write.csv(data_demo_13, "cleaned_data/tx_enroll_2012_13.csv", row.names = FALSE)
# 
# # 2013-14 -----
# data_demo_14 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2013-2014.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # clean everything
# data_demo_14 <- clean_demo(data_demo_14)
# 
# # clean year
# data_demo_14[data_demo_14 == '2013-2014'] <- '2014'
# 
# # merge the datasets together
# data_demo <- full_join(data_demo, data_demo_14)
# 
# # write.csv(data_demo_14, "cleaned_data/tx_demo_2013-14.csv", row.names = FALSE)
# 
# # 2014-15 -----
# data_demo_15 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2014-2015.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # clean everything
# data_demo_15 <- clean_demo(data_demo_15)
# 
# # clean year
# data_demo_15[data_demo_15 == '2014-2015'] <- '2015'
# 
# # merge the datasets together
# data_demo <- full_join(data_demo, data_demo_15)
# 
# # write.csv(data_demo_15, "cleaned_data/tx_enroll_2014_15.csv", row.names = FALSE)
# 
# 
# # 2015-16 -----
# data_demo_16 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2015-2016.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # clean everything
# data_demo_16 <- clean_demo(data_demo_16)
# 
# # data_demo_16 year
# data_demo_16[data_demo_16 == '2015-2016'] <- '2016'
# 
# # merge the datasets together
# data_demo <- full_join(data_demo, data_demo_16)
# 
# # write.csv(data_demo_16, "cleaned_data/tx_enroll_2015_16.csv", row.names = FALSE)
# 
# 
# # 2016-17 -----
# data_demo_17 <- read.csv("data/demo/Enrollment Report_Statewide_Campuses_Ethnicity_2016-2017.csv", header = TRUE, skip = 2, stringsAsFactors = FALSE)
# 
# # get rid of column not common with others
# data_demo_17$Grade.Group <- NULL
# 
# # clean everything
# data_demo_17 <- clean_demo(data_demo_17)
# 
# # data_demo_16 year
# data_demo_17[data_demo_17 == '2016-2017'] <- '2017'
# 
# # merge the datasets together
# data_demo <- full_join(data_demo, data_demo_17)
# 
# # write.csv(data_demo_17, "cleaned_data/tx_enroll_2015_16.csv", row.names = FALSE)
# 
# 
# # Finish -----
# # Save as .csv file
# # write.csv(data_demo,"cleaned_data/tx_enroll_2012_16.csv", row.names = FALSE)
# 
# 
#  

# School Info --------------------------------------------------------------------------------------------------------
# need to read .dat files
library(readr)

# 2012-13 -----
# dataset: school info
data_info_13 <- read_csv("data/perf/CREF_201213.dat")
colnames(data_info_13) <- list_info

# 2013-14 -----
# dataset: school info
data_info_14 <- read_csv("data/perf/CREF_201314.dat")
colnames(data_info_14) <- list_info

# 2014-15 -----
# dataset: school info
data_info_15 <- read_csv("data/perf/CREF_201415.dat")
colnames(data_info_15) <- list_info

# 2015-16 -----
# dataset: school info
data_info_16 <- read_csv("data/perf/CREF_201516.dat")
colnames(data_info_16) <- list_info
# get rid of the one column the other info datasets dont have
data_info_16$REGNNAME <- NULL
data_info_16[,14] <- NULL

# Demographicsv2 --------------------------------------------------------------------------------------------------------
# 2012-13 -----
# dataset: student info
data_stud_13 <- read_csv("data/perf/CSTUD_201213.dat")

# change colnames
colnames(data_stud_13) <- stud_list

# select only what we want
data_stud_13 <- data_stud_13 %>% 
                select(c("CAMPUS_NUM","ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "DAEP_COUNT", 
                         "ECONDISADV_COUNT",  "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT", 
                         "NONEDDISADV_COUNT", "PACISLAND_COUNT",  "ATRISK_COUNT", "TWOORMORE_COUNT", 
                         "WHITE_COUNT"))

# combine with school info
data_enroll_13 <- left_join(data_info_13, data_stud_13, by="CAMPUS_NUM")

# turn -1 and . into NAs
data_enroll_13[data_enroll_13 == '-1'] <- NA
data_enroll_13[data_enroll_13 == '.'] <- NA

# turn NAs into -99
data_enroll_13[is.na(data_enroll_13)] <- -99

# turn Y => 1 and N => 0
data_enroll_13[data_enroll_13 == 'Y'] <- 1
data_enroll_13[data_enroll_13 == 'N'] <- 0

# get rid of commas and tildas
data_enroll_13[data_enroll_13 == ','] <- " "
data_enroll_13[data_enroll_13 == '~'] <- "-"

# get rid of special characters
data_enroll_13$CAMPUS_NAME <- no_more_special_characters(data_enroll_13$CAMPUS_NAME)
data_enroll_13$COUNTY_NAME <- no_more_special_characters(data_enroll_13$COUNTY_NAME)
data_enroll_13$DISTRICT_NAME <- no_more_special_characters(data_enroll_13$DISTRICT_NAME)
data_enroll_13$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_enroll_13$CAMPUS_DISTRICT_PAIR_NAME)
data_enroll_13$REGION <- no_more_special_characters(data_enroll_13$REGION)

# add year
data_enroll_13$YEAR <- 2013

# put year first
data_enroll_13 <- data_enroll_13 %>% 
  select(YEAR, everything())

# finish
write.csv(data_enroll_13, "cleaned_data/tx_enroll_2012_13", row.names = FALSE)

# 2013-14 -----
# dataset: student info
data_stud_14 <- read_csv("data/perf/CSTUD_201314.dat")

# change colnames
colnames(data_stud_14) <- stud_list

# select only what we want
data_stud_14 <- data_stud_14 %>% 
  select(c("CAMPUS_NUM","ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "DAEP_COUNT", 
           "ECONDISADV_COUNT",  "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT", 
           "NONEDDISADV_COUNT", "PACISLAND_COUNT",  "ATRISK_COUNT", "TWOORMORE_COUNT", 
           "WHITE_COUNT"))

# combine with school info
data_enroll_14 <- left_join(data_info_14, data_stud_14, by="CAMPUS_NUM")

# turn -1 and . into NAs
data_enroll_14[data_enroll_14 == '-1'] <- NA
data_enroll_14[data_enroll_14 == '.'] <- NA

# turn NAs into -99
data_enroll_14[is.na(data_enroll_14)] <- -99

# turn Y => 1 and N => 0
data_enroll_14[data_enroll_14 == 'Y'] <- 1
data_enroll_14[data_enroll_14 == 'N'] <- 0

# get rid of commas and tildas
data_enroll_14[data_enroll_14 == ','] <- " "
data_enroll_14[data_enroll_14 == '~'] <- "-"

# get rid of special characters
data_enroll_14$CAMPUS_NAME <- no_more_special_characters(data_enroll_14$CAMPUS_NAME)
data_enroll_14$COUNTY_NAME <- no_more_special_characters(data_enroll_14$COUNTY_NAME)
data_enroll_14$DISTRICT_NAME <- no_more_special_characters(data_enroll_14$DISTRICT_NAME)
data_enroll_14$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_enroll_14$CAMPUS_DISTRICT_PAIR_NAME)
data_enroll_14$REGION <- no_more_special_characters(data_enroll_14$REGION)

# add year
data_enroll_14$YEAR <- 2014

# put year first
data_enroll_14 <- data_enroll_14 %>% 
  select(YEAR, everything())

# merge
data_enroll <- full_join(data_enroll_13, data_enroll_14)

# finish
write.csv(data_enroll_14, "cleaned_data/tx_enroll_2013_14", row.names = FALSE)

# 2014-15 -----
# dataset: student info
data_stud_15 <- read_csv("data/perf/CSTUD_201415.dat")

# change colnames
colnames(data_stud_15) <- stud_list

# select only what we want
data_stud_15 <- data_stud_15 %>% 
  select(c("CAMPUS_NUM","ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "DAEP_COUNT", 
           "ECONDISADV_COUNT",  "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT", 
           "NONEDDISADV_COUNT", "PACISLAND_COUNT",  "ATRISK_COUNT", "TWOORMORE_COUNT", 
           "WHITE_COUNT"))

# combine with school info
data_enroll_15 <- left_join(data_info_15, data_stud_15, by="CAMPUS_NUM")

# turn -1 and . into NAs
data_enroll_15[data_enroll_15 == '-1'] <- NA
data_enroll_15[data_enroll_15 == '.'] <- NA

# turn NAs into -99
data_enroll_15[is.na(data_enroll_15)] <- -99

# turn Y => 1 and N => 0
data_enroll_15[data_enroll_15 == 'Y'] <- 1
data_enroll_15[data_enroll_15 == 'N'] <- 0

# get rid of commas and tildas
data_enroll_15[data_enroll_15 == ','] <- " "
data_enroll_15[data_enroll_15 == '~'] <- "-"

# get rid of special characters
data_enroll_15$CAMPUS_NAME <- no_more_special_characters(data_enroll_15$CAMPUS_NAME)
data_enroll_15$COUNTY_NAME <- no_more_special_characters(data_enroll_15$COUNTY_NAME)
data_enroll_15$DISTRICT_NAME <- no_more_special_characters(data_enroll_15$DISTRICT_NAME)
data_enroll_15$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_enroll_15$CAMPUS_DISTRICT_PAIR_NAME)
data_enroll_15$REGION <- no_more_special_characters(data_enroll_15$REGION)

# add year
data_enroll_15$YEAR <- 2015

# put year first
data_enroll_15 <- data_enroll_15 %>% 
  select(YEAR, everything())


# merge
data_enroll <- full_join(data_enroll, data_enroll_15)

# finish
write.csv(data_enroll_15, "cleaned_data/tx_enroll_2014_15", row.names = FALSE)

# 2015-16 -----
# dataset: student info
data_stud_16 <- read_csv("data/perf/CSTUD_201516.dat")

# change colnames
colnames(data_stud_16) <- stud_list

# select only what we want
data_stud_16 <- data_stud_16 %>% 
  select(c("CAMPUS_NUM","ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "DAEP_COUNT", 
           "ECONDISADV_COUNT",  "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT", 
           "NONEDDISADV_COUNT", "PACISLAND_COUNT",  "ATRISK_COUNT", "TWOORMORE_COUNT", 
           "WHITE_COUNT"))

# combine with school info
data_enroll_16 <- left_join(data_info_16, data_stud_16, by="CAMPUS_NUM")

# turn -1 and . into NAs
data_enroll_16[data_enroll_16 == '-1'] <- NA
data_enroll_16[data_enroll_16 == '.'] <- NA

# turn NAs into -99
data_enroll_16[is.na(data_enroll_16)] <- -99

# turn Y => 1 and N => 0
data_enroll_16[data_enroll_16 == 'Y'] <- 1
data_enroll_16[data_enroll_16 == 'N'] <- 0

# get rid of commas and tildas
data_enroll_16[data_enroll_16 == ','] <- " "
data_enroll_16[data_enroll_16 == '~'] <- "-"

# get rid of special characters
data_enroll_16$CAMPUS_NAME <- no_more_special_characters(data_enroll_16$CAMPUS_NAME)
data_enroll_16$COUNTY_NAME <- no_more_special_characters(data_enroll_16$COUNTY_NAME)
data_enroll_16$DISTRICT_NAME <- no_more_special_characters(data_enroll_16$DISTRICT_NAME)
data_enroll_16$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_enroll_16$CAMPUS_DISTRICT_PAIR_NAME)
data_enroll_16$REGION <- no_more_special_characters(data_enroll_16$REGION)

# add year
data_enroll_16$YEAR <- 2016

# put year first
data_enroll_16 <- data_enroll_16 %>% 
                    select(YEAR, everything())

# merge
data_enroll <- full_join(data_enroll, data_enroll_16)

# finish
write.csv(data_enroll_16, "cleaned_data/tx_enroll_2015_16", row.names = FALSE)

# final finish -----
write.csv(data_enroll, "cleaned_data/tx_enroll_2013_16", row.names = FALSE)

# Performance --------------------------------------------------------------------------------------------------------
# For grades 3-11
# need to read .dat files
library(readr)

# 2012-13 -----
# read in csv file
# dataset: satisfactory 
data_satisf_13 <- read_csv("data/perf/CSTAAR4_201213.dat")
# dataset: advanced
data_adv_13 <- read_csv("data/perf/CSTAAR6_201213.dat")


# change colnames
colnames(data_satisf_13) <- colnames_satisf_131415
colnames(data_adv_13) <- colnames_adv_131415


# change col order
data_satisf_13 <- setcolorder(data_satisf_13, satisf_target_order)
data_adv_13 <- setcolorder(data_adv_13, adv_target_order)

# combine them together
data_perf_13 <- left_join(data_satisf_13, data_adv_13, by="CAMPUS_NUM")
data_perf_13 <- left_join(data_info_13, data_perf_13, by= "CAMPUS_NUM")

# add year to it
data_perf_13$YEAR <- 2013

# changes the order of the columns 
data_perf_13 <- setcolorder(data_perf_13, combined_target_order)

# turn -1 and . into NAs
data_perf_13[data_perf_13 == '-1'] <- NA
data_perf_13[data_perf_13 == '.'] <- NA

# turn NAs into -99
data_perf_13[is.na(data_perf_13)] <- -99

# turn Y => 1 and N => 0
data_perf_13[data_perf_13 == 'Y'] <- 1
data_perf_13[data_perf_13 == 'N'] <- 0

# get rid of commas and tildas
data_perf_13[data_perf_13 == ','] <- " "
data_perf_13[data_perf_13 == '~'] <- "-"

# get rid of special characters
data_perf_13$CAMPUS_NAME <- no_more_special_characters(data_perf_13$CAMPUS_NAME)
data_perf_13$COUNTY_NAME <- no_more_special_characters(data_perf_13$COUNTY_NAME)
data_perf_13$DISTRICT_NAME <- no_more_special_characters(data_perf_13$DISTRICT_NAME)
data_perf_13$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_perf_13$CAMPUS_DISTRICT_PAIR_NAME)
data_perf_13$REGION <- no_more_special_characters(data_perf_13$REGION)

# select only the columns we need
data_perf_13 <- data_perf_13 %>% 
              select(c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME",
                       "REGION", "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN",
                       "SATISF_RATE_READING_F", "ADV_RATE_READING_F", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
                       "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M"))

write.csv(data_perf_13, "cleaned_data/tx_perf_2012_13.csv", row.names = FALSE)


# 2013-14 -----
# read in csv file
# dataset: satisfactory 
data_satisf_14 <- read_csv("data/perf/CSTAAR4_201314.dat")
# dataset: advanced
data_adv_14 <- read_csv("data/perf/CSTAAR6_201314.dat")


# change colnames
colnames(data_satisf_14) <- colnames_satisf_131415
colnames(data_adv_14) <- colnames_adv_131415


# change col order
data_satisf_14 <- setcolorder(data_satisf_14, satisf_target_order)
data_adv_14 <- setcolorder(data_adv_14, adv_target_order)
# data_info_14 <- setcolorder(data_info_14, list_info_order)

# combine them together
data_perf_14 <- left_join(data_satisf_14, data_adv_14, by="CAMPUS_NUM")
data_perf_14 <- left_join(data_info_14, data_perf_14, by= "CAMPUS_NUM")

# add year to it
data_perf_14$YEAR <- 2014

# changes the order of the columns 
data_perf_14 <- setcolorder(data_perf_14, combined_target_order)

# turn -1 and . into NAs
data_perf_14[data_perf_14 == '-1'] <- NA
data_perf_14[data_perf_14 == '.'] <- NA

# turn NAs into -99
data_perf_14[is.na(data_perf_14)] <- -99

# turn Y => 1 and N => 0
data_perf_14[data_perf_14 == 'Y'] <- 1
data_perf_14[data_perf_14 == 'N'] <- 0

# get rid of commas and tildas
data_perf_14[data_perf_14 == ','] <- " "
data_perf_14[data_perf_14 == '~'] <- "-"

# get rid of special characters
data_perf_14$CAMPUS_NAME <- no_more_special_characters(data_perf_14$CAMPUS_NAME)
data_perf_14$COUNTY_NAME <- no_more_special_characters(data_perf_14$COUNTY_NAME)
data_perf_14$DISTRICT_NAME <- no_more_special_characters(data_perf_14$DISTRICT_NAME)
data_perf_14$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_perf_14$CAMPUS_DISTRICT_PAIR_NAME)
data_perf_14$REGION <- no_more_special_characters(data_perf_14$REGION)

# select only the columns we need
data_perf_14 <- data_perf_14 %>% 
  select(c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME",
           "REGION", "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN",
           "SATISF_RATE_READING_F", "ADV_RATE_READING_F", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
           "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M"))

# merge into one
data_perf <- full_join(data_perf_13, data_perf_14)

write.csv(data_perf_14, "cleaned_data/tx_perf_2013_14.csv", row.names = FALSE)

# 2014-15 -----
# read in csv file
# dataset: satisfactory 
data_satisf_15 <- read_csv("data/perf/CSTAAR4_201415.dat")
# dataset: advanced
data_adv_15 <- read_csv("data/perf/CSTAAR6_201415.dat")


# change colnames
colnames(data_satisf_15) <- colnames_satisf_131415
colnames(data_adv_15) <- colnames_adv_131415

# change col order
data_satisf_15 <- setcolorder(data_satisf_15, satisf_target_order)
data_adv_15 <- setcolorder(data_adv_15, adv_target_order)

# combine them together
data_perf_15 <- left_join(data_satisf_15, data_adv_15, by="CAMPUS_NUM")
data_perf_15 <- left_join(data_info_15, data_perf_15, by= "CAMPUS_NUM")

# add year to it
data_perf_15$YEAR <- 2015

# changes the order of the columns 
data_perf_15 <- setcolorder(data_perf_15, combined_target_order)

# turn -1 and . into NAs
data_perf_15[data_perf_15 == '-1'] <- NA
data_perf_15[data_perf_15 == '.'] <- NA

# turn NAs into -99
data_perf_15[is.na(data_perf_15)] <- -99

# turn Y => 1 and N => 0
data_perf_15[data_perf_15 == 'Y'] <- 1
data_perf_15[data_perf_15 == 'N'] <- 0

# get rid of commas and tildas
data_perf_15[data_perf_15 == ','] <- " "
data_perf_15[data_perf_15 == '~'] <- "-"

# get rid of special characters
data_perf_15$CAMPUS_NAME <- no_more_special_characters(data_perf_15$CAMPUS_NAME)
data_perf_15$COUNTY_NAME <- no_more_special_characters(data_perf_15$COUNTY_NAME)
data_perf_15$DISTRICT_NAME <- no_more_special_characters(data_perf_15$DISTRICT_NAME)
data_perf_15$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_perf_15$CAMPUS_DISTRICT_PAIR_NAME)
data_perf_15$REGION <- no_more_special_characters(data_perf_15$REGION)

# select only the columns we need
data_perf_15 <- data_perf_15 %>% 
  select(c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME",
           "REGION", "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN",
           "SATISF_RATE_READING_F", "ADV_RATE_READING_F", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
           "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M"))

# merge into one
data_perf <- full_join(data_perf, data_perf_15)

write.csv(data_perf_15, "cleaned_data/tx_perf_2014_15.csv", row.names = FALSE)

# 2015-16 -----
# read in csv file
# dataset: satisfactory 
data_satisf_16 <- read_csv("data/perf/CSTAAR4_201516.dat")
# dataset: advanced
data_adv_16 <- read_csv("data/perf/CSTAAR6_201516.dat")


# change colnames
colnames(data_satisf_16) <- colnames_satisf_16
colnames(data_adv_16) <- colnames_adv_16

# change col order
data_satisf_16 <- setcolorder(data_satisf_16, satisf_target_order)
data_adv_16 <- setcolorder(data_adv_16, adv_target_order)

# combine them together
data_perf_16 <- left_join(data_satisf_16, data_adv_16, by="CAMPUS_NUM")
data_perf_16 <- left_join(data_info_16, data_perf_16, by= "CAMPUS_NUM")

# add year to it
data_perf_16$YEAR <- 2016

# changes the order of the columns 
data_perf_16 <- setcolorder(data_perf_16, combined_target_order)

# turn -1 and . into NAs
data_perf_16[data_perf_16 == '-1'] <- NA
data_perf_16[data_perf_16 == '.'] <- NA

# turn NAs into -99
data_perf_16[is.na(data_perf_16)] <- -99

# turn Y => 1 and N => 0
data_perf_16[data_perf_16 == 'Y'] <- 1
data_perf_16[data_perf_16 == 'N'] <- 0

# get rid of commas and tildas
data_perf_16[data_perf_16 == ','] <- " "
data_perf_16[data_perf_16 == '~'] <- "-"

# get rid of special characters
data_perf_16$CAMPUS_NAME <- no_more_special_characters(data_perf_16$CAMPUS_NAME)
data_perf_16$COUNTY_NAME <- no_more_special_characters(data_perf_16$COUNTY_NAME)
data_perf_16$DISTRICT_NAME <- no_more_special_characters(data_perf_16$DISTRICT_NAME)
data_perf_16$CAMPUS_DISTRICT_PAIR_NAME <- no_more_special_characters(data_perf_16$CAMPUS_DISTRICT_PAIR_NAME)
data_perf_16$REGION <- no_more_special_characters(data_perf_16$REGION)

# select only the columns we need
data_perf_16 <- data_perf_16 %>% 
  select(c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM", "CAMPUS_DISTRICT_PAIR_NAME",
           "REGION", "GRADE_TYPE", "CAMPUS_DISTRICT_PAIR_NUM", "RATED_UNDER_AEA_PROCEDURES", "CHARTER", "GRADE_SPAN",
           "SATISF_RATE_READING_F", "ADV_RATE_READING_F", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
           "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M"))

# merge into one
data_perf <- full_join(data_perf, data_perf_16)

write.csv(data_perf_16, "cleaned_data/tx_perf_2015_16.csv", row.names = FALSE)

# Finish  -----
# Save as .csv file
write.csv(data_perf,"cleaned_data/tx_perf_2013_16.csv", row.names = FALSE)


# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/tx_perf_enroll_clean.Rdata")
# Save as .RDS 
saveRDS(data_enroll, file="cleaned_data/tx_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/tx_perf_clean.rds")


