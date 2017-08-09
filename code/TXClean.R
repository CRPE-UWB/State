#### Center on Reinventing Public Education #### 
# Description: Cleaning data obtained from the website of Texas' Department of Education 
#              on Performance and Demographics/Enrollment
# Title: Cleaning Texas 
# Created by: Kevin Cha on 07-17-17
# Updated by: Kevin Cha on 08-08-17
# Data from: 
#    2015-16: https://rptsvr1.tea.texas.gov/perfreport/tapr/2016/xplore/DownloadSelData.html
#    2014-15: https://rptsvr1.tea.texas.gov/perfreport/tapr/2015/xplore/DownloadSelData.html
#    2013-14: https://rptsvr1.tea.texas.gov/perfreport/tapr/2014/xplore/DownloadSelData.html
#    2012-13: https://rptsvr1.tea.texas.gov/perfreport/tapr/2013/xplore/DownloadSelData.html
#    2011-12: https://rptsvr1.tea.texas.gov/cgi/sas/broker
#    2010-11: https://rptsvr1.tea.texas.gov/cgi/sas/broker
# Codebooks:
#    2015-16: https://rptsvr1.tea.texas.gov/perfreport/tapr/2016/xplore/taprref.html
#    2014-15: https://rptsvr1.tea.texas.gov/perfreport/tapr/2015/xplore/taprref.html
#    2013-14: https://rptsvr1.tea.texas.gov/perfreport/tapr/2014/xplore/taprref.html
#    2012-13: https://rptsvr1.tea.texas.gov/perfreport/tapr/2013/xplore/taprref.html  
#    2011-12: https://rptsvr1.tea.texas.gov/perfreport/aeis/2012/xplore/aeisref.html
#    2010-11: https://rptsvr1.tea.texas.gov/perfreport/aeis/2011/xplore/aeisref.html
# Link to Github: https://github.com/CRPE-UWB/State
# Notes: 
#   -2012 only has data for Grades 10-11 => Doesn't Exist

# Setup --------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/al_tx_clean") #MAC
setwd("C:/Users/phato_000/Documents/CRPE/state/TX") #PC

library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
# need to read .dat files
library(readr)

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


clean_it <- function(df) {
  # turn -1 and . into NAs
  df[df == '-1'] <- NA
  df[df == '.'] <- NA
  
  # turn NAs into -99
  df[is.na(df)] <- -99
  
  # turn Y => 1 and N => 0
  df[df == 'Y'] <- 1
  df[df == 'N'] <- 0
  
  # get rid of commas and tildas
  df[df == ','] <- " "
  df[df == '~'] <- "-"
  
  # get rid of special characters
  df$CAMPUS_NAME <- no_more_special_characters(df$CAMPUS_NAME)
  df$COUNTY_NAME <- no_more_special_characters(df$COUNTY_NAME)
  df$DISTRICT_NAME <- no_more_special_characters(df$DISTRICT_NAME)
  
  return(df)
}



# Lists --------------------------------------------------------------------------------------------------------
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

list_info_1112 <- c("CAMPUS_NUM", "CAMPUS_NAME", "CHARTER", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME")
list_info_1316 <- c("CAMPUS_NUM", "CAMPUS_NAME", "CHARTER", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM")

stud_list_1112 <- c("CAMPUS_NUM", "ALL_COUNT2", "ASIAN_PCT2", "BLACK_PCT2", "ECONDISADV_PCT2", "HIPSANIC_PCT2",
                    "AMINDIAN_PCT2", "LEP_PCT2", "PACISLAND_PCT2", "ATRISK_PCT2", "TWOORMORE_PCT2", "WHITE_PCT2",
                    "ALL_COUNT", "ASIAN_COUNT", "ASIAN_PCT", "BLACK_COUNT", "BLACK_PCT", "ECONDISADV_COUNT",
                    "ECONDISADV_PCT", "HISPANIC_COUNT", "HISPANIC_PCT", "AMINDIAN_COUNT", "AMINDIAN_PCT", "LEP_COUNT",
                    "LEP_PCT", "PACISLAND_COUNT", "PACISLAND_PCT", "ATRISK_COUNT", "ATRISK_PCT", "TWOORMORE_COUNT",
                    "TWOORMORE_PCT", "WHITE_COUNT", "WHITE_PCT")
stud_list_1316 <- c("CAMPUS_NUM", "ALL_COUNT", "ASIAN_COUNT", "ASIAN_PCT", "BLACK_COUNT", "BLACK_PCT", "DAEP_COUNT", "DAEP_PCT",
               "ECONDISADV_COUNT", "ECONDISADV_PCT", "HISPANIC_COUNT", "HISPANIC_PCT", 
               "AMINDIAN_COUNT", "AMINDIAN_PCT", "LEP_COUNT", "LEP_PCT", "NONEDDISADV_COUNT", "NONEDDISADV_PCT",
               "PACISLAND_COUNT", "PACISLAND_PCT", "ATRISK_COUNT", "ATRISK_PCT", "TWOORMORE_COUNT", "TWOORMORE_PCT",
               "WHITE_COUNT", "WHITE_PCT")
stud_dlist_1112 <- c("CAMPUS_NUM", 
                     "ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "ECONDISADV_COUNT",
                     "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT",
                     "PACISLAND_COUNT",  "ATRISK_COUNT",  "TWOORMORE_COUNT", "WHITE_COUNT")
stud_dlist_1316 <- c("CAMPUS_NUM",
                     "ALL_COUNT", "ASIAN_COUNT", "BLACK_COUNT", "ECONDISADV_COUNT", 
                     "HISPANIC_COUNT", "AMINDIAN_COUNT", "LEP_COUNT", 
                     "PACISLAND_COUNT", "ATRISK_COUNT",  "TWOORMORE_COUNT", "WHITE_COUNT")

perf_list_11 <- 
  c("CAMPUS_NUM", "SATISF_RATE_ALL", "SATISF_RATE_MATH", "SATISF_RATE_READING", "SATISF_RATE_ALL_BLACK",
    "SATISF_RATE_MATH_BLACK", "SATISF_RATE_READING_BLACK", "SATISF_RATE_ECONDISADV", "SATISF_RATE_ECONDISADV_MATH", "SATISF_RATE_ECONDISADV_READING",
    "SATISF_RATE_ALL_F", "SATISF_RATE_MATH_F", "SATISF_RATE_READING_F", "SATISF_RATE_ALL_HIPSANIC", "SATISF_RATE_MATH_HISPANIC", "SATISF_RATE_READING_HISPANIC",
    "SATISF_RATE_ALL_AMINDIANS", "SATISF_RATE_MATH_AMINDIANS", "SATISF_RATE_READING_AMINDIANS", "SATISF_RATE_ALL_LEP", 
    "SATISF_RATE_MATH_LEP", "SATISF_RATE_READING_LEP", "SATISF_RATE_ALL_M", "SATISF_RATE_MATH_M", "SATISF_RATE_READING_M",
    "SATISF_RATE_ALL_SPED", "SATISF_RATE_MATH_SPED", "SATISF_RATE_READING_SPED", "SATISF_RATE_ALL_WHITE", "SATISF_RATE_MATH_WHITE", 
    "SATISF_RATE_READING_WHITE", "MEDIAN1", "MEDIAN2", "MEDIAN3", "SATISF_RATE_ALL_TWOORMORE",
    "SATISF_RATE_MATH_TWOORMORE", "SATISF_RATE_READING_TWOORMORE", "SATISF_RATE_ALL_ASIAN", "SATISF_RATE_MATH_ASIAN", "SATISF_RATE_READING_ASIAN",
    "SATISF_RATE_ALL_PACISLAND", "SATISF_RATE_MATH_PACISLAND", "SATISF_RATE_READING_PACISLAND")

perf_dlist <-c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "CHARTER", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME", "DISTRICT_NUM",  
           "SATISF_RATE_READING_F", "ADV_RATE_READING_F", "SATISF_RATE_READING_M", "ADV_RATE_READING_M",
           "SATISF_RATE_MATH_F", "ADV_RATE_MATH_F", "SATISF_RATE_MATH_M", "ADV_RATE_MATH_M")
perf_dlist_11 <-c("YEAR", "CAMPUS_NUM", "CAMPUS_NAME", "CHARTER", "COUNTY_NAME", "COUNTY_NUM", "DISTRICT_NAME",  
               "SATISF_RATE_READING_F", "SATISF_RATE_READING_M", 
               "SATISF_RATE_MATH_F", "SATISF_RATE_MATH_M")

# Read in Each Dataset --------------------------------------------------------------------------------------------------------
# dataset: school info
data_info_11 <- read_csv("data/CREF_201011.dat")
# dataset: school info
data_info_12 <- read_csv("data/CREF_201112.dat")
# dataset: school info
data_info_13 <- read_csv("data/CREF_201213.dat")
# dataset: school info
data_info_14 <- read_csv("data/CREF_201314.dat")
# dataset: school info
data_info_15 <- read_csv("data/CREF_201415.dat")
# dataset: school info
data_info_16 <- read_csv("data/CREF_201516.dat")
# dataset: student info
data_stud_11 <- read_csv("data/CSTUD_201011.dat")
# dataset: student info
data_stud_12 <- read_csv("data/CSTUD_201112.dat")
# dataset: student info
data_stud_13 <- read_csv("data/CSTUD_201213.dat")
# dataset: student info
data_stud_14 <- read_csv("data/CSTUD_201314.dat")
# dataset: student info
data_stud_15 <- read_csv("data/CSTUD_201415.dat")
# dataset: student info
data_stud_16 <- read_csv("data/CSTUD_201516.dat")
# dataset: performance
data_perf_11 <- read_csv("data/CTAKS10.dat")
# dataset: satisfactory 
data_satisf_13 <- read_csv("data/CSTAAR4_201213.dat")
# dataset: advanced
data_adv_13 <- read_csv("data/CSTAAR6_201213.dat")
# dataset: satisfactory 
data_satisf_14 <- read_csv("data/CSTAAR4_201314.dat")
# dataset: advanced
data_adv_14 <- read_csv("data/CSTAAR6_201314.dat")
# dataset: satisfactory 
data_satisf_15 <- read_csv("data/CSTAAR4_201415.dat")
# dataset: advanced
data_adv_15 <- read_csv("data/CSTAAR6_201415.dat")
# dataset: satisfactory 
data_satisf_16 <- read_csv("data/CSTAAR4_201516.dat")
# dataset: advanced
data_adv_16 <- read_csv("data/CSTAAR6_201516.dat")

# School Info --------------------------------------------------------------------------------------------------------

# 2010-11 -----
# keep certain columns
data_info_11 <- data_info_11 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME")
# change column names
colnames(data_info_11) <- list_info_1112
# make sure they character
data_info_11$CAMPUS_NAME <- as.character(data_info_11$CAMPUS_NAME)
data_info_11$COUNTY_NAME <- as.character(data_info_11$COUNTY_NAME)
data_info_11$DISTRICT_NAME <- as.character(data_info_11$DISTRICT_NAME)

# 2011-12 -----
# keep certain columns
data_info_12 <- data_info_12 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME")
# change column names
colnames(data_info_12) <- list_info_1112
# make sure they character
data_info_12$CAMPUS_NAME <- as.character(data_info_12$CAMPUS_NAME)
data_info_12$COUNTY_NAME <- as.character(data_info_12$COUNTY_NAME)
data_info_12$DISTRICT_NAME <- as.character(data_info_12$DISTRICT_NAME)

# 2012-13 -----
# keep certain columns
data_info_13 <- data_info_13 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME", "DISTRICT")
# change column names
colnames(data_info_13) <- list_info_1316
# make sure they character
data_info_13$CAMPUS_NAME <- as.character(data_info_13$CAMPUS_NAME)
data_info_13$COUNTY_NAME <- as.character(data_info_13$COUNTY_NAME)
data_info_13$DISTRICT_NAME <- as.character(data_info_13$DISTRICT_NAME)

# 2013-14 -----
# keep certain columns
data_info_14 <- data_info_14 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME", "DISTRICT")
# change column names
colnames(data_info_14) <- list_info_1316
# make sure they character
data_info_14$CAMPUS_NAME <- as.character(data_info_14$CAMPUS_NAME)
data_info_14$COUNTY_NAME <- as.character(data_info_14$COUNTY_NAME)
data_info_14$DISTRICT_NAME <- as.character(data_info_14$DISTRICT_NAME)

# 2014-15 -----
# keep certain columns
data_info_15 <- data_info_15 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME", "DISTRICT")
# change column names
colnames(data_info_15) <- list_info_1316
# make sure they character
data_info_15$CAMPUS_NAME <- as.character(data_info_15$CAMPUS_NAME)
data_info_15$COUNTY_NAME <- as.character(data_info_15$COUNTY_NAME)
data_info_15$DISTRICT_NAME <- as.character(data_info_15$DISTRICT_NAME)

# 2015-16 -----
# keep certain columns
data_info_16 <- data_info_16 %>% select("CAMPUS", "CAMPNAME", "CFLCHART", "CNTYNAME", "COUNTY", "DISTNAME", "DISTRICT")
# change column names
colnames(data_info_16) <- list_info_1316
# make sure they character
data_info_16$CAMPUS_NAME <- as.character(data_info_16$CAMPUS_NAME)
data_info_16$COUNTY_NAME <- as.character(data_info_16$COUNTY_NAME)
data_info_16$DISTRICT_NAME <- as.character(data_info_16$DISTRICT_NAME)



# Demographics --------------------------------------------------------------------------------------------------------
# 2010-11 -----
# change colnames
colnames(data_stud_11) <- stud_list_1112

# select only what we want
data_stud_11 <- data_stud_11 %>% 
  select(stud_dlist_1112)

# combine with school info
data_enroll_11 <- left_join(data_info_11, data_stud_11, by="CAMPUS_NUM")

# clean it
data_enroll_11 <- clean_it(data_enroll_11) 

# add year
data_enroll_11$YEAR <- 2011

# put year first
data_enroll_11 <- data_enroll_11 %>% 
  select(YEAR, everything())

# finish
write.csv(data_enroll_11, "cleaned_data/tx_enroll_2011.csv", row.names = FALSE)

# 2011-12 -----
# change colnames
colnames(data_stud_12) <- stud_list_1112

# select only what we want
data_stud_12 <- data_stud_12 %>% 
  select(stud_dlist_1112)

# combine with school info
data_enroll_12 <- left_join(data_info_12, data_stud_12, by="CAMPUS_NUM")

# clean it
data_enroll_12 <- clean_it(data_enroll_12) 

# add year
data_enroll_12$YEAR <- 2012

# put year first
data_enroll_12 <- data_enroll_12 %>% 
  select(YEAR, everything())

# combine them
data_enroll <- full_join(data_enroll_11, data_enroll_12)

# finish
write.csv(data_enroll_12, "cleaned_data/tx_enroll_2012.csv", row.names = FALSE)


# 2012-13 -----
# change colnames
colnames(data_stud_13) <- stud_list_1316

# select only what we want
data_stud_13 <- data_stud_13 %>% 
  select(stud_dlist_1316)

# combine with school info
data_enroll_13 <- left_join(data_info_13, data_stud_13, by="CAMPUS_NUM")

# clean it
data_enroll_13 <- clean_it(data_enroll_13) 

# add year
data_enroll_13$YEAR <- 2013

# put year first
data_enroll_13 <- data_enroll_13 %>% 
  select(YEAR, everything())

# combine them
data_enroll <- full_join(data_enroll, data_enroll_13)

# finish
write.csv(data_enroll_13, "cleaned_data/tx_enroll_2013.csv", row.names = FALSE)


# 2013-14 -----
# change colnames
colnames(data_stud_14) <- stud_list_1316

# select only what we want
data_stud_13 <- data_stud_13 %>% 
  select(stud_dlist_1316)

# combine with school info
data_enroll_14 <- left_join(data_info_14, data_stud_14, by="CAMPUS_NUM")

# clean it
data_enroll_14 <- clean_it(data_enroll_14) 

# add year
data_enroll_14$YEAR <- 2014

# put year first
data_enroll_14 <- data_enroll_14 %>% 
  select(YEAR, everything())

# combine them
data_enroll <- full_join(data_enroll, data_enroll_14)

# finish
write.csv(data_enroll_14, "cleaned_data/tx_enroll_2014.csv", row.names = FALSE)


# 2014-15 -----
# change colnames
colnames(data_stud_15) <- stud_list_1316

# select only what we want
data_stud_15 <- data_stud_15 %>% 
  select(stud_dlist_1316)

# combine with school info
data_enroll_15 <- left_join(data_info_15, data_stud_15, by="CAMPUS_NUM")

# clean it
data_enroll_15 <- clean_it(data_enroll_15) 

# add year
data_enroll_15$YEAR <- 2015

# put year first
data_enroll_15 <- data_enroll_15 %>% 
  select(YEAR, everything())

# combine them
data_enroll <- full_join(data_enroll, data_enroll_15)

# finish
write.csv(data_enroll_15, "cleaned_data/tx_enroll_2015.csv", row.names = FALSE)


# 2015-16 -----
# change colnames
colnames(data_stud_16) <- stud_list_1316

# select only what we want
data_stud_16 <- data_stud_16 %>% 
  select(stud_dlist_1316)

# combine with school info
data_enroll_16 <- left_join(data_info_16, data_stud_16, by="CAMPUS_NUM")

# clean it
data_enroll_16 <- clean_it(data_enroll_16) 

# add year
data_enroll_16$YEAR <- 2016

# put year first
data_enroll_16 <- data_enroll_16 %>% 
  select(YEAR, everything())

# combine them
data_enroll <- full_join(data_enroll, data_enroll_16)

# finish
write.csv(data_enroll_16, "cleaned_data/tx_enroll_2016.csv", row.names = FALSE)


# final finish -----
write.csv(data_enroll, "cleaned_data/tx_enroll_2011_16.csv", row.names = FALSE)

# Performance --------------------------------------------------------------------------------------------------------
# For grades 3-11

# 2010-11 -----
# change colnames
colnames(data_perf_11) <- perf_list_11

data_perf_11 <- left_join(data_info_11, data_perf_11, by= "CAMPUS_NUM")

# add year to it
data_perf_11$YEAR <- 2011

# select only the columns we need
data_perf_11 <- data_perf_11 %>% 
  select(perf_dlist_11)

# clean it
data_perf_11 <- clean_it(data_perf_11)

# write it
write.csv(data_perf_11, "cleaned_data/tx_perf_2011.csv", row.names = FALSE)

# 2012-13 -----
# change colnames
colnames(data_satisf_13) <- colnames_satisf_131415
colnames(data_adv_13) <- colnames_adv_131415

# combine them together
data_perf_13 <- left_join(data_satisf_13, data_adv_13, by="CAMPUS_NUM")
data_perf_13 <- left_join(data_info_13, data_perf_13, by= "CAMPUS_NUM")

# add year to it
data_perf_13$YEAR <- 2013

# select only the columns we need
data_perf_13 <- data_perf_13 %>% 
  select(perf_dlist)

# clean it
data_perf_13 <- clean_it(data_perf_13)

# join it
data_perf <- full_join(data_perf_11, data_perf_13)

# write it
write.csv(data_perf_13, "cleaned_data/tx_perf_2013.csv", row.names = FALSE)


# 2013-14 -----
# change colnames
colnames(data_satisf_14) <- colnames_satisf_131415
colnames(data_adv_14) <- colnames_adv_131415

# combine them together
data_perf_14 <- left_join(data_satisf_14, data_adv_14, by="CAMPUS_NUM")
data_perf_14 <- left_join(data_info_14, data_perf_14, by= "CAMPUS_NUM")

# add year to it
data_perf_14$YEAR <- 2014

# select only the columns we need
data_perf_14 <- data_perf_14 %>% 
  select(perf_dlist)

# clean it
data_perf_14 <- clean_it(data_perf_14)

# merge into one
data_perf <- full_join(data_perf, data_perf_14)

# write it
write.csv(data_perf_14, "cleaned_data/tx_perf_2014.csv", row.names = FALSE)

# 2014-15 -----
# change colnames
colnames(data_satisf_15) <- colnames_satisf_131415
colnames(data_adv_15) <- colnames_adv_131415

# combine them together
data_perf_15 <- left_join(data_satisf_15, data_adv_15, by="CAMPUS_NUM")
data_perf_15 <- left_join(data_info_15, data_perf_15, by= "CAMPUS_NUM")

# add year to it
data_perf_15$YEAR <- 2015

# select only the columns we need
data_perf_15 <- data_perf_15 %>% 
  select(perf_dlist)

# clean it
data_perf_15 <- clean_it(data_perf_15)

# merge into one
data_perf <- full_join(data_perf, data_perf_15)

write.csv(data_perf_15, "cleaned_data/tx_perf_2015.csv", row.names = FALSE)

# 2015-16 -----
# change colnames
colnames(data_satisf_16) <- colnames_satisf_16
colnames(data_adv_16) <- colnames_adv_16

# combine them together
data_perf_16 <- left_join(data_satisf_16, data_adv_16, by="CAMPUS_NUM")
data_perf_16 <- left_join(data_info_16, data_perf_16, by= "CAMPUS_NUM")

# add year to it
data_perf_16$YEAR <- 2016

# select only the columns we need
data_perf_16 <- data_perf_16 %>% 
                  select(perf_dlist)

# clean it
data_perf_16 <- clean_it(data_perf_16)

# merge into one
data_perf <- full_join(data_perf, data_perf_16)

write.csv(data_perf_16, "cleaned_data/tx_perf_2016.csv", row.names = FALSE)

# Finish  -----
# Save as .csv file
write.csv(data_perf,"cleaned_data/tx_perf_2013_16.csv", row.names = FALSE)


# Final Finish --------------------------------------------------------------------------------------------------------
# Save as .RData file
save.image(file="cleaned_data/tx_perf_enroll_clean.Rdata")
# Save as .RDS 
saveRDS(data_enroll, file="cleaned_data/tx_enroll_clean.rds")
saveRDS(data_perf, file="cleaned_data/tx_perf_clean.rds")


