# Header ------------------------------------------------------------------

# Script name: BWTX OBOT/MAT FY22 Admissions Demographics

# Purpose of script: Run all demographics into tables for everyone admitted in FY22

# Author: Matthew Perez

# Date Created: 11/4/2022

# Notes:

# Be sure to create an R Project

# Install and load libraries ----------------------------------------------
require(tidyverse)
require(data.table)
library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(formattable)

# Set working directory ---------------------------------------------------
 

# Importing Data ----------------------------------------------------------
BWTX_Demo <- read.csv("W:\\BWTX Data, Analytics, & Evaluation\\OBOT & MAT\\Demographics\\Adolescent___Adult_Treatment_Services_Demographic_Summary.csv") %>%
  select(1:6)

# Data Manipulation -------------------------------------------------------

###Combining and creating new Race and Ethnicity column
BWTX_Demo$Race_Ethnicity <- 
  ifelse((BWTX_Demo$Ethnicity %in% c("Hispanic or Latino")), "Hispanic", (str_c(BWTX_Demo$Race, ', ', BWTX_Demo$Ethnicity)))

###Create new column for program type
BWTX_Demo$Program_Type <- ifelse(grepl("OBOT", BWTX_Demo$Loc.Legal.Name), "OBOT", "MAT")

###Create bins for age
BWTX_Demo <- BWTX_Demo %>%
  mutate(Agegroup = case_when(Age >= 18 & Age <= 25 ~ '18-25',
                              Age >= 26 & Age <= 35 ~ '26-35',
                              Age >= 36 & Age <= 45 ~ '36-45',
                              Age >= 46 & Age <= 55 ~ '46-55',
                              Age >= 56 & Age <= 65 ~ '56-65',
                              Age >= 66 ~ '66 and older'))

### Demographic Tables combined

race_all <- BWTX_Demo %>%
  group_by(Race_Ethnicity) %>%
  summarise(Count = n()) %>%
  adorn_totals("row") %>%
  mutate(Percent = (Count / Count[9])) %>%
  mutate(Percent = percent(Percent))

age_all <- BWTX_Demo %>%
  group_by(Agegroup) %>%
  summarise(Count = n()) %>%
  adorn_totals("row") %>%
  mutate(Percent = (Count / Count[7])) %>%
  mutate(Percent = percent(Percent))

gender_all <- BWTX_Demo %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  adorn_totals("row") %>%
  mutate(Percent = (Count / Count[3])) %>%
  mutate(Percent = percent(Percent))

### Demographic Tables by Program

race_program <- BWTX_Demo %>%
  group_by(Race_Ethnicity, Program_Type) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Program_Type, values_from = 'n()') %>%
  adorn_totals("row") %>%
  mutate(OBOT_Percentage = (OBOT / OBOT[9]), MAT_Percentage = (MAT / MAT[9])) %>%
  mutate(OBOT_Percentage = percent(OBOT_Percentage), MAT_Percentage = percent(MAT_Percentage))


age_program <- BWTX_Demo %>%
  group_by(Agegroup, Program_Type) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Program_Type, values_from = 'n()') %>%
  adorn_totals("row") %>%
  mutate(OBOT_Percentage = (OBOT / OBOT[7]), MAT_Percentage = (MAT / MAT[7])) %>%
  mutate(OBOT_Percentage = percent(OBOT_Percentage), MAT_Percentage = percent(MAT_Percentage))

gender_program <- BWTX_Demo %>%
  group_by(Gender, Program_Type) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Program_Type, values_from = 'n()') %>%
  adorn_totals("row") %>%
  mutate(OBOT_Percentage = (OBOT / OBOT[3]), MAT_Percentage = (MAT / MAT[3])) %>%
  mutate(OBOT_Percentage = percent(OBOT_Percentage), MAT_Percentage = percent(MAT_Percentage))

###Demographic tables by Provider

race_provider <- BWTX_Demo %>%
  group_by(Race_Ethnicity, Loc.Legal.Name) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Loc.Legal.Name, values_from = 'n()') %>%
  adorn_totals("row") %>%
  split(.,.$Loc.Legal.Name)


age_provider <- BWTX_Demo %>%
  group_by(Agegroup, Program_Type) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Program_Type, values_from = 'n()') %>%
  adorn_totals("row")

gender_provider <- BWTX_Demo %>%
  group_by(Gender, Program_Type) %>%
  summarise(n()) %>%
  pivot_wider(names_from = Program_Type, values_from = 'n()') %>%
  adorn_totals("row")
  
###Location level

obot_org <- list(df$`Alamo Center Education and Treatment - OBOT - 105046`, df$`CARMAhealth - UT OBOT - 102854`,
                 df$`Center for Recovery and Wellness Resources - UT OBOT - 105058`, df$`CMS Austin on Ferguson UT OBOT 122709`,
                 df$`CMS Austin on William Cannon UT OBOT 000048`, df$`CMS Cedar Park Site UT OBOT 104827`,
                 df$`CMS San Antonio Site UT OBOT 104840`, df$`Emergence Health Network-UT OBOT-000244`,
                 df$`GROUNDSWELL, LLC dba CARS Corpus Christi - UT OBOT - 104062`, df$`GROUNDSWELL, LLC dba CARS Tomball - UT OBOT - 103524`,
                 df$`Harmony Road Recovery - UT- OBOT - 105369`, df$`Healthcare for the Homeless UT OBOT 105059`,
                 df$`Integral Care  UT OBOT  103344`, df$`LifePath Systems - UT OBOT - 104010`, df$`LifePath Systems - UT OBOT - 104899`,
                 df$`Lukner Medical Clinic, PLLC - UT OBOT - 105367`, df$`Metro Treatment of Texas- UT- UT OBOT 900179`,
                 df$`Metro Treatment of Texas NW- UT OBOT- 100182`, df$`MHMR of Tarrant County - UT OBOT - 103896`,
                 df$`Nexus Recovery Center - UT OBOT - 112643`, df$`Olive Branch Recovery-UT OBOT- 105257`,
                 df$`Permian Basin Community Centers-UT OBOT-902514`, df$`Permian Basin Community Centers-UT OBOT-903983`,
                 df$`Pursuit of Hope UT OBOT 103050`, df$`Resolute Addiction and Behavioral Health - UT OBOT - 105256`,
                 df$`Semper Healthcare Services - UT OBOT -105309`, df$`Texas Clinic Galleria - UT OBOT`,
                 df$`Texas Clinic Grand Parkway - UT OBOT`, df$`Texas Clinic Melbourne - UT OBOT`,
                 df$`Texoma Community Center - UT OBOT - 100405`, df$`Tropical Texas Behavioral Health UT OBOT 102571`,
                 df$`Tropical Texas Behavioral Health UT OBOT 103617`, df$`Tropical Texas Behavioral Health UT OBOT 104501`,
                 df$`TX OTP, LLC dba MAT Texas-UT OBOT-105018`, df$`UT HSC at Houston HEROES Site - UT OBOT - 104934`)


race <- function(x) {
  x %>%
    group_by(Race_Ethnicity) %>%
    summarise(Count = n()) %>%
    mutate(Percent = paste0(round(Count/sum(Count)*100, 2), "%"))
}

age_group <- function(x) {
  x %>%
    group_by(Agegroup) %>%
    summarise(Count = n()) %>%
    mutate(Percent = paste0(round(Count/sum(Count)*100, 2), "%"))
}

gender <- age_group <- function(x) {
  x %>%
    group_by(Gender) %>%
    summarise(Count = n()) %>%
    mutate(Percent = paste0(round(Count/sum(Count)*100, 2), "%"))
}



obot_race_result <- obot_org %>%
  lapply( race )

obot_age_result <- obot_org %>%
  lapply( age_group )

obot_gender_result <- obot_org %>%
  lapply( gender )

obot_race_result[[31]]
obot_age_result[[31]]
obot_gender_result[[31]]


# Data Visualization ------------------------------------------------------



# Data Reporting ----------------------------------------------------------

