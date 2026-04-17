# Penelope Overholt
# Master's project code
# 2026

# This file includes code to format county level data from NCHS, and covariate Data from ACS

### Mortality Data #########

# Data is from 2005-2024 Detailed Mortality – All Counties files from the Division of Vital Statistics, NCHS
# This is the code used to format the county level data files from txt to csv
# American Community Survey data is used to get population estimates and covariate data (code at the bottom)

# get county codes (FIPS) here: https://www.cdc.gov/nchs/nvss/instruction-manuals.htm (part 8)
# part 8, https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Manuals/Mortality/COUNTY_CODES.txt
# variable location and definitions in 2RecordLayout_RestrictedUse_DetailedMortAllCounties_2003-latest_03-03-2025.pdf
# cause of death codes 
# ICD-10 from https://www.cdc.gov/nchs/data/datalinkage/underlying_and_multiple_cause_of_death_codes.pdf
# or here https://www.cdc.gov/nchs/nvss/manuals/2025/2e-vol1-2025.html
# Overdose ICD-10 codes:
# Accidental poisoning and exposure to noxious substances (X40-X49) 
# Accidental poisoning by and exposure to drugs and other biological substances (X40X44) 
# Accidental poisoning by and exposure to other and unspecified solid or liquids substances (X45-X46,X48-X49) 
# Accidental poisoning by and exposure to other gases and vapors (X47) 
# Intentional self-poisoning (suicide) by and exposure to drugs and other biological substances (X60-X64) 
# Intentional self-poisoning (suicide) by and exposure to other and unspecified solid or liquid substances and their vapors (X65-X66,X68-X69)
# Intentional self-poisoning (suicide) by and exposure to other gases and vapors (X67)

# The data was formatted in 3 steps, each with their own csv file. 
# Steps 2 and 3 have counterparts, 2a and 3a, which are essentially the same but 2a and 3a include more variables then 2 and 3.

# About the CSV files:
# Step 1:
# cntyMortality2005-2023.csv is the first step of formatting. 
# It is just all years of the data (2005-2023) in one place, formatted into columns.
# Very big, slow to load
# each entry is one person

# Step 2:
# cntyOverdose2005-2023.csv is the second step. Further formats the first step.
# Only keeps mortality codes that are overdose codes.
# Converts the county codes into county names. 
# each entry is one person
# Subsets to only keep columns of interest: "FIPS", "State","County","PopulationCntyCode","Year", "Underlying.Cause.of.death.Code"

# Step 3: 
# cntyOverdoseCounts2005-2023.csv is the third step, and further formats the second step.
# formats county names to no longer have special characters or whitespace.
# Aggregates instances of overdose by county (FIPS) by year.
# Merges with more detailed population data (from ACS).
# creates the overdose rate per 100K (total county) population.

# Step 2a: 
# cntyOverdoseMoreInfo2005-2023.csv 
# is like step 2, but includes more columns of data.
# Only keeps mortality codes that are overdose codes.
# Converts the county codes into county names. 
# each entry is one person
# "FIPS","State","County","PopulationCntyCode","Year",
# Subsets to only keep columns of interest: "Underlying.Cause.of.death.Code",
# "education","sex","age","maritalStatus","race","moreRaceCodes","conditions"

# Step 3a: 
# cntyOverdoseCountsMoreInfo2005-2023.csv 
# Is like step 3, but includes more columns of data (following from step 2a).
# Formats county names to no longer have special characters
# Merges with more detailed population data.
# creates the overdose rate per 100K (total county) population.
# each county is listed one time per year, with each measurement (column) as a count of each factor level. 
# So, say: in county 01, in 2015, 10 males, 7 female, 5 single, 12 married, 16 white, 1 black, etc

library(dplyr) # inner_join, left_join
library(tidyr)#pivot_wider, complete

###################################
### STEP 1 ######
### Makes cntyMortality2005-2023.csv
### format the Data into one file
#this takes all the difference data files and compiles them into one
# as well as formats the data into columns with header names

### getDataInColumns
# this is a helper function
# it takes the data that is from readLines() and parses each line into the correct columns
getDataInColumns <- function(dataLines)
{
  temp <- matrix(ncol=24,nrow= length(dataLines))
  
  for(i in 1:nrow(dataLines))
  {
    temp[i,1] <- substr(dataLines[i], 19, 19) #Resident/nonResident 1/2
    temp[i,2] <- substr(dataLines[i], 20,20) #ResidentStatus
    temp[i,3] <- substr(dataLines[i], 21,22) # state of occurrence, FIPS (partial, first 2)
    temp[i,4] <- substr(dataLines[i], 23,25) # county of occurrence, FIPS (partial, last 3)
    temp[i,5] <- substr(dataLines[i], 21,25) # FIPS, 5 digits
    
    temp[i,6] <- substr(dataLines[i], 26,27) # more location info
    temp[i,7] <- substr(dataLines[i], 28,28) # population size code of county
    temp[i,8] <- substr(dataLines[i], 29,46) # location of residence
    temp[i,9] <- substr(dataLines[i], 47,54) # reserved
    temp[i,10] <- substr(dataLines[i], 55,60) # place of birth
    temp[i,11] <- substr(dataLines[i], 63,63) # Education, is just location 63
                                              # 61 is old education number system
                                              # 64 tells you which edu number system is in effect
    temp[i,12] <- substr(dataLines[i], 65,66)# monthOfDeath
    temp[i,13] <- substr(dataLines[i], 69,69)# sex
    temp[i,14] <- substr(dataLines[i], 70,82)# age, 75-76 is a 5 year range, Recode 52
    temp[i,15] <- substr(dataLines[i], 83,83)# placeOfDeath
    temp[i,16] <- substr(dataLines[i], 84,84)# maritaStatus
    temp[i,17] <- substr(dataLines[i], 85,85)# dayOfWeekOfDeath
    temp[i,18] <- substr(dataLines[i], 102,105)# currentDataYear
    temp[i,19] <- substr(dataLines[i], 106,145)# mannerOfDeath stuff
    temp[i,20] <- substr(dataLines[i], 146,149)# Underlying.Cause.of.death.Code
    temp[i,21] <- substr(dataLines[i], 150,161)# other death codes
    temp[i,22] <- substr(dataLines[i], 163,443)# conditions
    temp[i,23] <- substr(dataLines[i], 445,446)# race not valid in 2020 and onward
    temp[i,24] <- substr(dataLines[i], 447,490)# more race codes, valid for only certain years. 
       # None of the races codes are valid for the entirety of 2005-2024, and none are compatible.
  }
  tempdf <- as.data.frame(temp)
  names(tempdf) <- c("Resident","ResidentStatus",
                     "StateOfOccurrence","CntyOfOccurrence","FIPS","moreLocationInfo","PopulationCntyCode",
                     "locationOfResidence","reserved","placeofBirth","education",
                     "monthofDeath","sex","age","placeofDeath","maritalStatus","dayofWeekofDeath","Year",
                     "mannerofDeathStuff","Underlying.Cause.of.death.Code","otherDeathCodes","conditions",
                     "race","moreRaceCodes")
  return(tempdf)
}

### end getDataInColumns ###

dfYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2005.USPSAllCnty/MULT2005.USAllCnty.txt"))))

for(i in 2005:2017)
{
  fileName <- paste("Penelope Overholt_11-18-2025/MULT",i,".USPSAllCnty/MULT",i,".USAllCnty.txt",sep="")
  dfCurYear <- getDataInColumns(matrix(c(readLines(fileName))))
  dfYear <- rbind(dfYear,dfCurYear)
}
  
#2018
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2018.USPSAllCnty/Mort2018US.AllCnty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)

# write.csv(dfYear,paste("cntyMortality2005-2018.csv"),row.names = FALSE)
# dfYear <- read.csv("cntyMortality2005-2018.csv")
  
#2019
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/Mult2019.USPSAllCnty/Mult2019US.AllCnty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)

#2020
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2020.AllCnty/MULT2020.USAllCnty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)

#2021
# race recode 40 was moved, does not effect current indexing in getDataInColumns()
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2021.AllCnty/MULT2021US.AllCnty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)
  
#2022
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2022USPS.AllCnty_Doc_updated/MULT2022us.AllCnty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)

# write.csv(dfYear,"cntyMortality2005-2022.csv",row.names = FALSE)

# when I first got access to the data, the 2023 file was bad, and I had to request it again. 
# So, I made a file up to 2022, then added 2023 later.

#######
#2023 
dfCurYear <- getDataInColumns(matrix(c(readLines("Penelope Overholt_11-18-2025/MULT2023USPS.AllCnty_Doc_updated/Mort2023.US.AllCounty.txt"))))
dfYear <- rbind(dfYear,dfCurYear)
# write.csv(dfYear,paste("cntyMortality2005-2023.csv"),row.names = FALSE)

# 2024 # I did request 2024 data, but NCHS kept postponing when it would be done, and now I am done with the project.

### END STEP 1 ###
########################################

########################################
### STEP 2 ###
### makes cntyOverdose2005-2023.csv ###
### make new file with only columns and rows I need
dfYear <- read.csv("cntyMortality2005-2023.csv")
dfYear$State <- tolower(state.name[match(dfYear$StateOfOccurrence, state.abb)])
dfYear$State <- ifelse(dfYear$StateOfOccurrence == "DC","district of columbia",dfYear$State)
dfYear <- dfYear[,c("FIPS","State","PopulationCntyCode",
                    "Year","Underlying.Cause.of.death.Code")]

countyCode <- read.csv("CountyCodes.txt",sep=",",header=T,colClasses = c(COUNTY_CODE = "character"))
countyCode$FIPS <- paste(countyCode$STATE_CODE,countyCode$COUNTY_CODE,sep="")
countyCode$COUNTY_NAME <- tolower(countyCode$COUNTY_NAME)

dfYear <- full_join(dfYear,countyCode, by="FIPS", relationship = "many-to-many",keep=TRUE)

dfYear <- dfYear[dfYear$STATE_CODE != "AK" & dfYear$STATE_CODE != "HI" & 
                   dfYear$STATE_CODE != "AS" & dfYear$STATE_CODE != "GU" & 
                   dfYear$STATE_CODE != "MP" & dfYear$STATE_CODE != "PR" & 
                   dfYear$STATE_CODE != "VI" & !is.na(dfYear$STATE_CODE) ,] # only want contiguous US

names(dfYear)[which(names(dfYear)=="COUNTY_NAME")] <- "County"
names(dfYear)[which(names(dfYear)=="FIPS.x")] <- "FIPS"

dfYear <- dfYear[,c("FIPS","State","County","PopulationCntyCode","Year",
                    "Underlying.Cause.of.death.Code")]

#ICD-10 from https://www.cdc.gov/nchs/data/datalinkage/underlying_and_multiple_cause_of_death_codes.pdf
# or here https://www.cdc.gov/nchs/nvss/manuals/2025/2e-vol1-2025.html
dfYear <- dfYear[dfYear$Underlying.Cause.of.death.Code == "X40 " |
               dfYear$Underlying.Cause.of.death.Code == "X41 " |
               dfYear$Underlying.Cause.of.death.Code == "X42 " |
               dfYear$Underlying.Cause.of.death.Code == "X43 " |
               dfYear$Underlying.Cause.of.death.Code == "X44 " |
               dfYear$Underlying.Cause.of.death.Code == "X45 " |
               dfYear$Underlying.Cause.of.death.Code == "X46 " |
               dfYear$Underlying.Cause.of.death.Code == "X47 " |
               dfYear$Underlying.Cause.of.death.Code == "X48 " |
               dfYear$Underlying.Cause.of.death.Code == "X49 " |
               dfYear$Underlying.Cause.of.death.Code == "X60 " |
               dfYear$Underlying.Cause.of.death.Code == "X61 " |
               dfYear$Underlying.Cause.of.death.Code == "X62 " |
               dfYear$Underlying.Cause.of.death.Code == "X63 " |
               dfYear$Underlying.Cause.of.death.Code == "X64 " |
               dfYear$Underlying.Cause.of.death.Code == "X65 " |
               dfYear$Underlying.Cause.of.death.Code == "X66 " |
               dfYear$Underlying.Cause.of.death.Code == "X67 " |
               dfYear$Underlying.Cause.of.death.Code == "X68 " |
               dfYear$Underlying.Cause.of.death.Code == "X69 ",]
#save
# write.csv(dfYear,"cntyOverdose2005-2023.csv",row.names = FALSE )

### END STEP 2 ###
########################

######################
### STEP 3 ###
### Makes cntyOverdoseCounts2005-2023.csv ###
### Counts of overdose mortality codes #####################
# this takes the formatted data file (step 2) and aggregates by counts of overdose deaths

dfYear <- read.csv("cntyOverdose2005-2023.csv") #(step 2 data)
#no special characters
dfYear$County <- gsub("[[:punct:]]", '', dfYear$County)#remove special characters
dfYear$County <- gsub(' city', '', dfYear$County) 

# get total death counts
dfYear$Death <- 1
dfYear <- aggregate(dfYear$Death, by=list(dfYear$FIPS,dfYear$State,dfYear$County,dfYear$Year),FUN=sum)
names(dfYear) <- c("FIPS","State","County","Year","Deaths")

# handle missing data
# if a county does not apear, that is because it had no deaths (I assume)
# first step is to make sure all counties are included for all years
countyCode <- read.csv("CountyCodes.txt",sep=",",header=T,colClasses = c(COUNTY_CODE = "character"))
countyCode$FIPS <- paste(countyCode$STATE_CODE,countyCode$COUNTY_CODE,sep="")
countyCode$COUNTY_NAME <- tolower(countyCode$COUNTY_NAME)
dfYear <- full_join(dfYear,countyCode, by="FIPS", relationship = "many-to-many",keep=TRUE)
names(dfYear)[which(names(dfYear)=="FIPS.y")] <- "FIPS" #make look like earlier step, but here is FIPS.y
dfYear <- complete(dfYear,FIPS,Year) #make sure every FIPS occurs every year
dfYear <- dfYear[!is.na(dfYear$Year),]
# Now, when the Deaths column has a NA, that means that no overdose deaths were reported for that county.
# which I will assume that means 0 zero overdose deaths happened in that county, see below.
# Except for connecticut in 2022 and after, ACS updated the FIPS to account for CT changing to a 
# new county equivalent system. BUT, NCHS mortality data did not.
dfYear[is.na(dfYear$Deaths),"Deaths"] <- 0 # replace NA deaths in a county with zero
  
# population
pop <- read.csv("cntyPopulation2005-2023.csv")
dfYear <- left_join(dfYear,pop, by = c("FIPS","Year"))

# SD113 went under a name change in 2015 and got a new FIPS code SD102.
dfYear[dfYear$FIPS == "SD113" & dfYear$Year >=2015,"population"] <- pop[pop$FIPS == "SD102" & pop$Year >= 2015,"population"]

#overdose rate per 100K
dfYear$rate <- dfYear$Deaths/dfYear$population*100000

names(dfYear)[which(names(dfYear)=="State.x")] <- "State"
names(dfYear)[which(names(dfYear)=="County.x")] <- "County"

StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

dfYear$abb <- substr(dfYear$FIPS, 1, 2)
dfYear <- left_join(dfYear,StateNameNum, by = c("abb"="Abbreviation"))
dfYear$FIPS_num <- paste(dfYear$Number,substr(dfYear$FIPS, nchar(dfYear$FIPS) - 3 + 1, nchar(dfYear$FIPS)),sep="")
 
dfYear <- dfYear[,c("FIPS","FIPS_num","State","County","Year","Deaths","population","rate")]

#save
# write.csv(dfYear,"cntyOverdoseCounts2005-2023.csv",row.names = FALSE)

### END STEP 3 ###
###############################
#
#
#
### STEP 2A ###
### makes cntyOverdoseMoreInfo2005-2023.csv
### is like step 2, but keeps more columns of data
dfYear <- read.csv("cntyMortality2005-2023.csv") # step 1 data
dfYear$State <- tolower(state.name[match(dfYear$StateOfOccurrence, state.abb)])
dfYear$State <- ifelse(dfYear$StateOfOccurrence == "DC","district of columbia",dfYear$State) 

dfYear <- dfYear[,c("FIPS","State","PopulationCntyCode","Year",
                    "Underlying.Cause.of.death.Code",
                    "education","sex","age","maritalStatus","race","moreRaceCodes","conditions")]

countyCode <- read.csv("CountyCodes.txt",sep=",",header=T,colClasses = c(COUNTY_CODE = "character"))
countyCode$FIPS <- paste(countyCode$STATE_CODE,countyCode$COUNTY_CODE,sep="")
countyCode$COUNTY_NAME <- tolower(countyCode$COUNTY_NAME)

dfYear <- full_join(dfYear,countyCode, by="FIPS", relationship = "many-to-many",keep=TRUE)

dfYear <- dfYear[dfYear$STATE_CODE != "AK" & dfYear$STATE_CODE != "HI" & 
                   dfYear$STATE_CODE != "AS" & dfYear$STATE_CODE != "GU" & 
                   dfYear$STATE_CODE != "MP" & dfYear$STATE_CODE != "PR" & 
                   dfYear$STATE_CODE != "VI" & !is.na(dfYear$STATE_CODE) ,] # only want contiguous US

names(dfYear)[which(names(dfYear)=="COUNTY_NAME")] <- "County"
names(dfYear)[which(names(dfYear)=="FIPS.x")] <- "FIPS"

dfYear <- dfYear[,c("FIPS","State","County","PopulationCntyCode","Year",
                    "Underlying.Cause.of.death.Code",
                    "education","sex","age","maritalStatus","race","moreRaceCodes","conditions")] 

#ICD-10 from https://www.cdc.gov/nchs/data/datalinkage/underlying_and_multiple_cause_of_death_codes.pdf
# or here https://www.cdc.gov/nchs/nvss/manuals/2025/2e-vol1-2025.html
dfYear <- dfYear[dfYear$Underlying.Cause.of.death.Code == "X40 " |
               dfYear$Underlying.Cause.of.death.Code == "X41 " |
               dfYear$Underlying.Cause.of.death.Code == "X42 " |
               dfYear$Underlying.Cause.of.death.Code == "X43 " |
               dfYear$Underlying.Cause.of.death.Code == "X44 " |
               dfYear$Underlying.Cause.of.death.Code == "X45 " |
               dfYear$Underlying.Cause.of.death.Code == "X46 " |
               dfYear$Underlying.Cause.of.death.Code == "X47 " |
               dfYear$Underlying.Cause.of.death.Code == "X48 " |
               dfYear$Underlying.Cause.of.death.Code == "X49 " |
               dfYear$Underlying.Cause.of.death.Code == "X60 " |
               dfYear$Underlying.Cause.of.death.Code == "X61 " |
               dfYear$Underlying.Cause.of.death.Code == "X62 " |
               dfYear$Underlying.Cause.of.death.Code == "X63 " |
               dfYear$Underlying.Cause.of.death.Code == "X64 " |
               dfYear$Underlying.Cause.of.death.Code == "X65 " |
               dfYear$Underlying.Cause.of.death.Code == "X66 " |
               dfYear$Underlying.Cause.of.death.Code == "X67 " |
               dfYear$Underlying.Cause.of.death.Code == "X68 " |
               dfYear$Underlying.Cause.of.death.Code == "X69 ",]
#save
# write.csv(dfYear,"cntyOverdoseMoreInfo2005-2023.csv",row.names = FALSE )

### END STEP 2A ###
##########################

##########################
### STEP 3A ###
### makes cntyOverdoseCountsMoreInfo2005-2023.csv
### Is like step 3, but follows with more data columns as defined in step 2a.
### Counts of overdose mortality codes #####################

# this takes the formatted data file from step 2a 
# each county is listed one time per year, with each measurement (column) is a count of each factor level. 
# So, say: in county 01, in 2015, 10 males, 7 female, 5 single, 12 married, 16 white, 1 black, etc

dfYear <- read.csv("cntyOverdoseMoreInfo2005-2023.csv",colClasses = c(race = "character")) # from step 2a

#no special characters
dfYear$County <- gsub("[[:punct:]]", '', dfYear$County)#remove special characters
dfYear$County <- gsub(' city', '', dfYear$County)

# get total death counts
dfYear$Death <- 1
death_aggr <- aggregate(dfYear$Death, by=list(dfYear$FIPS,dfYear$State,dfYear$County,dfYear$Year),FUN=sum)
names(death_aggr) <- c("FIPS","State","County","Year","Deaths")
  
# get frequency of each factor level by county by year

dfYear$education <- as.factor(dfYear$education)
dfYear$race <- as.factor(dfYear$race)

# Education
freq_edu <- dfYear %>%
  group_by(Year, FIPS, education) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = education,
    values_from = freq,
    values_fill = 0  
  )
names(freq_edu)[3:12] <- c(paste("education_",names(freq_edu)[3:12],sep=""))
# does this: table(dfYear[!is.na(dfYear$education) & dfYear$FIPS == "AL003" & dfYear$Year == 2016 ,"education"])

# Sex: 2 levels, M for male, and F for female
freq_sex <- dfYear %>%
  group_by(Year, FIPS, sex) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = sex,
    values_from = freq,
    values_fill = 0 
  )
names(freq_sex)[3:4] <- c(paste("sex_",names(freq_sex)[3:4],sep=""))

## Age: 6 levels, I am defining my own ranges as follows:
# age, old levels range, new levels
# 0-14, code <= 28, 1
# 15-24 , 29 <= code <= 30, 2
# 25-44, 31 <= code <= 34, 3
# 45-64, 35 <= code <= 38, 4
# 65 and over, 39<=code<=51, 5
# not stated, code == 52, 6
dfYear$age_old <- substr(dfYear$age, 6,7)
dfYear$age <- NA
dfYear$age <- ifelse(dfYear$age_old <= "28", "1",dfYear$age)
dfYear$age <- ifelse(dfYear$age_old >= "29" & dfYear$age_old <= "30", "2",dfYear$age)
dfYear$age <- ifelse(dfYear$age_old >= "31" & dfYear$age_old <= "34", "3",dfYear$age)
dfYear$age <- ifelse(dfYear$age_old >= "35" & dfYear$age_old <= "38", "4",dfYear$age)
dfYear$age <- ifelse(dfYear$age_old >= "39" & dfYear$age_old <= "51", "5",dfYear$age)
dfYear$age <- ifelse(dfYear$age_old == "52", "6",dfYear$age)

freq_age <- dfYear %>%
  group_by(Year, FIPS, age) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = age,
    values_from = freq,
    values_fill = 0
  )
names(freq_age)[3:8] <- c(paste("age_",names(freq_age)[3:8],sep=""))

# Marital Status:
freq_maritalStatus <- dfYear %>%
  group_by(Year, FIPS, maritalStatus) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = maritalStatus,
    values_from = freq,
    values_fill = 0  
  )
names(freq_maritalStatus)[3:7] <- c(paste("maritalStatus_",names(freq_maritalStatus)[3:7],sep=""))

# Race:
freq_race <- dfYear %>%
  group_by(Year, FIPS, race) %>%
  summarise(freq = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = race,
    values_from = freq,
    values_fill = 0 
  )
names(freq_race)[3:17] <- c(paste("race_",names(freq_race)[3:17],sep=""))

# join everything together
dfYear <- left_join(death_aggr,freq_edu,by=c("FIPS","Year"))
dfYear <- left_join(dfYear,freq_sex,by=c("FIPS","Year"))
dfYear <- left_join(dfYear,freq_age,by=c("FIPS","Year"))
dfYear <- left_join(dfYear,freq_maritalStatus,by=c("FIPS","Year"))
dfYear <- left_join(dfYear,freq_race,by=c("FIPS","Year"))

# handle missing data
# if a county does not apear, that is because it had no deaths (I assume)
# first step is to make sure all counties are included for all years
countyCode <- read.csv("CountyCodes.txt",sep=",",header=T,colClasses = c(COUNTY_CODE = "character"))
countyCode$FIPS <- paste(countyCode$STATE_CODE,countyCode$COUNTY_CODE,sep="")
countyCode$COUNTY_NAME <- tolower(countyCode$COUNTY_NAME)
dfYear <- full_join(dfYear,countyCode, by="FIPS",keep=TRUE)
names(dfYear)[which(names(dfYear)=="FIPS.y")] <- "FIPS" #make look like earlier step, but here is FIPS.y
dfYear <- complete(dfYear,FIPS,Year) #make sure every FIPS occurs every year
dfYear <- dfYear[!is.na(dfYear$Year),]
dfYear <- dfYear[,-c(which(names(dfYear)=="FIPS.x"),which(names(dfYear) == "STATE_CODE"),
                 which(names(dfYear) == "COUNTY_NAME"),which(names(dfYear) == "COUNTY_CODE")),]

# Now, when the Deaths column has a NA, that means that no overdose deaths were reported for that county.
# which I will assume that means 0 zero overdose deaths happened in that county, see below
# excepts for Connecticut in 2022 and after, ACS updated the FIPS to account for CT changing to a 
# new county equivalent system. BUT, NCHS mortality data did not.
for(i in c(which(names(dfYear) == "Deaths"):ncol(dfYear)))
{
  dfYear[,i] <- as.numeric(unlist(dfYear[,i]))
}
dfYear[is.na(dfYear$Deaths),c(which(names(dfYear) == "Deaths"):ncol(dfYear))] <- 0 # replace NA deaths in a county with zero

# population
pop <- read.csv("cntyPopulation2005-2023.csv")
dfYear <- left_join(dfYear,pop, by = c("FIPS","Year"))

# SD113 went under a name change in 2015 and got a new FIPS code SD102.
dfYear[dfYear$FIPS == "SD113" & dfYear$Year >=2015,"population"] <- pop[pop$FIPS == "SD102" & pop$Year >= 2015,"population"]

#overdose rate per 100K
dfYear$rate <- dfYear$Deaths/dfYear$population*100000

names(dfYear)[which(names(dfYear)=="State.x")] <- "State"
names(dfYear)[which(names(dfYear)=="County.x")] <- "County"

dfYear <- dfYear[,-c(which(names(dfYear)=="State.y"),which(names(dfYear) == "County.y")),]

### get number for state abreviation
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

dfYear$abb <- substr(dfYear$FIPS, 1, 2)
dfYear <- left_join(dfYear,StateNameNum, by = c("abb"="Abbreviation"))
dfYear$FIPS_num <- paste(dfYear$Number,substr(dfYear$FIPS, nchar(dfYear$FIPS) - 3 + 1, nchar(dfYear$FIPS)),sep="")
 
dfYear <- dfYear[,-c(which(names(dfYear) == "abb"),which(names(dfYear) == "Number"),
                     which(names(dfYear) == "State_Name"))]


# write.csv(dfYear,"cntyOverdoseCountsMoreInfo2005-2023.csv",row.names = FALSE)
### end 3A


### END MORTALITY DATA ###############################################

#### POPULATION DATA ######################################################
#### Get population data by county by year ##############
# from the American Community Survey
# acs 1 year estimates used for 2005 to 2009
# acs 5 year estimates used for 2010 to 2023
library(tidycensus)

# census_api_key("ff6020a79d44671b988bb9109e3372cb980a3491", install = TRUE)

dfPop <- as.data.frame(get_acs(geography="county",variables="B01003_001",year=2005,survey = "acs1"))
dfPop$Year <- "2005"

for(i in 2006:2023)
{
  if(i < 2010)
  {
    dfPopCur <- as.data.frame(get_acs(geography="county",variables="B01003_001",year=i,survey = "acs1"))
    dfPopCur$Year <- i
    dfPop <- rbind(dfPop,dfPopCur)
  }else{
  
    dfPopCur <- as.data.frame(get_acs(geography="county",variables="B01003_001",year=i,survey = "acs5"))
    dfPopCur$Year <- i
    dfPop <- rbind(dfPop,dfPopCur)
  }
}

dfPop$State <- sapply(strsplit(dfPop$NAME, ', '), `[`, 2)
dfPop$County <- tolower(sapply(strsplit(dfPop$NAME, ','), `[`, 1))
dfPop$County <- gsub(" county", "", dfPop$County)
dfPop$County <- gsub(" parish", "", dfPop$County)
dfPop$County <- gsub(" borough", "", dfPop$County)
dfPop$County <- gsub(" city", "", dfPop$County)

dfPop$FIPS <- paste(ifelse(dfPop$State == "District of Columbia","DC",
                           state.abb[match(dfPop$State, state.name)]),
                    substr(dfPop$GEOID, nchar(dfPop$GEOID) - 3 + 1, nchar(dfPop$GEOID)),sep="")
dfPop$State <- tolower(dfPop$State)
dfPop <- dfPop[,c("FIPS","State","County","Year","estimate")]
names(dfPop) <- c("FIPS","State","County","Year","population")
dfPop$County <- gsub("[[:punct:]]", '', dfPop$County)#remove all special characters
 
# write.csv(dfPop,"cntyPopulation2005-2023.csv",row.names = FALSE)

#### This is to convert between the number FIPS for the state and the letter abreviations
temp <- as.data.frame(get_acs(geography="county",variables="B01003_001",year=2005,survey = "acs1"))
temp <- as.data.frame(substr(dfPop$GEOID, 1, 2))
temp$state <- sapply(strsplit(dfPop$NAME, ', '), `[`, 2)
temp <- temp[!duplicated(temp), ]
temp$abb <- ifelse(temp$state == "District of Columbia","DC",
                           state.abb[match(temp$state, state.name)])
names(temp) <- c("Number","State_Name","Abbreviation")

# write.csv(temp,"StateNumToAbb.csv",row.names = FALSE)

### END POPULATION DATA ##############################3

#### COVARIATE DATA ######################################################
#### Get covariate data by county by year ##############
# from the American Community Survey
# acs 5 year estimates used for 2012 to 2024
# variable documentation: 'table shells" on Census Bureau website
# here: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww2.census.gov%2Fprograms-surveys%2Facs%2Ftech_docs%2Ftable_shells%2F2024%2FShells%2FS1810.xlsx&wdOrigin=BROWSELINK

library(tidycensus)

# census_api_key("ff6020a79d44671b988bb9109e3372cb980a3491", install = TRUE)

dfCov <- as.data.frame(get_acs(geography="county",variable=c(paste("S1810_C02_0",c(39,55),sep="")),year=2015,survey = "acs5"))
dfCov$Year <- 2015
for(i in 2016:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable=c(paste("S1810_C02_0",c(39,55),sep="")),year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov <- rbind(dfCov,dfCovCur)
}
dfCov <- pivot_wider(dfCov[,c("GEOID","Year","estimate","variable")],,names_from = variable,values_from = estimate)
names(dfCov) <- c("GEOID","Year","Cognitive","SelfCare")      

dfCov_2 <- as.data.frame(get_acs(geography="county",variable="S2701_C02_001",year=2015,survey = "acs5"))
dfCov_2$Year <- 2015
for(i in 2016:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable="S2701_C02_001",year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov_2 <- rbind(dfCov_2,dfCovCur)
}
dfCov_2 <- dfCov_2[,c("GEOID","estimate","Year")]
names(dfCov_2) <- c("GEOID","Insured","Year")   

dfCov_3 <- as.data.frame(get_acs(geography="county",variable="S1701_C02_001",year=2015,survey = "acs5"))
dfCov_3$Year <- 2015
for(i in 2016:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable="S1701_C02_001",year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov_3 <- rbind(dfCov_3,dfCovCur)
}
dfCov_3 <- dfCov_3[,c("GEOID","estimate","Year")]
names(dfCov_3) <- c("GEOID","Poverty","Year") 

dfCov_4 <- as.data.frame(get_acs(geography="county",variable=c(paste("S1501_C01_00",c(2,3,7,8,9),sep="")),year=2015,survey = "acs5"))
dfCov_4$Year <- 2015
for(i in 2016:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable=c(paste("S1501_C01_00",c(2,3,7,8,9),sep="")),year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov_4 <- rbind(dfCov_4,dfCovCur)
}
dfCov_4 <- aggregate(dfCov_4$estimate, by=list(dfCov_4$GEOID,dfCov_4$Year),FUN=sum)
names(dfCov_4) <- c("GEOID","Year","Education") # education highschool and below

#marital status percent estimates by county, 2010-2024 acs5
dfCov_5 <- as.data.frame(get_acs(geography="county",variable=c(paste("S1201_C0",c(2:6),"_001",sep="")),year=2010,survey = "acs5"))
dfCov_5$Year <- 2010
for(i in 2011:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable=c(paste("S1201_C0",c(2:6),"_001",sep="")),year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov_5 <- rbind(dfCov_5,dfCovCur)
}
dfCov_5 <- dfCov_5[,-c(which(names(dfCov_5) == "moe"))]
dfCov_5 <- pivot_wider(dfCov_5,names_from = variable,values_from = estimate)
names(dfCov_5)[4:8] <- c("maritalStatus_M","maritalStatus_W","maritalStatus_D","maritalStatus_S","maritalStatus_MS")  

#married, widowed, divorced, single, married but separated

#median household income
dfCov_6 <- as.data.frame(get_acs(geography="county",variable="S1901_C01_012",year=2010,survey = "acs5"))
dfCov_6$Year <- 2010
for(i in 2011:2024)
{
    dfCovCur <- as.data.frame(get_acs(geography="county",variable="S1901_C01_012",year=i,survey = "acs5"))
    dfCovCur$Year <- i
    dfCov_6 <- rbind(dfCov_6,dfCovCur)
}
dfCov_6 <- dfCov_6[,c("GEOID","Year","estimate")]
names(dfCov_6)[3] <- "medianIncome"

dfCov <- left_join(dfCov,dfCov_2,by=c("GEOID","Year"))
dfCov <- left_join(dfCov,dfCov_3,by=c("GEOID","Year"))
dfCov <- left_join(dfCov,dfCov_4,by=c("GEOID","Year"))
dfCov <- left_join(dfCov,dfCov_4,by=c("GEOID","Year"))
dfCov <- left_join(dfCov_5,dfCov,by=c("GEOID","Year"))
dfCov <- left_join(dfCov_6,dfCov,by=c("GEOID","Year"))

# write.csv(dfCov,"cntyCovar2015-2024.csv",row.names = FALSE)
#must read in as: dfCov <- read.csv("cntyCovar2015-2024.csv",colClasses = c(GEOID = "character"))

### END COVARIATE DATA ##########################
