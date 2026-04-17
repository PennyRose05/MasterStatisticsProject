# Penelope Overholt
# Master's project code
# 2026

# This file includes the code for formatting the public use mortality data from CDC
# As well as the code for ACS data retrieval.

## format the data
# CDC WONDER has slightly different names and formats of the data than what NCHS reports for the restricted files. 
# So, this is to make the public data compatible for code I wrote for the restricted data.

library(dplyr)
library(tidyr)

data05 <- read.csv("drug_0509.csv",colClasses = c(County.Code = "character"))
data10 <- read.csv("drug_1014.csv",colClasses = c(County.Code = "character"))
data15 <- read.csv("drug_1519.csv",colClasses = c(County.Code = "character"))
data <- rbind(data05,data10)
data <- rbind(data,data15)
data <- data[,c("County","County.Code","Year.Code","Deaths")]
names(data)[2:3] <- c("FIPS_num","Year")

data20 <- read.csv("drug_2023.csv",colClasses = c(County.Code = "character"))
data20 <- data20[,c("County","County.Code","Year.Code","Deaths")]
names(data20)[2:3] <- c("FIPS_num","Year")

data <- rbind(data,data20)

StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

data$abb <- sub(".*,\\s*", "", data$County)
data$County <- sub(",.*", "", data$County)
data <- left_join(data,StateNameNum, by = c("abb"="Abbreviation"))
names(data)[which(names(data)=="State_Name")] <- "State"
data$FIPS <- paste(data$abb,substr(data$FIPS_num,3,5),sep="") 

data <- aggregate(data$Deaths, by=list(data$FIPS,data$FIPS_num,data$State,data$County,data$Year),FUN=sum)
names(data) <- c("FIPS","FIPS_num","State","County","Year","Deaths")

# handle missing data
# if a county does not apear, that is because it had no deaths (I assume)
# first step is to make sure all counties are included for all years
countyCode <- read.csv("CountyCodes.txt",sep=",",header=T,colClasses = c(COUNTY_CODE = "character"))
countyCode$FIPS <- paste(countyCode$STATE_CODE,countyCode$COUNTY_CODE,sep="")
countyCode$COUNTY_NAME <- tolower(countyCode$COUNTY_NAME)
data <- full_join(data,countyCode, by="FIPS", relationship = "many-to-many",keep=TRUE)
names(data)[which(names(data)=="FIPS.y")] <- "FIPS" #
data <- complete(data,FIPS,Year) #make sure every FIPS occurs every year
data <- data[!is.na(data$Year),]

data$abb <- substr(data$FIPS,1,2)
data <- left_join(data,StateNameNum, by = c("abb"="Abbreviation"))
data$FIPS_num <- paste(data$Number,substr(data$FIPS,3,5),sep="") 
# Now, when the Deaths column has a NA, that means that no overdose deaths were reported for that county.
# which I will assume that means 0 zero overdose deaths happened in that county, see below
# excepts for connecticut in 2022 and after, ACS updated the FIPS to account for CT changing to a 
# new county equivalent system. BUT, NCHS mortality data did not.
#NOTE: for the public use data, a NA may be because of privacy suppression, NOT necessarily 0 deaths. 
# I include this here because it is what I did with the restricted data, also we need to handle NAs.
data[is.na(data$Deaths),"Deaths"] <- 0 # replace NA deaths in a county with zero

#join population
pop <- read.csv("cntyPopulation2005-2023.csv")
data <- left_join(data,pop, by = c("FIPS","Year"))

# SD113 went under a name change in 2015 and got a new FIPS code SD102.
data[data$FIPS == "SD113" & data$Year >=2015,"population"] <- pop[pop$FIPS == "SD102" & pop$Year >= 2015,"population"]
data[data$FIPS == "SD113","FIPS"] <- "SD102"
data[data$FIPS == "SD102","FIPS_num"] <-"46102"

#overdose rate per 100K
data$rate <- data$Deaths/data$population*100000

names(data)[which(names(data)=="State.y")] <- "State"
names(data)[which(names(data)=="County.y")] <- "County"

### update some FIPS codes
  #51515 was a county, but joined 51019 in 2013.
  # since I only use one shape file for all years, I am adding 51515's data to 51019 for all years
if(sum(data$FIPS_num %in% "51515")>0)
{
  data[data$FIPS_num == "51019","Deaths"] <- data[data$FIPS_num == "51019","Deaths"] + data[data$FIPS_num == "51515","Deaths"]
  data[data$FIPS_num == "51019" & data$Year > 2009 & data$Year < 2014,"population"] <- data[data$FIPS_num == "51019" & data$Year > 2009 & data$Year < 2014,"population"] + data[data$FIPS_num == "51515"& data$Year > 2009 & data$Year < 2014,"population"]
  data[data$FIPS_num == "51019", "rate"] <- data[data$FIPS_num == "51019", "Deaths"]/data[data$FIPS_num == "51019", "population"]*100000
  
  data <- data[data[,"FIPS_num"] != "51515",]
}

data <- data %>% filter(substr(FIPS_num,1,2) %in% c(StateNameNum[c(1,3:11,13:51),1]))#just contiguous USA and DC
 
data <- data[,c("FIPS","FIPS_num","State","County","Year","Deaths","population","rate")]
# write.csv(data,"toydata.csv",row.names = FALSE)


#### POPULATION DATA ######################################################
#### Get population data by county by year ##############
# from the American Community Survey
# acs 1 year estimates used for 2005 to 2009
# acs 5 year estimates used for 2010 to 2023
library(tidycensus)

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


#### COVARIATE DATA ######################################################
#### Get covariate data by county by year ##############
# from the American Community Survey
# acs 5 year estimates used for 2012 to 2024
# variable documentation: 'table shells" on Census Bureau website
# here: https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww2.census.gov%2Fprograms-surveys%2Facs%2Ftech_docs%2Ftable_shells%2F2024%2FShells%2FS1810.xlsx&wdOrigin=BROWSELINK

library(tidycensus)

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
