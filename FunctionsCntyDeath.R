# Penelope Overholt
# Master's project code
# 2026

# functions for the county level data

library(ggplot2)#map_data
library(dplyr) #inner_join, left_join
library(tidyr) # pivot_longer
library(cowplot) # plot_grid

# for tables
library(gridExtra) #grid.arrange,tablegrob
library(grid) #textGrob
library(gtable) #gtable_add_rows

#look at counties with high rate and low population
  # dfCounts <- read.csv("cntyOverdoseCounts2005-2023.csv")
  # dfCounts[!is.na(dfCounts$rate) & dfCounts$rate > 310,] #only 1
  # diag(table(dfCounts[!is.na(dfCounts$rate) & dfCounts$rate > 200,"Year"],
  #            dfCounts[!is.na(dfCounts$rate) & dfCounts$rate > 200,"Year"])) 

# check what states are missing population data by year
# dfYear  <- read.csv("cntyOverdoseCounts2005-2023.csv",colClasses = c(FIPS_num = "character"))
# table(dfYear [is.na(dfYear $population),"State"],dfYear [is.na(dfYear $population),"Year"])
# Connecticut replaced their county system in 2022

# Functions Included Here
# suppressData
# CntyDrugDeathYearplot (uses step 3 data)
# CntyDrugDeathYearplotMore (uses step 3A data)
# getOverdoseData

### suppressData
# suppress data with low counts, per the restricted data use agreement:
# "Counts, rates, and percentages for sub-national geographic areas will be
# suppressed and not displayed in any manner if they are based on 1 to 9
# observations in the numerator, denominator, or total, regardless of the
# number of years combined. This data suppression rule applies to all text,
# tables, and figures (including maps) contained in main and supplemental files."

# this is a helper function that will take in a data frame, a chosen count variable 
# that needs to be suppressed, and a rate variable(s) that is made from the count variable.
# example: the death count and the rate per 100k.
suppressData <- function(df,var_count,var_rate)
{
  df[!is.na(df[[var_count]]) & df[[var_count]] <= 9 & df[[var_count]] >=1,var_rate] <- NA
  df[!is.na(df[[var_count]]) &df[[var_count]] <= 9 & df[[var_count]] >=1,var_count] <- NA
  return(df)
}

### CntyDrugDeathYearplot
# Drug data
# choose a year
# choose a state, can be a list, defaults to all states
# like, CntyDrugDeathYearplot(state=c("WV","PA","OH","KY","DE","MD","VA","DC"))
# CntyDrugDeathYearplot(state=c("NM","UT","TX","OK","CO","AZ"))
CntyDrugDeathYearplot <- function(choose_year=2023,rate=TRUE,state = "all",suppress=TRUE)
{
  StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
  dfCounts <- getOverdoseData(state=state,choose_year=choose_year,more=FALSE)
  
  if(length(state) == 1)
  {
    if(state == "all")
    {
      state_list <- c(StateNameNum[c(1,3:11,13:51),1])
    }else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
    }
  } else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
  }
  
  # shapefile download here: https://www.census.gov/cgi-bin/geo/shapefiles/index.php
  # in 2022 Connecticut switched to county equivalent system, which is not interchangeable with the previous county geograpy
  # NCHS mortality data DID NOT switch over
  # ACS data DID switch over
  # map_county <- tigris::counties(year = choose_year, state= c(StateNameNum[c(1,3:11,13:51),3])) # equivalent of if-else below
  if(choose_year < 2022)
  {
    map_state <- sf::st_read("ShapeFiles/tl_2021_us_state.shp",stringsAsFactors=F,as_tibble=T) 
    map_state <- map_state %>% filter(STATEFP %in% state_list)
    map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
    map_county <- map_county %>% filter(STATEFP %in% state_list)
    
  }else{
    map_state <- sf::st_read("ShapeFiles/tl_2022_us_state.shp",stringsAsFactors=F,as_tibble=T) 
    map_state <- map_state %>% filter(STATEFP %in% state_list)
    map_county <- sf::st_read("ShapeFiles/tl_2022_us_county.shp",stringsAsFactors=F,as_tibble=T)
    map_county <- map_county %>% filter(STATEFP %in% state_list)
  }
  
  # suppress data with low counts
  if(suppress == TRUE) # this should be true when displaying plot results for writes-up and such
  {
    dfCounts <- suppressData(dfCounts, "Deaths","rate")
  }
  
  map_county <- left_join(map_county,dfCounts, by = c("GEOID" = "FIPS_num"))
  
  if(rate==TRUE)
  {
  # View(dfCounts[is.na(dfCounts$rate),])
  # View(map_county[is.na(map_county$rate),])
  
  # NA rates can be from a NA population, or from the data not containing the county
  # nrow(map_county[is.na(map_county$rate) & is.na(map_county$population),]) 
  # nrow(map_county[is.na(map_county$population),]) 
    
    #the scale gets unwieldy if the full range of rates is included. 
    # So, I truncate the legend's limits, and make any out of bound (OOB) values black
    max_rate=200
    OOB_df <- map_county[!is.na(map_county$rate) & map_county$rate > max_rate,]
    # this is for counties that do not have population data but do have death counts
    missingPop <- map_county[is.na(map_county$population) & !is.na(map_county$Deaths),]
    # this is for counties that are missing death counts, none should appear
    noDeath <- map_county[is.na(map_county$Deaths),]

    ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = rate),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = rate))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
      
        # ggnewscale::new_scale_fill()+
        # geom_sf(data=missingPop, mapping=aes(fill = rate))+
        #   scale_fill_gradient(limits=c(-1,-1),breaks=seq(-1,-1,by=1),
        #                     labels=c(""),na.value = "red",
        #                        low="red",high="red",name=paste("Missing Pop data"))+
      
        # ggnewscale::new_scale_fill()+
        #   geom_sf(data=noDeath, mapping=aes(fill = rate))+
        #     scale_fill_gradient(limits=c(-1,-1),breaks=seq(-1,-1,by=1),
        #                       labels=c(""),na.value = "green",
        #                          low="green",high="green",name=paste("No Death Counts"))+
      
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title="Overdose Death Rate per 100K",subtitle=choose_year,x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

  }else{
    ggplot()+
        geom_sf(data = map_county,aes(fill = Deaths)) +
        scale_fill_gradient(limits=c(0,3000),na.value = "lightgrey",
                               low="orange",high="purple",name="count")+
        labs(title="Overdose Death Count",subtitle=choose_year,x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))
  }
}

### CntyDrugDeathYearplotMore
## age Levels
# 0-14, 1
# 15-24 , 2
# 25-64, 3
# 65 and over, 4
# not stated, 5

# education Levels 
# 1 ...  8th grade or less 
# 2 ...  9 - 12th grade, no diploma 
# 3 ...  high school graduate or GED completed 
# 4 ...  some college credit, but no degree 
# 5 ...  Associate degree 
# 6 ...  Bachelor’s degree 
# 7 ...  Master’s degree 
# 8 ...  Doctorate or professional degree 
# 9 ...  Unknown 

# sex levels
# M ...  Male 
# F ... Female 

# maritalStatus levels
# S ... Never married, single 
# M ... Married 
# W ... Widowed 
# D ... Divorced 
# N ... Marital Status not on certificate 
# U ... Marital Status unknown 

# race levels
# 1 ...  White 
# 2 ...  Black 
# 3 ...  American Indian (includes Aleuts and Eskimos) 
# 4 ...  Chinese 
# 5 ...  Japanese 
# 6 ...  Hawaiian (includes Part-Hawaiian) 
# 7 ...  Filipino 
# 18 ...  Asian Indian 
# 28 ...  Korean 
# 38 ...  Samoan 
# 48 ...  Vietnamese 
# 58 ...  Guamanian 
# 68 ...  Other Asian or Pacific Islander in areas reporting codes 18-58 
# 78 ...  Combined other Asian or Pacific Islander, includes 18-68 
#      for areas that do not report them separately 

# This function will output the ratio of the chosen variable at a specified level 
# out of the total overdose deaths for a given year by each county.
# Levels are defined above, should be like: "education_1", do not include leading zeroes
# can choose a list of what states to look at, defaults to all
CntyDrugDeathYearplotMore <-function(choose_year=2023,state = "all",choose_var="maritalStatus_S",suppress=TRUE)
{
  dfCounts <- getOverdoseData(state=state,choose_year=choose_year,more=TRUE)
  
  dfCounts <- dfCounts[dfCounts$Year == choose_year,]
  # get ratio of choose_var out of total overdoses
  dfCounts$choose_var_rate <- dfCounts[,choose_var]/dfCounts$Deaths
  # if total deaths is zero, that leads to a divide by zero error. does not mean missing data
  # dfCounts[dfCounts$Deaths == 0 & dfCounts[,choose_var] == 0,]
  
  dfCounts <- dfCounts[,c("FIPS_num","choose_var_rate")]
  #join data with state and county mapping
  StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
  if(length(state) == 1)
  {
    if(state == "all")
    {
      state_list <- c(StateNameNum[c(1,3:11,13:51),1])
    }else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
    }
  } else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
  }
  
  if(choose_year < 2022)
  {
    map_state <- sf::st_read("ShapeFiles/tl_2021_us_state.shp",stringsAsFactors=F,as_tibble=T) 
    map_state <- map_state %>% filter(STATEFP %in% state_list)
    map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
    map_county <- map_county %>% filter(STATEFP %in% state_list)
  }else{
    map_state <- sf::st_read("ShapeFiles/tl_2022_us_state.shp",stringsAsFactors=F,as_tibble=T) 
    map_state <- map_state %>% filter(STATEFP %in% state_list)
    map_county <- sf::st_read("ShapeFiles/tl_2022_us_county.shp",stringsAsFactors=F,as_tibble=T)
    map_county <- map_county %>% filter(STATEFP %in% state_list)
  }

  if(suppress == TRUE) # this should be true when displaying plot results for writes-up and such
  {
    dfCounts <- suppressData(dfCounts, "Deaths","rate")
    dfCounts <- suppressData(dfCounts, choose_var,"choose_var_rate")
  }
  #join data with state and county mapping
  map_county <- left_join(map_county,dfCounts, by = c("GEOID" = "FIPS_num"))
  
  ggplot()+
        geom_sf(data = map_county,aes(fill = choose_var_rate),color=NA) +
        scale_fill_gradient(limits=c(0,1),na.value = "lightgrey",
                               low="orange",high="purple",name="Ratio")+
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Count of",choose_var,"Deaths out of Total Overdose Deaths"),
             subtitle=choose_year,x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))
}

### getOverdoseData ##########
# this function will retrieve the specfied states/years 
# will default to reading in the public data (toy). 
# variables included for "more" are defined in formatCntyData.R, Step 3A data. NOT available for the public data I collected.

getOverdoseData <- function(state,choose_year="all",more=FALSE,toy=TRUE)
{
  StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
  
  if(length(state) == 1)
  {
    if(state == "all")
    {
      state_list <- c(StateNameNum[c(1,3:11,13:51),1])
    }else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
    }
  } else{
    state_list <- StateNameNum %>% filter(Abbreviation %in% state)
    state_list <- c(state_list[,1])
  }
  
  if(toy)#public data
  {
    data <- read.csv("toyData.csv",colClasses = c(FIPS_num = "character"))
  } else{
    if(more)
    {
      data <- read.csv("cntyOverdoseCountsMoreInfo2005-2023.csv", colClasses = c(FIPS_num = "character"))
    }else{
      data <- read.csv("cntyOverdoseCounts2005-2023.csv",colClasses = c(FIPS_num = "character"))
    }
  }
  
  data <- data %>% filter(substr(FIPS_num,1,2) %in% state_list)
  
  if(choose_year != "all")
  {
    data <- data[data$Year == choose_year,] 
  }
  
  ### update some FIPS codes
  #51515 was a county, but joined 51019 in 2013.
  # since I only use one shape file for all years, I am adding 51515's data to 51019 for all years
  if(sum(data$FIPS_num %in% "51515")>0)
  {
    data[data$FIPS_num == "51019","Deaths"] <- data[data$FIPS_num == "51019","Deaths"] + data[data$FIPS_num == "51515","Deaths"]
    data[data$FIPS_num == "51019" & data$Year > 2009 & data$Year < 2014,"population"] <- data[data$FIPS_num == "51019" & data$Year > 2009 & data$Year < 2014,"population"] + data[data$FIPS_num == "51515"& data$Year > 2009 & data$Year < 2014,"population"]
    data[data$FIPS_num == "51019", "rate"] <- data[data$FIPS_num == "51019", "Deaths"]/data[data$FIPS_num == "51019", "population"]*100000
    if(more)
    {
      for(i in which(names(data)=="education_NA"):which(names(data)=="race_NA"))
      {
         data[data$FIPS_num == "51019",i] <- data[data$FIPS_num == "51019",i] + data[data$FIPS_num == "51515",i]
      }
    }
    data <- data[data[,"FIPS_num"] != "51515",]
  }
  
# SD113 went under a name change in 2015 and got a new FIPS code SD102.
data[data$FIPS == "SD113","FIPS"] <- "SD102"
data[data$FIPS == "SD102","FIPS_num"] <-"46102"
  
  return(data)
}



