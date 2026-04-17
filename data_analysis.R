# Penelope Overholt
# Master's project code
# 2026

# ACS state level POPULATION DATA retrieval code at bottom

source("FunctionsCntyDeath.R")
data <- getOverdoseData("all","all",more=TRUE)
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
dfPop <- read.csv("statePopulation2005-2023.csv", colClasses = c(GEOID = "character"))

##################################################################################################
#County level #################################################

#rate increse year to year by state
temp <- data[,c("FIPS_num","Year","State","County","rate","Deaths","population")]
temp <- temp[!is.na(temp$Deaths),]
temp <- temp[!is.na(temp$population),]

rate_change <- temp %>%
  group_by(FIPS_num) %>%
  arrange(FIPS_num,Year) %>% 
  mutate(change =  100*(rate - lag(rate))/lag(rate)) %>%
  ungroup()

# a negative value means the death count decreased from year to year

rate_change[!is.infinite(rate_change$change)& order(rate_change$change,decreasing=T), ]
summary(rate_change[!is.infinite(rate_change$change),])

rate_change[substr(rate_change$FIPS_num,1,2) == "54",]
summary(rate_change[!is.infinite(rate_change$change)&substr(rate_change$FIPS_num,1,2) == "54",])

##################################################################################################
# STATE level #####################################################
data_state <- data
data_state$state <- substr(data_state$FIPS_num,1,2)

data_state <- aggregate(list(data_state$Deaths,
                             data_state$sex_M,data_state$education_1,
                             data_state$education_2,data_state$education_3,data_state$education_NA,
                             data_state$education_9,data_state$age_3,data_state$age_4),
                        by=list(data_state$state,data_state$Year),FUN=sum,na.rm=T)

names(data_state) <- c("state","Year","Deaths","sex_M","education_1","education_2","education_3","education_NA","education_9","age_3","age_4")

# the education levels reported in some states prior to 2015 are completely NA
# since each column is a count of the education level, this would mean that high school and below is 0
# but is more appropriately NA, since the count of NAs is the total count of death (education_9 is also NA)
data_state$edu <- ifelse((data_state$education_NA+ data_state$education_9) == data_state$Deaths, NA, 
                         data_state$education_1+data_state$education_2+data_state$education_3)

data_state <- left_join(data_state,dfPop[,c("Year","GEOID","estimate")],by=c("Year"="Year","state"="GEOID"))
names(data_state)[which(names(data_state)=="estimate")] <- "population"

data_state$rate <- data_state$Deaths/data_state$population*100000
data_state <- left_join(data_state,StateNameNum,by=c("state"="Number"))

rate_change_state <- data_state %>%
  group_by(state) %>%
  arrange(state,Year) %>% 
  mutate(change =  100*(rate - lag(rate))/lag(rate)) %>%
  ungroup()

summary(rate_change_state)
#the max is a 323% increase, the min is a 66.6% decrease

rate_change_state[order(rate_change_state$change,decreasing=T), ]
rate_change_state[rate_change_state$Year==2023&order(rate_change_state$change,decreasing=T), ]
rate_change_state[rate_change_state$State_Name == "West Virginia", ]

hist(rate_change_state$change)

## look at 90th percentile
data_state[order(data_state$rate,decreasing=TRUE),]

temp <- data_state[data_state$rate >= quantile(data_state$rate, 0.90),]

table(temp$State_Name,temp$Year)#count of State in top 10% by Year
table(temp$State_Name)
table(temp$Year)

#state with maximum overdose rate by year
max_ind <- matrix(nrow=length(unique(data_state$Year)),ncol=7,0)

for(i in unique(data_state$Year))
{
  data_yr <- data_state[data_state$Year == i,]
  max_ind[i-2004,1] <- i
  
  max_ind[i-2004,2] <- which(data_yr$rate == max(data_yr$rate,na.rm=T))
  max_ind[i-2004,3] <- data_yr[as.numeric(max_ind[i-2004,2]),"Abbreviation"]
  max_ind[i-2004,4] <- data_yr[as.numeric(max_ind[i-2004,2]),"rate"]
  
  max_ind[i-2004,5] <- which(data_yr$rate == sort(data_yr$rate, decreasing = TRUE)[2])
  max_ind[i-2004,6] <- data_yr[as.numeric(max_ind[i-2004,5]),"Abbreviation"]
  max_ind[i-2004,7] <- data_yr[as.numeric(max_ind[i-2004,5]),"rate"]
}
max_ind <- as.data.frame(max_ind)
names(max_ind) <- c("Year","ind","abb","MaxRate","ind_2","abb_2","Max_2_Rate")
max_ind$MaxRate <- round(as.numeric(max_ind$MaxRate),2)
max_ind$Max_2_Rate <- round(as.numeric(max_ind$Max_2_Rate),2)
max_ind
##################################################################################################

##################################################################################################
## NATIONAL LEVEL #################################################
data_natl <- data_state

data_natl_edu <- data_natl[!is.na(data_natl$edu),]

data_natl <- aggregate(list(data_natl$Deaths, data_natl$population,
                            data_natl$sex_M, data_natl$edu,
                            data_natl$age_3,data_natl$age_4),
                       by=list(data_natl$Year),FUN=sum,na.rm=T)

data_natl_edu <- aggregate(list(data_natl_edu$Deaths, data_natl_edu$population,
                            data_natl_edu$sex_M, data_natl_edu$edu,
                            data_natl_edu$age_3,data_natl_edu$age_4),
                       by=list(data_natl_edu$Year),FUN=sum,na.rm=T)

names(data_natl) <- c("Year","Deaths","population","sex_M","edu","age_3","age_4")#HS, 25-44,45-64
names(data_natl_edu) <- c("Year","Deaths","population","sex_M","edu","age_3","age_4")

data_natl$rate <- data_natl$Deaths/data_natl$population*100000

rate_change_natl <- data_natl %>%
  arrange(Year) %>% 
  mutate(change =  100*(rate - lag(rate))/lag(rate)) %>%
  ungroup()

rate_change_natl[,c("Year","Deaths","population","rate","change")] 

summary(rate_change_natl) 

hist(rate_change_natl$change)

ggplot(rate_change_natl)+
  geom_line(aes(x=Year,y=change))
##################################################################################################

##################################################################################################
### compare state and national levels #################################

ggplot(data_state%>% filter(Abbreviation %in% unique(c(max_ind$abb,max_ind$abb_2 ))))+
  geom_line(aes(x=Year,y=rate,group=Abbreviation,color=Abbreviation))+
  geom_line(data=data_natl,aes(x=Year,y=rate,color="National"))+
  labs(title="Overdose rate of States that are 1 or 2nd in any year", 
       subtitle="Comparison of the State and National Level",
       x="Year",y="Overdose rate per 100k")+
  scale_color_manual( name = "Legend", values = c("DC"="orange","NM"="darkgrey","NV"="#004949","RI"="blue",
                                                  "WV"="#FF6DB6",#"WY"="purple",
                                                  "National" = "red"))+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

ggplot(data_state%>% filter(Abbreviation %in% c("AR","TN","WV","KY")))+
  geom_line(aes(x=Year,y=rate,group=Abbreviation,color=Abbreviation))+
  geom_line(data=data_natl,aes(x=Year,y=rate,color="National"))

cowplot::plot_grid(
ggplot(rate_change_state)+
  geom_line(aes(x=Year,y=change,group=state,color="States"))+
  geom_line(data=rate_change_natl,aes(x=Year,y=change,color="National"),linewidth =1)+
  labs(title="Percent Change of the Overdose Rate",
       subtitle="Comparison of the State and National Level",
       x="Year",y="Percent Change")+
  scale_color_manual( name = "Legend", values = c("States" = "black", "National" = "red"))+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15)) ,

ggplot(data_state )+
  geom_line(aes(x=Year,y=100*sex_M/Deaths,group=state,color="States"))+
  geom_line(data=data_natl,aes(x=Year,y=100*sex_M/Deaths,color="National"),linewidth=1)+
  labs(title="Percent of Overdose Deaths that are Males", 
       subtitle="Comparison of the State and National Level",
       x="Year",y="Percent")+
  scale_color_manual( name = "Legend", values = c("States" = "black", "National" = "red"))+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15)) ,

ggplot()+
  geom_line(data=data_state,aes(x=Year,y=100*edu/Deaths,group=state,color="States"))+
  geom_line(data=data_natl_edu,aes(x=Year,y=100*edu/Deaths,color="National"),linewidth=1)+
  labs(title="Percent of Overdose Deaths that are High School Graduates and Below", 
       subtitle="Comparison of the State and National Level",
       x="Year",y="Percent")+
  scale_color_manual( name = "Legend", values = c("States" = "black", "National" = "red"))+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15)) ,
ggplot()+
  geom_line(data=data_state,aes(x=Year,y=100*(age_3+age_4)/Deaths,group=state,color="States"))+
  geom_line(data=data_natl,aes(x=Year,y=100*(age_3+age_4)/Deaths,color="National"),linewidth=1)+
  labs(title="Percent of Overdose Deaths that are between 25 and 64 years old", 
       subtitle="Comparison of the State and National Level",
       x="Year",y="Percent")+
  scale_color_manual( name = "Legend", values = c("States" = "black", "National" = "red"))+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15)), 
ncol=2)

##########################################################################################################
##########################################################################################################
## MORAN'S I #####################################################

source("FunctionsCntyDeath.R")
library(sf)#st_read
library(spdep) #moran.test, poly2nb
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

###########################################################################################
# moran I as count level by each year #####################################################
# I am removing CT so that there is consistency for this test across all years
state_list <- c(StateNameNum[c(1,3:6,8:11,13:51),1])
choose_state <- StateNameNum[c(1,3:6,8:11,13:51),3]

map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(substr(GEOID,1,2) %in% state_list)

data <- getOverdoseData(state=choose_state,choose_year="all")

poly.nb.narm <- poly2nb(map_county,queen=T) 
W = nb2listw(poly.nb.narm, style="W",zero.policy=T) #same weight matrix for each year

moran_year <- matrix(ncol=4,nrow=14)
for(i in 2010:2023)
{
  map_county_join <- left_join(map_county,data[data$Year == i,],by=c("GEOID"="FIPS_num"))

# https://www.paulamoraga.com/book-spatial/spatial-autocorrelation.html

  moran_year[(i-2009),1] <- i
  moran <- moran.test(map_county_join$rate,W)
  moran_year[(i-2009),2] <- moran$estimate[1]
  moran_year[(i-2009),3] <- moran$p.value#signif means positive spatial cor
  lmoran <- localmoran(map_county_join$rate,W,alternative = "greater")
  moran_year[(i-2009),4] <- sum(ifelse(lmoran[, "Pr(z > E(Ii))"]<=.05,1,0))/nrow(lmoran)#proportion of signif for positive corr
}
moran_year <- as.data.frame(moran_year)
names(moran_year) <- c("Year","MoranI","P.value","signifLocal")
moran_year 

#local moran plot
map_county$lmI <- lmoran[, "Ii"] # local Moran's I
map_county$lmp <- lmoran[, "Pr(z > E(Ii))"]# p-values corresponding to alternative greater

ggplot(map_county)+
  geom_sf(aes(fill=ifelse(lmp<=.05,"1","0")))+
  scale_fill_manual( values = c("1" = "green", "0" = "purple") ) 

### 

###########################################################################################
# moran I at STATE level by each year #####################################################
# I am removing CT so that there is consistency for this test across all years

state_list <- c(StateNameNum[c(1,3:11,13:51),1])
choose_state <- c(StateNameNum[c(1,3:11,13:51),1])

map_state <- sf::st_read("ShapeFiles/tl_2021_us_state.shp",stringsAsFactors=F,as_tibble=T) 
map_state <- map_state %>% filter(STATEFP %in% state_list)

poly.nb.narm <- poly2nb(map_state,queen=T) 
W = nb2listw(poly.nb.narm, style="W") 

moran_year <- matrix(ncol=4,nrow=19)
for(i in 2005:2023)
{
  map_state_join <- left_join(map_state,data_state[data_state$Year == i,],by=c("STATEFP"="state"))

  moran_year[(i-2004),1] <- i
  moran <- moran.test(map_state_join$rate,W)
  moran_year[(i-2004),2] <- moran$estimate[1]
  moran_year[(i-2004),3] <- moran$p.value#signif means positive spatial cor
  lmoran <- localmoran(map_state_join$rate,W,alternative = "greater")
  moran_year[(i-2004),4] <- sum(ifelse(lmoran[, "Pr(z > E(Ii))"]<=.05,1,0))/nrow(lmoran)#proportion of signif for positive corr
  
  ind <- which(lmoran[,"Pr(z > E(Ii))"] <= .05)
  if(!is.null(names(ind)))
  {
    cat(i,paste(c(st_drop_geometry( map_state[ind,"NAME"]))),paste(c( lmoran[ind,"Pr(z > E(Ii))"])),"\n")
  }
}
moran_year <- as.data.frame(moran_year)
names(moran_year) <- c("Year","MoranI","P.value","signifLocal")
moran_year 

map_state$lmI <- lmoran[, "Ii"] # local Moran's I
map_state$lmp <- lmoran[, "Pr(z > E(Ii))"]# p-values corresponding to alternative greater

ggplot(map_state)+
  geom_sf(aes(fill=ifelse(lmp<=.05,"1","0")))+
  scale_fill_manual( values = c("1" = "green", "0" = "purple") ) 

### 

data <- getOverdoseData(state="all",choose_year="all",more=TRUE)
ggplot(data)+
  geom_point(aes(x=Year,y=rate, color=substr(FIPS_num,1,2)))+
  lims(y=c(0,400))



#### POPULATION DATA ######################################################
#### Get population data by state by year ##############
# from the American Community Survey
# acs 1 year estimates used for 2005 to 2009
# acs 5 year estimates used for 2010 to 2023
# library(tidycensus)
dfPop <- as.data.frame(get_acs(geography="state",variables="B01003_001",year=2005,survey = "acs1"))
dfPop$Year <- 2005

for(i in 2006:2023)
{
  if(i < 2010)
  {
    dfPopCur <- as.data.frame(get_acs(geography="state",variables="B01003_001",year=i,survey = "acs1"))
    dfPopCur$Year <- i
    dfPop <- rbind(dfPop,dfPopCur)
  }else{

    dfPopCur <- as.data.frame(get_acs(geography="state",variables="B01003_001",year=i,survey = "acs5"))
    dfPopCur$Year <- i
    dfPop <- rbind(dfPop,dfPopCur)
  }
}
# # write.csv(dfPop,"statePopulation2005-2023.csv",row.names = FALSE)
###

###########################################################################
##### Covariate DATA ######################################################
### the covariates are highly correlated with eachother
source("FunctionsCntyDeath.R")
data <- getOverdoseData("all","all",more=TRUE)

### join covar data 
dfCov <- read.csv("cntyCovar2015-2024.csv",colClasses = c(GEOID = "character"))
dfCov$Year <- as.numeric(dfCov$Year)
data <- left_join(data, dfCov, by=c("FIPS_num"="GEOID", "Year"))
data <- data[data$Year >= 2015,] # some covar are valid for 2015 to 2024, some 2010 to 2024

cor(data[!is.na(data$Poverty)&!is.na(data$rate)&!is.na(data$medianIncome),
         c("rate","Cognitive","SelfCare","Insured","Education","Poverty","maritalStatus_D_acs", "medianIncome")])

# correlation of covar: an analysis
cor(data[!is.na(data$Poverty)&substr(data$FIPS,1,2)=="WV"&!is.na(data$rate),c("rate","Cognitive","SelfCare","Insured","Education","Poverty")])
# correlation by state
state <- unique(substr(data$FIPS,1,2))
correlation <- matrix(nrow=length(state),ncol=6)
for(i in 1:length(state))
{
  correlation[i,1] <- state[i]
  correlation[i,2] <- cor(data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),"rate"],data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),c("Cognitive")])
  correlation[i,3] <- cor(data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),"rate"],data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),c("SelfCare")])
  correlation[i,4] <- cor(data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),"rate"],data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),c("Insured")])
  correlation[i,5] <- cor(data[substr(data$FIPS,1,2)==state[i]& !is.na(data$Poverty)&!is.na(data$rate),"rate"],data[substr(data$FIPS,1,2)==state[i]&!is.na(data$Poverty)&!is.na(data$rate),c("Poverty")])
  correlation[i,6] <- cor(data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),"rate"],data[substr(data$FIPS,1,2)==state[i]&!is.na(data$rate),c("Education")])
}
correlation <- as.data.frame(correlation)
names(correlation) <- c("State","Cognitive","SelfCare","Insured","Poverty","Education")
correlation <- correlation %>%
  mutate(across(c(Cognitive,SelfCare,Insured,Poverty,Education), ~ round(as.numeric(.x), 2)))

# correlation by year
yr <- unique(data$Year)
correlation <- matrix(nrow=length(yr),ncol=6)
for(i in 1:length(yr))
{
  correlation[i,1] <- yr[i]
  correlation[i,2] <- cor(data[data$Year==yr[i]&!is.na(data$rate),"rate"],data[data$Year==yr[i]&!is.na(data$rate),c("Cognitive")])
  correlation[i,3] <- cor(data[data$Year==yr[i]&!is.na(data$rate),"rate"],data[data$Year==yr[i]&!is.na(data$rate),c("SelfCare")])
  correlation[i,4] <- cor(data[data$Year==yr[i]&!is.na(data$rate),"rate"],data[data$Year==yr[i]&!is.na(data$rate),c("Insured")])
  correlation[i,5] <- cor(data[data$Year==yr[i]& !is.na(data$Poverty)&!is.na(data$rate),"rate"],data[data$Year==yr[i]&!is.na(data$Poverty)&!is.na(data$rate),c("Poverty")])
  correlation[i,6] <- cor(data[data$Year==yr[i]&!is.na(data$rate),"rate"],data[data$Year==yr[i]&!is.na(data$rate),c("Education")])
}
correlation <- as.data.frame(correlation)
names(correlation) <- c("Year","Cognitive","SelfCare","Insured","Poverty","Education")
correlation <- correlation %>%
  mutate(across(c(Cognitive,SelfCare,Insured,Poverty,Education), ~ round(as.numeric(.x), 2)))

# plot them, they all look the same:
temp <- suppressData(data,"Deaths","rate")
par(mfrow=c(2,3))
plot(temp$Cognitive,temp$rate,ylab="Overdose Death Rate",xlab="Cognitive Disability Estimate")
plot(temp$SelfCare,temp$rate,ylab="Overdose Death Rate",xlab="Selfcare Disability Estimate")
plot(temp$Insured,temp$rate,ylab="Overdose Death Rate",xlab="Insured Estimate")
plot(temp$Poverty,temp$rate,ylab="Overdose Death Rate",xlab="Poverty Estimate")
plot(temp$Education,temp$rate,ylab="Overdose Death Rate",xlab="Highschool Diploma or less")
boxplot(temp[,c(9:13)]/temp$population,main="Rate: Covar/population")

par(mfrow=c(3,3))
for(i in unique(temp$Year))
{
 plot(temp[temp$Year == i,"Cognitive"],temp[temp$Year == i,"rate"],main=paste("Year:",i),
      ylab="Overdose Death Rate",xlab="Cognitive Disability Estimate") 
}

### end covar data issues

