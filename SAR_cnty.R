# Penelope Overholt
# Master's project code
# 2026

### SAR model 

# this analysis looks at a SAR model on overdose death rate at the county level

library(sf) 
library(spdep) 
library(spatialreg) # spautolm

source("FunctionsCntyDeath.R")

#choose state(s) to run analysis on
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

choose_state <- "WV"
state_list <- StateNameNum %>% filter(Abbreviation %in% c(choose_state))
state_list <- c(state_list[,1])

map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(STATEFP %in% state_list)

data <- getOverdoseData(choose_state,"all")
data <- data[data$Year >= 2015,]
dfCov <- read.csv("cntyCovar2015-2024.csv",colClasses = c(GEOID = "character"))
dfCov$Year <- as.numeric(dfCov$Year)
data <- left_join(data, dfCov, by=c("FIPS_num"="GEOID", "Year"))

mse_year <- matrix(ncol=7,nrow=length(c(2015:2022)))

sum_sq_pred_1 <- 0
sum_abs_pred_1 <- 0
n_sum_sq_1 <- 0

sum_sq_pred <- 0
sum_abs_pred <- 0
n_sum_sq <- 0

for(i in 2015:2022)
{
  map_county_join <- left_join(map_county,data[data$Year == i,],by=c("GEOID"="FIPS_num"))
  
  poly.nb.narm <- poly2nb(map_county_join) 
  W = nb2listw(poly.nb.narm, style="W",zero.policy=T) 
  # coords <- st_centroid(st_geometry(map_county_join), of_largest_polygon=TRUE)
  # poly.nb <- poly2nb(map_county_join)
  # plot(st_geometry(map_county_join), border="grey")
  # plot(poly.nb, coords, add=TRUE)
  
  sar_1 = spautolm(rate~1, data=map_county_join,W, family="SAR") #for no covariates, intercept only

  sar = spautolm(rate~Cognitive+SelfCare+Insured+Poverty+Education+maritalStatus_D_acs+medianIncome,
                 data=map_county_join,W, family="SAR") 
  
  cat("YEAR:",i,"----------------------\n")
  cat("INTERCEPT:",i,"\n")
  print(summary(sar_1))
  cat("COVAR:",i,"\n")
  print(summary(sar))
  cat("MORAN:",i,"\n")
  print(moran.test(map_county_join$rate,W))
  
  mse_year[(i-2014),1] <- i
  mse_year[(i-2014),2] <- mean((sar_1$fit$residuals)^2)
  mse_year[(i-2014),3] <- sqrt(mse_year[(i-2014),2])
  mse_year[(i-2014),4] <- mean(abs(sar_1$fit$residuals))#mean abs error
  mse_year[(i-2014),5] <- mean((sar$fit$residuals)^2)
  mse_year[(i-2014),6] <- sqrt(mse_year[(i-2014),5])
  mse_year[(i-2014),7] <- mean(abs(sar$fit$residuals))#mean abs error
  
  sum_sq_pred_1 <- sum((sar_1$fit$residuals)^2)+sum_sq_pred_1
  sum_abs_pred_1 <- sum(abs(sar_1$fit$residuals))+sum_abs_pred_1
  n_sum_sq_1 <- length(sar_1$fit$residuals)+n_sum_sq_1
  
  sum_sq_pred <- sum((sar$fit$residuals)^2)+sum_sq_pred
  sum_abs_pred <- sum(abs(sar$fit$residuals))+sum_abs_pred
  n_sum_sq <- length(sar$fit$residuals)+n_sum_sq

}

all_rmse_1 <- sqrt(sum_sq_pred_1/n_sum_sq_1)
all_rmse_1
all_mae_1 <- sum_abs_pred_1/n_sum_sq_1
all_mae_1

all_rmse <- sqrt(sum_sq_pred/n_sum_sq)
all_rmse
all_mae <- sum_abs_pred/n_sum_sq
all_mae

mse_year <- as.data.frame(mse_year)
names(mse_year) <- c("Year","mse_int","rmse_int","mae_int","mse","rmse","mae")
mse_year

res <- as.data.frame(matrix(c(sar$fit$residuals,names(sar$fit$residuals)),ncol=2))
names(res) <- c("res","row_num")
pred <- as.data.frame(matrix(c(sar$fit$fitted.values,names(sar$fit$fitted.values)),ncol=2))
names(pred) <- c("pred","row_num")

res1 <- as.data.frame(matrix(c(sar_1$fit$residuals,names(sar_1$fit$residuals)),ncol=2))
names(res1) <- c("res1","row_num")
pred1 <- as.data.frame(matrix(c(sar_1$fit$fitted.values,names(sar_1$fit$fitted.values)),ncol=2))
names(pred1) <- c("pred1","row_num")

map_county$row_num <- row.names(map_county)
map_county <- left_join(map_county,res,by="row_num")
map_county <- left_join(map_county,pred,by="row_num")
map_county$res <- as.numeric(map_county$res)
map_county$pred <- as.numeric(map_county$pred)

map_county <- left_join(map_county,res1,by="row_num")
map_county <- left_join(map_county,pred1,by="row_num")
map_county$res1 <- as.numeric(map_county$res1)
map_county$pred1 <- as.numeric(map_county$pred1)
  
mae <- mean(abs(sar$fit$residuals))
mae
mse <- mean((sar$fit$residuals)^2)
sqrt(mse)

mae1 <- mean(abs(sar_1$fit$residuals))
mae1
mse1 <- mean((sar_1$fit$residuals)^2)
sqrt(mse1)

map_state <- sf::st_read("ShapeFiles/tl_2022_us_state.shp",stringsAsFactors=F,as_tibble=T) 
map_state <- map_state %>% filter(STATEFP %in% state_list)

# map_county_supp <- suppressData(map_county,"Deaths",c("pred","res","rate"))# for restricted data
map_county_supp <-map_county

# make plots
# NOTE: when fitting with the public data, you may have negative predicted values. 
# This means that for the pred.plot and pred.plot1 below, if a county is grey it is because the predicted value <0.

library(ggplot2) 

max_rate=200
OOB_df <- map_county_supp[!is.na(map_county_supp$pred) & map_county_supp$pred > max_rate,]
pred.plot <- ggplot()+
        geom_sf(data = map_county_supp,mapping=aes(fill = pred),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = pred))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Predicted Overdose Death Rate per 100K in",2022),
             subtitle=paste("RMSE:",round(sqrt(mse),2)),x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

plot_grid(CntyDrugDeathYearplot(choose_year=2022,state=choose_state),pred.plot)

OOB_df <- map_county_supp[!is.na(map_county_supp$pred1) & map_county_supp$pred1 > max_rate,]
pred.plot1 <- ggplot()+
        geom_sf(data = map_county_supp,mapping=aes(fill = pred1),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = pred1))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Predicted Overdose Death Rate per 100K in",2022),
             subtitle=paste("RMSE:",round(sqrt(mse1),2)),x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))
plot_grid(CntyDrugDeathYearplot(choose_year=2022,state=choose_state),pred.plot1)

res.plot <- ggplot()+
        geom_sf(data = map_county_supp,mapping=aes(fill = res),color=NA)+
        scale_fill_gradient2(low="orange", mid = "grey95",high="purple",
                             midpoint=0,name="Residuals")+
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste(title="Predicted Overdose Death Rate Residuals for",2022),
             x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))
