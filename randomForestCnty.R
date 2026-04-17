# Penelope Overholt
# Master's project code
# 2026

source("FunctionsCntyDeath.R")

library(randomForest) #randomForest
library(forecast) #accuracy()

set.seed(1)
 
choose_state = "WV"
data <- getOverdoseData(state=choose_state,choose_year="all",more=FALSE)
  
data <- data[data$Year >= 2015,]
curr_FIPS <- unique(data$FIPS)
info <- matrix(ncol=8,nrow=length(curr_FIPS))

sum_sq_pred <- 0
sum_sq_pred_bench <- 0
n_sum_sq <- 0

sum_abs_pred <- 0
sum_abs_pred_bench <- 0

par(mfrow=c(5,5))

for(i in 1:length(curr_FIPS))
{
  temp <- data[data$FIPS == curr_FIPS[i] & !is.na(data$rate),]
  data_ts <- ts(temp$rate,
                start=c(min(temp$Year),1),frequency=1)

  test_yr=2023
  chooseLag=2

  data_ts_org <- window(data_ts,end = c((test_yr-1),1)) #train with 2015-2022
  lag_order <- chooseLag
  horizon <- 1 # the forecast horizon
  data_ts_mbd <- embed(data_ts_org, (lag_order + 1))

  y_train <- data_ts_mbd[, 1] # the target
  X_train <- data_ts_mbd[, -1] # everything but the target
  y_test <- window(data_ts, start = c(test_yr,1), end = c(test_yr,1))
  X_test <- data_ts_mbd[nrow(data_ts_mbd), c(1:(lag_order))]
  set.seed(1)
   # fit the model
   fit_rf <- randomForest(X_train, y_train)
   mse <- fit_rf$mse
   trainingRMSE <- sqrt(mse[length(mse)])
   # predict using the test set
   forecasts_rf <- predict(fit_rf, X_test)

    y_fore <- ts(
     forecasts_rf,
     start = c(test_yr, 1),
     frequency = 1
    )

# add the forecasts to the original table

  testacc=accuracy(y_fore, y_test)
  temp <- forecast(naive(data_ts_org), h = horizon)
  benchmark <- accuracy(temp,y_test) #is a basic model to compare to

  info[i,1] <- curr_FIPS[i]
  info[i,2] <- trainingRMSE
  info[i,3] <- testacc[2] # testing RMSE
  info[i,4] <- y_fore # predict for test year
  info[i,5] <- benchmark[1,2]#training RMSE
  info[i,6] <- benchmark[2,2]#testing RMSE
  info[i,7] <- temp$mean
  info[i,8] <- length(data_ts)
  
  # plot(data_ts,main=paste(curr_FIPS[i],info[i,3]))+
  #   points(y_fore,col="red",pch=16)
  
  obs_train <- data_ts_org[3:length(data_ts_org)]
  sum_sq_pred <- sum((obs_train - fit_rf$predicted)^2)+sum_sq_pred
  sum_sq_pred_bench <- sum((data_ts_org[2:length(data_ts_org)] - temp$fitted[2:length(temp$fitted)] )^2)+sum_sq_pred_bench
  n_sum_sq <- length(fit_rf$predicted)+n_sum_sq
  
  sum_abs_pred <- sum(abs(obs_train - fit_rf$predicted))+sum_abs_pred
  sum_abs_pred_bench <- sum(abs(data_ts_org[2:length(data_ts_org)] - temp$fitted[2:length(temp$fitted)] ))+sum_abs_pred_bench
}

all_rmse_train <- sqrt(sum_sq_pred/n_sum_sq)
all_rmse_train_bench <- sqrt(sum_sq_pred_bench/n_sum_sq)

all_mae_train <- sum_abs_pred/n_sum_sq
all_mae_train_bench <- sum_abs_pred_bench/n_sum_sq

info <- as.data.frame(info)
names(info) <- c("FIPS","TrainRMSE","TestRMSE","predict","benchmark_train","benchmark_test","bench_pred","n")

par(mfrow=c(1,1))
plot(x=factor(info$FIPS),info$TestRMSE)+
  points(x=factor(info$FIPS),info$benchmark_test,col="red")+
  abline(h=c(39.58))

data_test <- data[data$Year == test_yr,]
data_test <- left_join(data_test,info,by=c("FIPS"))
data_test$predict <- as.numeric(data_test$predict)
data_test$bench_pred <- as.numeric(data_test$bench_pred)

all_rmse <- sqrt(mean((data_test$rate -  data_test$predict)^2))
all_rmse_bench <- sqrt(mean((data_test$rate -  data_test$bench_pred)^2))

all_mae <- mean(abs(data_test$rate -  data_test$predict))
all_mae_bench <- mean(abs(data_test$rate -  data_test$bench_pred))

## 90th percentile 
obs_top10 <- data_test[data_test$rate >= quantile(data_test$rate, 0.90),]
obs_top10[order(obs_top10$rate,decreasing=TRUE),c("FIPS_num","County","rate")]#observed 90th percentile

pred_top10 <- data_test[data_test$predict >= quantile(data_test$predict, 0.90),]
pred_top10[order(pred_top10$predict,decreasing=TRUE),c("FIPS_num","County","predict")]#predicted 90th percentile

rmse_top10 <- data_test[as.numeric(data_test$TestRMSE) >= quantile(as.numeric(data_test$TestRMSE), 0.90),]
rmse_top10[order(as.numeric(rmse_top10$TestRMSE),decreasing=TRUE),c("FIPS_num","County","TestRMSE","rate","predict")]#RMSE 90th percentile

pred_top10_bench <- data_test[data_test$bench_pred >= quantile(data_test$bench_pred, 0.90),]
pred_top10_bench[order(pred_top10_bench$bench_pred,decreasing=TRUE),c("FIPS_num","County","predict")]#predicted 90th percentile


#makes plots and figures, must suppress data for outputting raw predictions or counts
data_supp <- suppressData(data_test,"Deaths",c("predict","bench_pred","rate"))

StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
choose_state <- c("WV") # west virginia
state_list <- StateNameNum %>% filter(Abbreviation %in% choose_state)
state_list <- c(state_list[,1])

map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(STATEFP %in% state_list)
map_county <- left_join(map_county,data_supp[data_supp$Year == test_yr,c("FIPS_num","predict","bench_pred","rate")] , 
                       by = c("GEOID"="FIPS_num"))
map_state <- sf::st_read("ShapeFiles/tl_2021_us_state.shp",stringsAsFactors=F,as_tibble=T) 
map_state <- map_state %>% filter(STATEFP %in% state_list)

max_rate=200

# random forest
OOB_df <- map_county[!is.na(map_county$predict) & map_county$predict > max_rate,]
pred.plot <- ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = predict),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = rate))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Predicted Overdose Death Rate per 100K in",test_yr),
             subtitle=paste("Taining RMSE:",round(all_rmse_train,2),
                            "Test RMSE:",round(all_rmse,2)), x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

plot_grid(CntyDrugDeathYearplot(choose_year=test_yr,state=choose_state),pred.plot)

# random walk
OOB_df <- map_county[!is.na(map_county$bench_pred) & map_county$bench_pred > max_rate,]
pred.plot <- ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = bench_pred),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = rate))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Predicted Overdose Death Rate per 100K in",test_yr),
             subtitle=paste("Test RMSE:",round(all_rmse_bench,2)), x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

plot_grid(CntyDrugDeathYearplot(choose_year=test_yr,state=choose_state),pred.plot)


# summary table
# for tables
library(grid) #textGrob
library(gtable) #gtable_add_rows

myt <- ttheme_default(
  core = list(bg_params=list(fill=c("white", "lightgrey"))),
  colhead = list(fg_params=list(col="black"),
  bg_params=list(fill="thistle3"))
)


# this outputs the exact pred values
table_data <- data_supp[!is.na(data_supp$rate),c("FIPS","rate","predict")]
plottable <- tableGrob(table_data, theme = myt)
table <- gtable_add_rows(
     plottable,
     heights =  unit(5,"mm"),
     pos = 0)
table <- gtable_add_grob(
    table,
    textGrob(paste("Random Forest Prediction for",choose_state,"in",test_yr),gp=gpar(fontsize=15)),
    1, 1, 1, ncol(table))

grid.arrange(table)


#### SPATIAL RF
# https://blasbenito.github.io/spatialRF/
# Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, 77(1), 1-17. doi:10.18637/jss.v077.i01
# Blas M. Benito (2025). spatialRF: Easy Spatial Regression with Random Forest. R package version 1.1.5. doi: 10.5281/zenodo.17992636. url: https://blasbenito.github.io/spatialRF/
  
source("FunctionsCntyDeath.R")
library(spatialRF)
library(sf)

choose_state <- c("WV") # west virginia
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
state_list <- StateNameNum %>% filter(Abbreviation %in% choose_state)
state_list <- c(state_list[,1])

test_yr = 2023

mse_year <- matrix(ncol=4,nrow=length(c(2015:2022)))
map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(substr(GEOID,1,2) %in% state_list)
coordinates <- st_coordinates(st_centroid(st_geometry(map_county), of_largest_polygon=TRUE) )[,1:2]
distance_matrix <- as.matrix(dist(coordinates))

coordinates <- as.data.frame(coordinates)
names(coordinates) <- c("x","y")

sum_sq_pred <- 0
sum_abs_pred <- 0
n_sum_sq <- 0

data <- getOverdoseData(state=choose_state,choose_year="all")
data <- data[data$Year >= 2015,]
#covar
dfCov <- read.csv("cntyCovar2015-2024.csv",colClasses = c(GEOID = "character"))
dfCov$Year <- as.numeric(dfCov$Year)
data <- left_join(data, dfCov, by=c("FIPS_num"="GEOID", "Year"))

for(i in 2015:2022)
{
  data_join <- left_join(map_county,data[data$Year == i,], by=c("GEOID"="FIPS_num"))

  rf_spatial_moran <- rf_spatial(
      data = data_join,
      dependent.variable.name = "rate",
      predictor.variable.names = c("Cognitive","SelfCare","Insured","Poverty","Education","maritalStatus_D_acs","medianIncome"),
      distance.matrix = distance_matrix,
      distance.thresholds = 0,
      method = "mem.moran.sequential",
      n.cores = 1,
      verbose=F#stop automatix outputs
  )
      
  rf_imp <- rf_importance(
    rf_spatial_moran,
    xy = coordinates
  )
  
  png(paste("rf_importance_",i,".png",sep=""), width = 300, height = 200)
  print(plot_importance(rf_spatial_moran))

  dev.off()#must run after saving plots
  
  # https://blasbenito.github.io/spatialRF/articles/spatial_models.html: Spatial predictors are named spatial_predictor_X_Y, where X is the neighborhood distance at which the predictor is generated, and Y is the index of the predictor.
  # higher val, more important
  
  mse_year[(i-2014),1] <- i
  mse_year[(i-2014),2] <- mean((data_join$rate - rf_spatial_moran$predictions$values)^2)
  mse_year[(i-2014),3] <- sqrt(mse_year[(i-2014),2])
  mse_year[(i-2014),4] <- mean(abs(data_join$rate - rf_spatial_moran$predictions$values))#mean abs error
  
  sum_sq_pred <- sum((data_join$rate - rf_spatial_moran$predictions$values)^2)+sum_sq_pred
  sum_abs_pred <- sum(abs(data_join$rate - rf_spatial_moran$predictions$values))+sum_abs_pred
  n_sum_sq <- length(rf_spatial_moran$predictions$values)+n_sum_sq
}

all_rmse <- sqrt(sum_sq_pred/n_sum_sq)
all_rmse
all_mae <- sum_abs_pred/n_sum_sq
all_mae
mse_year <- as.data.frame(mse_year)
names(mse_year) <- c("Year","MSE","RMSE","MAE")
mse_year 

data$pred <- rf_spatial_moran$predictions$values

data_supp <- suppressData(data[data$Year == 2022,],"Deaths",c("pred","rate"))

map_county <- left_join(map_county,data_supp,
                       by = c("GEOID"="FIPS_num"))
map_state <- sf::st_read("ShapeFiles/tl_2021_us_state.shp",stringsAsFactors=F,as_tibble=T) 
map_state <- map_state %>% filter(STATEFP %in% state_list)
  
max_rate=200
OOB_df <- map_county[!is.na(map_county$pred) & map_county$pred > max_rate,]
pred.plot <- ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = pred),color=NA)+
        scale_fill_gradient(limits=c(0,max_rate),na.value = "lightgrey",
                               low="orange",high="purple",name="Rate per 100K",
                            guide = guide_colorbar(order = 1))+
      
        ggnewscale::new_scale_fill() + #for OOB rates
        geom_sf(data=OOB_df, mapping=aes(fill = rate))+
        scale_fill_gradient(limits=c(max_rate,1200),breaks=seq(max_rate,1200,by=1200),
                            labels=c(""),na.value = "lightgrey",
                               low="black",high="black",name=paste("Rate >", max_rate))+
        guides(fill = guide_colorbar(barheight = unit(.5, "cm")))+  # Reduce height
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste("Predicted Overdose Death Rate per 100K in",2022),
             subtitle=paste("RMSE:",round(sqrt(mse_year[9,2]),2)),x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

plot_grid(CntyDrugDeathYearplot(choose_year=test_yr,state=choose_state),pred.plot)
