# Penelope Overholt
# Master's project code
# 2026

### GPBoost

# this analysis fits GPBoost models

library(gpboost)
library(sf) #st_centroid, st_coordinates

source("FunctionsCntyDeath.R")

# demonstration: https://medium.com/data-science/mixed-effects-machine-learning-for-longitudinal-panel-data-with-gpboost-part-iii-523bb38effc
# w/ coords demo: https://medium.com/data-science/mixed-effects-machine-learning-with-gpboost-for-grouped-and-areal-spatial-econometric-data-b26f8bddd385

#group_data is the ID for each group
#data is predictor variables, should include response, year, and any covariates
#label is name of response variable

set.seed(2025)
## choose state(s) to run analysis on
StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))

# choose_state <- c("ME","VT","NH","MA","RI","CT","NY","NJ","PA","DE","MD", "DC") # northeast states
# choose_state <- c("GA","NC","SC","VA") # southeast coast
# choose_state <- c("AR","TN","WV","KY") # appalachian
# choose_state <- c("FL","AL","MS","LA") # gulf coast
# choose_state <- c("AZ","NM","OK","TX") #Southwest states
# choose_state <- c("WI","MI","OH","IL","IN") # great lakes
# choose_state <- c("ND","SD","NE","KS","MN","IA","MO") # midwest states
# choose_state <- c("WA","MT","ID","WY","CO","UT","NV","CA","OR") # western states

choose_state <- c("WV") # west Virginia

state_list <- StateNameNum %>% filter(Abbreviation %in% choose_state)
state_list <- c(state_list[,1])
 
# choose_state <- "all"
# state_list <- c(StateNameNum[c(1,3:11,13:51),1])#all

# get centroids of each state
map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(STATEFP %in% state_list)
centroids <- map_county %>% st_centroid()
centroids$long <- st_coordinates(centroids[,"geometry"])[,1]
centroids$lat <- st_coordinates(centroids[,"geometry"])[,2]

test_yr = 2023
data <- getOverdoseData(choose_state,"all")

### join covar data 
dfCov <- read.csv("cntyCovar2015-2024.csv",colClasses = c(GEOID = "character"))
dfCov$Year <- as.numeric(dfCov$Year)
data <- left_join(data, dfCov, by=c("FIPS_num"="GEOID", "Year"))
data <- data[data$Year >= 2015,] # some covar are valid for 2015 to 2024, some 2010 to 2024

### join map data
data <- inner_join(data,centroids,by=c("FIPS_num"="GEOID")) # want inner join

# data[is.na(data[,"long"]),] # check for issues in data. if a cnty does not have long or lat that is an issue.

data <- data[,-which(names(data)=="geometry")]   #remove geometry 
data <- data[!is.na(data$rate),] # remove NA rate

data_train <- data.matrix(data[data$Year < test_yr,])
data_test <- data.matrix(data[data$Year == test_yr,])

# about fitting GPBoost
# there are three types here: Gaussian Process by group, Gaussian Process by cluster, Gaussian Process (shared): Time
# There are 4 places where the code changes based on what model you choose. 
# They are: FIT GPBOOST model, TRAIN MODEL, TRAINING PREDICTION, TEST PREDICTION
# Gaussian Process by cluster is NOT include in my write up

## FIT GPBOOST model
# Gaussian Process by group: Space, Random Effect: Year, Fixed Effect: -many- ##############3
rand_effect <- c("Year")
fixed_effect <- c("Cognitive", "SelfCare","Insured" ,"Poverty",
                  "Education" ,"maritalStatus_D_acs","medianIncome")

gp_model <- GPModel(group_data = data_train[,c("FIPS_num")], #grouped random effects, each group_data has its own intercept
                    group_rand_coef_data = data_train[,rand_effect],#random  effects (slope) covars for group_data
                    ind_effect_group_rand_coef =c(seq(1,1,length.out = length(rand_effect))),
                    gp_coords = data_train[,c("long","lat")],#Gaussian process random effects
                    # cluster_ids = ,# independent spatial and grouped random effects for each cluster but shared fixed effects
                    likelihood = "gaussian",cov_function = "exponential")#21.72 and 32.69
# #end
 
# Gaussian Process by cluster: Time, Random Effect: Year and Cognitive, Fixed Effect: Year, Cognitive, FIPS_num
# cognitive optional
# # each cluster_ids has a separate temporal Gaussian process (gp_coords)
# rand_effect <- c("Cognitive")
# fixed_effect <- c(rand_effect,"FIPS_num")
# 
# gp_model <- GPModel(gp_coords = data_train[,c("Year")], 
#                     group_rand_coef_data = data_train[,rand_effect],#add covar
#                     ind_effect_group_rand_coef = c(seq(1,1,length.out = length(rand_effect))),
#                     likelihood = "gaussian",cov_function="exponential",
#                     cluster_ids = data_train[,"FIPS_num"])
# #end

# Gaussian Process (shared): Time, Fixed Effect:-many-##################3
# single temporal Gaussian process (gp_coords) shared across group_data 
# rand_effect <- c("")
# fixed_effect <- c("Cognitive", "SelfCare","Insured" ,"Poverty",
#                   "Education" ,"maritalStatus_D_acs","medianIncome")
# 
# gp_model <- GPModel(group_data=data_train[,"FIPS_num"],
#                     # group_rand_coef_data = data_train[,rand_effect],#add covar
#                     # ind_effect_group_rand_coef = c(seq(1,1,length.out = length(rand_effect))),
#                     gp_coords = data_train[,c("Year")], 
#                     likelihood = "gaussian",cov_function="exponential")
#end 

### end model definitions
################################################################

## make data set
data_bst <- gpb.Dataset(data = data_train[,c(fixed_effect)], #fixed effects 
                        label = data_train[,"rate"])#response data

####### TRAIN MODEL #####

# Gaussian Process by cluster (I did not find optimal tuning parameters for this model)
# params <- list(learning_rate = 0.01, max_depth = 2, num_leaves = 2^10,
#                min_data_in_leaf = 10, lambda_l2 = 10)#old
# nrounds <- 5 #37  # number of boosting iterations 

# Find optimal tuning parameters
# opt_params <- gpb.grid.search.tune.parameters(param_grid = param_grid, params = other_params,
#                                               num_try_random = NULL, folds = folds,
#                                               data = data_bst, gp_model = gp_model,
#                                               nrounds = 1000, early_stopping_rounds = 10,
#                                               verbose_eval = 1, metric = "mse")
# opt_params 

# Gaussian Process by group
params <- list(learning_rate = 1, max_depth = 10, num_leaves = 2^10,
               min_data_in_leaf = 10, lambda_l2 = 0)#best for space
nrounds <- 880#best for space, too much for the 8 regions
# nrounds <- 40 # for space all usa regions
#end

# Gaussian Process (shared): Time
# params <- list(learning_rate = 1, max_depth = 10, num_leaves = 2^10,
#                min_data_in_leaf = 10, lambda_l2 = 10)#best for time
# nrounds <- 90#is best for time# number of boosting iterations 
#end

gpbst <- gpb.train(data = data_bst, gp_model = gp_model, nrounds = nrounds, 
                   params = params, verbose = 0) 
#gpboost() below  is the same as gpb.train() above
# gpbst <- gpboost(data = data_bst, gp_model = gp_model,nrounds = nrounds, params = params, verbose = 1) #same as gpb.train() above
 
# look at model outputs
summary(gp_model)# Estimated covariance parameters
var(data_train[,"rate"]) #variance of response
# predict_training_data_random_effects(gp_model)

View(gpb.importance(gpbst))
# higher "Gain" means the covar is more important for reducing model error
feature_importances <- gpb.importance(gpbst, percentage = TRUE)
gpb.plot.importance(feature_importances,  measure = "Gain", 
                    main = "Split-based variable importances")

##############################################################
####### TRAINING PREDICTION #####
# Gaussian Process by group
train_pred <- predict(gpbst, data = data_train[,c(fixed_effect)],
                gp_coords_pred = data_train[, c("long", "lat")],
                group_data_pred = data_train[,c("FIPS_num")],
                group_rand_coef_data_pred = data_train[,rand_effect],
                # cluster_ids_pred = ,
                predict_var = TRUE)
# # end
 
# Gaussian Process by cluster
# train_pred <- predict(gpbst, data = data_train[,c(fixed_effect)],
#                 gp_coords_pred = data_train[, "Year"],
#                 cluster_ids_pred = data_train[,"FIPS_num"],
#                 group_rand_coef_data_pred = data_train[,rand_effect],
#                 predict_var = TRUE) 
# #end

# Gaussian Process (shared): Time
# train_pred <- predict(gpbst, data = data_train[,c(fixed_effect)],
#                 group_data_pred = data_train[,c("FIPS_num")],
#                 gp_coords_pred = data_train[, "Year"],
#                 # group_rand_coef_data_pred = data_train[,rand_effect],
#                 predict_var = TRUE)
#end 
##############################################################

train_mse <- mean((data_train[,"rate"] - train_pred$response_mean)^2) 
sqrt(train_mse)
train_mae <- mean(abs(data_train[,"rate"] - train_pred$response_mean)) 
train_mae

##############################################################
####### TEST PREDICTION #####
# Gaussian Process by group
pred <- predict(gpbst, data = data_test[,c(fixed_effect)],
                gp_coords_pred = data_test[, c("long", "lat")],
                group_data_pred = data_test[,"FIPS_num"],
                group_rand_coef_data_pred = data_test[,rand_effect],
                predict_var = TRUE)

# Gaussian Process by cluster
# pred <- predict(gpbst, data = data_test[,c(fixed_effect)],
#                 gp_coords_pred = data_test[, "Year"],
#                 cluster_ids_pred = data_test[,"FIPS_num"],
#                 group_rand_coef_data_pred = data_test[,rand_effect],
#                 predict_var = TRUE)
# # end

# Gaussian Process (shared): Time
# pred <- predict(gpbst, data = data_test[,c(fixed_effect)],
#                 group_data_pred = data_test[,c("FIPS_num")],
#                 gp_coords_pred = data_test[, "Year"],
#                 # group_rand_coef_data_pred = data_test[,rand_effect],
#                 predict_var = TRUE)
# end 
##############################################################

y_pred <- pred$response_mean #test set prediction
test_mse <- mean((data_test[,"rate"] - y_pred)^2)
sqrt(test_mse)
test_mae <- mean(abs(data_test[,"rate"] - y_pred))
test_mae

data$pred[data$Year == test_yr] <- pred$response_mean #test set prediction
data$pred[data$Year < test_yr] <- train_pred$response_mean
data$var[data$Year == test_yr] <- pred$response_var
data$res <- data$rate - data$pred

## 90th percentile 
data_2023 <- data[data$Year == test_yr,]
mean(data_2023$pred)#predicted mean
pred_top10 <- data_2023[data_2023$pred >= quantile(data_2023$pred, 0.90),]
pred_top10[order(pred_top10$pred,decreasing=TRUE),c("FIPS_num","County","pred")]#predicted 90th percentile

# ggplot(data=data) +
#     geom_point(aes(y = res, x = pred))+
#     labs(title = paste("Fitted vs Residuals"),
#          subtitle=paste("AndersonDarling:",nortest::ad.test(data$res)$p.value),
#          x = "Fitted", y = "Residuals") +
#     theme_classic()+
#     theme(
#        axis.text = element_text(size = 14,face="bold"),
#        axis.title = element_text(size = 15),
#        plot.title = element_text(size = 18, face = "bold"),
#        legend.text = element_text(size = 14),
#        legend.title = element_text(size = 15))

#folder <- "gpboost_pred" # save each region/model, this is for boost iteration = 5, no covars
# folder <- "gpboost_pred_time" # save each region/model for the temporal gpboost model
# folder <- "gpboost_pred_space" # save each region/model for the spatial gpboost model
# write.csv(data,paste(folder,"/gpboost_northeast.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_southeast.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_appalachian.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_gulfcoast.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_southwest.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_greatlakes.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_midwest.csv",sep=""),row.names = F)
# write.csv(data,paste(folder,"/gpboost_west.csv",sep=""),row.names = F)

##############################################################
#makes plots and figures, must suppress data for outputting raw predictions or counts
# data_supp <- suppressData(data,"Deaths",c("pred","var","res","rate")) #for restricted data
data_supp <- data 

# make table of predictions
myt <- ttheme_default(
  core = list(bg_params=list(fill=c("white", "lightgrey"))),
  colhead = list(fg_params=list(col="black"),
  bg_params=list(fill="thistle3"))
)

# this outputs the exact pred values
# table_data <- data_supp[!is.na(data_supp$pred) & data_supp$Year == test_yr,c("FIPS","rate","pred","res")]
# names(table_data) <- c("FIPS","rate","predicted","residual")
# plottable <- tableGrob(table_data, theme = myt)
# table <- gtable_add_rows(
#      plottable,
#      heights =  unit(5,"mm"),
#      pos = 0)
# table <- gtable_add_grob(
#     table,
#     textGrob(paste("GPBoost Prediction for",choose_state,"in",test_yr),gp=gpar(fontsize=15)),
#     1, 1, 1, ncol(table))
# 
# grid.arrange(table)

# make table of summary
table_sum_data <- as.data.frame(matrix(c(
                           round(summary(data[data$Year < test_yr & !is.na(data$rate),"rate"]),2),
                           round(summary(data[data$Year < test_yr & !is.na(data$rate),"pred"]),2),
                           round(summary(data[data$Year < test_yr & !is.na(data$rate),"res"]),2),
                           round(summary(data[data$Year == test_yr & !is.na(data$rate),"rate"]),2),
                           round(summary(data[data$Year == test_yr & !is.na(data$rate),"pred"]),2),
                           round(summary(data[data$Year == test_yr & !is.na(data$rate),"res"]),2)),
                         byrow=T,ncol=6,nrow=6))
names(table_sum_data) <- c("Min","1st Qu.","Median","Mean","3rd Qu.","Max")
row.names(table_sum_data) <- c("Training Set Rate","Training Set Prediction","Training Set Residuals",
                               "Test Set Rate","Test Set Prediction","Test Set Residuals")

plottable <- tableGrob(table_sum_data, theme = myt)
table_sum <- gtable_add_rows(
     plottable, 
     heights =  unit(5,"mm"),
     pos = 0)
table_sum <- gtable_add_grob(
    table_sum, 
    textGrob(paste("Summary Statistics of GPBoost Predictions and Residuals"),gp=gpar(fontsize=15)), 
    1, 1, 1, ncol(table_sum))

grid.arrange(table_sum)

# make state plots

## get all predictions from each model all together
# folder <- "gpboost_pred" #  each region/model, this is for boost iteration = 5, no covars
# folder <- "gpboost_pred_time" #  each region/model for the temporal gpboost model
# folder <- "gpboost_pred_space" # save each region/model for the spatial gpboost model
# data <- read.csv(paste(folder,"/gpboost_appalachian.csv",sep=""),colClasses = c(FIPS_num = "character"))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_greatlakes.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_gulfcoast.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_midwest.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_northeast.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_southeast.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_southwest.csv",sep=""),colClasses = c(FIPS_num = "character")))
# data <- rbind(data,read.csv(paste(folder,"/gpboost_west.csv",sep=""),colClasses = c(FIPS_num = "character")))
# train_mse <- mean((data[data$Year < test_yr,"res"])^2,na.rm=TRUE)
# test_mse <- mean((data[data$Year == test_yr,"res"])^2,na.rm=TRUE)
# train_mae <- mean(abs(data[data$Year < test_yr,"res"]),na.rm=TRUE)
# test_mae <- mean(abs(data[data$Year == test_yr,"res"]),na.rm=TRUE)
# StateNameNum <- read.csv("StateNumToAbb.csv",colClasses = c(Number = "character"))
# state_list <- c(StateNameNum[c(1,3:11,13:51),1])
# choose_state = "all"
# data_supp <- suppressData(data,"Deaths",c("pred","var","res","rate")) #suppress
# map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
# map_county <- map_county %>% filter(STATEFP %in% state_list)
# map_county <- left_join(map_county,data_supp[data_supp$Year==2023,],by=c("GEOID"="FIPS_num"))
## percentile 
# data_2023 <- data[data$Year == test_yr,]
# mean(data_2023$pred)
# pred_top10 <- data_2023[data_2023$pred >= quantile(data_2023$pred, 0.90),]
# View(pred_top10[order(pred_top10$pred,decreasing=TRUE),c("FIPS_num","State","County","pred")])
# mean(data_2023$rate)
# obs_top10 <- data_2023[data_2023$rate >= quantile(data_2023$rate, 0.90),]
# View(obs_top10[order(obs_top10$rate,decreasing=TRUE),c("FIPS_num","State","County","rate")])

## end for all models

map_county <- sf::st_read("ShapeFiles/tl_2021_us_county.shp",stringsAsFactors=F,as_tibble=T)
map_county <- map_county %>% filter(STATEFP %in% state_list)

map_county <- left_join(map_county,data_supp[data_supp$Year == test_yr,c("FIPS_num","pred","var","res","rate")] , 
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
        labs(title=paste("Predicted Overdose Death Rate per 100K in",test_yr),
             subtitle=paste("Training RMSE:",round(sqrt(train_mse),2),
                            "Test RMSE:",round(sqrt(test_mse),2)),x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

plot_grid(CntyDrugDeathYearplot(choose_year=test_yr,state=choose_state),pred.plot)

var.plot <- ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = var),color=NA)+
        scale_fill_gradient(na.value = "lightgrey",low="orange",high="purple",
                            name="Error")+
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste(title="Drug Death Prediction Error for",test_yr),
             x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

res.plot <- ggplot()+
        geom_sf(data = map_county,mapping=aes(fill = res),color=NA)+
        scale_fill_gradient2(low="orange", mid = "grey95",high="purple",
                             midpoint=0,name="Residuals")+
        geom_sf(data = map_state,color="grey",linewidth=.25,fill=NA)+
        labs(title=paste(title="Predicted Overdose Death Rate Residuals for",test_yr),
             x="Longitude",y="Latitude")+
        theme_classic()+
        theme(
           axis.text = element_text(size = 14,face="bold"),
           axis.title = element_text(size = 15),
           plot.title = element_text(size = 18, face = "bold"),
           legend.text = element_text(size = 14),
           legend.title = element_text(size = 15))

