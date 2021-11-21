library("lubridate")
library("dplyr")
library("caret")
library("survival")
library("reshape2")
library('xgboost')
library('gbm')
library("glmnet")
library("randomForestSRC")
library("ggRandomForests") 
library(purrr)
library(cluster)

file <- read.csv("C:/Users/Jangampradip/Downloads/train_MpHjUjU.csv")


file$MMM.YY <- as_date(file$MMM.YY) + months(1) - days(1)
file$Dateofjoining <- as_date(file$Dateofjoining)
file$LastWorkingDate <- as_date(file$LastWorkingDate)

file$REP_TENURE <- time_length(as_date(ifelse(is.na(file$LastWorkingDate),file$MMM.YY,file$LastWorkingDate )) - file$Dateofjoining,unit = "days")


data <- as.data.frame(file %>% group_by(Emp_ID) %>% filter(MMM.YY == max(MMM.YY)))
            


################# business value last 6 months

past_b_value <- file %>%
  group_by(Emp_ID) %>%
  mutate(my_ranks = order(MMM.YY, decreasing=TRUE)) %>%
  filter(my_ranks %in% c(1,2,3,4,5,6,7,8,9,10,11,12))

past_b_value <- past_b_value[,names(past_b_value) %in% c("Emp_ID","my_ranks","Total.Business.Value")]

past_b_value <- dcast(past_b_value,Emp_ID~my_ranks,value.var = "Total.Business.Value", fill = NA )

colnames(past_b_value)[2:13] <- paste("business_value_month",colnames(past_b_value)[2:13],  sep = "_")

past_b_value$avg_bvalue_last_3_month <- rowMeans(past_b_value[2:4], na.rm=TRUE)
past_b_value$max_bvalue_last_3_month <-  do.call(pmax, c(past_b_value[2:4], list(na.rm=TRUE)))
past_b_value$min_bvalue_last_3_month <-  do.call(pmin, c(past_b_value[2:4], list(na.rm=TRUE)))

past_b_value$std_bvalue_last_3_month <- apply(past_b_value[2:4],1, sd, na.rm = TRUE)



past_b_value$avg_bvalue_last_6_month <- rowMeans(past_b_value[2:7], na.rm=TRUE)
past_b_value$max_bvalue_last_6_month <-  do.call(pmax, c(past_b_value[2:7], list(na.rm=TRUE)))
past_b_value$min_bvalue_last_6_month <-  do.call(pmin, c(past_b_value[2:7], list(na.rm=TRUE)))

past_b_value$std_bvalue_last_6_month <- apply(past_b_value[2:7],1, sd, na.rm = TRUE)



past_b_value$bvalue_mom <- past_b_value$business_value_month_1 - past_b_value$business_value_month_2

past_b_value$bvalue_qoq <- rowMeans(past_b_value[2:4], na.rm=TRUE) - rowMeans(past_b_value[5:7], na.rm=TRUE)

past_b_value[,2:13] <- NULL

################# historical rating

file$quarter <- quarter(file$MMM.YY, fiscal_start = 1, with_year = TRUE)

past_rating <- file %>%
  group_by(Emp_ID,quarter) %>%
  summarise(Quarterly.Rating = max(Quarterly.Rating)) %>%
  mutate(my_ranks = order(quarter, decreasing=TRUE)) %>%
  filter(my_ranks %in% c(1,2,3,4))

past_rating <- past_rating[,names(past_rating) %in% c("Emp_ID","my_ranks","Quarterly.Rating")]

past_rating <- dcast(past_rating,Emp_ID~my_ranks,value.var = "Quarterly.Rating", fill = NA )

colnames(past_rating)[2:5] <- paste("quarterly_rating",colnames(past_rating)[2:5],  sep = "_")

past_rating$avg_rating_last_2_quarter <- rowMeans(past_rating[2:3], na.rm=TRUE)

past_rating$avg_rating_last_4_quarter <- rowMeans(past_rating[2:5], na.rm=TRUE)
past_rating$max_rating_last_4_quarter <-  do.call(pmax, c(past_rating[2:5], list(na.rm=TRUE)))
past_rating$min_rating_last_4_quarter <-  do.call(pmin, c(past_rating[2:5], list(na.rm=TRUE)))


past_rating$rating_qoq <- past_rating$quarterly_rating_1 - past_rating$quarterly_rating_2


past_rating[,2:5] <- NULL


################### last quarter avg business value vs rating

rating_last_quarter <- file %>%
  group_by(Emp_ID,quarter) %>%
  summarise(rating_last_quarter = max(Quarterly.Rating),
            avg_bvalue_last_quarter = mean(Total.Business.Value,na.rm = TRUE)) %>%
  mutate(my_ranks = order(quarter, decreasing=TRUE)) %>%
  filter(my_ranks %in% c(2))

rating_last_quarter$Last_quarter_value_vs_rating <- rating_last_quarter$avg_bvalue_last_quarter/rating_last_quarter$rating_last_quarter

rating_last_quarter <- rating_last_quarter[,!names(rating_last_quarter) %in% c("quarter","my_ranks")]


################################ avg business value by city, month

avg_bvalue_citywise <- as.data.frame( file %>% group_by(City, MMM.YY) %>%
  summarise(avg_bvalue_by_city_month= mean(Total.Business.Value, na.rm=TRUE)) )


file <- merge(file,avg_bvalue_citywise, by=c("City", "MMM.YY"), all.x = TRUE)

file$avg_bvalue_by_city_month <- file$Total.Business.Value/file$avg_bvalue_by_city_month


avg_bvalue_citywise <- file %>%
  group_by(Emp_ID) %>%
  mutate(my_ranks = order(MMM.YY, decreasing=TRUE)) %>%
  filter(my_ranks %in% c(1,2,3,4,5,6))

avg_bvalue_citywise <- avg_bvalue_citywise[,names(avg_bvalue_citywise) %in% c("Emp_ID","my_ranks","avg_bvalue_by_city_month")]

avg_bvalue_citywise <- dcast(avg_bvalue_citywise,Emp_ID~my_ranks,value.var = "avg_bvalue_by_city_month", fill = NA )

colnames(avg_bvalue_citywise)[2:7] <- paste("bvalue_vs_city_avg",colnames(avg_bvalue_citywise)[2:7],  sep = "_")

avg_bvalue_citywise$bvalue_vs_city_last_6_month <- rowMeans(avg_bvalue_citywise[,2:7],na.rm = TRUE)

avg_bvalue_citywise$bvalue_vs_city_last_3_month <- rowMeans(avg_bvalue_citywise[,2:4],na.rm = TRUE)

avg_bvalue_citywise[,2:7] <- NULL

################# join historical data

data <- merge(data, past_b_value, by="Emp_ID", all.x = TRUE)

data <- merge(data, past_rating, by="Emp_ID", all.x = TRUE)

data <- merge(data, rating_last_quarter, by="Emp_ID", all.x = TRUE)

data <- merge(data, avg_bvalue_citywise, by="Emp_ID", all.x = TRUE)

################# salary above or below avg

data <- data %>% group_by(Designation) %>% mutate(avg_salary_same_rank = mean(Salary,na.rm=TRUE)) %>% ungroup()

data$tenure_bin <- ifelse(data$REP_TENURE<=180,"0-6 months",
                          ifelse(data$REP_TENURE<=360,"6-12 months",
                                 ifelse(data$REP_TENURE<=720,"12-24 months",
                                        ifelse(data$REP_TENURE<=1080,"24-36 months","36+ months"))))

data$age_bin <- ifelse(data$Age<=25,"<25 years",
                          ifelse(data$Age<=30,"25-30 years",
                                 ifelse(data$Age<=35,"30-35 years",
                                        ifelse(data$Age<=40,"35-40 years",
                                               ifelse(data$Age<= 45, "40-45 years","45+ years")))))


data <- data %>% group_by(tenure_bin) %>% mutate(avg_salary_same_tenure = mean(Salary,na.rm=TRUE)) %>% ungroup()

data <- data %>% group_by(age_bin) %>% mutate(avg_salary_same_age = mean(Salary,na.rm=TRUE),
                                              sd_salary_same_age = sd(Salary,na.rm=TRUE)
                                              ) %>% ungroup()

data <- data %>% group_by(Education_Level) %>% mutate(avg_salary_same_edu = mean(Salary,na.rm=TRUE)) %>% ungroup()

data <- data %>% group_by(City) %>% mutate(avg_salary_same_city = mean(Salary,na.rm=TRUE)) %>% ungroup()


data$avg_salary_same_rank <- data$Salary - data$avg_salary_same_rank
data$avg_salary_same_tenure <- data$Salary - data$avg_salary_same_tenure
data$avg_salary_same_age <- data$Salary - data$avg_salary_same_age
data$avg_salary_same_edu <- data$Salary - data$avg_salary_same_edu
data$avg_salary_same_city <- data$Salary - data$avg_salary_same_city

################



data$EMPLOYEE_STATUS <- ifelse(is.na(data$LastWorkingDate),0,1)

data$promotion <- data$Designation-data$Joining.Designation

#data$promotion_cycle <- data$REP_TENURE/data$promotion

data$tenure_bin <- NULL
data$age_bin <- NULL
data$avg_bvalue_last_12_month <- NULL 

write.csv(data,"C:/Users/Jangampradip/Downloads/awwwdata.csv", row.names = FALSE)

####################

## to numeric OHE

idvar <- c("MMM.YY","Emp_ID","Dateofjoining","LastWorkingDate")
ids <- data[idvar]


dummies <- dummyVars(~., data[,!names(data) %in% idvar])
data <- as.data.frame(predict(dummies, data[,!names(data) %in% idvar]))

########################## clustering

clusterdata <- data
clusterdata$EMPLOYEE_STATUS <- NULL
clusterdata[is.na(clusterdata)] <- 0

# final model
kmeans1 <-kmeans(clusterdata, 4)
#kmeans1$cluster
#View(kmeans1$centers)
#kmeans1$size

data$cluster <- paste0("cluster_",as.character(kmeans1$cluster))

dummies <- dummyVars(~., data[,!names(data) %in% idvar])
data <- as.data.frame(predict(dummies, data[,!names(data) %in% idvar]))



########################## survival regression

data_surv <- data




######################## xgboost #############################

# ############################ AFT ################################
# 
# label_lower_bound <- data$REP_TENURE
# 
# label_upper_bound <- ifelse(data$EMPLOYEE_STATUS==0,+Inf,data$REP_TENURE)
# 
# 
# dtrain <- xgb.DMatrix(as.matrix(data[,!names(data) %in% c("EMPLOYEE_STATUS","REP_TENURE","T")]))
# setinfo(dtrain, 'label_lower_bound', label_lower_bound)
# setinfo(dtrain, 'label_upper_bound', label_upper_bound)
# 
# 
# # for aft model 1 logistic
# param4 <- list(max_depth=6, 
#                eta = 0.01, 
#                objective='survival:aft',
#                eval_metric='aft-nloglik',
#                aft_loss_distribution='logistic',
#                aft_loss_distribution_scale=1.20,
#                tree_method= 'hist',
#                #gamma = 1, 
#                min_child_weight = 3,
#                subsample = 0.6, 
#                colsample_bytree = 0.6)
# 
# 
# 
# xgb_model <- xgb.train(data = dtrain,  
#                        params = param4,
#                        nrounds = 500, verbose = TRUE)
# 
# imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
# xgb.ggplot.importance(importance_matrix = imp_fearture[1:20], rel_to_first = TRUE)
# 
# pred.xgb.train <- log(predict(xgb_model, dtrain))
# 
# Hmisc::rcorr.cens(pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))
# 
# data_surv$model_aft_logistic <- pred.xgb.train
# 
# ###############################################
# 
# # aft model 2 normal dist
# param5 <- list(max_depth=6, 
#                eta = 0.01, 
#                objective='survival:aft',
#                eval_metric='aft-nloglik',
#                aft_loss_distribution='normal',
#                aft_loss_distribution_scale=1.20,
#                tree_method= 'hist',
#                #gamma = 1, 
#                min_child_weight = 3,
#                subsample = 0.6, 
#                colsample_bytree = 0.6)
# 
# 
# 
# xgb_model <- xgb.train(data = dtrain,  
#                        params = param5,
#                        nrounds = 500, verbose = TRUE)
# 
# imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
# xgb.ggplot.importance(importance_matrix = imp_fearture[1:20], rel_to_first = TRUE)
# 
# pred.xgb.train <- log(predict(xgb_model, dtrain))
# 
# Hmisc::rcorr.cens(pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))
# 
# data_surv$model_aft_normal <- pred.xgb.train




############################# Cox #################################

# new time label with sign indicates censored or not(negative values are considered right censored).

data$T <- data$REP_TENURE
data$T[data$EMPLOYEE_STATUS==0] = - data$T[data$EMPLOYEE_STATUS==0]


dtrain <- xgb.DMatrix(as.matrix(data[,!names(data) %in% c("EMPLOYEE_STATUS","REP_TENURE","T")]), label=data[, "T"])


################# creating multiple models

######################################################## for model1

param1 <- list(max_depth=4, 
              eta = 0.01, 
              objective = "survival:cox", 
              eval_metric = "cox-nloglik",
              gamma = 1, 
              min_child_weight = 7,
              subsample = 0.6, 
              colsample_bytree = 0.6)

model_cv <- xgb.cv(param1, dtrain, nrounds=5000, nfold=5, early_stopping_rounds = 30)

ggplot(model_cv$evaluation_log, aes(iter)) + 
  geom_line(aes(y = train_cox_nloglik_mean, linetype = "dotted"),colour="blue") + 
  geom_line(aes(y = test_cox_nloglik_mean),colour = "red") + theme(legend.position = "none") + labs(x="Iterations", y="Train/Test nloglik") + ggtitle("Learning Curve")


xgb_model <- xgb.train(data = dtrain,  
                       params = param1,
                       nrounds = model_cv$best_iteration, verbose = TRUE)

imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
xgb.ggplot.importance(importance_matrix = imp_fearture[1:20], rel_to_first = TRUE)

pred.xgb.train <- log(predict(xgb_model, dtrain))

Hmisc::rcorr.cens(-pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))

time.interest <- seq(1,max(data$REP_TENURE))

# Estimate the cumulative baseline hazard function using training data
basehaz.cum <- basehaz.gbm(data$REP_TENURE, data$EMPLOYEE_STATUS, pred.xgb.train, t.eval = time.interest, cumulative = TRUE)



data$Prob <- 999

for (i in 1:nrow(data)){
  surf.i <- exp(-exp(pred.xgb.train[i])*basehaz.cum)
  
  
  if (data[i,]$REP_TENURE + 180 <= 2582 ){
    find_value = data[i,]$REP_TENURE + 180
  }
  else {
    find_value = 2582
  }
  
  data[i,"Prob"]  <- surf.i[match(find_value,time.interest)]
  
}

surf.i <- exp(-exp(pred.xgb.train[1])*basehaz.cum)
data$Prob[1]  <- surf.i[match(data[i,]$REP_TENURE,time.interest)]  


data_surv$model1 <- data$Prob # for model 1 with param1
data$Prob <- NULL


######################################################### model 2
# for model2
param2 <- list(max_depth=8, 
               eta = 0.01, 
               objective = "survival:cox", 
               eval_metric = "cox-nloglik",
               gamma = 3, 
               min_child_weight = 5,
               subsample = 0.6, 
               colsample_bytree = 0.6)


model_cv <- xgb.cv(param2, dtrain, nrounds=5000, nfold=5, early_stopping_rounds = 30)

ggplot(model_cv$evaluation_log, aes(iter)) + 
  geom_line(aes(y = train_cox_nloglik_mean, linetype = "dotted"),colour="blue") + 
  geom_line(aes(y = test_cox_nloglik_mean),colour = "red") + theme(legend.position = "none") + labs(x="Iterations", y="Train/Test nloglik") + ggtitle("Learning Curve")


xgb_model <- xgb.train(data = dtrain,  
                       params = param2,
                       nrounds = model_cv$best_iteration, verbose = TRUE)

imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
xgb.ggplot.importance(importance_matrix = imp_fearture[1:20], rel_to_first = TRUE)

pred.xgb.train <- log(predict(xgb_model, dtrain))

Hmisc::rcorr.cens(-pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))

time.interest <- seq(1,max(data$REP_TENURE))

# Estimate the cumulative baseline hazard function using training data
basehaz.cum <- basehaz.gbm(data$REP_TENURE, data$EMPLOYEE_STATUS, pred.xgb.train, t.eval = time.interest, cumulative = TRUE)



data$Prob <- 999

for (i in 1:nrow(data)){
  surf.i <- exp(-exp(pred.xgb.train[i])*basehaz.cum)
  
  
  if (data[i,]$REP_TENURE + 180 <= 2582 ){
    find_value = data[i,]$REP_TENURE + 180
  }
  else {
    find_value = 2582
  }
  
  data[i,"Prob"]  <- surf.i[match(find_value,time.interest)]
  
}

surf.i <- exp(-exp(pred.xgb.train[1])*basehaz.cum)
data$Prob[1]  <- surf.i[match(data[i,]$REP_TENURE,time.interest)]  


data_surv$model2 <- data$Prob # for model 1 with param1
data$Prob <- NULL


################################################################ model 3

# for model 3
param3 <- list(max_depth=6, 
               eta = 0.01, 
               objective = "survival:cox", 
               eval_metric = "cox-nloglik",
               gamma = 2, 
               min_child_weight = 5,
               subsample = 0.6, 
               colsample_bytree = 0.6)



model_cv <- xgb.cv(param3, dtrain, nrounds=5000, nfold=5, early_stopping_rounds = 30)

ggplot(model_cv$evaluation_log, aes(iter)) + 
  geom_line(aes(y = train_cox_nloglik_mean, linetype = "dotted"),colour="blue") + 
  geom_line(aes(y = test_cox_nloglik_mean),colour = "red") + theme(legend.position = "none") + labs(x="Iterations", y="Train/Test nloglik") + ggtitle("Learning Curve")


xgb_model <- xgb.train(data = dtrain,  
                       params = param3,
                       nrounds = model_cv$best_iteration, verbose = TRUE)

imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
xgb.ggplot.importance(importance_matrix = imp_fearture[1:30], rel_to_first = TRUE)

pred.xgb.train <- log(predict(xgb_model, dtrain))

Hmisc::rcorr.cens(-pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))

time.interest <- seq(1,max(data$REP_TENURE))

# Estimate the cumulative baseline hazard function using training data
basehaz.cum <- basehaz.gbm(data$REP_TENURE, data$EMPLOYEE_STATUS, pred.xgb.train, t.eval = time.interest, cumulative = TRUE)



data$Prob <- 999

for (i in 1:nrow(data)){
  surf.i <- exp(-exp(pred.xgb.train[i])*basehaz.cum)
  
  
  if (data[i,]$REP_TENURE + 180 <= 2582 ){
    find_value = data[i,]$REP_TENURE + 180
  }
  else {
    find_value = 2582
  }
  
  data[i,"Prob"]  <- surf.i[match(find_value,time.interest)]
  
}

surf.i <- exp(-exp(pred.xgb.train[1])*basehaz.cum)
data$Prob[1]  <- surf.i[match(data[i,]$REP_TENURE,time.interest)]  


data_surv$model3 <- data$Prob # for model 1 with param1
data$Prob <- NULL




############################# Final model #####################################

# stacked model

dtrain <- xgb.DMatrix(as.matrix(data_surv[,!names(data_surv) %in% c("EMPLOYEE_STATUS","REP_TENURE","T")]), label=data[, "T"])


param <- list(max_depth=6, 
              eta = 0.01, 
              objective = "survival:cox", 
              eval_metric = "cox-nloglik",
              gamma = 3, 
              min_child_weight = 7,
              subsample = 0.7, 
              colsample_bytree = 0.6)

model_cv <- xgb.cv(param, dtrain, nrounds=5000, nfold=5, early_stopping_rounds = 30)

ggplot(model_cv$evaluation_log, aes(iter)) + 
  geom_line(aes(y = train_cox_nloglik_mean, linetype = "dotted"),colour="blue") + 
  geom_line(aes(y = test_cox_nloglik_mean),colour = "red") + theme(legend.position = "none") + labs(x="Iterations", y="Train/Test nloglik") + ggtitle("Learning Curve")


xgb_model <- xgb.train(data = dtrain,  
                       params = param,
                       nrounds = model_cv$best_iteration, verbose = TRUE)

imp_fearture <- xgb.importance(colnames(dtrain), model = xgb_model)
xgb.ggplot.importance(importance_matrix = imp_fearture[1:20], rel_to_first = TRUE)

pred.xgb.train <- log(predict(xgb_model, dtrain))

Hmisc::rcorr.cens(-pred.xgb.train, Surv(data$REP_TENURE, data$EMPLOYEE_STATUS))

time.interest <- seq(1,max(data$REP_TENURE))

# Estimate the cumulative baseline hazard function using training data
basehaz.cum <- basehaz.gbm(data$REP_TENURE, data$EMPLOYEE_STATUS, pred.xgb.train, t.eval = time.interest, cumulative = TRUE)


data <- cbind(data,ids["Emp_ID"])

data$Prob <- 999

for (i in 1:nrow(data)){
  surf.i <- exp(-exp(pred.xgb.train[i])*basehaz.cum)
  
  
  if (data[i,]$REP_TENURE + 180 <= 2582 ){
    find_value = data[i,]$REP_TENURE + 180
  }
  else {
    find_value = 2582
  }
  
  data[i,"Prob"]  <- surf.i[match(find_value,time.interest)]
  
}

surf.i <- exp(-exp(pred.xgb.train[1])*basehaz.cum)
data$Prob[1]  <- surf.i[match(data[i,]$REP_TENURE,time.interest)]  





#################### prepare outout

predictdata <- read.csv("C:/Users/Jangampradip/Downloads/test_hXY9mYw.csv")

predictdata <- merge(predictdata, data[,names(data) %in% c("Emp_ID","Prob")], by="Emp_ID", all.x = TRUE)

predictdata$Target <- ifelse(predictdata$Prob>=.5,0,1)

predictdata$Prob <- NULL

write.csv(predictdata,"C:/Users/Jangampradip/Downloads/upload_stacked_added.csv", row.names = FALSE)



###################################

# Gridsearch CV with xgbcv for hypertuning

# xgb_grid = expand.grid(
#   nrounds = 5000,
#   eta = c(0.01),
#   max_depth = c(6),
#   min_child_weight= c(1,3,5),
#   gamma = c(0,1,2,3),
#   subsample = c(.7,.6,.8),
#   colsample_bytree = c(0.6,.7,.8)
# )
# 
# 
# record <- xgb_grid
# record$best_iter <- 0
# record$test_logloss <- Inf
# 
# best_param <- list()
# best_logloss <- Inf
# best_iteration <- 0
# 
# 
# for (iter in 1:nrow(xgb_grid)) {
# 
#   print(paste0("Current iteration no. : ", iter))
# 
#   param_list = list(
#     objective = "survival:cox", 
#     eval_metric = "cox-nloglik",
#     eta = xgb_grid[iter,"eta"],
#     gamma = xgb_grid[iter,"gamma"],
#     min_child_weight = xgb_grid[iter,"min_child_weight"],
#     max_depth = xgb_grid[iter,"max_depth"],
#     subsample = xgb_grid[iter,"subsample"],
#     colsample_bytree = xgb_grid[iter,"colsample_bytree"],
#     nthread=8
#   )
# 
#   xgbcv <- xgb.cv(data = dtrain, params = param_list,
#                   nfold = 5, nrounds = 5000,
#                   verbose = T, early_stopping_rounds = 30, maximize = FALSE)
# 
#   best_iteration_index <-  xgbcv$best_iteration
#   min_logloss <-  xgbcv$evaluation_log[best_iteration_index]$test_cox_nloglik_mean
#   
# 
#   record[iter,"best_iter"] = best_iteration_index
#   record[iter,"test_logloss"] = min_logloss
# 
#   if (min_logloss < best_logloss) {
#     best_logloss <- min_logloss
#     best_rmse_index <- best_iteration_index
#     best_param <- param_list
#   }
# }
# 
# 
# write.csv(record, "C:/Users/Jangampradip/Downloads/hackathon_gridsearch_results2.csv", row.names = FALSE)
# 
# best_param

#######################################

# clusterdata <- data
# clusterdata$EMPLOYEE_STATUS <- NULL
# clusterdata[is.na(clusterdata)] <- 0
# 
# # function to compute total within-cluster sum of square 
# wss <- function(k) {
#   kmeans(clusterdata, k, nstart = 10 )$tot.withinss
# }
# 
# 
# 
# # extract wss for 2-10 clusters
# wss_values <- sapply(2:10, wss)
# 
# plot(2:10, wss_values,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# 
# 
# 
# # function to compute average silhouette for k clusters
# avg_sil <- function(k) {
#   km.res <- kmeans(clusterdata, centers = k, nstart = 25)
#   ss <- silhouette(km.res$cluster, dist(clusterdata))
#   mean(ss[, 3])
# }
# 
# # Compute and plot wss for k = 2 to k = 10
# k.values <- 2:10
# 
# 
# # extract avg silhouette for 2-10 clusters
# avg_sil_values <- map_dbl(k.values, avg_sil)
# 
# plot(k.values, avg_sil_values,
#      type = "b", pch = 19, frame = FALSE, 
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")
# 
# ####################
# # final model
# kmeans1 <-kmeans(clusterdata, 4)
# #kmeans1$cluster
# View(kmeans1$centers)
# kmeans1$size
# 
# 
# data$cluster <- paste0("cluster_",as.character(kmeans1$cluster))



