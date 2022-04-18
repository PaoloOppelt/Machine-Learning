# libraries ---------------------------------------------------------------
library(ggplot2) 
library(data.table)
library(magrittr) 
library(tidyr) 
library(dplyr) 
library(lubridate)
library(patchwork)
library(tidymodels)
library(glmnet)
library(probably)
library(caret)
library(smotefamily)
library(themis)

# cleaning ----------------------------------------------------------------

# setwd("C:/Users/paolo/OneDrive/Dokumente/30 Studium/02 TUM/05 Specialization in Technology/Business Analytics and Machine Learning/R/BA&ML/data")

transactions <- fread("transactions.csv")
geo <- fread("geo.csv")
submission_random <- fread("submission_random.csv")
customers <- fread("customers.csv")
exchange_rates <- data.table(CURRENCY = c("Chinese Yuan","US Dollar","Pound Sterling","Euro"), Exchange_Rate = c(0.129,0.872,1.141,1))

#Encoding Sales Location & Sales Office in Geo and transaction to turn ugly names like "Zürich East" into "Z?rich East", Besançon Centre-Est
Encoding(geo$SALES_OFFICE) <- "UTF-8"
Encoding(geo$SALES_LOCATION) <- "UTF-8"
Encoding(transactions$SALES_LOCATION) <- "UTF-8"

# set seed to 2022 as given in the task
set.seed(2022)

# Paolo Notes Transactions DT:
# Offer Price median should be used to impute values
# ISIC 1675 NA's 
# customer factor

# Changing the order of the columns so that DV is on the very left
transactions <- transactions[,c(19, 1:18, 20:23)]
# not sure why I can't add these things to the recipe function
# maybe because it is not part of the recipe function
# I think only recipe functions can be in the recipe function
# Question: Where do I put these functions? So that it is reproducible?
transactions$MO_ID <- with(transactions, match(MO_ID, unique(MO_ID)))
transactions$SO_ID <- with(transactions, as.integer(factor(SO_ID, levels = unique(SO_ID))))
# I basically lose the minutes and hours
# however, I don't think they are relevant
transactions$MO_CREATED_DATE <- as.Date(transactions$MO_CREATED_DATE, format =  "%d.%m.%Y %H:%M")
transactions$SO_CREATED_DATE <- as.Date(transactions$SO_CREATED_DATE, format =  "%d.%m.%Y %H:%M")
# transactions$SO_CREATED_DATE <- format(transactions$SO_CREATED_DATE, "%Y-%m-%d %H:%M")
transactions$CUSTOMER <-substring(transactions$CUSTOMER, 3)
transactions$CUSTOMER <-substring(transactions$CUSTOMER,1, nchar(transactions$CUSTOMER)-2)
# replacing yes and no character strings with NA
is.na(transactions$END_CUSTOMER) <- transactions$END_CUSTOMER == "No" 
is.na(transactions$END_CUSTOMER) <- transactions$END_CUSTOMER == "Yes" 

# make customer variable numeric
transactions$CUSTOMER <- as.numeric(transactions$CUSTOMER)
transactions$END_CUSTOMER <- as.numeric(transactions$END_CUSTOMER)
transactions$SERVICE_LIST_PRICE <- as.numeric(transactions$SERVICE_LIST_PRICE)
transactions$SERVICE_COST <- as.numeric(transactions$SERVICE_COST)
transactions$MATERIAL_COST <- as.numeric(transactions$MATERIAL_COST)
transactions$COSTS_PRODUCT_A <- as.numeric(transactions$COSTS_PRODUCT_A)
# some numbers were dates --> these are now NA
transactions$COSTS_PRODUCT_B <- as.numeric(transactions$COSTS_PRODUCT_B)
transactions$COSTS_PRODUCT_C <- as.numeric(transactions$COSTS_PRODUCT_C)
transactions$COSTS_PRODUCT_D <- as.numeric(transactions$COSTS_PRODUCT_D)
transactions$COSTS_PRODUCT_E <- as.numeric(transactions$COSTS_PRODUCT_E)
transactions$ISIC <- as.factor(transactions$ISIC)

transactions$OFFER_STATUS <-tolower(transactions[['OFFER_STATUS']])
transactions$OFFER_STATUS <-as.factor(transactions$OFFER_STATUS)
# levels(transactions[['OFFER_STATUS']])

# Paolo: Replacing win & lose with won and lost
transactions$OFFER_STATUS <-gsub("win", "won", transactions$OFFER_STATUS)
transactions$OFFER_STATUS <-gsub("lose", "lost", transactions$OFFER_STATUS)

# Paolo: Replacing NA Grand Paris with Grand Paris so that merging can be conducted
transactions$SALES_LOCATION <-gsub("NA Grand Paris", "Grand Paris", transactions$SALES_LOCATION)


# CLEANING DATA: GEO
# checking the column types
sapply(geo, class)
# change COUNTRY variable to factor
geo$COUNTRY <- as.factor(geo$COUNTRY)
# delete rows for which SALES_OFFICE, SALES_BRANCH, SALES_LOCATION are all NAs
geo <- subset(geo, !is.na(geo$SALES_OFFICE) | !is.na(geo$SALES_BRANCH) | !is.na(geo$SALES_LOCATION))
# is SALES_OFFICE is NA then "NA" is included in the names of the SALES_LOCATION, delete it
geo$SALES_LOCATION <- ifelse(is.na(geo$SALES_OFFICE),substring(geo$SALES_LOCATION, 4),geo$SALES_LOCATION)

# CLEANING DATA: CUSTOMERS
# change country from Switzerland and France to CH and FR to be consistent with geo data table 
# and change the type to factor
customers$COUNTRY <- gsub("Switzerland", "CH", customers$COUNTRY)
customers$COUNTRY <- gsub("France", "FR", customers$COUNTRY)
customers$COUNTRY <- as.factor(customers$COUNTRY)
# change CURRENCY to factor
customers$CURRENCY <- as.factor(customers$CURRENCY)
# in OWNERSHIP change "No information" to NA
is.na(customers$OWNERSHIP) <- customers$OWNERSHIP == "No information"
# change OWNERSHIP to factor
customers$OWNERSHIP <- as.factor(customers$OWNERSHIP)
# change REV_CURRENT_YEAR from character to numeric
customers$REV_CURRENT_YEAR <- gsub('"','',customers$REV_CURRENT_YEAR)
customers$REV_CURRENT_YEAR <- as.numeric(customers$REV_CURRENT_YEAR)
# Fix CREATION_DATE to be of the Date type
# First change "/" to "." otherwise NAs will appear
customers$CREATION_YEAR <- gsub('/','.',customers$CREATION_YEAR)
# leave only the year since day and month are always the same and therefore irrelevant
customers$CREATION_YEAR <- as.Date(customers$CREATION_YEAR, format =  "%d.%m.%Y")
customers$CREATION_YEAR <- format(customers$CREATION_YEAR, "%Y")
# sort the table by CUSTOMER (customer ID)
customers <- customers[order(customers$COUNTRY, customers$CUSTOMER),]

# Paolo: take exchange rates into considerations
customers<-merge(customers,exchange_rates,by="CURRENCY",all.x=TRUE)
customers$REV_CURRENT_YEAR<-customers$REV_CURRENT_YEAR*customers$Exchange_Rate
customers$REV_CURRENT_YEAR.1<-customers$REV_CURRENT_YEAR.1*customers$Exchange_Rate
customers$REV_CURRENT_YEAR.2<-customers$REV_CURRENT_YEAR.2*customers$Exchange_Rate


# MergingData -------------------------------------------------------------

# Paolo: Adjustments of colnames Country in Geo and Customer because these are different countries
# Adding Customer to each column so that we can easily identify the data of the customer dt in the merged table

colnames(geo)<-gsub("COUNTRY","SALES_COUNTRY", colnames(geo))
colnames(customers)<-gsub("COUNTRY","CUSTOMER_COUNTRY", colnames(customers))
colnames(customers)<-gsub("CREATION_YEAR","CUSTOMER_CREATION_YEAR", colnames(customers))
colnames(customers)<-gsub("OWNERSHIP","CUSTOMER_OWNERSHIP", colnames(customers))
colnames(customers)<-gsub("CURRENCY","CUSTOMER_CURRENCY", colnames(customers))
colnames(customers)<-gsub("REV","CUSTOMER_REV", colnames(customers))

#Left merge to enrich transactions with geo information to be able to create primary key for merging
transactions_geo_merge<-merge(transactions, geo, by = "SALES_LOCATION",  all.x = TRUE)
# ISSUES: Some transactions don't have a SALES LOCATION, Some transactions don't have a customer id, some transactions don't have a country, some transaction have a customer id but the customer dt does not contain this Customer
# Enriched transactions with Primary Key 
transactions_geo_merge[,KEY:=paste(SALES_COUNTRY,CUSTOMER)]
# Enriched Customers with Primary Key 
customers[,KEY:=paste(CUSTOMER_COUNTRY,CUSTOMER)]
# MERGING TRANSACTION with enriched geo info with CUSTOMER DT
transactions_final_merge<-merge(transactions_geo_merge, customers, by = "KEY",  all.x = TRUE)
transactions_final_merge[,KEY:=NULL]


# DATA MANIPULATION: Replacing very low frequent feature properties----
# cluster Sales location with low frequency into "Other
freq_tab <- transactions_final_merge %>% group_by(SALES_LOCATION) %>% summarize(SALES_LOCATION_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("SALES_LOCATION"),all.x = TRUE)
transactions_final_merge[SALES_LOCATION_freq < 50]$SALES_LOCATION <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,SALES_LOCATION_freq:=NULL]

# cluster TECH with low frequency into "Other
freq_tab <- transactions_final_merge %>% group_by(TECH) %>% summarize(TECH_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("TECH"),all.x = TRUE)
transactions_final_merge[TECH_freq < 50]$TECH <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,TECH_freq:=NULL]
# cluster Sales Branch with low frequency into "Other"
freq_tab <- transactions_final_merge %>% group_by(SALES_BRANCH) %>% summarize(SALES_BRANCH_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("SALES_BRANCH"),all.x = TRUE)
transactions_final_merge[SALES_BRANCH_freq < 50]$SALES_BRANCH <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,SALES_BRANCH_freq:=NULL]
# cluster Business Type with low frequency into "Other"
freq_tab <- transactions_final_merge %>% group_by(BUSINESS_TYPE) %>% summarize(BUSINESS_TYPE_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("BUSINESS_TYPE"),all.x = TRUE)
transactions_final_merge[BUSINESS_TYPE_freq < 50]$BUSINESS_TYPE <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,BUSINESS_TYPE_freq:=NULL]
# cluster SALES OFFICE with low frequency into "Other"
freq_tab <- transactions_final_merge %>% group_by(SALES_OFFICE) %>% summarize(SALES_OFFICE_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("SALES_OFFICE"),all.x = TRUE)
transactions_final_merge[SALES_OFFICE_freq < 50]$SALES_OFFICE <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,SALES_OFFICE_freq:=NULL]
# cluster Offer Type with low frequency into "Other"
freq_tab <- transactions_final_merge %>% group_by(OFFER_TYPE) %>% summarize(OFFER_TYPE_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("OFFER_TYPE"),all.x = TRUE)
transactions_final_merge[OFFER_TYPE_freq <50]$OFFER_TYPE <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,OFFER_TYPE_freq:=NULL]

# cluster ISIC with low frequency into "Other"
freq_tab <- transactions_final_merge %>% group_by(ISIC) %>% summarize(ISIC_freq=n())
transactions_final_merge <- merge(x=transactions_final_merge,y=freq_tab,by=c("ISIC"),all.x = TRUE)
transactions_final_merge[ISIC_freq < 50]$ISIC <- 'Other' #set insignificant sales location to anywhere
transactions_final_merge[,ISIC_freq:=NULL]

summary(transactions_final_merge)

# REPLACING 0 & negative Numbers----

# n <- transactions_final_merge[SERVICE_COST<0, "SERVICE_COST"]
# transactions_final_merge[SERVICE_COST<0, "SERVICE_COST"] <- 0
# transactions_final_merge[COSTS_PRODUCT_A<0, "COSTS_PRODUCT_A"] <- 0
# transactions_final_merge[COSTS_PRODUCT_B<0, "COSTS_PRODUCT_B"] <- 0
# transactions_final_merge[COSTS_PRODUCT_C<0, "COSTS_PRODUCT_C"] <- 0
# transactions_final_merge[COSTS_PRODUCT_D<0, "COSTS_PRODUCT_D"] <- 0
# transactions_final_merge[COSTS_PRODUCT_E<0, "COSTS_PRODUCT_E"] <- 0
# transactions_final_merge[CUSTOMER_REV_CURRENT_YEAR<0, "CUSTOMER_REV_CURRENT_YEAR"] <- 0
# transactions_final_merge[CUSTOMER_REV_CURRENT_YEAR.1<0, "CUSTOMER_REV_CURRENT_YEAR.1"] <- 0
# transactions_final_merge[CUSTOMER_REV_CURRENT_YEAR.2<0, "CUSTOMER_REV_CURRENT_YEAR.2"] <- 0
#replacing 0 with NA
#hist(transactions_final_merge$SERVICE_LIST_PRICE, breaks=100)
# transactions_final_merge[SERVICE_LIST_PRICE == 0, "SERVICE_LIST_PRICE"] <- NA
# hist(transactions_final_merge$SERVICE_LIST_PRICE, breaks=100)

# hist(transactions_final_merge$SERVICE_COST, breaks=100)
# transactions_final_merge[SERVICE_COST == 0, "SERVICE_COST"] <- NA
# hist(transactions_final_merge$SERVICE_COST, breaks=100)

# hist(transactions_final_merge$MATERIAL_COST, breaks=100)
# transactions_final_merge[MATERIAL_COST == 0, "MATERIAL_COST"] <- NA
# hist(transactions_final_merge$MATERIAL_COST, breaks=100)

# transactions_final_merge[SERVICE_LIST_PRICE == 0, "CUSTOMER_REV_CURRENT_YEAR"] <- NA
# transactions_final_merge[SERVICE_LIST_PRICE == 0, "CUSTOMER_REV_CURRENT_YEAR.2"] <- NA
# transactions_final_merge %>% visdat::vis_miss()

# Set NAs for ISIC that contain 0 as value
transactions_final_merge[ISIC == "0", "ISIC"] <- NA

# Deleting duplicate or uncessary columns
transactions_final_merge[,Exchange_Rate:=NULL]
transactions_final_merge[,CUSTOMER.x:=NULL]
transactions_final_merge[,SALES_COUNTRY:=NULL]
transactions_final_merge[,CUSTOMER_REV_CURRENT_YEAR.1:=NULL]

#Summarize cost of products A-E 
transactions_final_merge[,COST_PRODUCTS:=COSTS_PRODUCT_A+COSTS_PRODUCT_B+COSTS_PRODUCT_C+COSTS_PRODUCT_D+COSTS_PRODUCT_E]
transactions_final_merge[,COSTS_PRODUCT_A:=NULL]
transactions_final_merge[,COSTS_PRODUCT_B:=NULL]
transactions_final_merge[,COSTS_PRODUCT_C:=NULL]
transactions_final_merge[,COSTS_PRODUCT_D:=NULL]
transactions_final_merge[,COSTS_PRODUCT_E:=NULL]

transactions_final_merge$OFFER_STATUS <-as.factor(transactions_final_merge$OFFER_STATUS)
transactions_final_merge$CUSTOMER_OWNERSHIP <-as.factor(transactions_final_merge$CUSTOMER_OWNERSHIP)
transactions_final_merge$TECH <-as.factor(transactions_final_merge$TECH)
transactions_final_merge$SALES_OFFICE <-as.factor(transactions_final_merge$SALES_OFFICE)
transactions_final_merge$SALES_BRANCH <-as.factor(transactions_final_merge$SALES_BRANCH)
transactions_final_merge$SALES_LOCATION <-as.factor(transactions_final_merge$SALES_LOCATION)
transactions_final_merge$BUSINESS_TYPE <-as.factor(transactions_final_merge$BUSINESS_TYPE)
transactions_final_merge$CUSTOMER_COUNTRY <-as.factor(transactions_final_merge$CUSTOMER_COUNTRY)
transactions_final_merge$CUSTOMER_CURRENCY <-as.factor(transactions_final_merge$CUSTOMER_CURRENCY)
transactions_final_merge$CUSTOMER_CREATION_YEAR <-as.factor(transactions_final_merge$CUSTOMER_CREATION_YEAR)
transactions_final_merge$OFFER_TYPE <-as.factor(transactions_final_merge$OFFER_TYPE)
transactions_final_merge$PRICE_LIST <-as.factor(transactions_final_merge$PRICE_LIST)
transactions_final_merge$ISIC <-as.factor(transactions_final_merge$ISIC)

# Ordering the levels of Offer Status for a ROC curve above diagonal
transactions_final_merge$OFFER_STATUS <- ordered(transactions_final_merge$OFFER_STATUS, levels = c("won", "lost"))

# Visualizing missing values# transactions_final_merge %>% visdat::vis_miss(warn_large_data = FALSE)
# checking correlations of variables without NAs
# transactions_final_merge %>% filter(OFFER_STATUS=="won") %>% select(OFFER_PRICE,SERVICE_LIST_PRICE,MATERIAL_COST,SERVICE_COST,CUSTOMER_REV_CURRENT_YEAR,CUSTOMER_REV_CURRENT_YEAR.1,CUSTOMER_REV_CURRENT_YEAR.2) %>% cor(method = "pearson", use = "complete.obs")

# Submission Training & Test Data----

dt_training_1<-transactions_final_merge[is.na(TEST_SET_ID)==TRUE]
dt_test_1<-transactions_final_merge[is.na(TEST_SET_ID)==FALSE]

# deleting all data relevant for submission
transactions_final_merge<-transactions_final_merge[is.na(TEST_SET_ID)]

 dt_training <- dt_training_1
 dt_training <- dt_training

# same as customer.x, we don't need this column twice
#dt_training <- dt_training[, CUSTOMER.y := NULL]
# rename the customer column
#colnames(customers)<-gsub("CUSTOMER.x","CUSTOMER", colnames(customers))

 dt_test <- dt_test_1
 dt_test <- dt_test


# unordered factor (nominal)
# customer factor
# tech factor
# offer type factor
# business type factor
# sales location

# cleaning_final_table ----------------------------------------------------

# Paolo:
# removing unnecessary data tables
rm(exchange_rates)
rm(geo)
rm(customers)
rm(transactions_geo_merge)
# rm(dt_test_1)
# rm(dt_training_1)

# CHI2
# library(FSelector)
# weights <- chi.squared(OFFER_STATUS ~ ., 
#                       data = dt_training)
# print(weights)
# subset <- cutoff.k(weights, 10)
# print(as.simple.formula(subset, "OFFER_STATUS"))
# print(subset)


# MachineLearning ---------------------------------------------------------


# SPLIT----
# test_split <- initial_split(transactions_final_merge, split = 0.75, strata = OFFER_STATUS)
# dt_training <- test_split %>% training()
# dt_test <- test_split %>% testing()

# DOWN/UPSAMPLING Initiation----

# train_tfm <- dt_training[is.na(TEST_SET_ID)==TRUE]
# won <- which(train_tfm$OFFER_STATUS=="won")
# lost <- which(train_tfm$OFFER_STATUS=="lost")

# DOWNSAMPLING----

  # won_downsample <- sample(won,length(lost))
  # dt_training <- train_tfm[c(won_downsample,lost),]

# UPSAMPLING ----

 #lost_upsample <- sample(lost,length(won),replace=TRUE)
# dt_training <- train_tfm[c(lost_upsample,won)]

# SMOTE ----

# RECIPE ----

  dt_rec <- recipe(
    OFFER_STATUS~ ., data = dt_training) %>% 
    # 1. Define Role
    update_role(MO_ID, new_role = "ID") %>% 
    update_role(SO_ID, new_role = "ID") %>% 
    # turn dates into decimals
    step_mutate_at(where(is.Date), fn = decimal_date) %>% 
    # impute all numeric columns with their median
    # 2. Impute
    step_impute_linear(SERVICE_LIST_PRICE,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS)) %>%
    step_impute_linear(MATERIAL_COST,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS,SERVICE_LIST_PRICE)) %>%
    step_impute_linear(SERVICE_COST,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS,SERVICE_LIST_PRICE,MATERIAL_COST)) %>%
    step_impute_linear(CUSTOMER.y,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS,SERVICE_LIST_PRICE,MATERIAL_COST,SERVICE_COST)) %>%
    step_impute_linear(CUSTOMER_REV_CURRENT_YEAR,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS,SERVICE_LIST_PRICE,MATERIAL_COST,SERVICE_COST,CUSTOMER.y)) %>%
    step_impute_linear(CUSTOMER_REV_CURRENT_YEAR.2,impute_with = imp_vars(OFFER_TYPE,SALES_OFFICE,BUSINESS_TYPE,SALES_BRANCH,TECH,SALES_LOCATION,MO_ID,SO_ID,OFFER_PRICE,PRICE_LIST,COST_PRODUCTS,SERVICE_LIST_PRICE,MATERIAL_COST,SERVICE_COST,CUSTOMER.y,CUSTOMER_REV_CURRENT_YEAR,)) %>%
    step_impute_median(all_numeric(),-has_role("ID"))%>%
    # step_impute_bag(all_predictors(), trees = 10) %>%
    #step_impute_knn(all_nominal(),impute_with = all_predictors(),-has_role("ID"))
    # ignoring novel factors
    # 3. Handle factor levels
    step_novel(all_predictors(), -all_numeric())  %>%
    # impute all other nominal (character + factor) columns with the value "none"
    step_unknown(all_nominal(), new_level = "none") %>% 
    step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
    # remove constant columns
    step_zv(all_predictors()) %>% 
    # 4. Discretize
    # remove variables that have a high correlation with each other
    # as this will lead to multicollinearity
    step_corr(all_numeric(), threshold = 0.99) %>% 
    # normalization --> centering and scaling numeric variables
    # mean = 0 and Sd = 1
    step_normalize(all_numeric()) %>%
    # 5. Dummy variables
    # creating dummary variables for nominal predictors
    step_dummy(all_nominal(), -all_outcomes())
  # 6. Normalization
  # 7. Multivariate transformation
  # step_pca(all_numeric_predictors())

dt_rec

dt_rec %>% summary()


# training the recipe object
# training argument specifies the data on which to train data preprocessing
# steps
dt_rec_prep <-  dt_rec %>% prep(training = dt_training)
dt_rec_prep

# transforming the training data
dt_training_prep <- dt_rec_prep %>% bake(new_data = NULL)
# a tibble with the transformed data is returned

# Transforming new data
dt_test_prep <- dt_rec_prep %>% bake(new_data = dt_test)

# Model RANDOM FOREST----


# Model specification

forest_model <- rand_forest(
  mode = "classification",
  engine = "ranger",
  mtry = 50,
  trees = NULL,
  min_n = NULL
)

forest_fit <- forest_model %>% 
  fit(OFFER_STATUS ~ ., 
      data = dt_training_prep)

class_pred <- predict(forest_fit,
                      new_data = dt_test_prep,
                      type = 'class')

prob_pred <- predict(forest_fit,
                     new_data = dt_test_prep,
                     type = 'prob')

# RESULTS DOWN/UPSAMPLING Alternative ----
# dt_results <- dt_results %>% mutate(.hard_pred = make_two_class_pred(.pred_won, levels(OFFER_STATUS), threshold = 0.77)) #because of our imbalance data
# dt_results %>% conf_mat(truth = OFFER_STATUS, estimate = .hard_pred) %>% summary()

# RESULTS RANDOM FOREST----
dt_results <- dt_test %>%  select(OFFER_STATUS,TEST_SET_ID) %>% bind_cols(class_pred, prob_pred)
#count(dt_results[.pred_class == 'won'])/count(dt_results)
#count(dt_training[OFFER_STATUS == 'won'])/count(dt_training)
dt_results$.pred_class <- droplevels(dt_results$.pred_class, 'none', drop = TRUE)
# dt_results %>% conf_mat(truth = OFFER_STATUS, estimate = .pred_class) %>% summary()
# dt_results %>% roc_curve(truth=OFFER_STATUS, .pred_won) %>% autoplot()
dt_results <- dt_results %>% mutate(.hard_pred = make_two_class_pred(.pred_won, levels(OFFER_STATUS), threshold = 0.77)) #because of our imbalance data
# dt_results %>% conf_mat(truth = OFFER_STATUS, estimate = .hard_pred) %>% summary()
# dt_results %>% roc_curve(truth=OFFER_STATUS, .pred_won) %>% autoplot()



# LOG REGRESSION ----



# RESULTS LOG REG ----


# WORKFLOWS ----
 # trans_metrics <- metric_set(sens,spec)
 # trans_wkfl <- workflow() %>% add_model(forest_model) %>% add_recipe(dt_rec)
# trans_wkfl_fit <- trans_wkfl %>% last_fit(split=test_split)
# trans_wkfl_fit%>% collect_metrics()
# trans_wkfl_preds <- trans_wkfl_fit %>% collect_predictions()
# trans_wkfl_preds %>% trans_metrics(truth=OFFER_STATUS,estimate = .pred_class,.pred_yes)

# CROSS VALIDATION: This might take a while, so just run if you have the feeling you can't improve the model/dataset anymore and want to check the min,max,median,mean,& sd for sens, spec & balance accuracy---- 
# trans_folds <-vfold_cv(dt_training,v=10,strata=OFFER_STATUS)
# trans_rs_fit <- trans_wkfl %>% fit_resamples(resamples= trans_folds, metrics = trans_metrics)
# trans_rs_fit %>% collect_metrics() %>% na.omit()
# rs_metrics <- trans_rs_fit %>% collect_metrics(summarize=FALSE)
# rs_metrics %>% group_by(.metric) %>% summarize(min = min(.estimate),median = median(.estimate),max = max(.estimate),mean = mean(.estimate),sd = sd(.estimate))

# Submission File ----

#Transforming won & lost into 0,1
submission_file <- dt_results %>% select(TEST_SET_ID,.pred_class)
submission_file$.pred_class <-gsub("won", 1, submission_file$.pred_class)
submission_file$.pred_class <-gsub("lost", 0, submission_file$.pred_class)
#adjust columnanems to necessary format
colnames(submission_file)<-gsub("TEST_SET_ID","id", colnames(submission_file))
colnames(submission_file)<-gsub(".pred_class","prediction", colnames(submission_file))
# creating csv without index column
write.csv(submission_file,file="predictions_AC Kingz_2.csv",row.names = FALSE,quote = FALSE)
