
# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)
library(forcats)
library(mlr)
library(parallelMap)
library(parallel)

# Load data ---------------------------------------------------------------

# ### Load Prospector processed data ###
# PROSPECTOR <- read_csv("prepped_set.csv")

### Load unprocessed data ###
ORIGINAL <- read_csv("prepped.csv")

# Format data -------------------------------------------------------------

### PROSPECTOR data formatting ###
# PROSPECTOR %<>%
#   select(-X1,
#          -`Apparel - Female Apparel MOB's_0`,
#          -`Apparel - Male Apparel MOB's_0`,
#          -`Upscale Card Holder_0`,
#          -`Bank, Financial Services - Banking_0`,
#          -`Beauty MOB's_0`,
#          -`Book MOB's_0`,
#          -`Children's Merchandise MOB's_0`,
#          -`Collectible MOB's_0`,
#          -`Travel and Entertainment Card Holder_0`,
#          -`Electronic MOB's_0`,
#          -`Finance Company, Financial Services - Install Credit_0`,
#          -`Food MOB's_0`,
#          -`Bank Card Holder_0`,
#          -`Gender - Input Individual_0`,
#          -`General Gifts and Merchandise MOB's_0`,
#          -`Gift MOB's_0`,
#          -`Health MOB's_0`,
#          -`Home Furnishing and Decorating MOB's_0`,
#          -`Marital Status_0`,
#          -`Merchandise - High Ticket Merchandise MOB's_0`,
#          -`Outdoor/Gardening MOB's_0`,
#          -`Credit Card Holder - Unknown Type_0`,
#          -`Presence of Children_0`,
#          -`Sports MOB's_0`,
#          -`Standard Retail, High Volume Low End Department Store Buyers_0`,
#          -`Standard Retail, Main Street Retail_0`,
#          -`Standard Retail, Standard Retail_0`,
#          -`Standard Specialty, Specialty_0`,
#          -`Standard Specialty, Specialty Apparel_0`,
#          -`Gas/Department/Retail Card Holder_0`,
#          -`Truck Owner_0`,
#          -`Premium Card Holder_0`,
#          -`Upscale Retail - High End Retail Buyers, Upscale Retail_0`,
#          -`Upscale Specialty, Travel / Personal Services_0`,
#          -`Occupation - Input Individual - Administration / Managerial_0`,
#          -`Occupation - Input Individual - Clerical / White Collar_0`,
#          -`Occupation - Input Individual - Professional / Technical_0`,
#          -`Vehicle - Dominant Lifestyle Indicator - Mini-Van Classification_0`,
#          -`Vehicle - Dominant Lifestyle Indicator - Truck Classification_0`,
#          -`Vehicle - Dominant Lifestyle Indicator - Regular Classification (Mid-size / Small)_0`)
# 
# names(PROSPECTOR) <- gsub(" ", "_", names(PROSPECTOR))
# names(PROSPECTOR) <- make.names(names(PROSPECTOR), unique = TRUE)
# 
# PROSPECTOR %<>%
#   mutate(Spend_1.log = log10(Spend_1))

### ORIGINAL data formatting ###

names(ORIGINAL) <- gsub(" ", "_", names(ORIGINAL))
names(ORIGINAL) <- make.names(names(ORIGINAL), unique = TRUE)

ORIGINAL %<>%
  filter(Is_Valid_Abilitec_ID == 1) %>%
  filter(Abilitec_Age <= 365 & Abilitec_Age >= 0) %>%
  filter(Turnkey_Standard_Bundle_Age <= 365 & Turnkey_Standard_Bundle_Age >= 0) %>%
  filter(Spend_1 > 0) %>%
  select(-X1,
         -Spend_2,
         -ProspectID,
         -Tenure_1,
         -Tenure_2,
         -Planholder_1,
         -Planholder_2,
         -RecordCount_1,
         -RecordCount_2,
         -Abilitec_ID,
         -Abilitec_ID_Append_Date,
         -Abilitec_Age,
         -Acxiom_Consumer_Append_Date,
         -Turnkey_Standard_Bundle_Age,
         -Turnkey_Standard_Bundle_Date,
         -Adult_Age_Ranges_Present_in_Household,
         -Apartment_Number,
         -Children.s_Age_Ranges_Present_in_Household,
         -First_Name,
         -First_Name_._1st_Individual,
         -First_Name_._2nd_Individual,
         -Household_Abilitec_ID,
         -InfoBase_Positive_Match_Indicator,
         -Mail_Order_Buyer_Categories,
         -Email,
         -Last_Name,
         -Postal_Code,
         -Ticketing_System_Account_ID,
         -Ticket_History_File_RecID,
         -ZipCode,
         -Is_Valid_Abilitec_ID,
         -Middle_Initial_._1st_Individual,
         -Middle_Initial_._2nd_Individual,
         -Name_._Gender_._1st_Individual,
         -Name_._Gender_._2nd_Individual,
         -Vehicle_._Truck.Motorcycle.RV_Owner,
         -Base_Record_Verification_Date,
         -Retail_Activity_Date_of_Last,
         -Credit_Card_Indicator,
         -Precision_Level,
         -Number_of_Sources,
         -Overall_Match_Indicator,
         -Occupation_._Detail_._Input_Individual,
         -Age_00_._02_Female:-Age_16_._17_Unknown_Gender,
         -Females_18.24:-Females_75.,
         -Males_18.24:-Males_75.,
         -Unknown_Gender_18.24:-Unknown_Gender_75.,
         -Business_Owner_._Accountant:-Vehicle_._Dominant_Lifestyle_Indicator_._Regular_Classification_.Mid.size_._Small.,
         -Purchase_0_._3_Months:-Purchase_7_._9_Months,
         -Retail_Purchases_._Categories,
         -Suppression_._Mail_._DMA,
         -Vehicle_._New_Used_Indicator_._1st_Vehicle,
         -Vehicle_._New_Used_Indicator_._2nd_Vehicle,
         -Personicx_Cluster_Code,
         -Personicx_Digital_Cluster_Code)

# Set missing data

ORIGINAL[ORIGINAL == -1] <- NA
ORIGINAL[ORIGINAL == "-1"] <- NA
ORIGINAL[ORIGINAL == -2] <- 0

# Format variables

ORIGINAL %<>%
  mutate(Business_Owner = ifelse(is.na(Business_Owner), -1, Business_Owner))

ORIGINAL %<>%
  separate(Retail_Activity_Date_of_Last, c(4,6), into = c("RA_Year", "RA_Month", "RA_remove")) %>%
  mutate(RA_Year = as.numeric(RA_Year)) %>%
  mutate(RA_Month = as.numeric(RA_Month)) %>%
  mutate(Retail_Activity_Months_Since = (year(now()) - RA_Year)*12 + RA_Month) %>%
  select(-RA_remove,-RA_Year,-RA_Month)

# Format variable types

ORIGINAL %<>%
  mutate(Business_Owner = as.factor(Business_Owner)) %>%
  mutate(PersonicX_Cluster = as.factor(PersonicX_Cluster)) %>%
  mutate(PersonicX_Digital_Cluster = as.factor(PersonicX_Digital_Cluster)) %>%
  mutate(Education_._Input_Individual = as.factor(Education_._Input_Individual)) %>%
  mutate(Education_._1st_Individual = as.factor(Education_._1st_Individual)) %>%
  mutate(Education_._2nd_Individual = as.factor(Education_._2nd_Individual)) %>%
  mutate(Home_Property_Type = as.factor(Home_Property_Type)) %>%
  mutate(Occupation_._Input_Individual = as.factor(Occupation_._Input_Individual)) %>%
  mutate(Occupation_._1st_Individual = as.factor(Occupation_._1st_Individual)) %>%
  mutate(Occupation_._2nd_Individual = as.factor(Occupation_._2nd_Individual)) %>%
  mutate(Retail_Purchases_._Most_Frequent_Category = as.factor(Retail_Purchases_._Most_Frequent_Category)) %>%
  mutate(Vehicle_._Dominant_Lifestyle_Indicator = as.factor(Vehicle_._Dominant_Lifestyle_Indicator)) %>%
  mutate(Personicx_Group_Number = as.factor(Personicx_Group_Number)) %>%
  mutate(Personicx_Digital_Group_Number = as.factor(Personicx_Digital_Group_Number)) %>%
  mutate(Race_Code = as.factor(Race_Code)) %>%
  mutate(County = as.factor(County))

# Remove NA variables below threshold (can this be converted to tuner preprocessing for interation)

NA_Remove_Threshold <- .4 #typically 0.3-0.5

ORIGINAL %<>%
  select_if(funs(mean(is.na(.)) < NA_Remove_Threshold))

# Create log spend variables

ORIGINAL %<>%
  mutate(Spend_1.log = log10(Spend_1))

# Remove outliers (same, can this be a tuner preprocess)

IQR_Outlier_Threshhold <- 3 #typically 1.5-3.5

ORIGINAL %<>%
  filter(!Spend_1.log %in% boxplot.stats(Spend_1.log,coef = IQR_Outlier_Threshhold)$out)

# Build Tasks -------------------------------------------------------------

### Set PROSPECTOR data tasks ###

# P.log.task = makeRegrTask(data = PROSPECTOR %>%
#                               select(-ProspectID,-bin_num,-Spend_1),
#                             target = "Spend_1.log",
#                             id = "Log of Spend, prospector")
# 
# P.task = makeRegrTask(data = PROSPECTOR %>%
#                         select(-ProspectID,-bin_num,-Spend_1.log10),
#                       target = "Spend_1",
#                       id = "Spend, prospector")

### Set ORIGINAL data tasks ###

# Set preprocessing steps in a task wrapper

task.wrpr <- function(task) {
  task %>%
    removeConstantFeatures(perc = .05) %>%
    mergeSmallFactorLevels(min.perc = .05) %>%
    createDummyFeatures() %>%
    normalizeFeatures()
}

NA_Remove_Threshold <- .3 #typically 0.3-0.5

ORIGINAL %<>%
  select_if(funs(mean(is.na(.)) < NA_Remove_Threshold))

# Set task

O.log.task = task.wrpr(makeRegrTask(data = ORIGINAL %>%
                                      select(-Spend_1),
                                    target = "Spend_1.log",
                                    id = "Log of Spend, original"))

O.020.log.task = task.wrpr(makeRegrTask(data = ORIGINAL %>%
                                      select(-Spend_1) %>%
                                      sample_frac(size = .2),
                                    target = "Spend_1.log",
                                    id = "Log of Spend, original"))

# O.task = task.wrpr(makeRegrTask(data = ORIGINAL %>%
#                                       select(-Spend_1.log),
#                                     target = "Spend_1",
#                                     id = "Spend, original"))

# Build Learners ----------------------------------------------------------

### Set NA remove and imputation wrapper ###

# trainRemoveNA <- function(data,target,args){
#   
# }
# 
# predictRemoveNA<- function(data,target,args,control){
#   
# }


lrnr.wrpr <- function (lrnr) {
  lrnr %>%
    # makePreprocWrapper(trainRemoveNA,
    #                    predictRemoveNA,
    #                    par.set = makeParamSet(),
    #                    par.vals = list()) %>%
    makePreprocWrapperCaret(ppc.knnImpute = TRUE) #Add factor mode impute with dummy variable creation eventually
}

### Set learners ###

# xgboost
xgb.lrn <- lrnr.wrpr(makeLearner(
  "regr.xgboost",
  nthread = detectCores(),
#  nrounds = 200,
  id = "xgboost"
))

# ridge regression / lasso
glmnet.lrn <- lrnr.wrpr(makeLearner("regr.glmnet",
                                    id = "ridge / lasso"))

# adaptive splines
aspline.lrn <- lrnr.wrpr(makeLearner("regr.earth",
                                     id = "adaptive splines"))

# generalized linear regression
glm.lrn <- lrnr.wrpr(makeLearner("regr.glm",
                                 id = "glm"))

# regression tree
rpart.lrn <- lrnr.wrpr(makeLearner("regr.rpart",
                                   id = "regression tree"))
# support vector machine
svm.lrn <- lrnr.wrpr(makeLearner("regr.svm",
                                 id = "support vector machine"))

# random forest
rf.lrn <- makeLearner("regr.randomForestSRC",
                      id = "random forest")

# Tune Learners (Do not run unless tuning) -----------------------------------------------------------

### Set cross validation ###
CV.tune_setting = makeResampleDesc("CV", iters = 3)

### xgboost tuning ### (only run overnight)

# Try a seperate nrounds tuning prior to tuning other params

#  Set parameter search space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 10L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("eta",lower = 0.01,upper = 0.3),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                       makeIntegerParam("nrounds",lower = 100L,upper = 400L),
                       makeDiscreteParam("gamma",values = c(0,.1,.2)))

# try a seperate gamma tuning after params are established

ctrl.grid <- makeTuneControlGrid(resolution = 3L)
ctrl.rand <- makeTuneControlRandom(maxit = 5000)

# Run tuning
parallelStartSocket(cpus = detectCores())

xgb.tune <- tuneParams(learner = xgb.lrn,
                       task = O.log.task,
                       resampling = CV.tune_setting,
                       measures = rmse,
                       par.set = params,
                       control = ctrl.rand,
                       show.info = TRUE)

parallelStop()

# Save out tuned params
save_tuning <- TRUE
if (save_tuning) {
  saveRDS(xgb.tune, file = "xgb.tune.rds")
}

# Import tuned params
import_tuning <- FALSE
if (import_tuning) {
  xgb.tune <- readRDS(file = "xgb.tune.rds")
}

# Set tuned learner
xgb.tuned.lrn <- setHyperPars(xgb.lrn, par.vals = xgb.tune$x)

### adaptive spline tuning ###



### ridge regression / lasso tuning ###

# Train models ------------------------------------------------------------

### Set training workflow ###

# Set learners
learners.list <- list(xgb.tuned.lrn)

# Set tasks
tasks.list <- list(O.log.task)

# Set measures
measures.list <- list(mse, rmse, rsq, timeboth)

# Set cross validation
CV.model_setting = makeResampleDesc("CV", iters = 5)

### Run training ###
parallelStartSocket(cpus = detectCores())

model.list <- benchmark(learners.list, tasks.list, CV.model_setting, measures = measures.list)

parallelStop()

# Benchmark models --------------------------------------------------------

# Extract benchmarks from current training
benchmarks.current <- getBMRAggrPerformances(model.list, as.df = TRUE) %>%
  mutate(run.date = now())

print(benchmarks.current)

# Save benchmarks with past trainings
if (file.exists("benchmarks.stored.csv")) {
  benchmarks.stored <- read_csv(file = "benchmarks.stored.csv") %>% 
    bind_rows(benchmarks.current)
} else {
  benchmarks.stored <- benchmarks.current
}

write_csv(benchmarks.stored, path = "benchmarks.stored.csv")

# Bin predictions ---------------------------------------------------------

selected.model <- benchmarks.current %>%
  filter(mse.test.mean == min(mse.test.mean)) %>%
  select(task.id, learner.id) %>%
  mutate_all(as.character)

# Run predictions

parallelStartSocket(cpus = detectCores())

final.lrnr <- learners.list[[match(selected.model[["learner.id"]], map_chr(learners.list,getLearnerId))]]

final.task <- tasks.list[[match(selected.model[["task.id"]], map_chr(tasks.list,getTaskId))]]

model.final <- train(final.lrnr, final.task)

PREDICTION <- bind_cols(ORIGINAL, as.data.frame(predict(model.final, task = final.task)))

parallelStop()

# make predictions on total dataset
# replicate binning from Python and compare to actual

PREDICTION %<>%
  mutate(Spend_1.predicted = 10^response) %>%
  mutate(bin_num.20.40.60.80.predicted = factor(ntile(response, 5))) %>%
  mutate(bin_num.20.40.60.80.actual = factor(ntile(truth, 5))) %>%
  mutate(bin_num.30.60.80.90.predicted = factor(ntile(response, 10))) %>%
  mutate(bin_num.30.60.80.90.predicted = fct_collapse(bin_num.30.60.80.90.predicted,
                                                    "1" = c("1","2","3"),
                                                    "2" = c("4","5","6"),
                                                    "3" = c("7","8"),
                                                    "4" = "9",
                                                    "5" = "10")) %>%
  mutate(bin_num.30.60.80.90.actual = factor(ntile(truth, 10))) %>%
  mutate(bin_num.30.60.80.90.actual = fct_collapse(bin_num.30.60.80.90.actual,
                                                    "1" = c("1","2","3"),
                                                    "2" = c("4","5","6"),
                                                    "3" = c("7","8"),
                                                    "4" = "9",
                                                    "5" = "10")) %>%
  mutate(bin_num.percentile.predicted = factor(ntile(truth, 100))) %>%
  mutate(bin_num.percentile.actual = factor(ntile(truth, 100)))

bin_accuarrcy_table <- xtabs(data = PREDICTION, formula = ~ bin_num.predicted + bin_num.actual)
print(bin_accuarrcy_table)

bin_mse <- mean((as.numeric(PREDICTION[["bin_num.30.60.80.90.actual"]]) - as.numeric(PREDICTION[["bin_num.30.60.80.90.predicted"]]))^2)
print(bin_mse)

bin_rmse <- sqrt(mean((as.numeric(PREDICTION[["bin_num.30.60.80.90.actual"]]) - as.numeric(PREDICTION[["bin_num.30.60.80.90.predicted"]]))^2))
print(bin_rmse)