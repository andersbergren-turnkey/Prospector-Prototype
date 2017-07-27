library(tidyverse)
library(lubridate)
library(modelr)
library(broom)
library(magrittr)
library(purrr)
library(mlr)
library(parallel)
library(parallelMap)
library(xgboost)

options(scipen=999)

# Load data
ORIGINAL <- read_csv("prepped.csv")

# Rename and remove variables
names(ORIGINAL) <- gsub(" ", "_", names(ORIGINAL))
names(ORIGINAL) <- make.names(names(ORIGINAL), unique = TRUE)

ORIGINAL %<>%
  filter(Is_Valid_Abilitec_ID == 1) %>%
  filter(Abilitec_Age <= 365 & Abilitec_Age >= 0) %>%
  filter(Turnkey_Standard_Bundle_Age <= 365 & Turnkey_Standard_Bundle_Age >= 0) %>%
  filter(Spend_1 > 0 | Spend_2 > 0) %>%
  mutate(Spend_2yravg = (Spend_1 + Spend_2)/2) %>%
  select(-X1,
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
         -Retail_Activity_Date_of_Last
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

ORIGINAL.NA_percent <- ORIGINAL %>%
  summarise_all(funs(mean(is.na(.))))

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

# Remove NA variables below threshold

NA_Remove_Threshold <- .5

ORIGINAL %<>%
  select_if(funs(mean(is.na(.)) < NA_Remove_Threshold))

# Create log spend variables

ORIGINAL %<>%
  mutate(Spend_1.log = log(Spend_1)) %>%
  mutate(Spend_2yravg.log = log(Spend_2yravg))

# Set preprocessing steps in a learner

lrnr.wrpr <- function (lrnr) {
  lrnr %>%
    makePreprocWrapperCaret(ppc.medianImpute = TRUE)
}

# Set a low intensity learner
xgb.lrn <- lrnr.wrpr(makeLearner("regr.xgboost",
                       nthread = detectCores(),
                       nrounds = 200,
                       id = "xgboost"))

# Set learner tuning

xgb.tuned.lrn = setHyperPars(xgb.lrn, par.vals = xgb.tune$x)

# Set a low intensity validation method
CV.setting = makeResampleDesc("CV", iters = 5)

# Set preprocessing steps in a task wrapper

task.wrpr <- function(task) {
  task %>%
    removeConstantFeatures(perc = .05) %>%
    mergeSmallFactorLevels(min.perc = .05) %>%
    createDummyFeatures() %>%
    normalizeFeatures()
}

# Set task

O.log.task = task.wrpr(makeRegrTask(data = ORIGINAL %>%
                            filter(Spend_1 > 0) %>%
                            select(-Spend_1,-Spend_2,-Spend_2yravg,-Spend_2yravg.log),
                          target = "Spend_1.log",
                          id = "Log of Spend, wrapped original"))

O.2yr.log.task = task.wrpr(makeRegrTask(data = ORIGINAL %>%
                            select(-Spend_1,-Spend_2,-Spend_2yravg,-Spend_1.log),
                          target = "Spend_2yravg.log",
                          id = "Log of 2yr Avg Spend, wrapped original"))

# Train models ####################################################################

measures.list = list(mse, rmse, rsq, expvar, timeboth)

parallelStartSocket(cpus = detectCores())

model.list <- benchmark(xgb.tuned.lrn, O.log.task, CV.setting, measures = measures.list)

parallelStop()

# Benchmark models --------------------------------------------------------

benchmarks.current <- getBMRAggrPerformances(model.list, as.df = TRUE) %>%
  mutate(run.date = now())

print(benchmarks.current)

# Save benchmark results --------------------------------------------------

if (file.exists("benchmarks.stored.csv")) {
  benchmarks.stored <- read_csv(file = "benchmarks.stored.csv") %>% 
    bind_rows(benchmarks.current)
} else {
  benchmarks.stored <- benchmarks.current
}

write_csv(benchmarks.stored, path = "benchmarks.stored.csv")
