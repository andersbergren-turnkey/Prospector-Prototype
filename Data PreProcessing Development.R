library(tidyverse)
library(modelr)
library(broom)
library(magrittr)
library(purrr)
library(mlr)

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
         -Credit_Card_Indicator)

# Set missing data

ORIGINAL[ORIGINAL == -1] <- NA
ORIGINAL[ORIGINAL == "-1"] <- NA
ORIGINAL[ORIGINAL == -2] <- 0

# Format variable types

print(names(ORIGINAL))

as.data.frame(count(ORIGINAL,Base_Record_Verification_Date))

# Set a low intensity learner
xgb.lrn <- makeLearner("regr.xgboost",
                       nthread = detectCores(),
                       max_depth = 6,
                       eta = .3,
                       nrounds = 100,
                       id = "xgboost")

# Set a low intensity validation method
CV4.setting = makeResampleDesc("CV", iters = 4)

# Set preprocessing steps in a wrapper
preprocess.wrpr <- function(lrnr) {
  lrnr %>%
    
}