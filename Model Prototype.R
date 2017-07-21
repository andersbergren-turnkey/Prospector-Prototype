# Install and Load Packages ----------------------------------------------------------

library(tidyverse)
library(modelr)
library(broom)
library(magrittr)
library(purrr)
library(mlr)
library(parallelMap)

# Machine learning algorithm packages
library(randomForest)
library(rpart)
library(MASS)
library(glmnet)
library(xgboost)
library(Cubist)
library(kknn)

select <- dplyr::select


# Load Data ---------------------------------------------------------------

PROSPECTOR <- read_csv("prepped_set.csv")

# Build tasks -------------------------------------------

PROSPECTOR %<>%
  select(-X1,
         -ProspectID,
         -bin_num,
         -`Apparel - Female Apparel MOB's_0`,
         -`Apparel - Male Apparel MOB's_0`,
         -`Upscale Card Holder_0`,
         -`Bank, Financial Services - Banking_0`,
         -`Beauty MOB's_0`,
         -`Book MOB's_0`,
         -`Children's Merchandise MOB's_0`,
         -`Collectible MOB's_0`,
         -`Travel and Entertainment Card Holder_0`,
         -`Electronic MOB's_0`,
         -`Finance Company, Financial Services - Install Credit_0`,
         -`Food MOB's_0`,
         -`Bank Card Holder_0`,
         -`Gender - Input Individual_0`,
         -`General Gifts and Merchandise MOB's_0`,
         -`Gift MOB's_0`,
         -`Health MOB's_0`,
         -`Home Furnishing and Decorating MOB's_0`,
         -`Marital Status_0`,
         -`Merchandise - High Ticket Merchandise MOB's_0`,
         -`Outdoor/Gardening MOB's_0`,
         -`Credit Card Holder - Unknown Type_0`,
         -`Presence of Children_0`,
         -`Sports MOB's_0`,
         -`Standard Retail, High Volume Low End Department Store Buyers_0`,
         -`Standard Retail, Main Street Retail_0`,
         -`Standard Retail, Standard Retail_0`,
         -`Standard Specialty, Specialty_0`,
         -`Standard Specialty, Specialty Apparel_0`,
         -`Gas/Department/Retail Card Holder_0`,
         -`Truck Owner_0`,
         -`Premium Card Holder_0`,
         -`Upscale Retail - High End Retail Buyers, Upscale Retail_0`,
         -`Upscale Specialty, Travel / Personal Services_0`,
         -`Occupation - Input Individual - Administration / Managerial_0`,
         -`Occupation - Input Individual - Clerical / White Collar_0`,
         -`Occupation - Input Individual - Professional / Technical_0`,
         -`Vehicle - Dominant Lifestyle Indicator - Mini-Van Classification_0`,
         -`Vehicle - Dominant Lifestyle Indicator - Truck Classification_0`,
         -`Vehicle - Dominant Lifestyle Indicator - Regular Classification (Mid-size / Small)_0`)

names(PROSPECTOR) <- gsub(" ", "_", names(PROSPECTOR))
names(PROSPECTOR) <- make.names(names(PROSPECTOR), unique = TRUE)

PROSPECTOR.log10 <- PROSPECTOR %>%
  mutate(Spend_1.log10 = log10(Spend_1)) %>%
  select(-Spend_1)


task.list <- list(
#  P.task = makeRegrTask(data = PROSPECTOR, target = "Spend_1"),
#  P.log10.task = makeRegrTask(data = PROSPECTOR.log10, target = "Spend_1.log10"),

  )


P.log10.10sample.task = makeRegrTask(data = sample_frac(PROSPECTOR.log10, .1), target = "Spend_1.log10")
P.log10.20sample.task = makeRegrTask(data = sample_frac(PROSPECTOR.log10, .2), target = "Spend_1.log10")

# Build learners ----------------------------------------------------------

formula.list <- list(
  rpart.lrn = makeLearner("regr.rpart"),
  lm.lrn = makeLearner("regr.lm"),
  glmnet.lrn = makeLearner("regr.glmnet")
  )

measures.list = list(mse, rsq, expvar, timeboth)
  

# Train models ------------------------------------------------------------

parallelStartSocket(4)

CV4.setting = makeResampleDesc("CV", iters = 4)
CV5.setting = makeResampleDesc("CV", iters = 5)
#CV4.instance = makeResampleInstance("CV", P.task, iters = 5)

model.list = benchmark(formula.list, P.log10.10sample.task, CV4.setting, measures = measures.list)

# Benchmark models --------------------------------------------------------

benchmarks.current <- getBMRAggrPerformances(model.list, as.df = TRUE)

benchmarks.stored <- mergeBenchmarkResults(list(benchmarks.stored, benchmarks.current))

# Run predictions ---------------------------------------------------------

parallelStop()

# Bin predictions ---------------------------------------------------------


