# Install and Load Packages ----------------------------------------------------------

library(tidyverse)
library(modelr)
library(broom)
library(magrittr)
library(purrr)
library(mlr)
library(parallelMap)
library(parallel)

# Machine learning algorithm packages
library(randomForest)
library(rpart)
library(glmnet)
library(xgboost)
library(Cubist)
library(kknn)

select <- dplyr::select


# Load Data ---------------------------------------------------------------

PROSPECTOR <- read_csv("prepped_set.csv")

# Prepare data ------------------------------------------------------------

PROSPECTOR %<>%
  select(-X1,
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

PROSPECTOR %<>%
  mutate(Spend_1.log10 = log10(Spend_1))

# Build tasks -------------------------------------------

P.log10.10sample.task = makeRegrTask(data = PROSPECTOR %>% 
                                       select(-ProspectID,-bin_num,-Spend_1) %>%
                                       sample_frac(.1),
                                     target = "Spend_1.log10")
P.log10.20sample.task = makeRegrTask(data = PROSPECTOR %>%
                                       select(-ProspectID,-bin_num,-Spend_1) %>%
                                       sample_frac(.2),
                                     target = "Spend_1.log10")

P.log10.task = makeRegrTask(data = PROSPECTOR %>%
                              select(-ProspectID,-bin_num,-Spend_1),
                            target = "Spend_1.log10")

P.task = makeRegrTask(data = PROSPECTOR %>%
                              select(-ProspectID,-bin_num,-Spend_1.log10),
                            target = "Spend_1")

task.list <- list(P.log10.task)

current.task <- task.list

# Tune learners -----------------------------------------------------------



# Set learners ------------------------------------------------------------

formula.list <- list(
  lm.lrn = makeLearner("regr.lm", id = "linear reg"),
  ridge.lrn = makeLearner("regr.glmnet", alpha = 0, id = "ridge reg"),
  xgb.lrn = makeLearner("regr.xgboost", nthread = detectCores(), max_depth = 6, eta = .3,nrounds = 200, id = "xgboost")
  )

measures.list = list(mse, rmse, rsq, expvar, timeboth)

# Train models ------------------------------------------------------------

CV4.setting = makeResampleDesc("CV", iters = 4)
CV5.setting = makeResampleDesc("CV", iters = 5)
#CV4.instance = makeResampleInstance("CV", P.task, iters = 5)

parallelStartSocket(cpus = detectCores())

model.list = benchmark(formula.list, current.task, CV5.setting, measures = measures.list)

parallelStop()

# Benchmark models --------------------------------------------------------

benchmarks.current <- getBMRAggrPerformances(model.list, as.df = TRUE)

print(benchmarks.current)

if (exists("benchmarks.stored")) {
  benchmarks.stored <- bind_rows(benchmarks.stored, benchmarks.current)
} else {
  benchmarks.stored <- benchmarks.current
}

# Select model ------------------------------------------------------------

# find lowest mse model and extract learner id (change benchmarks to list for learner extraction)
selected.model <- benchmarks.stored %>%
  filter(mse.test.mean == min(mse.test.mean)) %>%
  pull(learner.id) %>%
  as.character()

# Run predictions ---------------------------------------------------------

parallelStartSocket(cpus = detectCores())

model.final = train(formula.list[[match(selected.model, map_chr(formula.list,getLearnerId))]], current.task)

PROSPECTOR.prediction <- bind_cols(PROSPECTOR, as.data.frame(predict(model.final, task = current.task)))

parallelStop()

# make predictions on total dataset

# Bin predictions ---------------------------------------------------------

# replicate binning from Python and compare to actual

PROSPECTOR.prediction %<>%
  mutate(Spend_1.predicted = 10^response) %>%
  mutate(bin_num.predicted = ntile(Spend_1.predicted, 5)) %>%
  mutate(bun_num.correct = bin_num == bin_num.predicted)

summary(PROSPECTOR.prediction$bun_num.correct)

xtabs(data = PROSPECTOR.prediction, formula = ~ bin_num.predicted + bin_num)

