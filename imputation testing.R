O.log.impute.test <- ORIGINAL %>%
  select(-Spend_1) %>%
  makeRegrTask(
    data = .,
    target = "Spend_1.log",
    id = "Log-normal Spend"
  ) %>%
  removeConstantFeatures(perc = 0.01) %>%
  impute(
    classes = list(
      numeric = imputeLearner("regr.rpart"),
      integer = imputeMean(),
      factor = imputeMode()
    )
  )

O.log.task <- O.log.impute.test[[1]] %>%
  createDummyFeatures() %>%
  removeConstantFeatures(perc = 0.01)

str(O.log.task)
test <- getTaskData(O.log.task)
summary(test$Personicx_Group_Number)
