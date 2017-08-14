O.log.impute.test <- ORIGINAL %>%
  select(-Spend_1) %>%
  makeRegrTask(
    data = .,
    target = "Spend_1.log",
    id = "Log-normal Spend"
  ) %>%
  createDummyFeatures() %>%
  removeConstantFeatures(perc = 0.01, na.ignore = TRUE) %>%
  removeConstantFeatures(perc = 0.01, na.ignore = FALSE)

O.log.task <- O.log.impute.test[[1]]

str(O.log.task)
test <- getTaskData(O.log.impute.test)
summary(test$Personicx_Group_Number)
