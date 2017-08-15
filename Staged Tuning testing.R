CV.tune_setting = makeResampleDesc("CV", iters = 4)

### Tuning stage 1 ###

ctrl.grid1 <- makeTuneControlGrid()

params1 <- makeParamSet(makeDiscreteParam("nrounds", values = c(50, 75, 100, 125, 150)),
                        makeDiscreteParam("eta", values = .1))

parallelStartSocket(cpus = detectCores())

xgb.params1 <- tuneParams(learner = xgb.meanimpute.lrn,
                          task = O.log.task,
                          resampling = CV.tune_setting,
                          measures = rmse,
                          par.set = params1,
                          control = ctrl.grid1,
                          show.info = TRUE)

parallelStop()

xgb.params1.lrn <- setHyperPars(xgb.meanimpute.lrn, par.vals = xgb.params1$x)

### Tuning stage 2 ###

ctrl.irace2 <- makeTuneControlIrace(maxExperiments = 500L)

params2 <- makeParamSet(makeIntegerParam("max_depth",lower = 4L,upper = 10L),
                        makeIntegerParam("min_child_weight",lower = 1L,upper = 20L),
                        makeNumericParam("subsample",lower = 0.5,upper = 1),
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                        makeNumericParam("gamma",lower = 0,upper = 0.4))

parallelStartSocket(cpus = detectCores())

xgb.params2 <- tuneParams(learner = xgb.params1.lrn,
                       task = O.log.task,
                       resampling = CV.tune_setting,
                       measures = rmse,
                       par.set = params2,
                       control = ctrl.irace2,
                       show.info = TRUE)

parallelStop()

xgb.params2.lrn <- setHyperPars(xgb.params1.lrn, par.vals = xgb.params2$x)

save_tuning <- TRUE
if (save_tuning) {
  saveRDS(xgb.params2.lrn, file = "xgb.params2.lrn.rds")
}

### Tuning stage 3 ###

ctrl.rand3 <- makeTuneRa(maxExperiments = 100L)

params3 <- makeParamSet(makeIntegerParam("nrounds",lower = 100L,upper = 1000L),
                        makeNumericParam("eta",lower = 0.01,upper = .1))

parallelStartSocket(cpus = detectCores())

xgb.params3 <- tuneParams(learner = xgb.params2.lrn,
                          task = O.log.task,
                          resampling = CV.tune_setting,
                          measures = rmse,
                          par.set = params3,
                          control = ctrl.irace3,
                          show.info = TRUE)

parallelStop()

xgb.params3.lrn <- setHyperPars(xgb.params2.lrn, par.vals = xgb.params3$x)

### Old method ###

# CV.tune_setting = makeResampleDesc("CV", iters = 4)
# 
# ### xgboost tuning ### (long run time)
# 
# # Try a seperate nrounds tuning prior to tuning other params
# 
# #  Set parameter search space
# params <- makeParamSet(makeIntegerParam("max_depth",lower = 4L,upper = 10L),
#                        makeIntegerParam("min_child_weight",lower = 1L,upper = 20L),
#                        makeNumericParam("subsample",lower = 0.5,upper = 1),
#                        makeNumericParam("eta",lower = 0.01,upper = 0.2),
#                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
#                        makeIntegerParam("nrounds",lower = 100L,upper = 300L),
#                        makeNumericParam("gamma",lower = 0,upper = 0.2))
# 
# # try a seperate gamma tuning after params are established
# 
# # establish an appropriate number of instances and times to run
# ctrl.rand <- makeTuneControlRandom(maxit = 3)
# ctrl.irace <- makeTuneControlIrace(maxExperiments = 200L)
# 
# # Run tuning
# parallelStartSocket(cpus = detectCores())
# 
# xgb.tune <- tuneParams(learner = xgb.meanimpute.lrn,
#                        task = O.log.task,
#                        resampling = CV.tune_setting,
#                        measures = rmse,
#                        par.set = params,
#                        control = ctrl.rand,
#                        show.info = TRUE)
# 
# parallelStop()
# 
# save_tuning <- TRUE
# if (save_tuning) {
#   saveRDS(xgb.tune, file = "xgb.tune.rds")
# }
# 
# import_tuning <- FALSE
# if (import_tuning) {
#   xgb.tune <- readRDS(file = "xgb.tune.rds")
# }
# 
# xgb.tuned.lrn <- setHyperPars(xgb.impute.lrn, par.vals = xgb.tune$x)