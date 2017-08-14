# set eta to .1 and tune nrounds at 100, 125, 150, 175, 200, 225, 250, 175, 300
# use irace tuning on other 5 params
# decrease eta to .05 and increase nrounds by x10

ctrl.rand <- makeTuneControlRandom(maxit = 15)
ctrl.irace <- makeTuneControlIrace(maxExperiments = 500L)
ctrl.grid <- makeTuneControlGrid(resolution = 3L)

# Tuning stage 1

params1 <- makeParamSet(makeDiscreteParam("nrounds", values = c(50, 75, 100, 125, 150)),
                        makeDiscreteParam("eta", values = .1))

parallelStartSocket(cpus = detectCores())

xgb.params1 <- tuneParams(learner = xgb.meanimpute.lrn,
                          task = O.log.task,
                          resampling = CV.tune_setting,
                          measures = rmse,
                          par.set = params1,
                          control = ctrl.grid,
                          show.info = TRUE)

parallelStop()

xgb.params1.lrn <- setHyperPars(xgb.meanimpute.lrn, par.vals = xgb.params1$x)

# Tuning stage 2

params2 <- makeParamSet(makeIntegerParam("max_depth",lower = 5L,upper = 10L),
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
                       control = ctrl.irace,
                       show.info = TRUE)

parallelStop()

xgb.param2.lrn <- setHyperPars(xgb.params1.lrn, par.vals = xgb.params2$x)

save_tuning <- TRUE
if (save_tuning) {
  saveRDS(xgb.param2.lrn, file = "xgb.param2.lrn.rds")
}

# Tuning stage 3

