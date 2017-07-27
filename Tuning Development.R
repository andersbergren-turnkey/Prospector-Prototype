#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 3L,upper = 10L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("eta",lower = 0.05,upper = 0.3),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

ctrl <- makeTuneControlRandom(maxit = 15L)

parallelStartSocket(cpus = detectCores())

mytune <- tuneParams(learner = xgb.lrn,
                     task = O.log.task,
                     resampling = CV.setting,
                     measures = rmse,
                     par.set = params,
                     control = ctrl,
                     show.info = TRUE)

parallelStop()

xgb.tune <- mytune

saveRDS(xgb.tune, file = "xgb.tune.rds")