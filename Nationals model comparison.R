ggplot(PREDICTION,aes(truth)) +
  geom_histogram() +
  xlim(c(0,5)) +
  ylim(c(0,30000))

ggplot(ORIGINAL,aes(Spend_1)) +
  geom_histogram() +
  scale_x_log10()
  coord_cartesian(xlim = c(0,400))

ggplot(PREDICTION,aes(Spend_1)) +
  geom_histogram() +
  xlim(c(0,1000)) +
  ylim(c(0,20000))

ggplot(PREDICTION,aes(y = Spend_1, x = factor(bin_num.predicted))) +
  geom_boxplot() +
  ylim(c(0,500))

Prospector_test <- read_csv("//turnk-vmapp03/TurnkeyModeler/Cache/c4379d50-57bc-e411-80dd-005056a63f84/2017_Nationals/Capacity/Model_SVC_30-60-80-90_fspvl-_95_ic-False/Reports/test_results.csv")

ggplot(PREDICTION,aes(y = Spend_1, x = factor(bin_num.30.60.80.90.predicted))) +
  stat_summary(fun.y = "mean", geom = "bar") +
  coord_cartesian(ylim = c(0,1500))

ggplot(Prospector_test,aes(y = Spend_1, x = factor(predicted_bin))) +
  stat_summary(fun.y = "mean", geom = "bar") +
  coord_cartesian(ylim = c(0,1500))

prospector_bin_count <- count(Prospector_test,bin_num)

ggplot(PREDICTION,aes(y = Spend_1, x = factor(bin_num.5percentile.predicted))) +
  stat_summary(fun.y = "mean", geom = "bar") +
  scale_x_discrete(breaks = seq(0,100,5)) +
  coord_cartesian(ylim = c(0,500))

ggplot(PREDICTION, aes(Spend_1,Spend_1.predicted)) +
  geom_point(alpha = .01) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

xtabs(data = PREDICTION)