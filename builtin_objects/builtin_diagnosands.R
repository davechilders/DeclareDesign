rm(list=ls())

library(DeclareDesign)

# built-in diagnosands ----------------------------------------------------

bias <- declare_diagnosand(diagnostic_statistic_text = "est - estimand", 
                           summary_function = mean, label = "bias")
rmse <- declare_diagnosand(diagnostic_statistic_text = "(est - estimand)^2", 
                           summary_function = function(x) sqrt(mean(x)), label = "RMSE")
power <- declare_diagnosand(diagnostic_statistic_text = "p < .05", 
                            summary_function = mean, label = "power")
coverage <- declare_diagnosand(diagnostic_statistic_text = "estimand <= ci_upper & estimand >= ci_lower", 
                               summary_function = mean, label = "coverage")
mean_estimate <- declare_diagnosand(diagnostic_statistic_text = "est", 
                                  summary_function = mean, label = "mean(estimate)")
sd_estimate <- declare_diagnosand(diagnostic_statistic_text = "est", 
                                  summary_function = sd, label = "sd(estimate)")
type_s_rate <- declare_diagnosand(diagnostic_statistic_text = "sign(est) != sign(estimand)", 
                                  summary_function = mean, label = "type S rate")
mean_estimand <- declare_diagnosand(diagnostic_statistic_text = "mean(estimand)",
                                    summary_function = mean, label = "mean(estimand)")

save(bias, rmse, power, coverage, mean_estimate, sd_estimate, type_s_rate, mean_estimand, file = "data/diagnosands.RData", 
     compress = TRUE)
