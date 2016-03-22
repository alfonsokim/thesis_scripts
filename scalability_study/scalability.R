
setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

one_billion <- read.csv("results_one_billion.csv")

str(one_billion)

one_billion$conts = one_billion$cont1 + one_billion$cont2

head(one_billion, 10)

scalability_plot(one_billion, measure_var="total", value_var="locs", group_var="size") +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Strong Scalability (total)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


scalability_plot(one_billion, measure_var="step1", value_var="locs", group_var="size") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 1)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

scalability_plot(one_billion, measure_var="step2", value_var="locs", group_var="size") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 2)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


scalability_plot(one_billion, measure_var="step3", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 3)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

scalability_plot(one_billion, measure_var="step4", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 150, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 4)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

scalability_plot(one_billion, measure_var="step5", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 5)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

scalability_plot(one_billion, measure_var="step6", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 6)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

scalability_plot(one_billion, measure_var="step7", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 7)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


scalability_plot(one_billion, measure_var="step8", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 8)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


scalability_plot(one_billion, measure_var="step0", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  ggtitle(expression(atop("Strong Scalability (step 0)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


scalability_plot(one_billion, measure_var="step9", value_var="locs", group_var="size") + 
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 9)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")