
setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

one_billion <- read.csv("results_one_billion.csv")

str(one_billion)

one_billion$conts = one_billion$cont1 + one_billion$cont2

head(one_billion, 10)

total_plot <- scalability_plot(one_billion, measure_var="total", value_var="locs", group_var="size")

total_plot +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Strong Scalability (total)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")


step1_plot <- scalability_plot(one_billion, measure_var="step1", value_var="locs", group_var="size")

step1_plot +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  ggtitle(expression(atop("Strong Scalability (step 1)", atop("One billion elements", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")
