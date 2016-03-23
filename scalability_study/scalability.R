
setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

one_billion <- read.csv("results_one_billion.csv")

str(one_billion)

one_billion$conts = one_billion$cont1 + one_billion$cont2

head(one_billion, 10)


?geom_point
scale_colour_manual(name="Execution time", 
                    values=c("black"="black", "blue"="blue"),
                    labels = c('Total','Step 1')) + 

steps <- c("Total", "Step 0", "Step 1", "Step 2", "Step 3", "Step 4",
           "Step 5", "Step 6", "Step 7", "Step 8", "Step 9")
dummy <- seq(1, 11)
labels <- data.frame(steps, x=dummy, y=dummy)

ggplot(labels, aes(x=x, y=y, colour=steps)) + 
  scalability_plot(one_billion, measure_var="total", value_var="locs", group_var="size", colour="black") +
  scalability_plot(one_billion, measure_var="step1", value_var="locs", group_var="size", colour="blue") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Scalability", atop("20 Gigabytes", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")





