
setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

one_billion <- read.csv("results_one_billion.csv")

str(one_billion)

one_billion$conts = one_billion$cont1 + one_billion$cont2

one_billion_se <- summarySE(one_billion, measurevar="total", groupvars=c("locs", "size"))
one_billion_se$size = as.factor(one_billion_se$size)
one_billion_se$locs = as.factor(one_billion_se$locs)

head(one_billion, 10)
head(one_billion_se, 10)

ggplot(one_billion_se, aes(x=locs, y=total, group=size)) + 
  geom_errorbar(aes(ymin=total-ci, ymax=total+ci), 
                colour="black", width=0.2, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1), size=0.5, colour="blue") +
  geom_point(position=position_dodge(0.1), size=3)
