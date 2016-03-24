
rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

small_inputs <- read.csv("results_small_inputs.csv")
small_inputs$conts = small_inputs$cont1 + small_inputs$cont2
head(small_inputs)

total <- summarySE(small_inputs, measurevar="total", groupvars=c("locs", "size"))
total$locs <- as.factor(total$locs)
head(total)

total.a <- melt(total, id.vars=c("locs", "size"), value.name="Time", 
                variable.name="Total", measure.vars=c("total"))


## mydf$task <- factor(mydf$task, levels = c("up", "down", "left", "right", "front", "back"))

total.a$labels <- factor(total.a$locs,
                         levels=levels(total.a$locs), 
                         labels=paste("Num cores: ", levels(total.a$locs), sep=""))
 ### Esta ya funciona
ggplot(total.a) + 
  geom_point(aes(x=size, y=Time), size=3) + 
  geom_line(aes(x=size, y=Time, group=1), 
            colour="darkblue", position=position_dodge(0.1), size=1) + 
  facet_wrap(~labels, scales="free", nrow=5, ncol=2) + 
  theme_bw() +
  ggtitle(expression(atop("Strong Scalability", atop("", "")))) +
  xlab("Input size") +
  ylab("Time (seconds)") +
  theme(axis.title=element_text(size=14))




