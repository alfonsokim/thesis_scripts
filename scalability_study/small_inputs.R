
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

ggplot(data=total.a, aes(x=locs, y=total)) + 
  geom_bar(aes(x=locs, y=Time),
           position="stack", stat="identity") + 
  geom_line(data=total, aes(x=locs, y=total), 
            position=position_dodge(0.1), size=1, color="darkblue") +
  # geom_point(aes(x=locs, y=total), 
  #            position=position_dodge(0.1), size=3) +
  # geom_errorbar(aes(x=locs, y=total, ymin=total-ci, ymax=total+ci),
  #               data=total.a, colour="black", width=0.2, 
  #               position=position_dodge(0.1)) +
  # scale_y_continuous(breaks=seq(0, 700, 50)) +
  facet_wrap(~size, nrow=5, ncol=2) + 
  theme_bw() +
  # scale_fill_brewer(palette="Paired", breaks=vars, labels=labels) + 
  ggtitle(expression(atop("Strong Scalability", atop("1 billion elements", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")


