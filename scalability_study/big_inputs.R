
rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
library("reshape2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

small_inputs <- read.csv("big_inputs.csv")
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
total.a$size <- as.factor(total.a$size)


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

total$size <- as.factor(total$size)
head(total)
str(total)

?facet_wrap

### Inversion 
ggplot(total) + 
  geom_line(aes(x=locs, y=total, colour=size, group=size),
            size=1) + 
  geom_point(aes(x=locs, y=total), size=3, color="blue") + 
  geom_errorbar(aes(x=locs, y=total, ymin=total-ci, ymax=total+ci),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  scale_color_brewer(palette="Spectral", name="Input size") +
  ggtitle(expression(atop("Strong Scalability", atop("by input size", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")

as.integer(as.character(total$size))

?as.factor

total$labels <- factor(total$size,
                       levels=levels(total$size), 
                       labels=paste((as.integer(as.character(levels(total$size))) / 1000000), " million", sep=""))

### facet_wrap 
ggplot(total) + 
  geom_line(aes(x=locs, y=total, group=1), size=1) + 
  geom_point(aes(x=locs, y=total), size=3, color="blue") + 
  geom_errorbar(aes(x=locs, y=total, ymin=total-ci, ymax=total+ci),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  scale_color_brewer(palette="Spectral", name="Input size") +
  ggtitle(expression(atop("Strong Scalability", atop("by input size", "")))) +
  facet_wrap(~ labels, nrow=3, ncol=3, scales="free_y") + 
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")



