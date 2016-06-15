
rm(list=ls())

setwd("C:\\Users\\EXADKQ\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\WeakScalability")
source("C:\\Users\\EXADKQ\\Documents\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

library("ggplot2")

thousand <- read.csv("results_hundred_thousand.csv")
thousand$conts <- thousand$cont1 + thousand$cont2
head(thousand, 20)


total <- summarySE(thousand, measurevar="total", groupvars=c("locs", "size"))
size.unit <- 8.1396888 / 51200000
total$size_n <- total$size * size.unit
total$locs <- as.factor(total$locs)


ggplot(data=total, aes(x=locs, y=total, group=1)) + 
  geom_bar(aes(y=size_n, colour="lightblue"), position="dodge", 
           stat="identity", fill="lightblue") +
  geom_text(aes(label=size), vjust=-1) +
  geom_line(position=position_dodge(0.1), size=1, colour="darkblue") +
  geom_point(aes(x=locs, y=total), 
             position=position_dodge(0.1), size=3, colour="black") +
  geom_errorbar(aes(x=locs, y=total, ymin=total-ci, ymax=total+ci),
                width=0.2, 
                position=position_dodge(0.1)) +
  scale_y_continuous(breaks=seq(0, 9, 1)) +
  scale_color_manual(name="", 
                     values=c("lightblue", "black"), 
                     labels=c("Input size", "Execution time")) +
  theme_bw() + 
  ggtitle(expression(atop("Weak Scalability", atop("100000 per core", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")


## ======================================================================================

one_million <- read.csv("results_one_million.csv")
one_million$conts <- one_million$cont1 + one_million$cont2
head(one_million, 20)


one_million.total <- summarySE(one_million, measurevar="total", groupvars=c("locs", "size"))
size.unit <- 12.291896 / 512000000
one_million.total$size_n <- one_million.total$size * size.unit
one_million.total$locs <- as.factor(one_million.total$locs)

one_million.time <- melt(one_million.total, id.vars=c("locs", "size"), measure.vars=c("total", "ci"))
## save(one_million.total, file="one_million_time")

labels=c("1M", "2M", "4M", "8M", "16M", "32M", 
         "64M", "128M", "256M", "512M")

ggplot(data=one_million.total, aes(x=locs, y=total, group=1)) + 
  geom_bar(aes(y=size_n, colour="lightblue"), position="dodge", 
           stat="identity", fill="lightblue") +
  geom_text(aes(y=2, label=labels), vjust=1) +
  geom_line(position=position_dodge(0.1), size=1, colour="darkblue") +
  geom_point(aes(x=locs, y=total), 
             position=position_dodge(0.1), size=2, colour="black") +
  geom_errorbar(aes(x=locs, y=total, ymin=total-ci, ymax=total+ci),
                width=0.2, 
                position=position_dodge(0.1)) +
  scale_y_continuous(breaks=seq(0, 15, 1)) +
  scale_color_manual(name="", 
                     values=c("lightblue", "black"), 
                     labels=c("Input size", "Execution time")) +
  theme_bw() + 
  ggtitle(expression(atop("Weak Scalability", atop("One million per core", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")

ggsave(filename="ws_one_million_2.png", path="Graficas") # , width=250, height=100, units='mm'


### Tamanios de entrada y cache
sizes <- data.frame(locs=one_million.total$locs, size=one_million.total$size)
str(sizes)
# sizes$input_size.gb <- sizes$size * 8 * 3 / 1024 / 1024 / 1024
# sizes$input_size.mb <- sizes$size * 8 * 3 / 1024 / 1024
# sizes$input_size.cores <- as.numeric(as.character(sizes$locs)) / sizes$input_size.mb / 2
sizes$cache.l1 <- as.numeric(as.character(sizes$locs)) * 256 / 1024
sizes$cache.l2 <- ceiling(as.numeric(as.character(sizes$locs)) / 2) * 2
sizes$cache.l3 <- ceiling(as.numeric(as.character(sizes$locs)) / 8) * 8

sizes.m <- melt(sizes, id.vars=c("locs", "size"))

ggplot(sizes.m) +
  geom_bar(aes(x=locs, y=value, group=variable, fill=variable), 
           position="dodge", stat="identity") + 
  facet_wrap(~ locs)






