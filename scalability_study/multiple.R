rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion\\pat_results")

library("ggplot2")
library("reshape2")
library(scales)
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

load("l1_l2_hit_rate")
load("one_million_time")

one_million.reduced <- dataframe(cores=one_million.total$locs, size=one_million.total$size, 
                                 variable=one_million.total$total, one_million.total$ci)


ggplot(one_million.l12_hit_ratio.m, group=variable) + 
  geom_point(aes(x=cores, y=value), size=3) + 
  geom_line(aes(x=cores, y=value, group=variable, colour=variable), 
            position=position_dodge(0.1), size=1) + 
  geom_errorbar(aes(x=cores, y=value,
                    ymin=value-value.err, 
                    ymax=value+value.err),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  scale_y_continuous(breaks=seq(0.96, 1, 0.002), labels=percent) +
  scale_color_discrete("Cache hit rate", 
                       labels=c("Cache L1", "Cache L2")) +
  theme_bw() +
  ggtitle(expression(atop("Cache performance", atop("One million elements per core", "")))) +
  xlab("Cores (Locations)") +
  ylab("Hit Rate") + 
  theme(axis.title.x = element_blank())

one_million.total$locs <- as.factor(one_million.total$locs)

labels=c("1M", "2M", "4M", "8M", "16M", "32M", 
         "64M", "128M", "256M", "512M")

ggplot(data=one_million.total, aes(x=locs, y=total, group=1)) + 
  # geom_bar(aes(y=size_n, colour="lightblue"), position="dodge", 
  #          stat="identity", fill="lightblue") +
  # geom_text(aes(y=2, label=labels), vjust=1) +
  geom_line(position=position_dodge(0.1), size=1, colour="darkblue") +
  geom_point(aes(x=locs, y=total), 
             position=position_dodge(0.1), size=3, colour="black") +
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
  ylab("Time (seconds)") + 
  theme(axis.title.x = element_blank())

library(gridExtra)
g1 <- ggplotGrob(cache)
g2 <- ggplotGrob(time)
colnames(g1) <- paste0(seq_len(ncol(g1)))
colnames(g2) <- paste0(seq_len(ncol(g2)))
grid.draw(join(g1, g2))

