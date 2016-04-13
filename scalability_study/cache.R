rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion\\pat_results")

library("ggplot2")
library("reshape2")
library(scales)
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

one_million <- read.csv("ws_one_million.csv")
str(one_million)

?rename

one_million.l1_access <- summarySE(one_million, measurevar="l1_access", 
                                   groupvars=c("cores", "size"))
one_million.l1_access$cores <- as.factor(one_million.l1_access$cores)
one_million.l1_access$size <- as.factor(one_million.l1_access$size)


ggplot(one_million.l1_access) + 
  geom_point(aes(x=cores, y=l1_access), size=3) + 
  geom_line(aes(x=cores, y=l1_access, group=1), 
            colour="darkblue", position=position_dodge(0.1), size=1) + 
  geom_errorbar(aes(x=cores, y=l1_access, ymin=l1_access-ci, ymax=l1_access+ci),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  # facet_wrap(~labels, scales="free", nrow=5, ncol=2) + 
  theme_bw() +
  ggtitle(expression(atop("Cache performance", atop("", "")))) +
  xlab("Input size") +
  ylab("Access")



one_million.l1_hit_ratio <- summarySE(one_million, measurevar="l1_hit_ratio", 
                                      groupvars=c("cores", "size"))
one_million.l1_hit_ratio$cores <- as.factor(one_million.l1_hit_ratio$cores)
one_million.l1_hit_ratio$size <- as.factor(one_million.l1_hit_ratio$size)

ggplot(one_million.l1_hit_ratio) + 
  geom_point(aes(x=cores, y=l1_hit_ratio), size=3) + 
  geom_line(aes(x=cores, y=l1_hit_ratio, group=1), 
            colour="darkblue", position=position_dodge(0.1), size=1) + 
  geom_errorbar(aes(x=cores, y=l1_hit_ratio, ymin=l1_hit_ratio-ci, 
                    ymax=l1_hit_ratio+ci),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  # facet_wrap(~labels, scales="free", nrow=5, ncol=2) + 
  theme_bw() +
  ggtitle(expression(atop("Cache performance", atop("", "")))) +
  xlab("Input size") +
  ylab("Hit Ratio")


one_million.l2_hit_ratio <- summarySE(one_million, measurevar="l2_hit_ratio", 
                                      groupvars=c("cores", "size"))
one_million.l2_hit_ratio$cores <- as.factor(one_million.l2_hit_ratio$cores)
one_million.l2_hit_ratio$size <- as.factor(one_million.l2_hit_ratio$size)


head(one_million.l1_hit_ratio)
head(one_million.l2_hit_ratio)

?merge

one_million.l12_hit_ratio <- merge(one_million.l1_hit_ratio, one_million.l2_hit_ratio,
                                   by=c("cores", "size", "N"), suffixes = c(".l1_hr",".l2_hr"))


ggplot(one_million.l2_hit_ratio) + 
  geom_point(aes(x=cores, y=l2_hit_ratio), size=3) + 
  geom_line(aes(x=cores, y=l2_hit_ratio, group=1), 
            colour="darkblue", position=position_dodge(0.1), size=1) + 
  geom_errorbar(aes(x=cores, y=l2_hit_ratio, ymin=l2_hit_ratio-ci, 
                    ymax=l2_hit_ratio+ci),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  # facet_wrap(~labels, scales="free", nrow=5, ncol=2) + 
  theme_bw() +
  ggtitle(expression(atop("Cache performance", atop("", "")))) +
  xlab("Input size") +
  ylab("Hit Ratio")

str(one_million.l12_hit_ratio)
one_million.l12_hit_ratio.m <- melt(one_million.l12_hit_ratio, id.vars=c("cores", "size", "N"),
                                    measure.vars=c("l1_hit_ratio", "l2_hit_ratio"))

one_million.l12_hit_ratio.err <- melt(one_million.l12_hit_ratio, id.vars=c("cores", "size", "N"),
                                      measure.vars=c("ci.l1_hr", "ci.l2_hr"))

one_million.l12_hit_ratio.err$n[one_million.l12_hit_ratio.err$variable=="ci.l1_hr"] <- rep("l1_hit_ratio", 10)
one_million.l12_hit_ratio.err$n[one_million.l12_hit_ratio.err$variable=="ci.l2_hr"] <- rep("l2_hit_ratio", 10)

one_million.l12_hit_ratio.err$variable <- one_million.l12_hit_ratio.err$n
one_million.l12_hit_ratio.err$n <- NULL

one_million.l12_hit_ratio.m <- merge(one_million.l12_hit_ratio.m, one_million.l12_hit_ratio.err,
                                     by=c("cores", "size", "N", "variable"), suffixes=c("", ".err"))


one_million.l12_hit_ratio.m$value <- one_million.l12_hit_ratio.m$value / 100
one_million.l12_hit_ratio.m$value.err <- one_million.l12_hit_ratio.m$value.err / 100

## save(one_million.l12_hit_ratio.m, file="l1_l2_hit_rate")

ggplot(one_million.l12_hit_ratio.m, group=variable) + 
  geom_point(aes(x=cores, y=value), size=3) + 
  geom_line(aes(x=cores, y=value, group=variable, colour=variable), 
            position=position_dodge(0.1), size=1) + 
  geom_errorbar(aes(x=cores, y=value,
                    ymin=value-value.err, 
                    ymax=value+value.err),
                colour="black", width=0.2, 
                position=position_dodge(0.1)) +
  # geom_vline(xintercept=5, linetype="dashed", size=0.5, color="red") + 
  # geom_vline(xintercept=4, linetype="dashed", size=0.5, color="blue") + 
  # facet_wrap(~labels, scales="free", nrow=5, ncol=2) + 
  scale_y_continuous(breaks=seq(0.96, 1, 0.002), labels=percent) +
  # scale_color_manual(values=c("#CC6666", "#9999CC")) + 
  # scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_discrete("Cache hit rate", 
                      labels=c("Cache L1", "Cache L2")) +
  theme_bw() +
  ggtitle(expression(atop("Cache performance", atop("One million elements per core", "")))) +
  xlab("Cores (Locations)") +
  ylab("Hit Rate")


one_million
ss_one_billion <- read.csv("../ss_one_billion.csv")

l1.s <- summarySE(ss_one_billion, measurevar="l1_hit_ratio", groupvars=c("cores", "size"))
l2.s <- summarySE(ss_one_billion, measurevar="l2_hit_ratio", groupvars=c("cores", "size"))
mpi.s <- summarySE(ss_one_billion, measurevar="mpi_count", groupvars=c("cores", "size"))

?merge

merge(l1.s, l2.s, by=c("cores", "size", "N"), all=TRUE)

ss_one_billion.m <- melt(ss_one_billion, id.vars=c("cores", "size"), 
                         measure.vars=c("l1_hit_ratio", "l2_hit_ratio", "mpi_count"))


ggplot(ss_one_billion.m, aes(x=cores, group=variable)) + 
  geom_point(aes(y=value, group=variable, colour=variable)) + 
  facet_wrap(~ variable, scales="free_y") +
  theme_bw()

