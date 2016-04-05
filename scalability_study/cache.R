rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion\\pat_results")

library("ggplot2")
library("reshape2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

one_million <- read.csv("ws_one_million.csv")
str(one_million)

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

