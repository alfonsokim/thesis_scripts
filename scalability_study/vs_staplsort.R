rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\VStaplSort")

library("ggplot2")
library("reshape2")
library("scales")
library("plyr")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

small_inputs <- read.csv("results_small_inputs.csv")
small_inputs <- small_inputs[small_inputs$size >= 1000000, ]
small_inputs$size <- as.factor(small_inputs$size)
small_inputs$locations <- as.factor(small_inputs$locations)

str(small_inputs)
head(small_inputs, 10)

cs_times <- summarySE(small_inputs, measurevar="columnsort", groupvars=c("locations", "size"))
cs_times <- rename(cs_times, c("ci"="columnsort_ci"))
cs_times$sd <- NULL
cs_times$se <- NULL

ss_times <- summarySE(small_inputs, measurevar="stapl_sort", groupvars=c("locations", "size"))
ss_times <- rename(ss_times, c("ci"="staplsort_ci"))
ss_times$sd <- NULL
ss_times$se <- NULL

all.times <- merge(cs_times, ss_times, by=c("locations", "size", "N"))
all.intervals <- data.frame(locations=all.times$locations, size=all.times$size, 
                            columnsort=all.times$columnsort, stapl_sort=all.times$stapl_sort,
                            columnsort_ci=all.times$columnsort_ci, staplsort_ci=all.times$staplsort_ci)
all.times$columnsort_ci <- NULL
all.times$staplsort_ci <- NULL

all.times.m <- melt(all.times, measure.vars=c("columnsort", "stapl_sort"), id.vars=c("locations", "size"))
all.times.m$label <- paste0("size: ", all.times.m$size)

all.intervals.m <- melt(all.intervals, id.vars=c("locations", "size"),
                        measure.vars=c("columnsort", "stapl_sort", "columnsort_ci", "staplsort_ci"))

ggplot(all.times.m) + 
  geom_point(aes(x=locations, y=value, group=variable, color=variable),
             size=3) +
  geom_line(aes(x=locations, y=value, group=variable, color=variable),
            size=1) + 
  facet_wrap(~ label, nrow=2, ncol=2, scales="free_y") + 
  scale_color_manual(name="", 
                     values=c(stapl_sort="blue",columnsort="red"),
                     labels=c("columnsort", "stapl::sort")) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  ggtitle(expression(atop("ColumnSort vs stapl::sort", atop("by input size", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")
  

#### ================================================================================
#### ================================================================================
#### ================================================================================

one_billion <- read.csv("results_one_billion.csv")
str(one_billion)
head(one_billion)

one_billion$locations <- as.factor(one_billion$locations)
one_billion_cs <- summarySE(one_billion, measurevar="columnsort", groupvars=c("locations", "size"))
one_billion_ss <- summarySE(one_billion, measurevar="stapl_sort", groupvars=c("locations", "size"))

melt(one_billion.s, measure.vars=)


ggplot(one_billion) + 
  geom_point(aes(x=locations, y=value, group=variable, color=variable),
             size=3) +
  geom_line(aes(x=locations, y=value, group=variable, color=variable),
            size=1) + 
  facet_wrap(~ label, nrow=2, ncol=2, scales="free_y") + 
  scale_color_manual(name="", 
                     values=c(stapl_sort="blue",columnsort="red"),
                     labels=c("columnsort", "stapl::sort")) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  ggtitle(expression(atop("ColumnSort vs Current Sort", atop("by input size", "")))) +
  xlab("Cores (Locations)") +
  ylab("Time (seconds)")


