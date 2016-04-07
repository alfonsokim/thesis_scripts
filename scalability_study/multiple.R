rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion\\pat_results")

library("ggplot2")
library("reshape2")
library(scales)
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\summary.R")

load("l1_l2_hit_rate")
load("ss_small_inputs_data")

total
one_million.l12_hit_ratio.m

substring("5000", 1, 1)

### total[total$size %in% c(1000, 10000, 100000, 1000000, 10000000), ]   WTF !!!!

total.r <- total[substring(as.character(total$size), 1, 1) == "1", ]

total.r$N <- NULL
total.r$sd <- NULL
total.r$se <- NULL

melt(total.r, measure.vars=c("total", "ci"))