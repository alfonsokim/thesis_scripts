rm(list=ls())

setwd("C:\\Users\\EXADKQ\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion")

library("ggplot2")
library("reshape2")
library("scales")
source("C:\\Users\\EXADKQ\\Documents\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

small_inputs <- read.csv("ss_small_inputs.csv")
head(small_inputs)

small_inputs.s <- summarySE(small_inputs, measurevar="mpi_count", groupvars=c("cores", "size"))
small_inputs.s$cores <- as.factor(small_inputs.s$cores)
small_inputs.s$size <- as.factor(small_inputs.s$size)

inputs.plot <- subset(small_inputs.s, !(size %in% c("1000", "5000", "10000", "50000", "100000", "500000")))

### Esta ya funciona
ggplot(inputs.plot) + 
  geom_line(aes(x=cores, y=mpi_count, colour=size, group=size),
            size=1) + 
  geom_point(aes(x=cores, y=mpi_count), size=2, color="blue") + 
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 8500000000, 1000000), labels=comma) +
  scale_color_brewer(palette="Spectral", name="Input size") +
  ggtitle(expression(atop("MPI Messages", atop("by input size", "")))) +
  xlab("Cores (Locations)") +
  ylab("Number of messages")


ggsave(filename="messages_input_size_2.png", path="Graficas")


one_billion <- read.csv("ss_one_billion.csv")
head(one_billion)
head(one_billion[, c("cores", "mpi_count", "mpi_bytes")], 50)

one_billion.s <- summarySE(one_billion, measurevar="mpi_count", groupvars=c("cores", "size"))
one_billion.s$cores <- as.factor(one_billion.s$cores)
one_billion.s$size <- as.factor(one_billion.s$size)


ggplot(one_billion.s) + 
  geom_line(aes(x=cores, y=mpi_count, group=1),
            size=1) + 
  geom_point(aes(x=cores, y=mpi_count), size=2, color="blue") + 
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 800000000, 5000000), labels=comma) +
  # scale_color_brewer(palette="Spectral", name="Input size") +
  ggtitle(expression(atop("MPI Messages", atop("one billion elements", "")))) +
  xlab("Cores (Locations)") +
  ylab("Number of messages")

ggsave(filename="messages_one_billion_2.png", path="Graficas")





plot.all <- rbind(inputs.plot, one_billion.s)

ggplot(plot.all) + 
  geom_line(aes(x=cores, y=mpi_count, colour=size, group=size),
            size=1) + 
  geom_point(aes(x=cores, y=mpi_count), size=3, color="blue") + 
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 8500000000, 1000000), labels=comma) +
  scale_color_brewer(palette="Spectral", name="Input size") +
  ggtitle(expression(atop("MPI Messages", atop("by input size", "")))) +
  xlab("Locations") +
  ylab("Number of messages")


mpi_16 <- read.csv("mpi_full_table_16.csv")









one_million <- read.csv("ws_one_million.csv")

one_million.s <- summarySE(one_million, measurevar="mpi_count", groupvars=c("cores", "size"))
one_million.s$cores <- as.factor(one_million.s$cores)
one_million.s$size <- as.factor(one_million.s$size)

head(one_million.s[, c("cores", "mpi_count", "size")], 50)

ggplot(one_million.s) + 
  # geom_bar(aes(y=size_n, colour="lightblue"), position="dodge", 
  #          stat="identity", fill="lightblue") +
  # geom_text(aes(y=2, label=labels), vjust=1) +
  geom_line(aes(x=cores, y=mpi_count, group=1),
            size=1) +
  geom_point(aes(x=cores, y=mpi_count), 
             size=3, colour="blue") +
  scale_y_continuous(breaks=seq(0, 110000000, 1000000), labels=comma) +
  scale_color_manual(name="", 
                     values=c("lightblue", "black"), 
                     labels=c("Input size", "Execution time")) +
  theme_bw() + 
  ggtitle(expression(atop("MPI Messages", atop("Weak scalability", "")))) +
  xlab("Locations") +
  ylab("Number of messages")
