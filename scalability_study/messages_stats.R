rm(list=ls())

library("ggplot2")
library("reshape2")
library("scales")

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion")

distances <- (2 ** seq(0, 9, 1))
1:512 %in% distances

messages <- data.frame()
for(i in 4:9) {
  messages_table <- read.csv(paste("mpi_full_table_", as.character(2 ** i), ".csv", sep=""))
  messages_table$message_bytes <- NULL
  messages_table$size_1 <- NULL
  messages_table$size_1.1 <- NULL
  messages_table$group <- messages_table$distance %in% distances
  messages_table.s <- rbind(subset(messages_table, group),
                            colSums(subset(messages_table, !group)))
  messages_table.s$locations <- as.factor(as.character(2 ** i))
  messages_table.s$group <- NULL
  messages_table.s$label <- ifelse(messages_table.s$distance <= ((2 ** i) / 2), 
                                          messages_table.s$distance, 512)
  messages <- rbind(messages, messages_table.s)
}
messages$message_rate.p <- messages$message_rate / 100

str(messages)

ggplot(messages) + 
  geom_bar(aes(x=locations, y=message_rate.p, fill=as.factor(label)),
           position="stack", stat="identity") + 
  theme_bw() +
  scale_y_continuous(labels = percent_format(), breaks=seq(0, 1, 0.1)) + 
  scale_fill_brewer(palette="Paired", name="Distance", 
                    labels=c(as.character(2 ** (0:7)), "Other")) + 
  ggtitle(expression(atop("MPI message distances", atop("by locations", "")))) +
  xlab("Cores (Locations)") +
  ylab("Message rate")

