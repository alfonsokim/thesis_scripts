
setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

one_billion <- read.csv("results_one_billion.csv")

str(one_billion)

one_billion$conts = one_billion$cont1 + one_billion$cont2

head(one_billion, 10)


?geom_point
scale_colour_manual(name="Execution time", 
                    values=c("black"="black", "blue"="blue"),
                    labels = c('Total','Step 1')) + 

steps <- c("Total", "Step 0", "Step 1", "Step 2", "Step 3", "Step 4",
           "Step 5", "Step 6", "Step 7", "Step 8", "Step 9")
dummy <- seq(1, 11)
labels <- data.frame(steps, x=dummy, y=dummy)

ggplot(labels, aes(x=x, y=y, colour=steps)) + 
  scalability_plot(one_billion, measure_var="total", value_var="locs", group_var="size", colour="black") +
  scalability_plot(one_billion, measure_var="step1", value_var="locs", group_var="size", colour="blue") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Scalability", atop("20 Gigabytes", "")))) +
  xlab("Locations") +
  ylab("Time (seconds)")

?merge

total <- summarySE(one_billion, measurevar="total", groupvars=c("locs", "size"))
step1 <- summarySE(one_billion, measurevar="step1", groupvars=c("locs", "size"))
strong_scaling <- merge(total, step1, by=c("locs", "size", "N"), suffixes=c(".total", ".step1"))

step2 <- summarySE(one_billion, measurevar="step2", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step2, by=c("locs", "size", "N"), suffixes=c(".total", ".step2"))
## suffixes dejo de funcionar...
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step2"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step2"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step2"

step3 <- summarySE(one_billion, measurevar="step3", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step3, by=c("locs", "size", "N"), suffixes=c(".total", ".step3"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step3"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step3"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step3"

step4 <- summarySE(one_billion, measurevar="step4", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step4, by=c("locs", "size", "N"), suffixes=c(".total", ".step4"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step4"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step4"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step4"

step5 <- summarySE(one_billion, measurevar="step5", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step5, by=c("locs", "size", "N"), suffixes=c(".total", ".step5"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step5"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step5"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step5"

step6 <- summarySE(one_billion, measurevar="step6", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step6, by=c("locs", "size", "N"), suffixes=c(".total", ".step6"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step6"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step6"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step6"

step7 <- summarySE(one_billion, measurevar="step7", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step7, by=c("locs", "size", "N"), suffixes=c(".total", ".step7"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step7"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step7"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step7"

step8 <- summarySE(one_billion, measurevar="step8", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step8, by=c("locs", "size", "N"), suffixes=c(".total", ".step8"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step8"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step8"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step8"

step9 <- summarySE(one_billion, measurevar="step9", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step9, by=c("locs", "size", "N"), suffixes=c(".total", ".step9"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step9"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step9"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step9"

step0 <- summarySE(one_billion, measurevar="step0", groupvars=c("locs", "size"))
strong_scaling <- merge(strong_scaling, step0, by=c("locs", "size", "N"), suffixes=c(".total", ".step0"))
colnames(strong_scaling)[names(strong_scaling) == "sd"] <- "sd.step0"
colnames(strong_scaling)[names(strong_scaling) == "se"] <- "se.step0"
colnames(strong_scaling)[names(strong_scaling) == "ci"] <- "ci.step0"


ggplot(strong_scaling, aes(x=as.factor(locs), y=total, group=size)) + 
  geom_errorbar(aes(ymin=total-ci.total, ymax=total+ci.total), 
                colour="black", width=0.2, position=position_dodge(0.1)) +
  geom_line(position=position_dodge(0.1), size=0.5, colour="blue") +
  geom_point(position=position_dodge(0.1), size=3) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Stron Scalability", atop("1 billion elements", "")))) +
  xlab("Cores") +
  ylab("Time (seconds)")

?paste

vars <- c("total", paste0("step", 0:9, sep=""))

strong_scaling.a <- melt(strong_scaling, id.vars=c("locs"), value.name="Time", 
                         variable.name="Step", measure.vars=vars)


ggplot(strong_scaling.a, aes(x=as.factor(locs), y=Time, group=Step, fill=Step)) + 
  geom_bar(aes(x=as.factor(locs), y=Time, group=Step, colour=Step),
           position="stack", stat="identity") + 
  geom_line(aes(y=Time), position=position_dodge(0.1), size=0.5) +
  # geom_point(position=position_dodge(0.1), size=3) +
  theme_bw() +
  # scale_y_continuous(breaks = seq(0, 700, 50)) +
  ggtitle(expression(atop("Stron Scalability", atop("1 billion elements", "")))) +
  xlab("Cores") +
  ylab("Time (seconds)")



