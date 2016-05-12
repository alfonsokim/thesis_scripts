
rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
library("reshape2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

small_inputs <- read.csv("results_small_inputs.csv")
small_inputs$conts = small_inputs$cont1 + small_inputs$cont2
head(small_inputs)

small_inputs <- small_inputs[small_inputs$size != 50000000, ]

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

### save(total, file="ss_small_inputs_data")

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




###### CACHE ######
path = "C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\Comunicacion\\ss_small_inputs.csv"
ss.small <- read.csv(path)
head(ss.small, 10)

ss.small <- ss.small[ss.small$size != 50000000, ]
str(total)

l1.s <- summarySE(ss.small, measurevar="l1_hit_ratio", groupvars=c("cores", "size"))
l2.s <- summarySE(ss.small, measurevar="l2_hit_ratio", groupvars=c("cores", "size"))
mpi.s <- summarySE(ss.small, measurevar="mpi_count", groupvars=c("cores", "size"))

l1.s$cores <- as.factor(l1.s$cores)
l1.s$size <- as.factor(l1.s$size)
l2.s$cores <- as.factor(l2.s$cores)
l2.s$size <- as.factor(l2.s$size)
mpi.s$cores <- as.factor(mpi.s$cores)
mpi.s$size <- as.factor(mpi.s$size)

?facet_wrap

ggplot() + 
  geom_point(aes(x=cores, y=l1_hit_ratio), 
             size=3, color="red", data=l1.s) + 
  geom_point(aes(x=cores, y=l2_hit_ratio),
             size=3, color="blue", data=l2.s) +
  geom_line(aes(x=cores, y=l1_hit_ratio, group=1), 
            size=1, color="red", data=l1.s) + 
  geom_line(aes(x=cores, y=l2_hit_ratio, group=1), 
            size=1, color="blue", data=l2.s) +
  facet_wrap( ~ size, nrow=2, ncol=5, scales="free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


ggplot() + 
  geom_point(aes(x=locs, y=total), 
             size=3, color="darkgreen", data=total) + 
  geom_line(aes(x=locs, y=total, group=1), 
            size=1, color="darkgreen", data=total) + 
  facet_wrap( ~ size, nrow=2, ncol=5, scales="free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


library(gtable)
library(grid)


l1.512 <- l1.s[l1.s$cores == 1, ]
l2.512 <- l2.s[l2.s$cores == 1, ]
mpi.512 <- mpi.s[mpi.s$cores == 1, ]
total.512 <- total[total$locs == 1, ]


cache <- ggplot() + 
  geom_point(aes(x=size, y=l1_hit_ratio, color="red"), 
             size=3, data=l1.512) + 
  geom_point(aes(x=size, y=l2_hit_ratio, color="blue"),
             size=3, data=l2.512) +
  geom_line(aes(x=size, y=l1_hit_ratio, group=1, color="red"), 
            size=1, data=l1.512) + 
  geom_line(aes(x=size, y=l2_hit_ratio, group=1, color="blue"), 
            size=1, data=l2.512) +
  theme_bw() +
  ggtitle(expression(atop("Cache Performance", atop("1 Core", "")))) +
  ylab("Cache hit ratio (%)") + 
  scale_y_continuous(breaks=seq(95, 100, 0.2)) + 
  scale_color_manual(name="", 
                     values=c("red", "blue"), 
                     labels=c("L2 Cache", "L1 Cache")) +
  theme(axis.title.x = element_blank())


time <- ggplot() + 
  geom_point(aes(x=size, y=total, color="darkgreen", group="darkgreen"), 
             size=3, data=total.512) + 
  geom_line(aes(x=size, y=total, group=1), 
            size=1, color="darkgreen", data=total.512) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Time (seconds)") + 
  scale_y_continuous(breaks=seq(0, 180, 20)) + 
  scale_color_manual(name="", 
                     values=c("darkgreen"), 
                     labels=c("Time")) +
  theme_bw()


grid.newpage()
grid.draw(rbind(ggplotGrob(cache), ggplotGrob(time), size = "last"))



### ==============================================================================
### ==============================================================================
### ==============================================================================
### ==============================================================================


l1.512 <- l1.s[l1.s$cores == 512, ]
l2.512 <- l2.s[l2.s$cores == 512, ]
mpi.512 <- mpi.s[mpi.s$cores == 512, ]
total.512 <- total[total$locs == 512, ]

mpi.512$mpi_count.s <- mpi.512$mpi_count / 1000000


cache <- ggplot() + 
  geom_point(aes(x=size, y=l1_hit_ratio, color="red"), 
             size=3, data=l1.512) + 
  geom_point(aes(x=size, y=l2_hit_ratio, color="blue"),
             size=3, data=l2.512) +
  geom_line(aes(x=size, y=l1_hit_ratio, group=1, color="red"), 
            size=1, data=l1.512) + 
  geom_line(aes(x=size, y=l2_hit_ratio, group=1, color="blue"), 
            size=1, data=l2.512) +
  theme_bw() +
  ggtitle(expression(atop("Cache Performance", atop("512 Cores", "")))) +
  ylab("Cache hit ratio (%)") + 
  scale_y_continuous(breaks=seq(95, 100, 0.5)) + 
  scale_color_manual(name="", 
                     values=c("red", "blue"), 
                     labels=c("L2 Cache", "L1 Cache")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(vjust=2))


messages <- ggplot() + 
  geom_point(aes(x=size, y=mpi_count.s, color="darkorange", group="darkorange"), 
             size=3, data=mpi.512) + 
  geom_line(aes(x=size, y=mpi_count.s, group=1), 
            size=1, color="darkorange", data=mpi.512) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("MPI Messages") + 
  scale_y_continuous(breaks=seq(0, 11, 1), labels=paste0(seq(0, 11, 1), " M")) + 
  scale_color_manual(name="", 
                     values=c("darkorange"), 
                     labels=c("Messages")) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(vjust=2))


time <- ggplot() + 
  geom_point(aes(x=size, y=total, color="darkgreen", group="darkgreen"), 
             size=3, data=total.512) + 
  geom_line(aes(x=size, y=total, group=1), 
            size=1, color="darkgreen", data=total.512) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Time (seconds)") + 
  scale_y_continuous(breaks=seq(5, 10, 0.2)) + 
  scale_color_manual(name="", 
                     values=c("darkgreen"), 
                     labels=c("Time")) +
  theme_bw()


grid.newpage()
grid.draw(rbind(ggplotGrob(cache), ggplotGrob(messages), ggplotGrob(time),
                size = "last"))


### ==============================================================================
### ==============================================================================
### ==============================================================================
### ==============================================================================
l1_l2 <- merge(l1.s, l2.s, by=c("cores", "N", "size"))
l1_l2 <- l1_l2[l1_l2$cores %in% c(1, 8, 64, 512), ]
l1_l2.m <- melt(l1_l2, id.vars=c("cores", "N", "size"),
                measure.vars=c("l1_hit_ratio", "l2_hit_ratio"))

library(plyr)
total <- rename(total, c("locs"="cores"))
total.r <- total[total$cores %in% c(1, 8, 64, 512), ]
mpi.r <- mpi.s[mpi.s$cores %in% c(1, 8, 64, 512), ]

l1_l2.m$type <- "Cache hit rate (%)"
l1.s$type <- "Cache hit rate (%)"
l2.s$type <- "Cache hit rate (%)"
total.r$type <- "Time (seconds)"
mpi.r$type <- "MPI Messages"

l1_l2.m$cores_label <- factor(l1_l2.m$cores, levels=c(1,8,64,512),
                              labels=paste0(c(1,8,64,512), " cores"))
total.r$cores_label <- factor(total.r$cores, levels=c(1,8,64,512),
                              labels=paste0(c(1,8,64,512), " cores"))
mpi.r$cores_label <- factor(mpi.r$cores, levels=c(1,8,64,512),
                            labels=paste0(c(1,8,64,512), " cores"))

str(ss.small)

ggplot() + 
  geom_point(aes(x=size, y=total), 
             size=3, color="darkgreen", data=total.r) + 
  geom_point(aes(x=size, y=mpi_count), 
             size=3, color="darkorange", data=mpi.r) + 
  geom_point(aes(x=size, y=value, color=variable, group=variable), 
             size=3, data=l1_l2.m) + 
  geom_line(aes(x=size, y=total, group=1), 
             size=1, color="darkgreen", data=total.r) + 
  geom_line(aes(x=size, y=mpi_count, group=1), 
             size=1, color="darkorange", data=mpi.r) + 
  geom_line(aes(x=size, y=value, color=variable, group=variable), 
            size=1, data=l1_l2.m) + 
  scale_color_manual(name="", 
                     values=c("red", "blue"), 
                     labels=c("L1 Cache", "L2 Cache")) +
  facet_wrap(~cores_label+type, nrow=4, ncol=3, scales="free_y") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_blank()) + 
  xlab("Input size") + 
  ggtitle(expression(atop("Cache and MPI Performance", atop("by input size", ""))))




