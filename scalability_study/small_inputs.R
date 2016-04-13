
rm(list=ls())

setwd("C:\\Users\\Alfonso\\Dropbox\\MCC\\Tesis\\Resultados\\Escalabilidad\\StrongScalability")

library("ggplot2")
library("reshape2")
source("c:\\Users\\Alfonso\\workspace\\thesis_scripts\\scalability_study\\scalability_plot.R")

small_inputs <- read.csv("results_small_inputs.csv")
small_inputs$conts = small_inputs$cont1 + small_inputs$cont2
head(small_inputs)

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

?merge

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
mpi.512 <- mpi.512[mpi.s$cores == 1, ]
total.512 <- total[total$locs == 1, ]


cache <- ggplot() + 
  geom_point(aes(x=size, y=l1_hit_ratio), 
             size=3, color="red", data=l1.512) + 
  geom_point(aes(x=size, y=l2_hit_ratio),
             size=3, color="blue", data=l2.512) +
  geom_line(aes(x=size, y=l1_hit_ratio, group=1), 
            size=1, color="red", data=l1.512) + 
  geom_line(aes(x=size, y=l2_hit_ratio, group=1), 
            size=1, color="blue", data=l2.512) +
  theme_bw()

time <- ggplot() + 
  geom_point(aes(x=size, y=total), 
             size=3, color="darkgreen", data=total.512) + 
  geom_line(aes(x=size, y=total, group=1), 
            size=1, color="darkgreen", data=total.512) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


#extract gtable
g1<-ggplot_gtable(ggplot_build(cache))
g2<-ggplot_gtable(ggplot_build(time))

#overlap the panel of the 2nd plot on that of the 1st plot

pp<-c(subset(g1$layout, name=="panel", se=t:r))
g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, 
                   pp$l)

ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

grid.newpage()
grid.draw(rbind(ggplotGrob(cache), ggplotGrob(time), size = "last"))

