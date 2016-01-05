#############################################################################
# Make graphs of preliminary data to develop code
# Started Dec 2015
#############################################################################
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=12)) # sized for pub
library(gridExtra)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data.csv", sep=""))
m3 <- dplyr::select(merged, -baseline, -predicted) %>%
  spread(model, change)
head(m3)
tail(m3)

####################################
#### make grid of plots showing response to mean annualtemp and precip 
# p1: 2 degree temp increase, response along MAT axis
d1 <- dplyr::filter(merged, mag==2 & var=="tmean"|var=="tmax")
p1 <- ggplot(data=d1, aes(x=MAT, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  scale_y_continuous(name="Percent Change") +
  scale_color_discrete(name="Model") +
  scale_fill_discrete(name="Model") +
  geom_hline(y=0) +
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
        legend.justification=c(1,1), legend.position=c(1,1),
        panel.background=element_blank(),plot.background=element_blank()) +
  ggtitle(expression("2"*~degree*"C Temperature Increase"))
  p1

# p2: 2 degree temp increase, response along MAP axis
p2 <- ggplot(data=d1, aes(x=MAP, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  xlab("Mean Annual Precipitation (mm)") +
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
        legend.position="none", axis.text.y=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),plot.background=element_blank()) +
  ylab("Percent Change") +
  ggtitle(expression("2"*~degree*"C Temperature Increase"))
p2

# p3: 10% precip increase, response along MAT axis
d2 <- dplyr::filter(merged, mag==1.1 & var=="ppt")
p3 <- ggplot(data=d2, aes(x=MAT, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  theme(legend.position="none") +
  xlab(expression("Mean Annual Temperature ("*~degree*"C)")) +
  ylab("Percent Change") +
  ggtitle("10% Precipitation Increase")

# p4: 10% precip increase, response along MAP axis
p4 <- ggplot(data=d2, aes(x=MAP, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  xlab("Mean Annual Precipitation (mm)") +
  theme(legend.position="none", axis.text.y=element_blank(),axis.title.y=element_blank()) +
  ggtitle("10% Precipitation Increase")

jpeg(paste(fpath, "sensitivity along climate gradients.jpeg", sep=""),
           width = 180, height = 180, units = 'mm', res = 300)
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()
################################################################################
# make grid of plots by climate space
################################################################################
# p1: temp scenario on x axis, change on y, box for each model
# temperature scenarios
d1 <- dplyr::filter(merged, mag==2|mag==4) %>%
  filter(var=="tmean"|var=="tmax") %>%
  filter(MAT>8)
p1 <- ggplot(data=d1, aes(x=factor(mag), y=change, color=factor(model), fill=model)) +
  geom_boxplot(position="dodge", notch=T, aes(fill=factor(model),outlier.color=model)) +
  scale_y_continuous(name="Percent Change") +
  #scale_x_discrete(name="Magnitude of Change") +
  scale_x_discrete(name=expression("Simulated Temperature Change ("*~degree*"C)")) +
  scale_color_discrete(name="Model") +
  scale_fill_discrete(name="Model") +
  geom_hline(y=0) #+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
       legend.justification=c(1,1), legend.position=c(1,1),
       panel.background=element_blank(),plot.background=element_blank()) +
 ggtitle(expression("2"*~degree*"C Temperature Increase"))
p1

# p2: 2 degree temp increase, response along MAP axis
p2 <- ggplot(data=d1, aes(x=MAP, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  xlab("Mean Annual Precipitation (mm)") +
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
        legend.position="none", axis.text.y=element_blank(), axis.title.y=element_blank(),
        panel.background=element_blank(),plot.background=element_blank()) +
  ylab("Percent Change") +
  ggtitle(expression("2"*~degree*"C Temperature Increase"))
p2

# p3: 10% precip increase, response along MAT axis
d2 <- dplyr::filter(merged, mag==1.1 & var=="ppt")
p3 <- ggplot(data=d2, aes(x=MAT, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  theme(legend.position="none") +
  xlab(expression("Mean Annual Temperature ("*~degree*"C)")) +
  ylab("Percent Change") +
  ggtitle("10% Precipitation Increase")

# p4: 10% precip increase, response along MAP axis
p4 <- ggplot(data=d2, aes(x=MAP, y=change, color=model, fill=model)) +
  geom_smooth(method=lm, alpha=.3, size=1.5) +
  geom_point() +
  geom_hline(y=0) +
  xlab("Mean Annual Precipitation (mm)") +
  theme(legend.position="none", axis.text.y=element_blank(),axis.title.y=element_blank()) +
  ggtitle("10% Precipitation Increase")

jpeg(paste(fpath, "sensitivity along climate gradients.jpeg", sep=""),
     width = 180, height = 180, units = 'mm', res = 300)
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()
p1 <- ggplot(d1, aes(x=AK, y=KR, color=MAT)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=0, slope=1)

jpeg
grid.arrange(p1, p2, p3, p4, ncol=2)
dev.off()





# look at climate space of Andy's plots
ggplot(data=m2, aes(x=MAT, y=MAP)) +
  geom_point()

# Plot 4 varieties of climate space
# cool, warm, dry, wet
# scenario on x axis, different color box for each model

m4 <- mutate(m3, )




