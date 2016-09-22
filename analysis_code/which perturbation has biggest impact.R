#############################################################################
# Look at which perturbation has the biggest impact on each site
# Splits change into 5 categories, counts sites per category per model
# Having same categories for all models doesn't work well- large changes from DRS,
# small changes from AK
#############################################################################
rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=20)) # sized for ppt
library(gridExtra)
library(splines)

# paths to data and folder for figures
dpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/"
fpath <- "/Users/poulterlab1/Box Sync/sageseer/ModelComparison/Figures/"

# functions for plotting
source("/Users/poulterlab1/Box Sync/sageseer/ModelComparison/code/plotting_functions.R")

# Pull in merged data and manipulate
merged <- read.csv(paste(dpath, "merged_data-co2.csv", sep=""))
m3 <- merged %>%
  mutate(change=predicted-baseline) %>%
  mutate(direction=ifelse(change>0,"Positive","Negative")) %>%
  na.omit() 
m4 <- gather(m3, baseline:predicted,key=time,value=value)

head(m3)

# Which perturbation has biggest impact at each site?
m5 <- m3 %>% mutate(abschange=abs(change)) %>%
  group_by(site,model) %>%
  mutate(winner=rank(abschange)) %>%
  select(site,model:mag,abschange,winner) %>%
  group_by(model,var,mag) %>%
  summarise(wins=sum(winner==6), n=n())
m5
# AK: temp biggest impact by far
# CC:prcp, but only by a little
# DRS: temp by a lot.
# DGVM: temperature

# Plot change by scenario. Use 2 most realistic perturbations per variable
# additional data wrangling:
m5 <- m3 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  group_by(model,var,mag, direction) %>%
  summarise(meanchange=mean(change), lower=meanchange-sd(change)/sqrt(length(change)),
            upper=meanchange+sd(change)/sqrt(length(change))) %>%
  filter(mag!=1.2&mag!=.2)
head(m5)

# Make a nice plot:
ggplot(data=m5, aes(x=as.factor(mag), y=meanchange, fill=var)) +
  geom_bar(stat="identity", position=position_dodge(width=0.9)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(width=0.9), width=.2) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00"), 
                    name="Variable",
                    breaks=c("ppt", "temp"),
                    labels=c("Precip", "Temp")) +
  xlab("Magnitude of Perturbation") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free")

## Would this look terrible as a boxlplot? YES- HARDER TO SEE PATTERN
m5 <- m3 %>% mutate(abschange=abs(change)) %>%
  mutate(direction=ifelse(change>0,"increase","decrease")) %>%
  filter(mag!=1.2&mag!=.2)

ggplot(data=m5, aes(x=as.factor(mag), y=change, fill=direction)) +
  geom_boxplot() +
  geom_hline(yintercept=0) +
  xlab("Magnitude of Perturbation") +
  ylab("Mean Change in Model Response") +
  facet_wrap(~model,scale="free")

################################################################################
# ALTERNATIVE WAY TO VISUALIZE IMPACT OF PERTURBATIONS
# Try making 5 categories based on the ratio of predicted:baseline
m4 <- m3 %>% #na.omit() %>%
  filter(baseline>1) %>%
  mutate(perc_change=(predicted/baseline)) %>%
  mutate(category=ifelse(perc_change>2, "large_increase", "nochange")) %>%
  mutate(category=ifelse(perc_change>=1.2&perc_change<=2, "small_increase", category)) %>%
  mutate(category=ifelse(perc_change<=.8&perc_change>=.5, "small_decrease", category)) %>%
  mutate(category=ifelse(perc_change<(.5), "large_decrease", category)) %>%
  mutate(cat2=ifelse(perc_change>1, "increase", "decrease")) %>%
  mutate(cat2=ifelse(perc_change<1.2&perc_change>.8, "nochange", cat2)) %>%
  select(site, model:mag,category:cat2)

head(m4)
# convert several variables to ordered factors to facilitate plotting
m4$catergory <- factor(m4$category, levels=c("large_decrease","small_decrease","nochange","small_increase","large_increase"))
m4$mag <- factor(m4$mag, levels=c(.9,1.1,1.2,.2,2,4))
levels(m4$mag) <- c("-10%","+10%","+20%","+.2C","+2C","+4C")

#pal <- choose_palette()
#pal(4) # get color codes so don't have to choose manually to re-create plot
cols <- c("#683071", "#AC97B1", "#8BA48B","#045306")
jpeg(paste(fpath, "temp_precp.jpeg", sep=""),
     width = 254, height = 150, units = 'mm', res = 300)
ggplot(data=m4, aes(x=factor(category), fill=model)) +
  geom_bar() +
  scale_fill_manual(values = rev(cols),name="Model",
                    breaks=c("AK","CC","DRS","DGVM"),
                    labels=c("TS","RF","GISS","DVM")) +
  scale_x_discrete(limits=c("large_decrease","small_decrease","nochange","small_increase","large_increase"),
                   labels=c("Large Decrease","Small Decrease","No Change","Small Increase","Large Increase")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.title.x = element_blank()) +
  ylab("# sites") +
  facet_wrap(~mag)
dev.off()

#############################
# cut the lowest levels of change
m5 <- filter(m4, mag!="-10%"&mag!="+.2C")
tab <- m5 %>% 
  group_by(mag,var,category) %>%
  summarise(perc=n()/(nrow(m5)*.25))
jpeg(paste(fpath, "temp_precp_highlevels.jpeg", sep=""),
     width = 254, height = 150, units = 'mm', res = 300)
ggplot(data=m5, aes(x=factor(category), fill=model)) +
  geom_bar() +
  #geom_bar(aes(y = (..count..)/sum(..count..))) + 
  #geom_text(aes(y = (..count..)/sum(..count..))) +
  scale_fill_manual(values = rev(cols),name="Model",
                    breaks=c("AK","CC","DRS","DGVM"),
                    labels=c("TS","RF","GISS","DVM")) +
  scale_x_discrete(limits=c("large_decrease","small_decrease","nochange","small_increase","large_increase"),
                   labels=c("Large Decrease","Small Decrease","No Change","Small Increase","Large Increase")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.title.x = element_blank()) +
  ylab("# sites") +
  facet_wrap(~mag)
dev.off()

################################################################################
# Similar plots: show sagebrush is tolerant of small changes
cols <- c("#683071", "#AC97B1", "#8BA48B","#045306")
nrow(m4)/6*.5

p1 <- ggplot(data=m4, aes(x=factor(category), fill=model)) +
  #geom_bar() +
  geom_bar(aes(y = (..count..)/sum(..count..)*6)) + 
  scale_fill_manual(values = rev(cols),name="Model",
                    breaks=c("AK","CC","DRS","DGVM"),
                    labels=c("TS","RF","GISS","DVM")) +
  scale_x_discrete(limits=c("large_decrease","small_decrease","nochange","small_increase","large_increase"),
                   labels=c("Large Decrease","Decrease","No Change","Increase","Large Increase")) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1), axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),panel.background = element_blank(),
        panel.grid.major.x = element_blank()) +
  ylab("% of sites") +
  facet_wrap(~mag)

dummy <- ggplot(data=m4, aes(x=factor(category), y = site,fill=model))+  
  geom_rect(aes(fill=var), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  scale_fill_manual(values = c("cadetblue2", "darkorange3")) +
  facet_wrap(~mag) +
  theme_minimal()
dummy

library(gtable)

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(dummy)

gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip_t", g2$layout$name)
g2$layout$t[panels] <- g2$layout$t[panels] - 1
g2$layout$b[panels] <- g2$layout$b[panels] - 1

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)

gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- transform(g1$layout, z= z-max(z), name="g2")
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}
## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.newpage()
grid.draw(new_plot)

jpeg(paste(fpath, "tolerant_smchange.jpeg", sep=""),
     width = 245, height = 150, units = 'mm', res = 300)
grid.draw(new_plot)
dev.off()
