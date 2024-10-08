### Calculate trophic position (TP)
# By Karen Jorgenson

# Setup
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Creat dataframe

# load isotope data. Filter for invert, select columns, concatenate site and group
dat <- read.csv("Data/Teton_Iso_Data_QC.csv")
TP_dat <- dat %>% filter(type == "invert") %>% select("group", "site", "d15N", "d13C", "FG") %>% mutate(group = as.factor(group), site = as.factor(site), FG = as.factor(FG)) %>% 
  data.frame() %>% na.omit() %>% rename(Site = "site", Group = "group", FG = "FG", d15N = "d15N", d13C = "d13C")

# remove spaces from site names
levels(TP_dat$Site)[levels(TP_dat$Site)=="Wind Cave"] <- "WindCave"
levels(TP_dat$Site)[levels(TP_dat$Site)=="AK Basin"] <- "AKBasin"

str(TP_dat)
levels(TP_dat$Site)
head(TP_dat)

### Calculating TP per individual

# mean baseline for each site
head(TP_dat)
b_avg <- TP_dat %>% filter(FG == "Baseline") %>% group_by(Site) %>% 
  summarise(count = length(Group),
            mean_C = mean(d13C), 
            mean_N = mean(d15N))

b_avg


# add baseline means to dataframe
TP_dat$base_N = with(TP_dat, ifelse(Site == "AKBasin" , b_avg$mean_N[1],
                              ifelse(Site == "Cloudveil", b_avg$mean_N[2],
                              ifelse(Site == "Delta", b_avg$mean_N[3],
                              ifelse(Site == "Grizzly", b_avg$mean_N[4],
                              ifelse(TP_dat$Site == "Gusher", b_avg$mean_N[5],
                              ifelse(Site == "NFTC", b_avg$mean_N[6],
                              ifelse(Site == "Paintbrush", b_avg$mean_N[7],
                              ifelse(Site == "SFTC", b_avg$mean_N[8],
                              ifelse(Site == "Skillet", b_avg$mean_N[9], b_avg$mean_N[10]))))))))))

TP_dat$base_N
head(TP_dat)

# calculate TP with one baseline formula

TEF <- 3.4 # N TEF
head(TP_dat)
TP_calc <- 2 + (TP_dat$d15N - TP_dat$base_N)/TEF

# Add to dataframe
TP_dat$TP_calc <- TP_calc

head(TP_dat)
write.csv(TP_dat, "Output/TP_dat.csv")


# Plot with all sites
p3 <- TP_dat %>% filter(Site != "Gusher") %>% ggplot(aes(Group, TP_calc)) + geom_boxplot() + 
  geom_point(aes(color= Site))  + geom_abline(slope=0, intercept = 2.5, linetype = 3) + 
  ylab("Trophic Position") + xlab("Taxon") +
  scale_color_manual(labels = c("Alaska Basin", "Cloudveil", "Delta", "Grizzly", "N Fork Teton Creek", "Paintbrush", "S Fork Teton Creek", "Skillet", "Wind Cave"), values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "dark orange", "#E6AB02", "#A6761D", "#666666")) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))
p3

ggsave("Output//Paper figures//TPs by taxa.png", plot = p3, width = 7, height = 5)




