### Compile environmental data and diet proportions

library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


setwd("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Environ variables")

## Load diet proportions
data_di <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//MixSIAR loop output//Diet_proportions_all.csv")
data_di$site <- as.factor(data_di$site) 
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
data_di %>% filter(site == "Grizzly")

# Combine Pikepoop and Algae_slime with other sources
data_di$source <- as.factor(data_di$source)
data_diet <- data_di %>% mutate(source = case_when(source %in% c("CPOM/Plant", "Pikapoop") ~ "CPOM",
                                                        source %in% c("Biofilm", "Algae_slime") ~ "Biofilm",
                                                      source == "Hydrurus" ~ "Hydrurus")) %>%
  group_by(site, taxa, source) %>% summarise(Mean = sum(Mean), SD = mean(SD))


data_diet
data_diet %>% group_by(taxa, site) %>% summarise(tot = sum(Mean)) %>% filter(tot < 1)

write.csv(data_diet, "diet_data_clean.csv")

## Load environmental data
Env_data <- read.csv("Environ_data.csv")

# Load and average IC data
IC_data <- read.csv("IC_2020.csv")

IC_means <- IC_data %>% group_by(site) %>%
  summarise(fluoride = mean(fluoride, na.rm = TRUE),
            chloride = mean(chloride, na.rm = TRUE),
            nitrate = mean(nitrate, na.rm = TRUE),
            sulfate = mean(sulfate, na.rm = TRUE))

# Load PCA data
PCA_dat <- read.table("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//PCA//PCA_dat.txt")
names(PCA_dat) <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5")
site <- rownames(PCA_dat)
rownames(PCA_dat) <- NULL
PCA_data <- cbind(site, PCA_dat[1:4])
PCA_data

# Merge environ, PCA and IC means
Envi_dat <- merge(Env_data, IC_means, all = TRUE)
Envi_data2 <- merge(Envi_dat, PCA_data, all = TRUE)

## Load and merge diversity data
H_dat <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//Diversity//H.csv")
H_dat <- H_dat %>% select("site", "H")
Envi_data <- merge(Envi_data2, H_dat)

# Add zeros when Hydrurus was not present:
site <- c("SFTC", "Grizzly", "Paintbrush")
datalist = list()
for (i in site){
  taxa <- data_diet %>% filter(site == i, source == "Biofilm") %>% select(taxa)
  n = length(taxa)
  dat <- data.frame(site = rep(i, n),  source = rep("Hydrurus", n), taxa = taxa, Mean = rep(0, n))
  datalist[[i]] <- dat # add it to list
}

Hyd_add = do.call(rbind, datalist) 

data_diet_H0 <- dplyr::bind_rows(data_diet, Hyd_add)

Envi_data$variable <- rownames(Envi_data)
env_diet_dat_H0 <- merge(data_diet_H0, Envi_data, all = TRUE) %>% filter(source != "NA", site != "NA") %>%
    filter(!site %in% "Gusher")

write.csv(env_diet_dat_H0, "env_diet_dat_H0.csv")

env_diet_dat_H0 <- read.csv("env_diet_dat_H0.csv")

# Plot of sit mean diet composition grouped by water source
means <- env_diet_dat_H0 %>% group_by(site, source) %>% summarise(Mean = mean(Mean, na.rm = TRUE),
                                                              W_source = Primary_water_source) %>% unique()

# Resource on axis
p <- means %>% ggplot(aes(fill = W_source)) +
  geom_boxplot(aes(source, Mean)) + 
  geom_point(aes(source, Mean), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("#2171b5", "#9ecae1", "#08306b"), name="Water Source") +
  #annotate(geom="text", x=2, y=0.38, label="a", color="#D95F02") +
  #annotate(geom="text", x=3, y=0.29, label="a", color="#D95F02") +
  xlab("Resource") + scale_x_discrete(labels = c('Biofilm','CPOM','Hydrurus')) + 
  ylab("Diet Proportion") + theme_bw() #+ coord_cartesian(clip="off") +
  #annotate("text",x=5,y=0,label="a)")
p
ggsave("Water_source_all_sites_1.png", width = 8, height = 6)

# Water source on axis
p100 <- means %>% ggplot() +
  geom_boxplot(aes(W_source, Mean, fill = source)) + 
  geom_point(aes(W_source, Mean, fill = source), cex = 2, shape = 21, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  #annotate(geom="text", x=2, y=0.38, label="a", color="#D95F02") +
  #annotate(geom="text", x=3, y=0.29, label="a", color="#D95F02") +
  labs(fill = "Resource") + xlab(NULL) + 
  ylab("Diet Proportion") + theme_bw() 
p100
ggsave("Water_source_all_sites_2.png", width = 5, height = 3.6)



