### Clean Diet Data
# By Karen Jorgenson

# Setup
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

## Load estimated diet proportions
data_di <- read.csv("Output//Diet_proportions_all.csv")
data_di$site <- as.factor(data_di$site) 
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
data_di %>% filter(site == "Grizzly")

# Combine Pikepoop and Algae_slime with other sources
data_di$source <- as.factor(data_di$source)
data_diet <- data_di %>% mutate(source = case_when(source %in% c("CPOM/Plant", "Pikapoop") ~ "CPOM",
                                                   source %in% c("Biofilm", "Algae_slime") ~ "Biofilm",
                                                   source == "Hydrurus" ~ "Hydrurus")) %>%
  dplyr::group_by(site, taxa, source) %>% summarise(Mean = sum(Mean), SD = mean(SD))

head(data_diet)

write.csv(data_diet, "Output//diet_data_clean.csv")
