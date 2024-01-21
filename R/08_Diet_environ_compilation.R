### Compile environmental data and diet proportions
# By Karen Jorgenson

# Setup
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

## Load estimated diet proportions
data_diet <- read.csv("Output//diet_data_clean.csv")

## Load environmental data
Envi_data <- read.csv("Data//Environ_data.csv")

# Load PCA data
PCA_dat <- read.table("Output//PCA_dat.txt")
names(PCA_dat) <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5")
site <- rownames(PCA_dat)
rownames(PCA_dat) <- NULL
PCA_data <- cbind(site, PCA_dat[1:4])
PCA_data

# Merge environ and PCA
Envi_data2 <- merge(Envi_data, PCA_data, all = TRUE)

## Load and merge diversity data
Div_dat <- read.csv("Output//Div.csv") %>% select("site", "H", "D")
Envi_data <- merge(Envi_data2, Div_dat)

write.csv(Envi_data, "Output//Envi_data.csv")

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
env_diet_dat <- merge(data_diet_H0, Envi_data, all = TRUE) %>% filter(source != "NA", site != "NA") %>%
    filter(!site %in% "Gusher")

write.csv(env_diet_dat, "Output//env_diet_dat.csv")




