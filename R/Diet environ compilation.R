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

# Plot of sit mean diet composition grouped by water source
means <- env_diet_dat %>% group_by(site, source) %>% summarise(Mean = mean(Mean, na.rm = TRUE),
                                                              W_source = Primary_water_source) %>% unique()

# Plot diet proportions by hydrologic source
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
ggsave("Output//Paper figures//Diet by Hydro.png", width = 5, height = 3.6)



