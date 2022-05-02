### TASR PCA
library(plyr)
library(dplyr) # This order to make group_by work right
library(ggbiplot)
library(ggfortify)
library(factoextra)
library(RColorBrewer)


setwd("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//PCA")

PCA <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR/Environ variables//Envi_data.csv")
names(PCA)
PCA_d <- PCA %>% select("site", "Primary_water_source","Elevation", "Tmean", "Tmax", "DO_mg_L", "SPC",  "TSS_g_L", "Chla_mg_m2", "pH", "fluoride", "chloride", "nitrate", "sulfate")
names(PCA_d)
row.names(PCA_d)<- PCA_d$site
PCA_1 <- prcomp(PCA_d[,c(4:8,10:14)],scale. = TRUE) # select variables
ind <- get_pca_ind(PCA_1)
ind
ind$coord
write.table(ind$coord, "PCA_dat.txt")

p1 <- autoplot(PCA_1, data = PCA_d, colour = 'Primary_water_source', loadings = TRUE, loadings.label = FALSE, loadings.colour = 1, size = 5) +
theme_classic() + labs(colour = "Hydrologic source") + xlim(-0.6,0.7) +
  geom_text(aes(label = "Tmean", x = -0.5, y = -0.07 ), cex = 4) +
  geom_text(aes(label = "Tmax", x = -0.5, y = -0.02 ), cex = 4) +
geom_text(aes(label = "TSS", x = -0.3, y = 0.06 ) , cex = 4) +
geom_text(aes(label = "pH", x = 0.24, y = 0.5 ), cex = 4) +
geom_text(aes(label = "Nitrate", x = 0.45, y = 0.2 ), cex = 4) +
geom_text(aes(label = "DO", x = 0.47, y = 0.02 ), cex = 4) +
  geom_text(aes(label = "Chloride", x = 0.38, y = -0.28 ), cex = 4) +
  geom_text(aes(label = "SPC", x = 0.23, y = -0.54 ), cex = 4) +
  geom_text(aes(label = "Sulfate", x = 0.07, y = -0.48 ), cex = 4) +
  geom_text(aes(label = "Fluoride", x = -0.15, y = -0.43 ), cex = 4) +
  scale_color_manual(values = c("#2171b5", "#9ecae1", "#08306b")) +
  scale_size_manual(values = 8)
  
p1

ggsave("PCA all.png", width = 6, height = 4)


# PCA of diet proportions
data_di <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Environ variables//diet_data_clean.csv")
data_di$site <- as.factor(data_di$site) 
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
head(data_di)

# Combine Pikepoop and Algae_slime with other sources
data_diet <- data_di %>% mutate(source = case_when(source %in% c("CPOM/Plant", "Pikapoop") ~ "CPOM/Plant",
                                                   source %in% c("Biofilm", "Algae_slime") ~ "Biofilm",
                                                   source == "Hydrurus" ~ "Hydrurus"))


diet_data <- data_diet %>% group_by(site, source) %>% summarise(Mean = mean(Mean, na.rm = TRUE)) 



library(tidyr)
diet_dat <- diet_data %>% spread(key = source, value = Mean)
diet_dat[is.na(diet_dat)] <- 0 
diet_dat <- as.data.frame(diet_dat)
diet_dat$Primary_Water_Source <- PCA_d$Primary_water_source

row.names(diet_dat)<- diet_dat$site
PCA_s <- prcomp(diet_dat[,2:4], center = TRUE,scale. = TRUE)
ind <- get_pca_ind(PCA_s)
ind
ind$coord
write.table(ind$coord, "PCA_s_dat.txt")
p2 <- autoplot(PCA_s, data = diet_dat, colour = 'Primary_Water_Source', label = FALSE, loadings = TRUE, loadings.label = FALSE, loadings.colour = 1, size = 5) +
theme_classic() +
  geom_text(aes(label = "Hydrurus", x = -0.5, y = 0.27 ), cex = 4) +
  geom_text(aes(label = "CPOM", x = -0.1, y = -0.58 ) , cex = 4) +
  geom_text(aes(label = "Biofilm", x = 0.54, y = 0.12 ), cex = 4) +
  scale_color_manual(values = c("#2171b5", "#9ecae1", "#08306b"), name = "Hydrologic source") +#labs(legend = NULL) + geom_text(aes(label = site), nudge_y = -0.02, nudge_x = 0.05)
xlim(-0.6, 0.7)
p2

ggsave("PCA diet props.png", width = 7, height = 4)

# Combine plots
library(ggpubr)
ggarrange(p2, p1, ncol=2, common.legend = TRUE, legend="right", labels = "auto",
          label.x = 0,
          label.y = 1)
ggsave("PCA both.png", width = 10, height = 4)                                                                             
