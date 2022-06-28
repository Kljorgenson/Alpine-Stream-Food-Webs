### Teton 2020 Invertebrate Diversity
# By Karen Jorgenson

# Setup
library(dplyr)
library(plyr)
library(tidyverse)
library(vegan)

setwd("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//Diversity")

# Load data
div_d <- read.csv("TetonInverts2020.csv") # Surber sample data
class_dat <- read.csv("Taxa classification.csv") # Taxaonomic classifications

# Add order and family to Surber samples data
div_dat <- merge(div_d, class_dat) # Merge taxaonomic classifications with Surber sample data
unique(div_dat$Family) # Check unique families
unique(div_d$Taxa) # Check unique taxa

# Add classifications for Rhyacophila groups
div_dat$Family <- ifelse(startsWith(div_dat$Taxa, "Rhy"), "Rhyacophilidae", div_dat$Family) 
div_dat$Genus <- ifelse(startsWith(div_dat$Taxa, "Rhy"), "Rhyacophila", div_dat$Genus)
div_dat$Order <- ifelse(startsWith(div_dat$Taxa, "Rhy"), "Trichoptera", div_dat$Order) 

### Shannon diversity index on Family by site
# Make matrix-like dataframe
Fam_matrix <- with(div_dat, tapply(Abundance, list(Stream, Family), FUN = mean))
Fam_matrix[is.na(Fam_matrix)] <- 0

## Calculate Family Shannon diversity index
H_fam <- diversity(Fam_matrix)
H_fam_dat <- data.frame(site = names(H_fam), H_fam = H_fam) # Make into dataframe
rownames(H_fam_dat) = NULL # Remove row names
H_fam_dat$site <- c("AK Basin", "Cloudveil", "Delta", "Grizzly", "Middle Teton", "NFTC", "Paintbrush", "SFTC", "Skillet", "South Cascade RG", "Gusher", "Wind Cave")
H_fam_dat

## Calculate H with lowest classification
# Add column Taxa2 to rename some taxa
div_dat %>% mutate(Taxa2 = case_when(Taxa == "Simuliidae Pupae" ~ "Simuliidae",
                                     Taxa == "Chironomidae Pupae" ~ "Non-Tanypodinae",
                                     Taxa == "Tipula (Arctotipula)" ~ "Tipula",
                                     Taxa %in% c("1Megarcys", "2Megarcys") ~ "Megarcys",
                                     !Taxa %in% c("Simuliidae Pupae", "Chironomidae Pupae","Tipula (Arctotipula)","1Megarcys", "2Megarcys") ~ Taxa   ))

# Make matrix-like dataframe
Fam_matrix2 <- with(div_dat, tapply(Abundance, list(Stream, Family), FUN = mean))
Fam_matrix2[is.na(Fam_matrix2)] <- 0

# calculate Family diversity
H <- diversity(Fam_matrix2)
H_dat <- data.frame(site = names(H), H = H)
rownames(H_dat) = NULL
H_dat$site <- c("AK Basin", "Cloudveil", "Delta", "Grizzly", "Middle Teton", "NFTC", "Paintbrush", "SFTC", "Skillet", "South Cascade RG", "Gusher", "Wind Cave")
H_dat




### Simpson's diversity index with lowest classification

D <- diversity(Fam_matrix2, index = "simpson")
D_dat <- data.frame(site = names(D), D = D)
rownames(D_dat) = NULL
D_dat$site <- c("AK Basin", "Cloudveil", "Delta", "Grizzly", "Middle Teton", "NFTC", "Paintbrush", "SFTC", "Skillet", "South Cascade RG", "Gusher", "Wind Cave")
D_dat
B<- merge(H_fam_dat, H_dat)
div_ind <- merge(B, D_dat)
div_ind
write.csv(div_ind, "Div.csv")
