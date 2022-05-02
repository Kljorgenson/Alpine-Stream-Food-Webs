### Teton Isotope Plots
# By Karen Jorgenson


## Setup
library(dplyr)
library(plyr)
library(MixSIAR)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)
library(tidyverse)


setwd("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR")

## Organize data
iso_dat <- read.csv("Teton_Iso_Data_QC.csv")
head(iso_dat)
names(iso_dat)
iso_inv <- iso_dat %>% filter(type == "invert")

# Merge trophic position data
dat_TP <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Trophic Position//TP_dat.csv")  
head(dat_TP)
iso_TP <- merge(iso_inv, dat_TP, by = c("d13C", "d15N"), all = TRUE ) %>% mutate(group = as.factor(group), site = as.factor(site)) 
head(iso_TP)

# Set all non predatory data to TP = 2
iso_TP$TP <- ifelse(iso_TP$group %in% c("Clinocera", "Sweltsa", "Lednia", "Megarcys", "Rhyacophila", "Simuliidae", "Turbellaria", "Ameletidae", "Baetidae"), iso_TP$TP_calc, 2)
iso_TP$TL<- ifelse(iso_TP$TP > 2.5, "P", "N") # P for predator, N for non-predator

head(iso_TP)

# Standard error function
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

# Calculate means and standard deviations for food sources
iso_means <- iso_dat %>% filter(date2 == "A") %>% group_by(group, site) %>%
  dplyr::summarize(count = n(),
            mC = mean(d13C, na.rm = TRUE), 
            SEC = SE(d13C), 
            mN = mean(d15N, na.rm = TRUE), 
            SEN = SE(d15N), 
            type = type) %>%
  unique() # sometimes 'summarize' is a jerk, so I added this

iso_means

#################################################################################################
## Isotope plots with ggplot
#################################################################################################

### Loop through sites
sites <- unique(iso_dat$site) %>% na.omit() # All sites
sites <- c("Wind Cave", "Skillet", "Delta",  "Cloudveil", "Paintbrush") # Sites that don't need adjustments
sites <- "Wind Cave" # Run single site

for(i in sites){
## raw data for inverts and sources
invert_dat <- iso_TP %>% filter(type == "invert", group == "Zapada", site == i, date2 == "A") %>% select(group, date, d15N, d13C, TL)
invert_dat
food_dat <- iso_means %>% filter(type == "food", site == i) %>% select(group, mN, mC, SEN, SEC) %>%
  filter(group %in% c("Algae_slime", "Hydrurus", "CPOM/Plant", "Biofilm", "Pikapoop"))

food_dat$SEC - food_dat$mC
#food_dat$group[3] <- "Small mammal feces" # Change source name for Grizzly

# plot
p0 <- ggplot(food_dat, aes( mC, mN)) + 
  geom_point(aes(mC + 0.4, mN + 1.4, group = group), size = 2) + # add points for food sources
    geom_errorbar(data = food_dat,                     # add x-axis error bars
                mapping = aes(x = mC + 0.4,
                              ymin = mN + 1.4 - SEN - 1.4, 
                              ymax = mN + 1.4 +SEN + 1.4), 
                width = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
  geom_errorbarh(data = food_dat,                     # add y-axis error bars
                 mapping = aes(y = mN + 1.4,
                               xmin = mC + 0.4 - SEC -1.4,
                               xmax = mC + 0.4 + SEC + 1.4),
                 height = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
  geom_errorbar(data = food_dat,                     # add x-axis error bars
                mapping = aes(x = mC + 0.4,
                              ymin = mN + 1.4 - SEN, 
                              ymax = mN + 1.4 +SEN), 
                width = 0, inherit.aes = FALSE, size = 0.7) +
  geom_errorbarh(data = food_dat,                     # add y-axis error bars
                 mapping = aes(y = mN + 1.4,
                               xmin = mC + 0.4 - SEC,
                               xmax = mC + 0.4 + SEC),
                 height = 0, inherit.aes = FALSE, size = 0.7) +

  geom_text(data = food_dat, aes(label=group),hjust= -0.4, vjust= -12, size = 3.5) +   # add labels for food sources
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none") +
  ggtitle(paste(i)) + # add title of site
  labs(x=expression(paste(delta, "13C")), y=expression(paste(delta, "15N"))) + # Make nice axis labels
  geom_point(data = invert_dat, aes (d13C, d15N, shape = TL, color = group), cex = 2) + # Add points for invertebrates
  theme(legend.position = "right") + scale_color_brewer(palette = "Paired", name = "Taxon") + 
  scale_shape_discrete(name = "Trophic Position", labels = c("Primary consumer","Predator")) +
  guides(shape = guide_legend(order = 2),color = guide_legend(order = 1))

  
p0

# save plot
ggsave(plot = p0, width = 6, height = 5, dpi = 300, filename = paste( i, "new.png"))
}


# Loop for SFTC
sites <- "SFTC"

for(i in sites){
  ## raw data for inverts and sources
  invert_dat <- iso_TP %>% filter(type == "invert", site == i, date2 == "A") %>% select(group, date, d15N, d13C, TL)
  invert_dat
  food_dat <- iso_means %>% filter(type == "food", site == i) %>% select(group, mN, mC, SEN, SEC) %>%
    filter(group %in% c("Algae_slime", "Hydrurus", "CPOM/Plant", "Biofilm", "Pikapoop"))
  food_dat$SEC - food_dat$mC
  
  # plot
  p0 <- ggplot(food_dat, aes( mC, mN)) + 
    geom_point(aes(mC + 0.4, mN + 1.4, group = group), size = 2) + # add points for food sources
    geom_errorbar(data = food_dat,                     # add x-axis error bars
                  mapping = aes(x = mC + 0.4,
                                ymin = mN + 1.4 - SEN - 1.4, 
                                ymax = mN + 1.4 +SEN + 1.4), 
                  width = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
    geom_errorbarh(data = food_dat,                     # add y-axis error bars
                   mapping = aes(y = mN + 1.4,
                                 xmin = mC + 0.4 - SEC -1.4,
                                 xmax = mC + 0.4 + SEC + 1.4),
                   height = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
    geom_errorbar(data = food_dat,                     # add x-axis error bars
                  mapping = aes(x = mC + 0.4,
                                ymin = mN + 1.4 - SEN, 
                                ymax = mN + 1.4 +SEN), 
                  width = 0, inherit.aes = FALSE, size = 0.7) +
    geom_errorbarh(data = food_dat,                     # add y-axis error bars
                   mapping = aes(y = mN + 1.4,
                                 xmin = mC + 0.4 - SEC,
                                 xmax = mC + 0.4 + SEC),
                   height = 0, inherit.aes = FALSE, size = 0.7) +
    geom_text(aes(label = "Filamentous alga", x = -15, y = 1.5), size = 3.5) +
    geom_text(aes(label = "Biofilm", x = -27.5, y = 1.1), size = 3.5) +
    geom_text(aes(label = "CPOM/Plant", x = -24.5, y = -1.5), size = 3.5) +# add labels for food sources
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       legend.position = "none") +
    scale_color_brewer(palette = "Paired", name = "Taxon") + # set custom colors
    ggtitle("S Fork Teton Creek") + # Add title of site
  labs(x=expression(paste(delta, "13C")), y=expression(paste(delta, "15N"))) + # make nice axis labels
  geom_point(data = invert_dat, aes (d13C, d15N, color=group, shape = TL), cex =2) + # add points for invertebrates
  theme(legend.position = "right") + scale_shape_discrete(name = "Trophic Position", labels = c("Primary consumer","Predator"))
p0

# save plot
ggsave(plot = p0, width = 8, height = 5, dpi = 300, filename = paste( i, "new.png"))
}

