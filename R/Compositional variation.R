### Composition variation
# By Karen Jorgenson

# Setup
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(compositions)
library(ggtern)
library(mvnormtest)

## organize data
Envi_data <- read.csv("Output//Envi_data.csv")

diet_data <- read.csv("Output//diet_data_clean.csv")

diet_means_data <- diet_data %>% group_by(site, source) %>% summarise(Mean = mean(Mean, na.rm = TRUE,
                                                                                  SD = sd(Mean, na.rm = TRUE)))
diet_means_data

# Spread resource data into seperate columns
spread_dat <- diet_data %>% select(site, taxa, source, Mean) %>% spread(key = source, value = Mean) 
spread_dat_2 <- diet_data %>% select(site, taxa, source, SD) %>% spread(key = source, value = SD)
names(spread_dat_2) <- c("site", "taxa", "Biofilm_SD", "CPOM_SD", "Hydrurus_SD")
spread_dat$Hydrurus[is.na(spread_dat$Hydrurus)] <- 0
Spread_dat <- merge(spread_dat, spread_dat_2, by = c("site", "taxa"))

# Set SD for zero Hydrurus to max SD for Hydrurus
Spread_dat$Hydrurus_SD[is.na(Spread_dat$Hydrurus_SD)] <- mean(spread_dat_2$Hydrurus_SD, na.rm = TRUE)

# Merge with environmental data
spread_env_dat <- merge(Spread_dat, Envi_data)
names(spread_env_dat)

### Explore variation in compositions
# Create acomp object to use function for metric variation from 'compositions' package
xc = acomp(spread_env_dat[, c("Biofilm","CPOM", "Hydrurus")])
summary(xc)

spread_env_dat$comp <- xc

## within-site variation
# Calculate within-site variation for each site
site <- unique(spread_env_dat$site)
y  <- NULL
for(i in site){
  s<- spread_env_dat %>% filter(site == i) %>% select(comp)
  temp <- data.frame(site = i, var = mvar(s))
  y <- rbind(y, temp)
}
site_var <- y
site_var
mean(site_var$var) # mean within site variation


summary(lm(site_var$var~Envi_data$H)) # site var does not increase with diversity
summary(lm(site_var$var~Envi_data$D))

# Mean within site variation by stream type
mean(site_var$var[c(1,9)]) # Subterranean ice
mean(site_var$var[c(2,3,8)]) # Glacier
mean(site_var$var[c(4,5,6,7)]) # Snowmelt
# ANOVA to test differences in within-site variation among stream types
site_var$W_source <- c("Subterranean ice", "Glacier", "Glacier", "Snowmelt","Snowmelt", "Snowmelt", "Snowmelt", "Glacier", "Subterranean ice")
summary(aov1 <- aov(var ~ W_source, data = site_var))


## Among-site variation using site means
# Calculate site diet porportion means
site_means <- spread_env_dat %>% group_by(site) %>% summarise(Biofilm = mean(Biofilm), CPOM = mean(CPOM), Hydrurus = mean(Hydrurus), Primary_water_source = unique(Primary_water_source))
site_means
# Among-site variation
among_var <- mvar(site_means[,2:4])
among_var

# Among-site variation by stream type
mvar(site_means[c(1,9),2:4]) # Subterranean ice
mvar(site_means[c(2,3,8),2:4]) # Glacier
mvar(site_means[c(4,5,6,7),2:4]) # Snowmelt

# Among-site variation for taxa at >= 3 sites
spread_env_dat %>% group_by(taxa) %>% summarise(n = length(taxa))
taxa <- c("Allomyia", "Ameletidae", "Baetidae", "Homophylax", "Lednia", "Midges", "Simuliidae", "Zapada")
y2  <- NULL
for(i in taxa){
  t<- spread_env_dat %>% filter(taxa == i) %>% select(comp)
  temp <- data.frame(taxa = i,var = mvar(t))
  y2 <- rbind(y2, temp)
}
y2

## Plots to display variation
# Ternary plot grouped by taxa
spread_env_dat$Hydrurus[spread_dat$Hydrurus < .01] <- 0.01 # Make Hydrurus more easily visualized
cols <- brewer.pal(7, "Dark2")
t1 <- spread_env_dat %>% filter(taxa %in% c("Allomyia", "Homophylax", "Lednia", "Midges", "Simuliidae", "Tipula", "Zapada")) %>% 
  ggtern(aes(Biofilm, CPOM, Hydrurus, fill = taxa)) + geom_point(cex = 4, shape = 21) + 
  theme_bw() + theme(legend.position= c(0.2,.7)) +
  scale_fill_manual(values = cols, name = "Taxa")
t1
ggsave("Output//taxa diet tern.png", width = 6, height = 6)


## Ternary plot with all taxa colored by stream type

w_source_means <- spread_env_mean_dat %>% group_by(Primary_water_source) %>% summarise(Biofilm = mean(Biofilm),
                                                                     CPOM = mean(CPOM),
                                                                     Hydrurus = mean(Hydrurus))

t1 <- spread_env_dat %>% 
  ggtern(aes(Biofilm, CPOM, Hydrurus)) + geom_point(aes(shape = site, color = Primary_water_source), stroke = 1.5) +
  scale_shape_manual(values=1:10) + guides(shape = FALSE) + theme_bw() +
  scale_color_manual(values = c("#2171b5", "#9ecae1", "#08306b"), name="Hydrologic Source") +
  theme(legend.position= c(.85,.83)) +
  geom_point(data = w_source_means, aes(Biofilm, CPOM, Hydrurus, fill = Primary_water_source), pch = 21, cex = 7, show.legend = FALSE) +
  scale_fill_manual(values = c("#2171b5", "#9ecae1", "#08306b"))
t1
ggsave("Output//Paper figures//taxa diet tern WS.png", width = 6, height = 6)

# Ternary plot of most common taxa colored by stream type
datws <-spread_env_dat %>% group_by(taxa, Primary_water_source) %>% summarise(Biofilm = mean(Biofilm),
                                                                      CPOM = mean(CPOM),
                                                                     Hydrurus = mean(Hydrurus))

t2 <- datws %>% filter(taxa %in% c("Allomyia", "Ameletidae", "Baetidae", "Homophylax", "Lednia", "Midges", "Simuliidae", "Tipula", "Zapada")) %>% ggtern(aes(Biofilm, CPOM, Hydrurus, color = Primary_water_source, shape = taxa)) + geom_point()
t2
ggsave("Output//taxa diet tern.png")


# Ternary plot of among-site taxa means
taxa_means <- spread_env_dat %>% filter(taxa %in% c("Allomyia", "Ameletidae", "Baetidae", "Homophylax", "Lednia", "Midges", "Simuliidae", "Tipula", "Zapada")) %>% 
  group_by(taxa) %>% summarise(Biofilm = mean(Biofilm), CPOM = mean(CPOM), Hydrurus = mean(Hydrurus))
taxa_means

t4 <- taxa_means %>% ggtern(aes(Biofilm, CPOM, Hydrurus, color = taxa)) + geom_point()
t4
ggsave("Output//taxa mean diet tern.png")

## Max and min resource use for Lednia and Zapada
spread_env_dat %>% filter(taxa == "Lednia")
spread_env_dat %>% filter(taxa == "Zapada")









