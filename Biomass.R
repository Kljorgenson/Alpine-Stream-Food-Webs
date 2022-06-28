### Teton Invert Biomass
# Karen Jorgenson

# Setup
library(tidyverse)
library(viridis)
library(DirichletReg)
library(ggtern)

setwd("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Biomass")

## Create dataframe with diet proportions and biomass
# Load diversity data
div_d <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//Diversity//TetonInverts2020.csv")
head(div_d)

# Change taxa to match diet taxonomic groups
div_dat <- div_d %>% mutate(taxa = case_when(startsWith(Taxa, "Rhy") ~ "Rhyacophilidae",
                                    Taxa %in% c("Simuliidae Pupae", "Heterocloeon", "Prosimuliium", "Helodon") ~ "Simuliidae",
                                    Taxa == "Non-Tanypodinae" ~ "Midges",
                                     Taxa == "Chironomidae Pupae" ~ "Midges",
                                     Taxa == "Tipula (Arctotipula)" ~ "Tipula",
                                     Taxa %in% c("1Megarcys", "2Megarcys") ~ "Megarcys",
                                    Taxa == "Nematoda" ~ "Oligochaeta",
                                     !Taxa %in% c("Rhyacophilidae", "Simuliidae Pupae", "Heterocloeon", "Prosimuliium", "Helodon", "Chironomidae Pupae", "Non-Tanypodinae", "Tipula (Arctotipula)","1Megarcys", "2Megarcys") ~ Taxa   )) %>% 
  select(Stream, taxa, Biomass_mg_m2) %>% filter(taxa != "Chironomidae Adult") %>% rename(site = Stream)

div_dat


# Rename stream names so that they match
div_dat$site <- as.factor(div_dat$site)
levels(div_dat$site) <- c("AK Basin",  "Cloudveil", "Delta", "Grizzly", "Middle Teton", "NFTC",  
"Paintbrush", "SFTC", "Skillet", "South Cascade RG", "The Gusher", "Wind Cave") 

# Merge biomass and diet proportions data
data_di <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Environ variables//diet_data_clean.csv")
data_di$site <- as.factor(data_di$site)
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
head(data_di)

diet_bm_dat <- merge(data_di, div_dat)
head(diet_bm_dat)
diet_bm_dat$Biomass_mg_m2 <- as.numeric(diet_bm_dat$Biomass_mg_m2)

diet_bm_dat %>% filter(site == "Cloudveil", source == "Biofilm") %>% summarise(sum = sum(Biomass_mg_m2, na.rm = TRUE))

## Calculate biomass supported by each food source

diet_bm_dat$biomass_t = diet_bm_dat$Biomass_mg_m2*diet_bm_dat$Mean # Biomass per source per taxa
head(diet_bm_dat)
div_dat
diet_bm_dat %>% filter(site == "Cloudveil") %>% summarise(tot = sum(biomass_t)) # Check that it sums to 100
diet_bm_dat %>% group_by(site, taxa, source, Mean) %>% summarise(biomass = sum(Biomass_mg_m2),
                                                                 biomass_t = biomass*Mean)

div_dat$Biomass_mg_m2 <- as.numeric(div_dat$Biomass_mg_m2)
site_bm <- diet_bm_dat %>% group_by(site, source) %>% summarise(biomass_ss = sum(biomass_t, na.rm = TRUE)) # Total biomass at site by source

bm_dat_site <- diet_bm_dat %>% group_by(site) %>% summarise(biomass_s = sum(biomass_t, na.rm = TRUE))
bm_dat <- merge(site_bm, bm_dat_site, all = TRUE)                                                                
bm_dat$biomass_per <- bm_dat$biomass_ss/bm_dat$biomass_s # % Biomass by resource

write.csv(bm_dat, "Biomass_dat.csv")
bm_dat %>% ggplot(aes(site, biomass_per, color = source)) + geom_point()

# add zeroes for Hydrurus
site <- c("SFTC", "Grizzly", "Paintbrush")

Hy_add <- data.frame(site = c("SFTC", "Grizzly", "Paintbrush"),  source = rep("Hydrurus", 3), biomass_per = rep(0, 3))

bm_data <- dplyr::bind_rows(bm_dat, Hy_add)

# load environmental data and merge
Envi_data <- read.csv("C://Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//SIF data//MixSIAR//Environ variables//Envi_data.csv")
Envi_data
bm_envi_dat <- merge(Envi_data, bm_data, all = TRUE)

# make data long
bm_envi_dat_long <- bm_envi_dat %>% gather(env_var, value, Elevation:H)
bm_envi_dat_long %>% filter(source == "Hydrurus")
# facet plot
env_var_labs <- c("Chla (mg/m2)", "Chloride (ug/L)", "DO (mg/)L", "Elevation (m)", "Nitrate (ug/L)", "PCA1", "PCA2", "pH", "SPC", "Sulfate (ug/L)", "Tmax (C)", "TSS (g/L)")
names(env_var_labs) <- c("Chla_mg_m2", "chloride", "DO_mg_L", "Elevation", "nitrate", "PCA1", "PCA2", "pH", "SPC", "sulfate", "Tmax", "TSS_g_L")

p <- bm_envi_dat_long %>% filter(env_var %in% c("Chla_mg_m2", "chloride", "DO_mg_L", "Elevation", "nitrate", "PCA1", "PCA2", "pH", "SPC", "sulfate", "Tmax", "TSS_g_L")) %>%
  ggplot() +
  geom_point(aes(value, biomass_per, color = source, pch = site)) + 
  geom_smooth(aes(value, biomass_per, color = source), method='lm') + ylab("% Biomass") +
  scale_shape_manual(values=1:10) + theme_bw() + facet_wrap(~env_var, scales = "free_x", labeller = labeller(env_var = env_var_labs)) +
  theme(axis.title.x=element_blank())
p

ggsave("Biomass envi.png")


# plot of site means for different water sources
dodge <- position_dodge(width=0.8)
p2 <- bm_envi_dat %>% ggplot(aes(Primary_water_source, biomass_per, fill = source)) + geom_boxplot(position = dodge) +
  geom_point(position = dodge, shape = 21, cex = 2) + theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") + xlab(NULL) +
  ylab("Biomass (%)") + labs(fill = "Resource")
p2

ggsave("Biomass by hydro.png", height = 4, width = 5)

### Dirichlet regression
# data
spread_bm_dat <- bm_envi_dat %>% select(site, source, biomass_per) %>% spread(key = source, value = biomass_per) 
#spread_bm_dat[is.na(spread_bm_dat)] <- 0.001
spread_bm_dat
spread_bm_envi_dat <- merge(spread_bm_dat, Envi_data, all = TRUE)
spread_bm_envi_dat

# plot
ggtern(spread_bm_envi_dat, aes(Biofilm, Hydrurus, CPOM, color = Primary_water_source)) + geom_point()

# model

DB <-DR_data(spread_bm_envi_dat[, 2:4])
plot(DB)

m1b <- DirichReg(DB ~ SPC, spread_bm_envi_dat, model = "common")
summary(m1b)

m2b <- DirichReg(DB ~ Tmean, spread_bm_envi_dat, model = "common")
summary(m2b)

m2.5b <- DirichReg(DB ~ Tmax, spread_bm_envi_dat, model = "common")
summary(m2.5b)

m3b <- DirichReg(DB ~ pH, spread_bm_envi_dat, model = "common")
summary(m3b)

m4b <- DirichReg(DB ~ nitrate, spread_bm_envi_dat, model = "common")
summary(m4b)

m5b <- DirichReg(DB ~ TSS_g_L, spread_bm_envi_dat, model = "common")
summary(m5b)

#spread_bm_envi_dat$Primary_water_source <- factor(spread_bm_envi_dat$Primary_water_source, levels=c( "Snowmelt", "Glacier","Subterranean ice"))

m6b <- DirichReg(DB ~ Primary_water_source, spread_bm_envi_dat, model = "common")
summary(m6b)

m7b <- DirichReg(DB ~ Elevation, spread_bm_envi_dat, model = "common")
summary(m7b)

m8b <- DirichReg(DB ~ DO_mg_L, spread_bm_envi_dat, model = "common")
summary(m8b)

m9b <- DirichReg(DB ~ slope, spread_bm_envi_dat, model = "common")
summary(m9b)

m10b <- DirichReg(DB ~ aspect, spread_bm_envi_dat, model = "common")
summary(m10b)

#m11b <- DirichReg(DB ~ fluoride, weights = w, spread_bm_envi_dat, model = "common")
#summary(m11b)

m12b <- DirichReg(DB ~ chloride, spread_bm_envi_dat, model = "common")
summary(m12b)

m13b <- DirichReg(DB ~ sulfate, spread_bm_envi_dat, model = "common")
summary(m13b)

m14b <- DirichReg(DB ~ Elevation + Tmean, spread_bm_envi_dat, model = "common")
summary(m14b) 

m16.2b <- DirichReg(DB ~ Primary_water_source + Tmax, spread_bm_envi_dat, model = "common")
summary(m16.2b)

m20b <- DirichReg(DB ~ PCA1, spread_bm_envi_dat, model = "common")
summary(m20b)
m21b <- DirichReg(DB ~ PCA2, spread_bm_envi_dat, model = "common")
summary(m21b)


### AICc
# AICc = AIC + (2k^2 + 2k)/(n-k-1)
# k is number of parameters
AIC_c <- function(model){-2*model$logLik + 2*model$npar*9/(9-model$npar-1)}
AICc_b <- data.frame(Formula = c("~ SPC", "~ Tmean", "~ Tmax", "~ pH", "~ nitrate", "~ TSS", "~ elevation", "~ DO", "~ slope", "~ aspect", "~ chloride", "~ sulfate", "~ Primary water source", "~ Elevation + Tmean", "~ Primary_water_source + Tmax", "~ PCA1", "~ PCA2"),
                       model = c("m1b", "m2b", "m2.5b", "m3b", "m4b", "m5b","m7b","m8b","m9b","m10b","m12b","m13b", "m6b", "m14b", "m16.2b","m20b","m21b"), AICc =c(AIC_c(m1b), AIC_c(m2b), AIC_c(m2.5b), AIC_c(m3b), AIC_c(m4b), AIC_c(m5b),  
                                                                                                                                                                            AIC_c(m7b), AIC_c(m8b), AIC_c(m9b), AIC_c(m10b), AIC_c(m12b), AIC_c(m13b), AIC_c(m6b), AIC_c(m14b),AIC_c(m16.2b), AIC_c(m20b), AIC_c(m21b)))
AICc_b

# table of AIC values for significant models
#Significant: Tmax, pH, nitrate, DO, aspect and chloride
AICc_b <- AICc_b %>% filter(!Formula %in% c("~ Tmax", "~ DO", "~ pH", "~ nitrate", "~ aspect", "~ slope", "~ elevation", "~ sulfate", "~ PCA1", "~ PCA2"))                                                                                                                                               
AICc_b$AICc <- round(AICc_b$AICc, 1)
AICc_B<-AICc_b[order(AICc_b$AICc),]
AICc_B

write.csv(AICc, "AICc biomass table.csv")

### facet plot

x2<- seq(min(spread_bm_envi_dat$Tmean), max(spread_bm_envi_dat$Tmean), length.out = 1000)
pred2 <- predict(m2b, newdata = data.frame(Tmean=x2))
preds2<- data.frame(source = c(rep("Biofilm", length(pred2[,1])), rep("CPOM", length(pred2[,2])), rep("Hydrurus", length(pred2[,3]))),
                      pred = c(pred2[,1], pred2[,2], pred2[,3]), x = rep(x2, 3), env_var = rep("Tmean", length(pred2[,1])),
                      sig = c(rep("y", length(pred2[,1])), rep("n", length(pred2[,1])), rep("n", length(pred2[,1])) )) 
summary(m12b)

x1<- seq(min(spread_bm_envi_dat$SPC), max(spread_bm_envi_dat$SPC), length.out = 1000)
pred1 <- predict(m1b, newdata = data.frame(SPC=x1))
preds1<- data.frame(source = c(rep("Biofilm", length(pred1[,1])), rep("CPOM", length(pred1[,2])), rep("Hydrurus", length(pred1[,3]))),
                    pred = c(pred1[,1], pred1[,2], pred1[,3]), x = rep(x1, 3), env_var = rep("SPC", length(pred1[,1])),
                    sig = c(rep("y", length(pred1[,1])), rep("y", length(pred1[,1])), rep("y", length(pred1[,1])) )) 

x5<- seq(min(spread_bm_envi_dat$TSS_g_L), max(spread_bm_envi_dat$TSS_g_L), length.out = 1000)
pred5 <- predict(m5b, newdata = data.frame(TSS_g_L=x5))
preds5<- data.frame(source = c(rep("Biofilm", length(pred5[,1])), rep("CPOM", length(pred5[,2])), rep("Hydrurus", length(pred5[,3]))),
                    pred = c(pred5[,1], pred5[,2], pred5[,3]), x = rep(x5, 3), env_var = rep("TSS_g_L", length(pred5[,1])),
                    sig = c(rep("y", length(pred5[,1])), rep("n", length(pred5[,1])), rep("n", length(pred5[,1])) )) 

x12<- seq(min(spread_bm_envi_dat$chloride), max(spread_bm_envi_dat$chloride), length.out = 1000)
pred12 <- predict(m12b, newdata = data.frame(chloride=x12))
preds12<- data.frame(source = c(rep("Biofilm", length(pred12[,1])), rep("CPOM", length(pred12[,2])), rep("Hydrurus", length(pred12[,3]))),
                     pred = c(pred12[,1], pred12[,2], pred12[,3]), x = rep(x12, 3), env_var = rep("chloride", length(pred12[,1])),
                     sig = c(rep("y", length(pred12[,1])), rep("n", length(pred12[,1])), rep("n", length(pred12[,1])) )) 


pred_dat <- rbind(preds2, preds1, preds5, preds12)
head(pred_dat)
pred_dat$env_var <- as.factor(pred_dat$env_var)
class(pred_dat$env_var)
any(is.na(env_dat_long$env_var))

levels(pred_dat$env_var)

# make diet dat long
env_dat_long <- spread_bm_envi_dat %>% select(site, Biofilm, CPOM, Hydrurus, Tmean, SPC, TSS_g_L, chloride) %>% 
  gather(env_var, value, Tmean:chloride) %>%
  select(site, Biofilm, CPOM, Hydrurus, env_var, value) %>% 
  gather(source, Mean, Biofilm:Hydrurus)
head(env_dat_long)
env_dat_long$env_var <- as.factor(env_dat_long$env_var)
env_dat_long$value <- as.numeric(env_dat_long$value)
levels(env_dat_long$env_var)

# plot

cl <- paste("Chloride (", mu,"g/L)")
env_var_labs <- c("SPC", "Tmean (\u00B0C)", "Chloride (ug/L)", "TSS (g/L)")
names(env_var_labs) <- c("SPC", "Tmean", "chloride", "TSS_g_L")
#env_dat_long$env_var <- factor(env_dat_long$env_var,      # Reordering group factor levels
#                         levels = c("PCA1", "PCA2", "pH", "Tmax", "aspect", "SPC"))

p <- ggplot(data = env_dat_long, aes(value, Mean, color = source)) +
  geom_point() + facet_wrap(~env_var, scales = "free_x", labeller = labeller(env_var = env_var_labs)) +
  geom_line(data = pred_dat, aes(x, pred, color = source, linetype = sig), size = 1) +
  scale_color_brewer("Resource", palette = "Dark2") + theme_bw() + ylab("% Biomass") +
  scale_linetype_manual(values=c("dashed", "solid"), guide = "none") + xlab(NULL)
p
ggsave("Dirichlet envi facet biomass.png", width = 5, height = 4)




