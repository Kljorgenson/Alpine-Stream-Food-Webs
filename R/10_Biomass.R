### Teton Invert Biomass
# Calculation and models
# Karen Jorgenson

# Setup
library(tidyverse)
library(DirichletReg)
library(ggtern)

## Create dataframe with diet proportions and biomass
# Load diversity data
div_d <- read.csv("Data/TetonInverts2020.csv")
head(div_d)

div_d <- div_d %>% dplyr::mutate(across(c(L1:L20),~ifelse(is.na(.), 0, 1))) %>% 
  dplyr::mutate(n = rowSums(across(L1:L20), na.rm = T))

# Change taxa to match diet taxonomic groups
div_dat <- div_d %>% dplyr::mutate(taxa = case_when(startsWith(Taxa, "Rhy") ~ "Rhyacophilidae",
                                    Taxa %in% c("Simuliidae Pupae", "Heterocloeon", "Prosimuliium", "Helodon") ~ "Simuliidae",
                                    Taxa == "Non-Tanypodinae" ~ "Midges",
                                     Taxa == "Chironomidae Pupae" ~ "Midges",
                                     Taxa == "Tipula (Arctotipula)" ~ "Tipula",
                                     Taxa %in% c("1Megarcys", "2Megarcys") ~ "Megarcys",
                                    Taxa == "Nematoda" ~ "Oligochaeta",
                                     !Taxa %in% c("Rhyacophilidae", "Simuliidae Pupae", "Heterocloeon", "Prosimuliium", "Helodon", "Chironomidae Pupae", "Non-Tanypodinae", "Tipula (Arctotipula)","1Megarcys", "2Megarcys") ~ Taxa   )) %>% 
  dplyr::select(Stream, taxa, Biomass_mg_m2, n) %>% filter(taxa != "Chironomidae Adult") %>% dplyr::rename(site = Stream)


# Rename stream names so that they match
div_dat$site <- as.factor(div_dat$site)
levels(div_dat$site) <- c("AK Basin",  "Cloudveil", "Delta", "Grizzly", "Middle Teton", "NFTC",  
"Paintbrush", "SFTC", "Skillet", "South Cascade RG", "The Gusher", "Wind Cave") 

# Merge biomass and diet proportions data
data_di <- read.csv("Output/diet_data_clean.csv")
data_di$site <- as.factor(data_di$site)
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
head(data_di)

diet_bm_dat <- merge(data_di, div_dat)
head(diet_bm_dat)
diet_bm_dat$Biomass_mg_m2 <- as.numeric(diet_bm_dat$Biomass_mg_m2)

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

# Add zeroes for Hydrurus
site <- c("SFTC", "Grizzly", "Paintbrush")

Hy_add <- data.frame(site = c("SFTC", "Grizzly", "Paintbrush"),  source = rep("Hydrurus", 3), biomass_per = rep(0, 3))

bm_data <- dplyr::bind_rows(bm_dat, Hy_add)

# Load environmental data and merge
Envi_data <- read.csv("Output//Envi_data.csv")
Envi_data
bm_envi_dat <- merge(Envi_data, bm_data, all = TRUE)


### Dirichlet regression
# Organize data
spread_bm_dat <- bm_envi_dat %>% dplyr::select(site, source, biomass_per) %>% spread(key = source, value = biomass_per) 
spread_bm_dat
spread_bm_envi_dat <- merge(spread_bm_dat, Envi_data, all = TRUE)
spread_bm_envi_dat

# Plot
#ggtern(spread_bm_envi_dat, aes(Biofilm, Hydrurus, CPOM, color = Primary_water_source)) + geom_point()

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

# Table of AIC values for significant models
# Significant: Tmax, pH, nitrate, DO, aspect and chloride
AICc_b <- AICc_b %>% filter(!Formula %in% c("~ Tmax", "~ DO", "~ pH", "~ nitrate", "~ aspect", "~ slope", "~ elevation", "~ sulfate", "~ PCA1", "~ PCA2"))                                                                                                                                               
AICc_b$AICc <- round(AICc_b$AICc, 1)
AICc_B<-AICc_b[order(AICc_b$AICc),]
AICc_B

write.csv(AICc, "Output/AICc biomass table.csv")

## Plot of site mean biomass proportions and model fits by hydrologic source
# Reformat data
means <- spread_bm_envi_dat[,c(1,2,3,4,6)] %>% pivot_longer(cols = 2:4, names_to = "source")
# Extract model fits
df <- as.data.frame(predict(m6b))
df$WS <- spread_env_m_dat$Primary_water_source
df <- df %>% pivot_longer(cols = 1:3)
df %>% ggplot(aes(WS, value, color = name)) + geom_boxplot()

# Plot biomass proportions by hydrologic source
p100 <- means %>% ggplot() +
  geom_boxplot(aes(Primary_water_source, value, col = source)) + 
  geom_point(aes(Primary_water_source, value, col = source), cex = 2, shape = 21, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_color_brewer(palette = "Dark2", labels = c("Biofilm", "CPOM", expression(italic("Hydrurus")))) +
  #annotate(geom="text", x=2, y=0.38, label="a", color="#D95F02") +
  #annotate(geom="text", x=3, y=0.29, label="a", color="#D95F02") +
  labs(col = "Resource") + xlab(NULL) + 
  ylab("Biomass Proportion") + theme_bw() +
  #scale_colour_continuous(guide = "none") +
  geom_boxplot(data = df, aes(WS, value, fill = name), width = 1, position = position_dodge(width = 0.75))

p100




### Facet plot of environmental variable models

x1<- seq(min(spread_bm_envi_dat$SPC), max(spread_bm_envi_dat$SPC), length.out = 1000)
pred1 <- predict(m1b, newdata = data.frame(SPC=x1))
preds1<- data.frame(source = c(rep("Biofilm", length(pred1[,1])), rep("CPOM", length(pred1[,2])), rep("Hydrurus", length(pred1[,3]))),
                    pred = c(pred1[,1], pred1[,2], pred1[,3]), x = rep(x1, 3), env_var = rep("SPC", length(pred1[,1])),
                    sig = c(rep("n", length(pred1[,1])), rep("y", length(pred1[,1])), rep("y", length(pred1[,1])) )) 

x5<- seq(min(spread_bm_envi_dat$TSS_g_L), max(spread_bm_envi_dat$TSS_g_L), length.out = 1000)
pred5 <- predict(m5b, newdata = data.frame(TSS_g_L=x5))
preds5<- data.frame(source = c(rep("Biofilm", length(pred5[,1])), rep("CPOM", length(pred5[,2])), rep("Hydrurus", length(pred5[,3]))),
                    pred = c(pred5[,1], pred5[,2], pred5[,3]), x = rep(x5, 3), env_var = rep("TSS_g_L", length(pred5[,1])),
                    sig = c(rep("y", length(pred5[,1])), rep("n", length(pred5[,1])), rep("n", length(pred5[,1])) )) 

x12<- seq(min(spread_bm_envi_dat$chloride), max(spread_bm_envi_dat$chloride), length.out = 1000)
pred12 <- predict(m12b, newdata = data.frame(chloride=x12))
preds12<- data.frame(source = c(rep("Biofilm", length(pred12[,1])), rep("CPOM", length(pred12[,2])), rep("Hydrurus", length(pred12[,3]))),
                     pred = c(pred12[,1], pred12[,2], pred12[,3]), x = rep(x12, 3), env_var = rep("chloride", length(pred12[,1])),
                     sig = c(rep("n", length(pred12[,1])), rep("n", length(pred12[,1])), rep("y", length(pred12[,1])) )) 


pred_dat.b <- rbind(preds1, preds5, preds12)
head(pred_dat.b)
pred_dat.b$env_var <- as.factor(pred_dat.b$env_var)

# make diet dat long
env_dat_long.b <- spread_bm_envi_dat %>% dplyr::select(site, Biofilm, CPOM, Hydrurus, SPC, TSS_g_L, chloride) %>% 
  gather(env_var, value, SPC:chloride) %>%
  dplyr::select(site, Biofilm, CPOM, Hydrurus, env_var, value) %>% 
  gather(source, Mean, Biofilm:Hydrurus)
head(env_dat_long.b)
env_dat_long.b$env_var <- as.factor(env_dat_long.b$env_var)
env_dat_long.b$value <- as.numeric(env_dat_long.b$value)
levels(env_dat_long.b$env_var)

# plot

cl <- paste("Chloride (", mu,"g/L)")
env_var_labs <- c("SPC", "Tmean (\u00B0C)", "Chloride (ug/L)", "TSS (g/L)")
names(env_var_labs) <- c("SPC", "Tmean", "chloride", "TSS_g_L")
#env_dat_long.b$env_var <- factor(env_dat_long.b$env_var,      # Reordering group factor levels
#                         levels = c("PCA1", "PCA2", "pH", "Tmax", "aspect", "SPC"))

p <- ggplot(data = env_dat_long.b, aes(value, Mean, color = source)) +
  geom_point() + facet_wrap(~env_var, scales = "free_x", labeller = labeller(env_var = env_var_labs)) +
  geom_line(data = pred_dat.b, aes(x, pred, color = source, linetype = sig), size = 1) +
  scale_color_brewer("Resource", palette = "Dark2") + theme_bw() + ylab("% Biomass") +
  scale_linetype_manual(values=c("dashed", "solid"), guide = "none") + xlab(NULL)
p




### Combined plot of diet proportions and biomass
env_dat_long.b2 <- spread_bm_envi_dat %>% dplyr::select(site, Biofilm, CPOM, Hydrurus, SPC, TSS_g_L, chloride, PCA2, Tmean) %>% 
  gather(env_var, value, SPC:chloride) %>%
  dplyr::select(site, Biofilm, CPOM, Hydrurus, env_var, value) %>% 
  gather(source, Mean, Biofilm:Hydrurus)
head(env_dat_long.b2)
env_dat_long.b$env_var <- as.factor(env_dat_long.b2$env_var)
env_dat_long.b$value <- as.numeric(env_dat_long.b2$value)
levels(env_dat_long.b2$env_var)


env_dat_long.b2$type <- "Biomass"
env_dat_long$type <- "Diet"
pred_dat.b$type <- "Biomass"
pred_dat$type <- "Diet"

comb_env_dat_long <- full_join(env_dat_long.b2, env_dat_long)
comb_pred_dat <- full_join(pred_dat.b, pred_dat)

comb_env_dat_long$env_var <- factor(comb_env_dat_long$env_var, levels = c("SPC", "chloride", "TSS_g_L", "Tmean", "PCA2"))
comb_env_dat_long$type <- factor(comb_env_dat_long$type, levels = c("Diet", "Biomass"))
comb_pred_dat$type <- factor(comb_pred_dat$type, levels = c("Diet", "Biomass"))

env.labs <- c("SPC (\U00B5S/cm)", "Chloride (\U00B5g/L)", "TSS (g/L)", "Tmean (\u00B0C)",  "PC2")
names(env.labs) <- c("SPC","chloride", "TSS_g_L","Tmean", "PCA2")
p <- ggplot(data = comb_env_dat_long, aes(value, Mean, color = source)) +
  facet_grid(rows = vars(type), cols = vars(env_var), scales = "free_x", labeller = labeller(env_var = env.labs)) +
  geom_point() + 
  geom_line(data = comb_pred_dat, aes(x, pred, color = source, linetype = sig), size = 1) +
  scale_color_brewer("Resource", palette = "Dark2",labels = c("Biofilm", "CPOM", expression(italic("Hydrurus")))) + theme_bw() + ylab("Proportion") +
  scale_linetype_manual(values=c("dashed", "solid"), guide = "none") + xlab(NULL) +
  theme(strip.background =element_rect(fill="white")) + scale_x_continuous(n.breaks = 3) 

p

ggsave("Output/Paper figures/Dirichlet envi facet biomass and diet.png", width = 7.5, height = 2.6, dpi = 600)

