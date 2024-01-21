### Model diet compositions with the Dirichlet distribution
# By Karen Jorgenson

## Setup
library(DirichletReg)
library(dplyr)
library(tidyverse)
library(ggtern)
library(RVAideMemoire)


## Organize data
Envi_data <- read.csv("Output//Envi_data.csv") # Environmental variables
diet_data <- read.csv("Output//diet_data_clean.csv") # Diet compositions

# Spread data
spread1 <- diet_data %>% select(site, taxa, source, Mean) %>% spread(key = source, value = Mean)

# Enter zeroes for sites without Hydrurus
spread1$Hydrurus[is.na(spread1$Hydrurus)] <- 0

# Calculate site means
diet_means_data <- spread1 %>% group_by(site) %>% summarise(n = length(Biofilm),
                                                           Biofilm = mean(Biofilm, na.rm = TRUE),
                                                           CPOM = mean(CPOM, na.rm = TRUE),
                                                           Hydrurus = mean(Hydrurus, na.rm = TRUE))
diet_means_data

# Combine diet and environmental data
spread_env_mean_dat <- merge(diet_means_data, Envi_data)

# Make all proportions sum to 1 (this is a slight adjustment)
spread_env_m_dat <- spread_env_mean_dat %>% group_by(site) %>% mutate(Biofilm = round(Biofilm/(Biofilm+CPOM+Hydrurus),2),
                                                      CPOM = round(CPOM/(Biofilm+CPOM+Hydrurus),2),
                                                      Hydrurus = round(Hydrurus/(Biofilm+CPOM+Hydrurus),2))
spread_env_m_dat


spread_env_m_dat %>% group_by(site) %>% summarise(tot = sum(Biofilm+CPOM+Hydrurus)) # Check that they sum to 1

## Test for multivariate normality
names(spread_env_m_dat)
mshapiro.test(t(spread_env_m_dat[,c(9,12,13,15,20)])) # Test environmental variables: pass
mshapiro.test(t(spread_env_m_dat[,c(3:5)])) # Test diet compositions: barely fails

### Construct models

# Plot site diet means
ggtern(spread_env_m_dat, aes(Biofilm, Hydrurus, CPOM, color = Primary_water_source)) + geom_point() +
  theme_bw() + scale_color_manual(values = 1:3, name = "Hydrologic source")

# Model using DirichletReg
# Common parameterization

DD <-DR_data(spread_env_m_dat[, c(3:5)]) # Select response variables
plot(DD)

# Models with single explanatory variables
m6c <- DirichReg(DD ~ Primary_water_source, spread_env_m_dat, model = "common")
summary(m6c)

m1c <- DirichReg(DD ~ SPC, spread_env_m_dat, model = "common")
summary(m1c)


m2c <- DirichReg(DD ~ Tmean, spread_env_m_dat, model = "common")
summary(m2c)

m2.5c <- DirichReg(DD ~ Tmax, spread_env_m_dat, model = "common")
summary(m2.5c)

m3c <- DirichReg(DD ~ pH, spread_env_m_dat, model = "common") 
summary(m3c) 

m4c <- DirichReg(DD ~ nitrate, spread_env_m_dat, model = "common")
summary(m4c)

m5c <- DirichReg(DD ~ TSS_g_L, spread_env_m_dat, model = "common")
summary(m5c)

m7c <- DirichReg(DD ~ Elevation, spread_env_m_dat, model = "common")
summary(m7c)

m8c <- DirichReg(DD ~ DO_mg_L, spread_env_m_dat, model = "common")
summary(m8c)

m9c <- DirichReg(DD ~ slope, spread_env_m_dat, model = "common")
summary(m9c)

m10c <- DirichReg(DD ~ aspect, spread_env_m_dat, model = "common")
summary(m10c)

m11c <- DirichReg(DD ~ fluoride, spread_env_mean_dat, model = "common")
summary(m11c) # Doesn't converge

m12c <- DirichReg(DD ~ chloride, spread_env_m_dat, model = "common")
summary(m12c)

m13c <- DirichReg(DD ~ sulfate, spread_env_m_dat, model = "common")
summary(m13c)

# Models with multiple environmental variables
m14c <- DirichReg(DD ~ Elevation + Tmean, spread_env_m_dat, model = "common")
summary(m14c)

#m14.1c <- DirichReg(DD ~ SPC + pH + fluoride + slope + Elevation, weights = w, spread_env_mean_dat, model = "common")
#summary(m14.1c)
#m14.2c <- DirichReg(DD ~ slope + SPC +fluoride + chloride + pH+ Elevation, weights = w, spread_env_mean_dat, model = "common")
#summary(m14.2c)
#m14.3c <- DirichReg(DD ~ slope + SPC +fluoride + nitrate +pH+ Elevation, weights = w, spread_env_mean_dat, model = "common")
#summary(m14.3c)

#m16.1c <- DirichReg(DD ~ Primary_water_source  + Elevation + TSS_g_L+ Tmax +DO_mg_L, weights = w, spread_env_m_dat, model = "common")
#summary(m16.1c)

# Models with hydrologic source and environmental variables
m16.2c <- DirichReg(DD ~ Primary_water_source + Tmax, spread_env_m_dat, model = "common")
summary(m16.2c)
m16.3c <- DirichReg(DD ~ Primary_water_source + Tmean, spread_env_m_dat, model = "common")
summary(m16.3c)

# Models with PCA components
m20c <- DirichReg(DD ~ PCA1, spread_env_m_dat, model = "common")
summary(m20c)
m21c <- DirichReg(DD ~ PCA2, spread_env_m_dat, model = "common")
summary(m21c)
m22c <- DirichReg(DD ~ PCA2 + PCA1, spread_env_m_dat, model = "common")
summary(m22c)

# Model comparison using corrected AIC
# npar = number of parameters
# sample size = 9
AIC_c <- function(model){-2*model$logLik + 2*model$npar*9/(9-model$npar-1)}

AICc_dat <- data.frame(Formula = c("~ SPC", "~ Tmean", "~ Tmax", "~ pH", "~ nitrate", "~ TSS", "~ elevation", "~ DO", "~ slope", "~ aspect", "~ chloride", "~ sulfate", "~ Primary water source", "~ Elevation + Tmean", "~ Primary_water_source + Tmax", "~ PCA1", "~ PCA2", "~ PCA1 + PCA2"),
                       model = c("m1c", "m2c", "m2.5c", "m3c", "m4c", "m5c","m7c","m8c","m9c","m10c","m12c","m13c", "m6c", "m14c", "m16.2c","m20c","m21c", "m22c"), AICc =c(AIC_c(m1c), AIC_c(m2c), AIC_c(m2.5c), AIC_c(m3c), AIC_c(m4c), AIC_c(m5c),  
                                                                                                                                                                    AIC_c(m7c), AIC_c(m8c), AIC_c(m9c), AIC_c(m10c), AIC_c(m12c), AIC_c(m13c), AIC_c(m6c), AIC_c(m14c),AIC_c(m16.2c), AIC_c(m20c), AIC_c(m21c),AIC_c(m22c)) )
AICc_dat

# Table of AIC values for significant models
# Significant variables: Tmax, pH, nitrate, TSS, elevation, DO, aspect and chloride
AICc_d <- AICc_dat %>% filter(!Formula %in% c("~slope", "~nitrate", "~TSS", "~ PCA2", "~ PCA1 + PCA2"))                                                                                                                                               
AICc_d$AICc <- round(AICc_d$AICc, 1)
AICc<-AICc_d[order(AICc_d$AICc),]
AICc

write.csv(AICc, "Output//AICc table.csv")


## Plot of site mean diet composition and model fits by hydrologic source
# Reformat data
means <- spread_env_m_dat[,c(1,3,4,5,7)] %>% pivot_longer(cols = 2:4, names_to = "source")
# Extract model fits
df <- as.data.frame(predict(m6c))
df$WS <- spread_env_m_dat$Primary_water_source
df <- df %>% pivot_longer(cols = 1:3)
df %>% ggplot(aes(WS, value, color = name)) + geom_boxplot()

# Plot diet proportions by hydrologic source
p100 <- means %>% ggplot() +
  geom_boxplot(aes(Primary_water_source, value, col = source)) + 
  geom_point(aes(Primary_water_source, value, col = source), cex = 2, shape = 21, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_color_brewer(palette = "Dark2") +
  #annotate(geom="text", x=2, y=0.38, label="a", color="#D95F02") +
  #annotate(geom="text", x=3, y=0.29, label="a", color="#D95F02") +
  labs(fill = "Resource") + xlab(NULL) + 
  ylab("Diet Proportion") + theme_bw() +
  #scale_colour_continuous(guide = "none") +
  geom_boxplot(data = df, aes(WS, value, fill = name), width = 1, position = position_dodge(width = 0.75))
  
p100
ggsave("Output//Paper figures//Diet by Hydro.png", width = 5, height = 3.6)



                                                                              
## Plot model results for representative variables
# Create big dataframe for plotting
# Predicted data
a <- rep(1,1000) # Set model weights to 1 to not weight the points

x10<- seq(min(spread_env_mean_dat$aspect), max(spread_env_mean_dat$aspect), length.out = 1000)
pred10 <- predict(m10c, newdata = data.frame(aspect=x10, w = a))
preds10<- data.frame(source = c(rep("Biofilm", length(pred10[,1])), rep("CPOM", length(pred10[,2])), rep("Hydrurus", length(pred10[,3]))),
                    pred = c(pred10[,1], pred10[,2], pred10[,3]), x = rep(x10, 3), env_var = rep("aspect", length(pred10[,1])),
                    sig = c(rep("n", length(pred10[,1])), rep("y", length(pred10[,1])), rep("n", length(pred10[,1])) )) 


x2.5<- seq(min(spread_env_mean_dat$Tmax), max(spread_env_mean_dat$Tmax), length.out = 1000)
pred2.5 <- predict(m2.5c, newdata = data.frame(Tmax=x2.5, w = a))
preds2.5<- data.frame(source = c(rep("Biofilm", length(pred2.5[,1])), rep("CPOM", length(pred2.5[,2])), rep("Hydrurus", length(pred2.5[,3]))),
                    pred = c(pred2.5[,1], pred2.5[,2], pred2.5[,3]), x = rep(x2.5, 3), env_var = rep("Tmax", length(pred2.5[,1])),
                    sig = c(rep("y", length(pred2.5[,1])), rep("y", length(pred2.5[,1])), rep("n", length(pred2.5[,1])) )) 
summary(m20c)
x3<- seq(min(spread_env_mean_dat$pH), max(spread_env_mean_dat$pH), length.out = 1000)
pred3 <- predict(m3c, newdata = data.frame(pH=x3, w = a))
preds3<- data.frame(source = c(rep("Biofilm", length(pred3[,1])), rep("CPOM", length(pred3[,2])), rep("Hydrurus", length(pred3[,3]))),
                    pred = c(pred3[,1], pred3[,2], pred3[,3]), x = rep(x3, 3), env_var = rep("pH", length(pred3[,1])),
                    sig = c(rep("y", length(pred3[,1])), rep("y", length(pred3[,1])), rep("y", length(pred3[,1])) )) 

x1<- seq(min(spread_env_mean_dat$SPC), max(spread_env_mean_dat$SPC), length.out = 1000)
pred1 <- predict(m1c, newdata = data.frame(SPC=x1, w = a))
preds1<- data.frame(source = c(rep("Biofilm", length(pred1[,1])), rep("CPOM", length(pred1[,2])), rep("Hydrurus", length(pred1[,3]))),
                    pred = c(pred1[,1], pred1[,2], pred1[,3]), x = rep(x1, 3), env_var = rep("SPC", length(pred1[,1])),
                    sig = c(rep("n", length(pred1[,1])), rep("y", length(pred1[,1])), rep("y", length(pred1[,1])) )) 

x7<- seq(min(spread_env_mean_dat$Elevation), max(spread_env_mean_dat$Elevation), length.out = 1000)
pred7 <- predict(m7c, newdata = data.frame(Elevation=x7, w = a))
preds7<- data.frame(source = c(rep("Biofilm", length(pred7[,1])), rep("CPOM", length(pred7[,2])), rep("Hydrurus", length(pred7[,3]))),
                    pred = c(pred7[,1], pred7[,2], pred7[,3]), x = rep(x7, 3), env_var = rep("Elevation", length(pred7[,1])),
                    sig = c(rep("n", length(pred7[,1])), rep("y", length(pred7[,1])), rep("n", length(pred7[,1])) )) 

x20<- seq(min(spread_env_mean_dat$PCA1), max(spread_env_mean_dat$PCA1), length.out = 1000)
pred20 <- predict(m20c, newdata = data.frame(PCA1=x20, w = a))
preds20<- data.frame(source = c(rep("Biofilm", length(pred20[,1])), rep("CPOM", length(pred20[,2])), rep("Hydrurus", length(pred20[,3]))),
                    pred = c(pred20[,1], pred20[,2], pred20[,3]), x = rep(x20, 3), env_var = rep("PCA1", length(pred20[,1])),
                    sig = c(rep("n", length(pred20[,1])), rep("y", length(pred20[,1])), rep("n", length(pred20[,1])) )) 

confint(m20c)
con <- predict(m20c, newdata = data.frame(PCA1=x20, w = a), interval="confidence")
head(con)
head(preds20)
summary(m20c)

# Make dataframe of predicted data
pred_dat <- rbind(preds2.5, preds3, preds1, preds7, preds10, preds20)
head(pred_dat)
pred_dat$env_var <- as.factor(pred_dat$env_var)
class(pred_dat$env_var)
any(is.na(env_dat_long$env_var))

levels(pred_dat$env_var)

head(spread_env_mean_dat)
# Make dataframe long: gather environmental variables
env_dat_long <- spread_env_mean_dat %>% select(site, Biofilm, CPOM, Hydrurus, Tmax, pH, SPC, aspect, Elevation, PCA1) %>% 
  gather(env_var, value, Tmax:PCA1) %>%
  select(site, Biofilm, CPOM, Hydrurus, env_var, value) %>% 
  gather(source, Mean, Biofilm:Hydrurus)
head(env_dat_long)
env_dat_long$env_var <- as.factor(env_dat_long$env_var)
env_dat_long$value <- as.numeric(env_dat_long$value)
levels(env_dat_long$env_var)

# Facet plot with 6 models
env_var_labs <- c("pH","Tmax (\u00B0C)","Aspect","SPC", "Elevation (m)", "PC1") # Set labels for facets
names(env_var_labs) <- c("pH","Tmax", "aspect","SPC", "Elevation", "PCA1")
env_dat_long$env_var <- factor(env_dat_long$env_var,      # Reorder factor levels
                         levels = c("pH", "Tmax", "aspect", "SPC", "Elevation", "PCA1"))


p <- ggplot(data = env_dat_long, aes(value, Mean, color = source)) +
  geom_point() + facet_wrap(~env_var, scales = "free_x", labeller = labeller(env_var = env_var_labs)) +
  geom_line(data = pred_dat, aes(x, pred, color = source, linetype = sig), size = 1) +
  scale_color_brewer("Resource", palette = "Dark2") + theme_bw() + ylab("Diet Proportion") +
  scale_linetype_manual(values=c("dashed", "solid"), guide = "none") + xlab(NULL) # Make lines dashed for non-significant relationships
p

ggsave("Output//Paper figures//Dirichlet envi facet.png", width = 7, height = 4)


## Taxa specific models
n <- spread %>% group_by(taxa) %>% summarise(n = length(taxa))
n

# Make all proportions sum to 1
spread_2 <- spread %>% group_by(site, taxa) %>% mutate(Biofilm = round(Biofilm/(Biofilm+CPOM+Hydrurus),2),
                                                                      CPOM = round(CPOM/(Biofilm+CPOM+Hydrurus),2),
                                                                      Hydrurus = round(Hydrurus/(Biofilm+CPOM+Hydrurus),2))

# Create dataframe
spread_2 %>% group_by(site, taxa) %>% summarise(tot = sum(Biofilm+CPOM+Hydrurus))
spread_taxa <- merge(spread_2, Envi_data)

# Select taxa and hydrologic sources
spread_t <- filter(spread_taxa, taxa == "Midges", Primary_water_source %in% c("Glacier", "Snowmelt"))

# Plot
ggtern(spread_t, aes(Biofilm, Hydrurus, CPOM, color = Primary_water_source)) + geom_point()

# Model
DT <-DR_data(spread_t[, c(3,4,5)]) # Select response variables
plot(DT)

tm1 <- DirichReg(DT ~ Primary_water_source, spread_t, model = "common")
summary(tm1)

# Compare use of CPOM between Lednia and Zapada
data_diet %>% filter(taxa %in% c("Lednia", "Zapada")) %>% group_by(taxa, source) %>% summarise(max = max(Mean),
                                                                                               mean = mean(Mean),
                                                                                               sd = sd(Mean))



### Model checks

# Explore residuals
Resid <- resid(dm1)
res <- data.frame(Resid[,1], Resid[,2], Resid[,3])

par(mfrow = c(1,2))
plot(fitted(dm1)[, 2], residuals(dm1, "standardized")[, 2], pch = 21, 
     ylim = c(-3, 3), xlab = "Fitted", ylab = "Standardized Residuals", 
     cex = 0.75, lwd = 0.5)
abline(h = 0, lty = 2)

plot(spread_env_dat$Tmean, residuals(dm1, "standardized")[, 2], pch = 21, 
     ylim = c(-3, 3), xlab = "Tmean", ylab = "Standardized Residuals", 
     cex = 0.75, lwd = 0.5)
abline(h = 0, lty = 2)

# Plot fitted residuals for 3 models
png("Output//Paper figures//Fitted residuals.png", width = 10, height = 6, units = "in", res = 1200)

par(mfrow= c(3,5), mar = c(4, 4, 2, 0.2))

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.5, paste("Tmax"), cex = 1.25, lwd = 0.5)

plot(fitted(m2.5c)[, 1], residuals(m2.5c, "standardized")[, 1], pch = 21, 
     ylim = c(-4, 4), main = "Biofilm", xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m2.5c)[, 2], residuals(m2.5c, "standardized")[, 2], pch = 21, 
     ylim = c(-4, 4), main = "CPOM", xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m2.5c)[, 3], residuals(m2.5c, "standardized")[, 3], pch = 21, 
     ylim = c(-4, 4), main = "Hydrurus", xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')


plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.5, paste("SPC"), cex = 1.25, lwd = 0.5)

plot(fitted(m1c)[, 1], residuals(m1c, "standardized")[, 1], pch = 21, 
     ylim = c(-4, 4), xlab = "", ylab = "Standardized Residuals", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m1c)[, 2], residuals(m1c, "standardized")[, 2], pch = 21, 
     ylim = c(-4, 4), xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m1c)[, 3], residuals(m1c, "standardized")[, 3], pch = 21, 
     ylim = c(-4, 4), xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')


plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.6, paste("Hydrologic"), cex = 1.25, lwd = 0.5)
text(x = 0.6, y = 0.4, paste("source"), cex = 1.25, lwd = 0.5)

plot(fitted(m6c)[, 1], residuals(m6c, "standardized")[, 1], pch = 21, 
     ylim = c(-5, 5), xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m6c)[, 2], residuals(m6c, "standardized")[, 2], pch = 21, 
     ylim = c(-5, 5), xlab = "Fitted Values", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(fitted(m6c)[, 3], residuals(m6c, "standardized")[, 3], pch = 21, 
     ylim = c(-5, 5), xlab = "", ylab = "", 
     cex = 1, lwd = 1)
abline(h = 0, lty = 2)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

dev.off() 

# QQ plots for 3 models
png("Output//Paper figures//QQ plot.png", width = 10, height = 6, units = "in", res = 1200)

par(mfrow= c(3,5), mar = c(4, 4, 2, 0.2))

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.5, paste("Tmax"), cex = 1.25, lwd = 0.5)

qqnorm(residuals(m2.5c, "standardized")[, 1], xlim = c(4,-4), ylim = c(4,-4), 
       main = "Biofilm",cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m2.5c, "standardized")[, 2], xlim = c(4,-4), ylim = c(4,-4), 
       main = "CPOM",cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m2.5c, "standardized")[, 3], xlim = c(4,-4), ylim = c(4,-4), 
        main = "Hydrurus", cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.5, paste("SPC"), cex = 1.25, lwd = 0.5)

qqnorm(residuals(m1c, "standardized")[, 1], xlim = c(3,-3), ylim = c(3,-3), 
        main = NULL, cex = 1, lwd = 1, xlab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m1c, "standardized")[, 2], xlim = c(3,-3), ylim = c(3,-3), 
       main = NULL, cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m1c, "standardized")[, 3], xlim = c(3,-3), ylim = c(3,-3), 
       main = NULL, cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.6, y = 0.6, paste("Hydrologic"), cex = 1.25, lwd = 0.5)
text(x = 0.6, y = 0.4, paste("source"), cex = 1.25, lwd = 0.5)

qqnorm(residuals(m6c, "standardized")[, 1], xlim = c(5,-5), ylim = c(5,-5), 
        main = NULL, cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m6c, "standardized")[, 2], xlim = c(5,-5), ylim = c(5,-5), 
       main = NULL, cex = 1, lwd = 1, ylab = "")
abline(0, 1, lwd = 1)
qqnorm(residuals(m6c, "standardized")[, 3], xlim = c(5,-5), ylim = c(5,-5), 
        main = NULL, cex = 1, lwd = 1, xlab = "", ylab = "")
abline(0, 1, lwd = 1)
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')

dev.off() 

