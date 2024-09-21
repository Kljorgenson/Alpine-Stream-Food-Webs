### Model diet compositions with the Dirichlet distribution
# By Karen Jorgenson

## Setup
library(DirichletReg)
library(dplyr)
library(tidyverse)
library(ggtern)
library(RVAideMemoire)


## Organize data
Envi_data <- read.csv("Output/Envi_data.csv") # Environmental variables
diet_data <- read.csv("Output/diet_data_clean.csv") # Diet compositions

# Spread data
spread1 <- diet_data %>% dplyr::select(site, taxa, source, Mean) %>% spread(key = source, value = Mean)

# Enter zeroes for sites without Hydrurus
spread1$Hydrurus[is.na(spread1$Hydrurus)] <- 0

# Calculate site means
diet_means_data <- spread1 %>% group_by(site) %>% summarise(n = length(Biofilm),
                                                           Biofilm = mean(Biofilm, na.rm = TRUE),
                                                           CPOM = mean(CPOM, na.rm = TRUE),
                                                           Hydrurus = mean(Hydrurus, na.rm = TRUE))
diet_means_data

# Combine diet and environmental data
spread_env_m_dat <- merge(diet_means_data, Envi_data)

### Construct models

# Reorder factors for hydrologic source
spread_env_m_dat$Primary_water_source <- factor(spread_env_m_dat$Primary_water_source, levels = c("Subterranean ice", "Snowmelt", "Glacier"))


# Model using DirichletReg
# Common parameterization

DD <-DR_data(spread_env_m_dat[, c(3:5)]) # Select response variables
plot(DD)

# Models with single explanatory variables
m6c <- DirichReg(DD ~ Primary_water_source, spread_env_m_dat, model = "common")
summary(m6c)
fitted(m6c, mu = TRUE, alpha = T, phi = T)
confint(m6c)


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

#m11c <- DirichReg(DD ~ fluoride, spread_env_m_dat, model = "common")
#summary(m11c) # Doesn't converge

m12c <- DirichReg(DD ~ chloride, spread_env_m_dat, model = "common")
summary(m12c)

m13c <- DirichReg(DD ~ sulfate, spread_env_m_dat, model = "common")
summary(m13c)

# Models with multiple environmental variables
m14c <- DirichReg(DD ~ SPC + Tmean, spread_env_m_dat, model = "common")
summary(m14c)


# Models with hydrologic source and environmental variables
m16.2c <- DirichReg(DD ~ Primary_water_source + Tmax, spread_env_m_dat, model = "common")
summary(m16.2c)


# Models with PCA components
m20c <- DirichReg(DD ~ PCA1, spread_env_m_dat, model = "common")
summary(m20c)
m21c <- DirichReg(DD ~ PCA2, spread_env_m_dat, model = "common")
summary(m21c)

# Model comparison using corrected AIC
# npar = number of parameters
# sample size = 9
AIC_c <- function(model){-2*model$logLik + 2*model$npar*9/(9-model$npar-1)}

AICc_d <- data.frame(Formula = c("~ SPC", "~ Tmean", "~ Tmax", "~ pH", "~ nitrate", "~ TSS", "~ elevation", "~ DO", "~ slope", "~ aspect", "~ chloride", "~ sulfate", "~ Primary water source", "~ SPC + Tmean", "~ Primary_water_source + Tmax", "~ PCA1", "~ PCA2"),
                       model = c("m1c", "m2c", "m2.5c", "m3c", "m4c", "m5c","m7c","m8c","m9c","m10c","m12c","m13c", "m6c", "m14c", "m16.2c","m20c","m21c"), AICc =c(AIC_c(m1c), AIC_c(m2c), AIC_c(m2.5c), AIC_c(m3c), AIC_c(m4c), AIC_c(m5c),  
                                                                                                                                                                    AIC_c(m7c), AIC_c(m8c), AIC_c(m9c), AIC_c(m10c), AIC_c(m12c), AIC_c(m13c), AIC_c(m6c), AIC_c(m14c),AIC_c(m16.2c), AIC_c(m20c), AIC_c(m21c)) )
AICc_d

# Table of AIC values for significant models
# Significant variables: Tmax, pH, nitrate, TSS, elevation, DO, aspect and chloride
AICc_d$AICc <- round(AICc_d$AICc, 1)
AICc<-AICc_d[order(AICc_d$AICc),]
AICc

write.csv(AICc, "Output/AICc table.csv")


                                                                              
## Plot model results for representative variables
# Create big dataframe for plotting
# Predicted data
a <- rep(1,1000) # Set model weights to 1 to not weight the points


x2<- seq(min(spread_env_m_dat$Tmean), max(spread_env_m_dat$Tmean), length.out = 1000)
pred2 <- predict(m2c, newdata = data.frame(Tmean=x2, w = a))
preds2<- data.frame(source = c(rep("Biofilm", length(pred2[,1])), rep("CPOM", length(pred2[,2])), rep("Hydrurus", length(pred2[,3]))),
                    pred = c(pred2[,1], pred2[,2], pred2[,3]), x = rep(x2, 3), env_var = rep("Tmean", length(pred2[,1])),
                    sig = c(rep("y", length(pred2[,1])), rep("y", length(pred2[,1])), rep("n", length(pred2[,1])) )) 
summary(m5c)
x3<- seq(min(spread_env_m_dat$TSS_g_L), max(spread_env_m_dat$TSS_g_L), length.out = 1000)
pred3 <- predict(m5c, newdata = data.frame(TSS_g_L=x3, w = a))
preds3<- data.frame(source = c(rep("Biofilm", length(pred3[,1])), rep("CPOM", length(pred3[,2])), rep("Hydrurus", length(pred3[,3]))),
                    pred = c(pred3[,1], pred3[,2], pred3[,3]), x = rep(x3, 3), env_var = rep("TSS_g_L", length(pred3[,1])),
                    sig = c(rep("y", length(pred3[,1])), rep("y", length(pred3[,1])), rep("n", length(pred3[,1])) )) 

x1<- seq(min(spread_env_m_dat$SPC), max(spread_env_m_dat$SPC), length.out = 1000)
pred1 <- predict(m1c, newdata = data.frame(SPC=x1, w = a))
preds1<- data.frame(source = c(rep("Biofilm", length(pred1[,1])), rep("CPOM", length(pred1[,2])), rep("Hydrurus", length(pred1[,3]))),
                    pred = c(pred1[,1], pred1[,2], pred1[,3]), x = rep(x1, 3), env_var = rep("SPC", length(pred1[,1])),
                    sig = c(rep("n", length(pred1[,1])), rep("y", length(pred1[,1])), rep("y", length(pred1[,1])) )) 

x7<- seq(min(spread_env_m_dat$chloride), max(spread_env_m_dat$chloride), length.out = 1000)
pred7 <- predict(m12c, newdata = data.frame(chloride=x7, w = a))
preds7<- data.frame(source = c(rep("Biofilm", length(pred7[,1])), rep("CPOM", length(pred7[,2])), rep("Hydrurus", length(pred7[,3]))),
                    pred = c(pred7[,1], pred7[,2], pred7[,3]), x = rep(x7, 3), env_var = rep("chloride", length(pred7[,1])),
                    sig = c(rep("n", length(pred7[,1])), rep("y", length(pred7[,1])), rep("y", length(pred7[,1])) )) 

x20<- seq(min(spread_env_m_dat$PCA2), max(spread_env_m_dat$PCA2), length.out = 1000)
pred20 <- predict(m21c, newdata = data.frame(PCA2=x20, w = a))
preds20<- data.frame(source = c(rep("Biofilm", length(pred20[,1])), rep("CPOM", length(pred20[,2])), rep("Hydrurus", length(pred20[,3]))),
                    pred = c(pred20[,1], pred20[,2], pred20[,3]), x = rep(x20, 3), env_var = rep("PCA2", length(pred20[,1])),
                    sig = c(rep("y", length(pred20[,1])), rep("n", length(pred20[,1])), rep("n", length(pred20[,1])) )) 


# Make dataframe of predicted data
pred_dat <- rbind(preds2, preds3, preds1, preds7, preds20)
head(pred_dat)
pred_dat$env_var <- as.factor(pred_dat$env_var)

# Make dataframe long: gather environmental variables
env_dat_long <- spread_env_m_dat %>% dplyr::select(site, Biofilm, CPOM, Hydrurus, Tmean, chloride, SPC, TSS_g_L, PCA2) %>% 
  gather(env_var, value, Tmean:PCA2) %>%
  dplyr::select(site, Biofilm, CPOM, Hydrurus, env_var, value) %>% 
  gather(source, Mean, Biofilm:Hydrurus)
head(env_dat_long)
env_dat_long$env_var <- as.factor(env_dat_long$env_var)
env_dat_long$value <- as.numeric(env_dat_long$value)
levels(env_dat_long$env_var)

# Facet plot with 6 models
cl <- expression(paste("Chloride (", mu,"g/L)"))
env_var_labs <- c("SPC",cl, "TSS (g/L)","Tmean (\u00B0C)", "PCA2") # Set labels for facets
names(env_var_labs) <- c("SPC","chloride", "TSS_g_L","Tmean", "PCA2")
env_dat_long$env_var <- factor(env_dat_long$env_var,      # Reorder factor levels
                         levels = c("SPC","chloride", "TSS_g_L","Tmean", "PCA2"))


p <- ggplot(data = env_dat_long, aes(value, Mean, color = source)) +
  facet_wrap(~env_var, scales = "free_x", nrow = 1, labeller = labeller(env_var = env_var_labs)) + geom_point() + 
  geom_line(data = pred_dat, aes(x, pred, color = source, linetype = sig), size = 1) +
  scale_color_brewer("Resource", palette = "Dark2") + theme_bw() + ylab("Diet Proportion") +
  scale_linetype_manual(values=c("dashed", "solid"), guide = "none") + xlab(NULL) # Make lines dashed for non-significant relationships
p

ggsave("Output/Paper figures/Dirichlet envi facet.png", width = 7, height = 4)








## Taxa specific models
n <- spread1 %>% group_by(taxa) %>% summarise(n = length(taxa))
n

# Create dataframe
spread1 %>% group_by(site, taxa) %>% summarise(tot = sum(Biofilm+CPOM+Hydrurus))
spread_taxa <- merge(spread1, Envi_data)

# Select taxa and hydrologic sources
spread_t <- filter(spread_taxa, taxa == "Zapada", Primary_water_source %in% c("Glacier", "Subterranean ice"))

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
png("Output/Paper figures/Fitted residuals.png", width = 10, height = 6, units = "in", res = 1200)

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
png("Output/Paper figures/QQ plot.png", width = 10, height = 6, units = "in", res = 1200)

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

