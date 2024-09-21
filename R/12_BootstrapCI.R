## Bootstrap confidence interals for diet and biomass by hydrologic source
# First run 09_Dirichlet_models.R and 10_Biomass.R

# Setup
library(Compositional)
library(MCMCpack)
library(data.table)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DirichletReg)

### Bootstrap Diet proportions

# Bootstrap function
boot.fun <- function(x){
  
dat <- spread_env_m_dat[, c(1,3:5, 7)]

# Separate by hydrologic source
sub <- dat %>% filter(Primary_water_source == "Subterranean ice")
glacier <- dat %>% filter(Primary_water_source == "Glacier")
snow <- dat %>% filter(Primary_water_source == "Snowmelt")

sub.dat <- sub[sample(nrow(sub), 2, replace = T),]
glacier.dat <- glacier[sample(nrow(glacier), 3, replace = T),]
snow.dat <-snow[sample(nrow(snow), 4, replace = T),]

data <- rbind(sub.dat, glacier.dat, snow.dat)

# Dirichlet regression model
DD <-DR_data(data[,2:4])
mod <- DirichReg(DD ~ Primary_water_source, data, model = "common")

df <- as.data.frame(fitted(mod, mu = TRUE, alpha = F, phi = F))
df$Primary_water_source <- c(rep("Subterranean ice", 2), rep("Glacier", 3), rep("Snowmelt", 4))
df <- df %>% unique()
return(df)
}

# Run 100 times
list <- lapply(1:100, boot.fun)
boot <- rbindlist(list)
head(boot)

# Summarise output
boot.ci <- boot  %>% pivot_longer(cols = 1:3, names_to = "source") %>% group_by(Primary_water_source, source) %>%
  summarise(p2.5 = quantile(value, probs = 0.025),
            p.50 = quantile(value, probs = 0.5),
            p97.5 = quantile(value, probs = 0.975),
            SD = sd(value))

# Raw data points by site
means <- spread_env_m_dat[,c(1,3,4,5,7)] %>% pivot_longer(cols = 2:4, names_to = "source")
means$Primary_water_source <- factor(means$Primary_water_source, levels = c( "Glacier", "Snowmelt", "Subterranean ice"))# Reorder factors for hydrologic source

## Fitted values from hydrologic source model
df <- as.data.frame(fitted(m6c, mu = TRUE)) 
df$Primary_water_source <- spread_env_m_dat$Primary_water_source
df <- df %>% pivot_longer(cols = 1:3, names_to = "source") %>% unique()


# Plot
p100 <- means %>% ggplot() +
  geom_point(aes(x=Primary_water_source, y=value, color = source), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2", labels = c("Biofilm", "CPOM", expression(italic("Hydrurus")))) +
  labs(col = "Resource") + xlab(NULL) + 
  ylab("Diet Proportion") + theme_bw() +
  geom_point(data = df, aes(Primary_water_source, value, color = source), size = 8, position = position_dodge(width = 0.75), shape = 95) +
  geom_errorbar(data = boot.ci, aes(x = Primary_water_source, y = p.50, ymin = p2.5, ymax = p97.5, color = source), width = 0.3, position = position_dodge(width = 0.75), key_glyph = "point") +
  theme(legend.position = "bottom")

p100

ggsave("Output//Paper figures//legend.png", width = 4, height = 3.5, dpi = 400)






### Bootstrap biomass models

boot.fun <- function(x){
  
  dat <- spread_bm_envi_dat[, c(1:4,6)]
  
  sub <- dat %>% filter(Primary_water_source == "Subterranean ice")
  glacier <- dat %>% filter(Primary_water_source == "Glacier")
  snow <- dat %>% filter(Primary_water_source == "Snowmelt")
  
  sub.dat <- sub[sample(nrow(sub), 2, replace = T),]
  glacier.dat <- glacier[sample(nrow(glacier), 3, replace = T),]
  snow.dat <-snow[sample(nrow(snow), 4, replace = T),]
  
  data <- rbind(sub.dat, glacier.dat, snow.dat)

  DD <-DR_data(data[,2:4])
  m6c <- DirichReg(DD ~ Primary_water_source, data, model = "common")
  
  df <- as.data.frame(fitted(m6c, mu = TRUE, alpha = F, phi = F))
  df$Primary_water_source <- c(rep("Subterranean ice", 2), rep("Glacier", 3), rep("Snowmelt", 4))
  df <- df %>% unique()
  return(df)
}


list.b <- lapply(1:100, boot.fun)
boot.b <- rbindlist(list.b)
head(boot.b)

boot.ci.b <- boot.b  %>% pivot_longer(cols = 1:3, names_to = "source") %>% group_by(Primary_water_source, source) %>%
  summarise(p2.5 = quantile(value, probs = 0.025),
            p.50 = quantile(value, probs = 0.5),
            p97.5 = quantile(value, probs = 0.975))


## Fitted values from hydrologic source model
df.b <- as.data.frame(fitted(m6b, mu = TRUE)) 
df.b$Primary_water_source <- spread_bm_envi_dat$Primary_water_source
df.b <- df.b %>% pivot_longer(cols = 1:3, names_to = "source") %>% unique()

# Raw data points by site
means.b <- spread_bm_envi_dat[,c(1,2,3,4,6)] %>% pivot_longer(cols = 2:4, names_to = "source")

# Plot
p101 <- means.b %>% ggplot() +
  geom_point(aes(Primary_water_source, value, col = source), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_color_brewer(palette = "Dark2", labels = c("Biofilm", "CPOM", expression(italic("Hydrurus")))) +
  labs(col = "Resource") + xlab(NULL) + 
  ylab("Biomass Proportion") + theme_bw() +
  geom_point(data = df.b, aes(Primary_water_source, value, col = source), size = 8, position = position_dodge(width = 0.75), shape = 95, key_glyph = "point") +
  geom_errorbar(data = boot.ci.b, aes(x = Primary_water_source, y = p.50, ymin = p2.5, ymax = p97.5, col = source), width = 0.3, position = position_dodge(width = 0.75),  key_glyph = "point") +
  theme(legend.position = "bottom")

p101

ggsave("Output/Paper figures/Biomass by hydro.png", width = 3.8, height = 3.5, dpi = 400)

