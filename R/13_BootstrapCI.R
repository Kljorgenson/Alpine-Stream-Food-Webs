## Bootstrap confidence interals for diet and biomass by hydrologic source
# First run 10_Dirichlet_models.R and 11_Biomass.R
library(Compositional)
library(MCMCpack)
library(data.table)

### Bootstrap Diet proportions

boot.fun <- function(x){
  
dat <- spread_env_m_dat[, c(1,3:5, 7)]

sub <- dat %>% filter(Primary_water_source == "Subterranean ice")
glacier <- dat %>% filter(Primary_water_source == "Glacier")
snow <- dat %>% filter(Primary_water_source == "Snowmelt")




sub.dat <- sub[sample(nrow(sub), 2, replace = T),]
glacier.dat <- glacier[sample(nrow(glacier), 3, replace = T),]
snow.dat <-snow[sample(nrow(snow), 4, replace = T),]

data <- rbind(sub.dat, glacier.dat, snow.dat)

data

# Sum to 1
data <- data %>% group_by() %>% mutate(Biofilm = round(Biofilm/(Biofilm+CPOM+Hydrurus),2),
                                                  CPOM = round(CPOM/(Biofilm+CPOM+Hydrurus),2),
                                                  Hydrurus = round(Hydrurus/(Biofilm+CPOM+Hydrurus),2))
DD <-DR_data(data[,2:4])
m6c <- DirichReg(DD ~ Primary_water_source, data, model = "common")

df <- as.data.frame(fitted(m6c, mu = TRUE, alpha = F, phi = F))
df$Primary_water_source <- c(rep("Subterranean ice", 2), rep("Glacier", 3), rep("Snowmelt", 4))
df <- df %>% unique()
return(df)
}

boot.fun()

list <- lapply(1:100, boot.fun)
boot <- rbindlist(list)
head(boot)

boot.ci <- boot  %>% pivot_longer(cols = 1:3, names_to = "source") %>% group_by(Primary_water_source, source) %>%
  summarise(p2.5 = quantile(value, probs = 0.025),
            p.50 = quantile(value, probs = 0.5),
            p97.5 = quantile(value, probs = 0.975),
            SD = sd(value))


p100 <- means %>% ggplot() +
  #geom_boxplot(aes(Primary_water_source, value, col = source)) + 
  geom_point(aes(Primary_water_source, value, color = source), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(col = "Resource") + xlab(NULL) + 
  ylab("Diet Proportion") + theme_bw() +
  #scale_colour_continuous(guide = "none") +
  geom_point(data = df, aes(WS, value, color = name), size = 12, position = position_dodge(width = 0.75), shape = 95) +
  geom_errorbar(data = boot.ci, aes(x = Primary_water_source, y = p.50, ymin = p2.5, ymax = p97.5, color = source), width = 0.3, position = position_dodge(width = 0.75), key_glyph = "point") +
  theme(legend.position = "bottom")

p100



p100 <- means %>% ggplot() +
  geom_boxplot(aes(Primary_water_source, value, col = source), alpha = 0) + 
  geom_point(aes(Primary_water_source, value, color = source), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  labs(fill = "Resource") + xlab(NULL) + 
  ylab("Diet Proportion") + theme_bw() +
  #scale_colour_continuous(guide = "none") +
  geom_point(data = df, aes(WS, value, fill = name), width = 1, size = 12, position = position_dodge(width = 0.75), shape = 95, key_glyph = "point") +
  geom_errorbar(data = boot.ci, aes(x = Primary_water_source, y = p.50, ymin = p2.5, ymax = p97.5, fill = source), width = 0.3, position = position_dodge(width = 0.75), key_glyph = "point")

p100




### Bootstrap Biomass
library(data.table)
boot.fun <- function(x){
  
  dat <- spread_bm_envi_dat[, c(1:4,6)]
  
  sub <- dat %>% filter(Primary_water_source == "Subterranean ice")
  glacier <- dat %>% filter(Primary_water_source == "Glacier")
  snow <- dat %>% filter(Primary_water_source == "Snowmelt")
  
  
  
  
  sub.dat <- sub[sample(nrow(sub), 2, replace = T),]
  glacier.dat <- glacier[sample(nrow(glacier), 3, replace = T),]
  snow.dat <-snow[sample(nrow(snow), 4, replace = T),]
  
  data <- rbind(sub.dat, glacier.dat, snow.dat)
  
  data
  
  # Sum to 1
  data <- data %>% group_by() %>% mutate(Biofilm = round(Biofilm/(Biofilm+CPOM+Hydrurus),2),
                                         CPOM = round(CPOM/(Biofilm+CPOM+Hydrurus),2),
                                         Hydrurus = round(Hydrurus/(Biofilm+CPOM+Hydrurus),2))
  DD <-DR_data(data[,2:4])
  m6c <- DirichReg(DD ~ Primary_water_source, data, model = "common")
  
  df <- as.data.frame(fitted(m6c, mu = TRUE, alpha = F, phi = F))
  df$Primary_water_source <- c(rep("Subterranean ice", 2), rep("Glacier", 3), rep("Snowmelt", 4))
  df <- df %>% unique()
  return(df)
}

boot.fun()

list.b <- lapply(1:100, boot.fun)
boot.b <- rbindlist(list.b)
head(boot.b)

boot.ci.b <- boot.b  %>% pivot_longer(cols = 1:3, names_to = "source") %>% group_by(Primary_water_source, source) %>%
  summarise(p2.5 = quantile(value, probs = 0.025),
            p.50 = quantile(value, probs = 0.5),
            p97.5 = quantile(value, probs = 0.975))


p100 <- means %>% ggplot() +
  #geom_boxplot(aes(Primary_water_source, value, col = source)) + 
  geom_point(aes(Primary_water_source, value, col = source), cex = 2, position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_color_brewer(palette = "Dark2", labels = c("Biofilm", "CPOM", expression(italic("Hydrurus")))) +
  #annotate(geom="text", x=2, y=0.38, label="a", color="#D95F02") +
  #annotate(geom="text", x=3, y=0.29, label="a", color="#D95F02") +
  labs(col = "Resource") + xlab(NULL) + 
  ylab("Biomass Proportion") + theme_bw() +
  #scale_colour_continuous(guide = "none") +
  geom_point(data = df, aes(WS, value, col = name), width = 1, size = 12, position = position_dodge(width = 0.75), shape = 95, key_glyph = "point") +
  geom_errorbar(data = boot.ci.b, aes(x = Primary_water_source, y = p.50, ymin = p2.5, ymax = p97.5, col = source), width = 0.3, position = position_dodge(width = 0.75),  key_glyph = "point")

p100
ggsave("Output//Paper figures//Biomass by hydro.png", width = 5, height = 3.6)


