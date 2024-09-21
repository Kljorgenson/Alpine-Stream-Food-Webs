### Clean Diet Data
# By Karen Jorgenson

# Setup
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(grid)

## Load estimated diet proportions
data_di <- read.csv("Output/Diet_proportions_all.csv")
data_di$site <- as.factor(data_di$site) 
levels(data_di$site) <- c("AK Basin","Cloudveil","Delta","Grizzly","NFTC", "Paintbrush", "SFTC", "Skillet", "Wind Cave" )
data_di %>% filter(site == "Grizzly")

# Combine Pikepoop and Algae_slime with other sources
data_di$source <- as.factor(data_di$source)
data_diet <- data_di %>% mutate(source = case_when(source %in% c("CPOM/Plant", "Pikapoop") ~ "CPOM",
                                                   source %in% c("Biofilm", "Algae_slime") ~ "Biofilm",
                                                   source == "Hydrurus" ~ "Hydrurus")) %>%
  dplyr::group_by(site, taxa, source) %>% dplyr::summarise(Mean = sum(Mean), SD = mean(SD))

head(data_diet)

write.csv(data_diet, "Output/diet_data_clean.csv")


### Plot of all diet proportions across sites
# Reorder site factor levels
data_diet$site <- factor(data_diet$site, 
                         levels = c("Cloudveil", "Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush"))

# Rename sites
site_labs <- c("Cloudveil", "Delta", "Skillet", "Alaska Basin", "Wind Cave","S Fork Teton Creek", "N Fork Teton Creek", "Grizzly", "Paintbrush")
names(site_labs) <- c("Cloudveil", "Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush")

## Plot
# Dummy variables to set y-axis by row
a <- data_diet %>% filter(site %in% c("Cloudveil", "Delta", "Skillet")) %>% ungroup() %>% select(taxa) %>% unique()
b <-data_diet %>% filter(site %in% c("AK Basin", "Wind Cave", "SFTC")) %>% ungroup() %>% select(taxa) %>% unique()
c <- data_diet %>% filter(site %in% c("NFTC", "Grizzly", "Paintbrush")) %>% ungroup() %>% select(taxa) %>% unique()
df <- data.frame(site = c(rep("Cloudveil", length(a$taxa)), rep("Delta", length(a$taxa)),rep("Skillet", length(a$taxa)),rep("AK Basin", length(b$taxa)),rep("Wind", length(b$taxa)),rep("SFTC", length(b$taxa)),rep("NFTC", length(c$taxa)),rep("Grizzly", length(c$taxa)),rep("Paintbrush", length(c$taxa))),
                 taxa = c(rep(a$taxa, 3),rep(b$taxa, 3),rep(c$taxa, 3)),
                 Mean = rep(0.5, 99),
                 source = rep("CPOM", 99))

df2 <- full_join(data_diet, df)%>% na.omit()

## Create ggplot plot
p_wrap <- df2  %>% ggplot(aes(Mean, reorder(taxa, desc(taxa)), color = source)) + geom_point() +
  facet_grid(~ site, labeller = labeller(site = site_labs), nrow = 3, space = "fixed", scales = "free_y") + geom_errorbar(aes(xmin=Mean-SD, xmax=Mean+SD), width=1) +
  scale_color_manual(name = "Resource", labels = c("Biofilm", "CPOM", expression(italic("Hydrurus"))), values = c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02", "#A6761D")) +
  xlab("Diet Proportion") + ylab("Taxon") + theme_bw() + theme(strip.text = element_text(color = 'white'), legend.position = "top", axis.text=element_text(size=8))
p_wrap

# Color facet labels by water source
png("Output//Paper figures//Diet props all.png", width = 3700, height = 5000, res = 600)
g <- ggplot_gtable(ggplot_build(p_wrap))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("#6baed6", "#6baed6", "#6baed6", "#08306b", "#08306b", "#6baed6", "#2171b5","#2171b5","#2171b5")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)
dev.off() 




