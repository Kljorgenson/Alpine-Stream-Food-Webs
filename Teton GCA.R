### Gut content analysis
# By Karen Jorgenson

library(plyr)
library(readxl)
library(tidyverse)
library(readxl)

setwd("C:/Users//Karen Jorgenson//OneDrive - University of Wyoming//Collins Lab//Teton Alpine Streams//Data//GCA")

# load data
GCA_areas <- read_xlsx("GCA areas.xlsx")
ID <- read_xlsx("GCA slides.xlsx")

GCA_da <- merge(GCA_areas, ID, by.x = 1, by.y = 1, all.x = TRUE) %>% 
  select("ID", "source", "per_area", "taxa", "site") %>%
  na.omit()
head(GCA_da)

GCA_da %>% filter(taxa == "Zapada", site == "Delta")

# Multiply by assimilation factors: diatoms, 0.3; amorphous detritus, 0.1; leaf litter, 0.1; 
# filamentous algae, 0.3; macrophytes, 0.1; fungi, 0.7; and animal material, 0.7 

GCA_dat <- GCA_da %>% mutate(assim = case_when(source %in% c("Hydrurus", "diatoms", "fil. algae") ~ per_area*0.3,
                                     source == "animal" ~ per_area*0.7,
                                     source == "plant" ~ per_area*0.1),
  taxa = case_when(taxa %in% c("R. Brunnea group", "R. Rotunda", "R. Vagrita group", "R. Alberta group") ~ "Rhyacophila",
  !taxa %in% c("R. Brunnea group", "R. Rotunda", "R. Vagrita group", "R. Alberta group") ~ taxa)) %>%
  group_by(ID) %>% mutate(per_assim = assim/sum(assim)*100)
head(GCA_dat)
GCA_dat %>% group_by(ID) %>% summarise(mean = sum(per_assim)) %>% na.omit()# check


# Calculate mean for taxa and site
GCA_data <- GCA_dat %>% group_by(source, taxa, site) %>% summarise( n = length(unique(ID)),
                  per_assim = mean(per_assim, na.rm = TRUE),
                  sd = sd(per_assim, na.rm = TRUE))
GCA_dat %>% filter(site == "SFTC", taxa == "Sweltsa") %>% print(n=32)
tail(GCA_data)
unique(GCA_dat$taxa)

write.csv(GCA_data, "GCA_data.csv")

GCA_data <- read.csv("GCA_data.csv")

# Facet plot of all sites
p_wrap <- GCA_data %>% filter(site != "Gusher", per_assim != 0) %>% ggplot(aes(per_assim, taxa, color = source)) + 
  geom_point(aes(pch = source),cex = 2, stroke = 1.5) +
  facet_wrap(~ site, ncol = 4) + geom_errorbar(aes(xmin=per_assim-sd, xmax=per_assim+sd), width=1) +
  geom_text(aes(115, taxa, label = n), color = 1, cex = 3) +
  scale_x_continuous(breaks=c(0,25,50, 75, 100)) + labs( x = "Diet Proportion", y = "Taxa", color = "Source", shape = "Source") +
  scale_shape_manual(values=c(3,4,1,2,6), labels = c("Animal", "Diatoms", "Filamentous algae", "Hydrurus", "Plant")) + 
  scale_color_manual(labels = c("Animal", "Diatoms", "Filamentous algae", "Hydrurus", "Plant"), values = c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02", "#A6761D")) +
  theme_bw()

p_wrap

# Plot with stacked bars
GCA_data$site <- as.factor(GCA_data$site)
GCA_data$site_2 <- factor(GCA_data$site, 
                         levels = c("Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush", "Gusher"))
levels(GCA_data$site)



site_lab <- c("Delta", "Skillet", "Alaska Basin", "Wind Cave","S Fork Teton Creek", "N Fork Teton Creek", "Grizzly", "Paintbrush")
names(site_lab) <- c("Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush")

p_wrap <- GCA_data %>% filter(site != "Gusher", source != "Other") %>% ggplot(aes(per_assim, reorder(taxa, desc(taxa)), fill = source)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~ site_2, ncol = 2, labeller = labeller(site_2 = site_lab)) +
  geom_text(aes(0.8, taxa, label = n), cex = 2, nudge_y = 0.1) +
  scale_x_continuous(breaks=c(0,.25,.50, .75, 1)) + labs( x = "Diet Proportion", y = "Taxon") +
  scale_fill_manual(name = "Resource", labels = c("Animal", "Diatoms", "Filamentous algae", "Hydrurus", "Plant"), values = c("#E6AB02", "#1B9E77", "#66A61E","#7570B3", "#D95F02")) +
  theme_bw() + theme(strip.text = element_text(color = 'white'), axis.text=element_text(size=8))

p_wrap

library(grid)
png("GCA facet stacked.png", width = 7, height = 8, units = "in", res = 200)
g <- ggplot_gtable(ggplot_build(p_wrap))
stripr <- which(grepl('strip-t', g$layout$name))
fills <- c("#6baed6", "#6baed6", "#6baed6", "#6baed6", "#2171b5","#2171b5", "#08306b", "#08306b")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)
dev.off()
