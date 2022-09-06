### MixSIAR loop and output for all sites
# By Karen Jorgenson


library(plyr)
library(dplyr) # This order to make group_by work right
library(MixSIAR)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(tidyr)
library(grid)
library(here)

### Create data frame
# Load raw isotope data
iso_dat <- read.csv(here("Data//Teton_Iso_Data_QC.csv")) 
head(iso_dat)

# Merge trophic position data
dat_TP <- read.csv(here("Output//TP_dat.csv"))  
head(dat_TP)
dat_2 <- merge(iso_dat, dat_TP, by = c("d13C", "d15N"), all = TRUE ) %>% mutate(group = as.factor(group), site = as.factor(site)) 
head(dat_2)

# Set all non predatory data to TP = 2
dat_2$TP <- ifelse(dat_2$group %in% c("Clinocera", "Sweltsa", "Megarcys", "Rhyacophila", "Simuliidae", "Turbellaria", "Ameletidae", "Baetidae"), dat_2$TP_calc, 2)
dat_3<- dat_2 %>% filter(TP < 2.5 | NA)
head(dat_3)

# Calculate means and standard errors for each group*site
iso_means <- dat_3 %>%  group_by(group, site) %>% filter(date2 == "A", !site %in% "Gusher") %>%
  summarise( type = type,
    n = length(group),
            Meand13C = mean(d13C, na.rm = TRUE), 
            SDd13C = sd(d13C, na.rm = TRUE), 
            Meand15N = mean(d15N, na.rm = TRUE), 
            SDd15N = sd(d15N, na.rm = TRUE) ) %>% unique()
iso_means

# Minimum and Maximum C isotope values for each source
iso_means %>% ungroup() %>% filter(group == "CPOM/Plant") %>% summarize(max = max(Meand13C),
                                                       min = min(Meand13C))
iso_means %>% ungroup() %>% filter(group == "Biofilm") %>% summarize(max = max(Meand13C),
                                                                        min = min(Meand13C))
iso_means %>% ungroup() %>% filter(group == "Hydrurus") %>% summarize(max = max(Meand13C),
                                                                        min = min(Meand13C))
### MixSIAR loop

site <- c("Wind Cave", "Skillet", "SFTC", "AK Basin", "Delta", "NFTC", "Cloudveil", "Grizzly", "Paintbrush") # Select all sites
site <- c("Delta", "Wind Cave") # Select individual site

#AK, Paintbrush, SFTC, Grizzly, Skillet
for( i in site){

# filter for site
invert_dat <- dat_3 %>% filter(type == "invert", site == i, date2 == "A") %>% ungroup  %>% select(group, d15N, d13C) %>% na.omit()
invert_dat
food_dat <- iso_means %>% filter(site == i, group %in% c("Biofilm", "Hydrurus", "CPOM/Plant", "Algae_slime", "Pika poop"))  %>% ungroup() %>%
  select(group, n, Meand15N, Meand13C, SDd15N, SDd13C)
food_dat

write.csv(invert_dat,"invert_dat.csv", 
          row.names = FALSE)
write.csv(food_dat,"food_dat.csv", 
          row.names = FALSE)


# load mixture data
mix <- load_mix_data(filename="invert_dat.csv",
                           iso_names=c("d13C","d15N"),
                           factors= "group",
                           fac_random=FALSE,
                           fac_nested= NULL,
                           cont_effects=NULL)


# load source data

source <- load_source_data(filename="food_dat.csv",
                                 source_factors=NULL,
                                 conc_dep=FALSE,
                                 data_type="means",
                                 mix)

# discr with TEF from Bunn et al. 2013 for N and Post 2002 for C
discr <- data.frame(group = source$source_names, Meand13C = rep(0.4, length(source$source_names)), Meand15N= rep(1.4, length(source$source_names)),	SDd13C= rep(1.4, length(source$source_names)),	SDd15N= rep(1.4, length(source$source_names)))
write.csv(discr,"discr.csv", 
          row.names = FALSE)
discr <-load_discr_data(filename="discr.csv", 
                              mix)
discr

# isospace plot
plot_data(filename=paste("iso_plot", i, sep = "_"), plot_save_pdf=FALSE, plot_save_png=FALSE, mix,source,discr)

# Default generalist prior (alpha = 1)
plot_prior(alpha.prior=1,source)

# Write the JAGS model file
model_filename <- paste("MixSIAR_model", i, sep = "_")   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

# Run model
jags.1 <- run_model(run="long", mix, source, discr, model_filename,  # Adjust run length as needed
                          alpha.prior = 1, resid_err, process_err)

# Analyze diagnostics and output
output_options <- list(summary_save = TRUE,
                             summary_name = paste("summary_statistics", i, sep = "_"),
                             sup_post = FALSE,
                             plot_post_save_pdf = TRUE,
                             plot_post_name = paste("posterior_density", i, sep = "_"),
                             sup_pairs = FALSE,
                             plot_pairs_save_pdf = FALSE,
                             plot_pairs_name = "pairs_plot",
                             sup_xy = FALSE,
                             plot_xy_save_pdf = FALSE,
                             plot_xy_name = "xy_plot",
                             gelman = TRUE,
                             heidel = TRUE,
                             geweke = TRUE,
                             diag_save = TRUE,
                             diag_name = paste("diagnostics", i, sep = "_"),
                             indiv_effect = FALSE,
                             plot_post_save_png = TRUE,
                             plot_pairs_save_png = FALSE,
                             plot_xy_save_png = FALSE) 

output_JAGS(jags.1, mix, source, output_options)

graphics.off()
}


### Dataframe of diet proportions

filelist = list.files(pattern = "summary_statistics.*.txt", full.names = TRUE, recursive = TRUE )
l <- lapply( filelist, fread )
names(l) <- basename( filelist ) # set file names
data_d <- rbindlist(l, idcol = TRUE, fill = TRUE) %>% separate(.id, into = c(NA, NA, "site", NA), sep = '[_.]') 
data_diet <- data_d %>% filter(!V1 %in% c("Epsilon.1", "Epsilon.2")) %>% 
  separate(V1, into = c(NA, "taxa", "source"), sep = '[.]')
names(data_diet) <- c("site", "taxa", "source", "Mean", "SD",  "p2.5", "p5", "p25", "p50", "p75", "p95", "p97.5")
data_diet %>% filter(site == "Grizzly")
write.csv(data_diet, "Output//Diet_proportions_all.csv")

data_diet <- read.csv("Output//Diet_proportions_all.csv")
### Plot of all diet proportions across sites
# Reorder site factor levels
data_diet$site <- factor(data_diet$site, 
                         levels = c("Cloudveil", "Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush"))

# Rename sites
site_labs <- c("Cloudveil", "Delta", "Skillet", "Alaska Basin", "Wind Cave","S Fork Teton Creek", "N Fork Teton Creek", "Grizzly", "Paintbrush")
names(site_labs) <- c("Cloudveil", "Delta", "Skillet", "AK Basin", "Wind Cave", "SFTC", "NFTC", "Grizzly", "Paintbrush")

## Plot
# Create ggplot
p_wrap <- data_diet %>% ggplot(aes(Mean, reorder(taxa, desc(taxa)), color = source)) + geom_point() +
  facet_wrap(~ site, labeller = labeller(site = site_labs)) + geom_errorbar(aes(xmin=Mean-SD, xmax=Mean+SD), width=1) +
  scale_color_manual(name = "Resource", labels = c("Biofilm", "CPOM", "Hydrurus"), values = c("#1B9E77", "#D95F02", "#7570B3", "#66A61E", "#E6AB02", "#A6761D")) +
  xlab("Diet Proportion") + ylab("Taxon") + theme_bw() + theme(strip.text = element_text(color = 'white'), axis.text=element_text(size=8))
p_wrap

# Color facet labels by water source
png("Output//Paper figures//Diet props all.png", width = 7, height = 7, units = "in", res = 200)
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




