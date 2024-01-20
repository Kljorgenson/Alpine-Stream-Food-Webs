### Teton Isotope Plots
# By Karen Jorgenson


## Setup
library(dplyr)
library(plyr)
library(MixSIAR)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)
library(tidyverse)


## Organize data
iso_dat <- read.csv("Data//Teton_Iso_Data_QC.csv")
head(iso_dat)
names(iso_dat)
iso_inv <- iso_dat %>% filter(type == "invert")

# Merge trophic position data
dat_TP <- read.csv("Output//TP_dat.csv")  
head(dat_TP)
iso_TP <- merge(iso_inv, dat_TP, by = c("d13C", "d15N"), all = TRUE ) %>% mutate(group = as.factor(group), site = as.factor(site)) 
head(iso_TP)

# Set all non predatory data to TP = 2
iso_TP$TP <- ifelse(iso_TP$group %in% c("Clinocera", "Sweltsa", "Lednia", "Megarcys", "Rhyacophila", "Simuliidae", "Turbellaria", "Ameletidae", "Baetidae"), iso_TP$TP_calc, 2)
iso_TP$TL<- ifelse(iso_TP$TP > 2.5, "P", "N") # P for predator, N for non-predator

head(iso_TP)

# Standard error function
SE <- function(x) sd(x, na.rm=TRUE)/sqrt(length(na.omit(x)))

# Calculate means and standard deviations for food sources
iso_means <- iso_dat %>% filter(date2 == "A") %>% group_by(group, site) %>%
  dplyr::summarize(count = n(),
            mC = mean(d13C, na.rm = TRUE), 
            SEC = SE(d13C), 
            mN = mean(d15N, na.rm = TRUE), 
            SEN = SE(d15N), 
            type = type) %>%
  unique() # sometimes 'summarize' is a jerk, so I added this

iso_means <- iso_means %>% mutate(group2 = ifelse(group == "Hydrurus" & site %in% c("AK Basin", "Wind Cave"), "Hydrurus sub", group))

iso_means2 <- iso_means %>% group_by(group2) %>%
  dplyr::summarize(n = n(),
                   mC2 = mean(mC, na.rm = TRUE), 
                   SEC2 = SE(SEC), 
                   mN2 = mean(mN, na.rm = TRUE), 
                   SEN2 = SE(SEN), 
                   type = type) %>%
  unique() # sometimes 'summarize' is a jerk, so I added this

#################################################################################################
## Isotope plots with ggplot
#################################################################################################

invert_dat <- iso_TP %>% filter(TL == "N", type == "invert", date2 == "A", site != "Gusher") %>% select(group, site, d15N, d13C) %>% na.omit()
invert_dat
food_dat <- iso_means2 %>% filter(type == "food") %>% select(group2, mN2, mC2, SEN2, SEC2, n) %>%
  filter(group2 %in% c("Hydrurus", "Hydrurus sub", "CPOM/Plant", "Biofilm"))

# plot
ggplot(food_dat, aes( mC2, mN2, color = group2)) + 
  geom_point(aes(mC2 + 0.4, mN2 + 1.4, group = group2), size = 2) + # add points for food sources
    geom_errorbar(data = food_dat,                     # add x-axis error bars
                mapping = aes(x = mC2 + 0.4,
                              ymin = mN2 + 1.4 - SEN2 - 1.4, 
                              ymax = mN2 + 1.4 +SEN2 + 1.4), 
                width = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
  geom_errorbarh(data = food_dat,                     # add y-axis error bars
                 mapping = aes(y = mN2 + 1.4,
                               xmin = mC2 + 0.4 - SEC2 -1.4,
                               xmax = mC2 + 0.4 + SEC2 + 1.4),
                 height = 0, inherit.aes = FALSE, linetype = 2, color = "grey") +
  geom_errorbar(data = food_dat,                     # add x-axis error bars
                mapping = aes(x = mC2 + 0.4,
                              ymin = mN2 + 1.4 - SEN2, 
                              ymax = mN2 + 1.4 +SEN2), 
                width = 0, inherit.aes = FALSE, size = 0.7) +
  geom_errorbarh(data = food_dat,                     # add y-axis error bars
                 mapping = aes(y = mN2 + 1.4,
                               xmin = mC2 + 0.4 - SEC2,
                               xmax = mC2 + 0.4 + SEC2),
                 height = 0, inherit.aes = FALSE, size = 0.7) +

  geom_text(data = food_dat, aes(label=group2),hjust= -0.4, vjust= -12, size = 3.5) +   # add labels for food sources
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     legend.position = "none") +
  labs(x=expression(paste(delta, "13C")), y=expression(paste(delta, "15N"))) + # Make nice axis labels
  geom_point(data = invert_dat, aes (d13C, d15N, color = group), cex = 2) + # Add points for invertebrates
  theme(legend.position = "right")

  
### MixSIAR
names(food_dat) <- c("group", "Meand15N", "Meand13C", "SDd15N", "SDd13C", "n")
 
 write.csv(invert_dat,"invert_dat.csv", 
            row.names = FALSE)
  write.csv(food_dat,"food_dat.csv", 
            row.names = FALSE)
  
  
  # load mixture data
  mix <- load_mix_data(filename="invert_dat.csv",
                       iso_names=c("d13C","d15N"),
                       factors= c("group", "site"),
                       fac_random=c(TRUE, FALSE),
                       fac_nested= c(FALSE, FALSE),
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
  model_filename <- paste("MixSIAR_model_combfood")   # Name of the JAGS model file
  resid_err <- TRUE
  process_err <- TRUE
  write_JAGS_model(model_filename, resid_err, process_err, mix, source)
  
  # Run model
  jags.1 <- run_model(run="long", mix, source, discr, model_filename,  # Adjust run length as needed
                      alpha.prior = 1, resid_err, process_err)
  
  # Analyze diagnostics and output
  output_options <- list(summary_save = TRUE,
                         summary_name = paste("summary_statistics"),
                         sup_post = FALSE,
                         plot_post_save_pdf = TRUE,
                         plot_post_name = paste("posterior_density"),
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
                         diag_name = paste("diagnostics"),
                         indiv_effect = FALSE,
                         plot_post_save_png = TRUE,
                         plot_pairs_save_png = FALSE,
                         plot_xy_save_png = FALSE) 
  
  output_JAGS(jags.1, mix, source, output_options)
  
save.image("comb food model.RData")


