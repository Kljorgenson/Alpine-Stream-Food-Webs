# Alpine-Stream-Food-Webs

This repository contains the data and code necessary to replicate the findings from the manuscript titled "Hydrology and trophic flexibility structure alpine stream food webs in the Teton Range, Wyoming, USA" by Karen L. Jorgenson, Scott Hotaling, Lusha M. Tronstad, Debra S. Finn, and Sarah M. Collins.

Rscripts:<br/>
* 01_Teton GCA.R: Calculate assimilated diet proportions from gut content analysis (GCA) data <br/> 
* 02_Trophic position.R: Calculate the trophic position (TP) of each invertebrate sample using the one baseline method <br/>
* 03_Isotope bioplots.R: Plot the raw isotope data with TEFs <br/>
* 04_MixSIAR diet loop.R: Estimate diet proportions for each taxa at each site using Bayesian mixing models <br/>
* 05_Clean diet data.R: Add zeroes for sites without Hydrurus and combine rare sources <br/>
* 06_PCA.R: Run principal component analyses to group sites by average diet proportions and environmental variables <br/>
* 07_Diversity 2020.R: Calculate diversity metrics for each site<br/>
* 08_Diet environ compilation.R: Combine diet and environmental data<br/>
* 09_Dirichlet models.R: Run multivariate regression models to explore relationships between diet and environment <br/>
* 10_Biomass.R: Calculate the biomass supported by each source and model relationships between biomass and environment <br/>
* 11_Compositional variation.R: Calculate within and among-site variation <br/> 
* 12_BootstrapCI.R: Bootstrap confidence intervals for the hydrologic source model <br/>
  
Raw data files: <br/> 
- Environ data.csv: Environmental data by site <br/> 
- GCA slide areas.xlsx: Proportional cover of food items on gut slides <br/> 
- GCA slides.xlsx:  GCA slide metadata, including site and taxa <br/> 
- Taxa classification.csv: Order, Family and Genus for Teton stream taxa <br/> 
- TetonInverts2020.csv: Surber sample data <br/> 
- Teton_Iso_Data_QC.csv: Carbon and nitrogen isotope data for invertebrates and food sources <br/>

Output data files: <br/>
- AIC table.csv: AICc values for environmental variable and diet proportion models <br/>
- AICc biomass table.csv: AICc values for environmental variable and biomass proportion models <br/>
- diet_data_clean.csv: Mean and SD of diet proportions reclassified into "CPOM", "Hydrurus" or "Biofilm"
- Diet_proportions_all.csv: Summary statistics from mixing models
- Div.csv: Shannon Index (H) and Simpson Index (D) values
- env_diet_dat.csv: Combined dataframe with diet proportions and environmental variables
- Envi_data.csv: Combined dataframe with environmental variables, PCA, and diversity indices
- GCA_data.csv: Summary of GCA data
- PCA_dat.txt: Environmental variables principal components analysis (PCA) output
- PCA_s_dat.txt: Diet proportions PCA output
- TP_dat.csv: Calculated trophic positions for each taxa
