# Alpine-Stream-Food-Webs

This contains the data and code necessary to replicate the findings from my thesis titled "Hydrologic source and trophic flexibility structure alpine stream food webs in the Teton Range, Wyoming".

R scripts that must be run in order:<br/>
1. Trophic position.R: Calculate the trophic position of each invertebrate sample using the one baseline method <br/>
2. MixSIAR diet loop.R: Estimate diet proportions for each taxa at each site using Bayesian mixing models <br/>
3. Clean diet data.R: Add zeroes for sites without Hydrurus and combine rare sources <br/>
4. PCA.R: Run principal component analyses to group sites by average diet proportions and environmental variables <br/>
5. Diversity 2020.R: Calculate diversity metrics for each site<br/>
6. Diet environ compilation.R: Combine diet and environmental data<br/>
7. Dirichlet models.R: Run multivariate regression models to explore relationships between diet and environment <br/>
8. Biomass.R: Calculate the biomass supported by each source and model relationships between biomass and environment <br/>
9. Compositional variation.R: Calculate within and among-site variation <br/> 

Other R scripts: <br/> 
Isotope bioplots.R: Plot the raw isotope data with TEFs <br/> 
Teton GCA.R: Calculate assimilated diet proportions from gut content analysis data <br/> 

Data files: <br/> 
Environ data.csv: Environmental data by site <br/> 
GCA slide areas.xlsx: Proportional cover of food items on gut slides <br/> 
GCA slides.xlsx:  Gut content analysis slide metadata, including site and taxa <br/> 
Taxa classification.csv: Order, Family and Genus for Teton stream taxa <br/> 
TetonInverts2020.csv: Surber sample data <br/> 
Teton_Iso_Data_QC.csv: C and N isotope data for invertebrates and food sources <br/> 
