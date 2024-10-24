## Description of code files

### 1_GAM_and_predict
This script uses GAM models to uncover the correlates of eBird population trends for 3 species.
Step 1. Prepare predictors and data for GAM models
Step 2. Loop over species: fit a GAM to each trend replicate, predict to 2021 land cover, and export datasets for plotting

### 2_Mapping
This script produces plots to visualize results from the manuscript
Step 1. Create map of species abundance
Step 2. Create map of species trend
Step 3. Create map of % land cover, geographic effect, & effect on abundance for each predictor
Step 4. Create 1D effect plot for each predictor

### 3_Return_on_investment
This script conducts a return on investment scenario using 2021 land cover values
Step 1. Loop through all species and predict to modified predictor surface
Step 2. Write functions for results
Step 3. Compare between species and export datasets
Step 4. Mapping
