## Description of data files

### landcover_2007.csv
Land cover values corresponding to the year 2007. 
srd_id = Unique ID for each pixel in the study area. The study area covers the full geographic range of all three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow). 
latitude = Latitude of pixel.
longitude = Longitude of pixel.
elevation = Elevation of pixel.
Remaining columns (from Barren.PLAND to Herbaceous.Wetlands.PLAND) give the percent cover of each land cover type within the pixel. See appendices accompanying the publication for a list of data sources. 

### landcover_2021.csv
Land cover values orresponding to the year 2021.
srd_id = Unique ID for each pixel in the study area. The study area covers the full geographic range of all three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow). 
latitude = Latitude of pixel.
longitude = Longitude of pixel.
elevation = Elevation of pixel.
Remaining columns (from Barren.PLAND to Herbaceous.Wetlands.PLAND) give the percent cover of each land cover type within the pixel. See appendices accompanying the publication for a list of data sources. 

### trend_and_abd_wide_2021.RData
A collection of four datasets giving pixel-wise estimates of relative abundance, mean trend, trend upper confidence interval, and trend lower confidence interval. Each dataset has one row for each species in the case study (3 species total), and one column for each pixel in the study area (3436 pixels total). 
cluster_data_abd = mean relative abundance estimates. 
cluster_data_trend = mean trend estimates given as percent per year rate of population change. 
cluster_data_trend_ll = lower 80% confidence interval on trend. 
cluster_data_trend_ul = upper 80% confidence interval on trend. 
