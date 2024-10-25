## Description of data files

### landcover_2007.csv
Land cover values corresponding to the year 2007. 
srd_id = Unique ID for each pixel in the study area. The study area covers the full geographic range of all three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow). 
latitude = Latitude of pixel.
longitude = Longitude of pixel.
elevation = Elevation of pixel.
Remaining columns (from Barren.PLAND to Herbaceous.Wetlands.PLAND) give the percent cover of each land cover type within the pixel. See appendices accompanying the publication for a list of data sources. 

### landcover_2021.csv
Land cover values corresponding to the year 2021.
srd_id = Unique ID for each pixel in the study area. The study area covers the full geographic range of all three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow). 
latitude = Latitude of pixel.
longitude = Longitude of pixel.
elevation = Elevation of pixel.
Remaining columns (from Barren.PLAND to Herbaceous.Wetlands.PLAND) give the percent cover of each land cover type within the pixel. See appendices accompanying the publication for a list of data sources. 

### abundance_wide_2021.csv
Spatially explicit relative abundance estimates from eBird Status for three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow). 
Columns give the srd_id values, which are unique IDs for each pixel in the study area. 
Row 1 = Brewer's sparrow
Row 2 = sage thrasher
Row 3 = sagebrush sparrow

### trend_reps_wide_2021.csv
Spatially explicit trend estimates in units of PPY (percent per year population change) from 2007 to 2021. For each of the three case study species (Brewer's sparrow, sage thrasher, and sagebrush sparrow), the dataset includes 100 replicates of trend estimates in each pixel. 
species = species code. "brespa" Brewer's sparrow, "sagthr" sage Thrasher, "sagspa1" sagebrush sparrow. 
fold = ID of the trend replicate ranging from 1-100.
Remaining columns give the srd_id values, which are unique IDs for each pixel in the study area. 
