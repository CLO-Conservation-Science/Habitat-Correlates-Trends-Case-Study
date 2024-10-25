# Habitat-Correlates-Trends-Case-Study
Data and code to run the analysis presented in:
Stillman, A.N., C.L. Davis, K.D. Dunham, V. Ruiz-Gutierrez, A.D. Rodewald, A. Johnston, T. Auer, M. Strimas-Mackey, S. Ligocki, and D. Fink. In review. A framework for assessing the habitat correlates of spatially explicit population trends. 

*Please contact the first author with any questions about the code or publication: Andrew N. Stillman (ans95[at]cornell.edu)

## Abstract
Halting or reversing widespread biodiversity loss will require detailed information on species’ trends and the habitats associated with population declines. However, constraints on conventional monitoring programs and commonplace approaches for trend estimation can make it difficult to obtain such information across species’ ranges. Here, we demonstrate how recent developments in machine learning and model interpretation, combined with increased data collection through large-scale participatory science programs, enable inferences on the habitat correlates of population trends across broad spatial extents. We highlight the utility of this approach using a case study of three songbird species which are strongly associated with the sagebrush ecosystem in North America: Brewer’s sparrow (Spizella breweri), sagebrush sparrow (Artemisiospiza nevadensis), and sage thrasher (Oreoscoptes montanus). Our analysis revealed complex, non-linear associations between land cover variables and species’ population trends as well as substantial interspecific variation in those associations despite the species’ similar reliance on sagebrush habitat. We then leveraged this information in an exploratory analysis to evaluate the potential return on investment for a simulated habitat modification aimed at reducing population declines for a single focal species while also considering potential co-benefits for the other two species. In doing so, we present a framework for assessing wildlife responses to potential habitat modification and environmental change at tractable scales for conservation and decision-making.

## Repository Directory
This respository will be archived on Zenodo after acceptance.

### Data
All data files necessary to run the analysis in this repository. Files include land cover descriptions for 2007, land cover desriptions for 2021, eBird Trend estimates spanning 2007-2021, and eBird Status estimates of relative abundance for 2021. 

### Code
Three R scripts necessary to complete the case study analysis. Files include a script to run the GAM models, a script to plot the results, and a script to simulate the potential effects of land cover change on bird population trends. 

### Results
This folder structure is necessary to receive and store results from the scripts. Outputs in the ROI_example folder are provided here, and model results can be generated using scripts in the Code folder.

### Acknowledgements
We are grateful to the many thousands of participants, reviewers, and partner organizations around the world who support and contribute to eBird. We thank the eBird and engineering teams for their tireless efforts to grow, support, and improve the project. Likewise, none of this work would be possible without the generous support of the Cornell Lab of Ornithology’s members, donors, and many staff teams. This work was funded by The Leon Levy Foundation, The Wolf Creek Foundation, and the National Science Foundation (ABI sustaining: DBI-1939187). This work used Bridges2 at Pittsburgh Supercomputing Center and Anvil (Song et al. 2022) at the Rosen Center for Advanced Computing at Purdue University through allocation DEB200010 from the Advanced Cyberinfrastructure Coordination Ecosystem: Services & Support (ACCESS) program, which is supported by NSF grants #2138259, #2138286, #2138307, #2137603, and #2138296.
