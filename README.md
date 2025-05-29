# Habitat-Correlates-Trends-Case-Study
Data and code to run the analysis presented in:
Stillman, A.N., C.L. Davis, K.D. Dunham, V. Ruiz-Gutierrez, A.D. Rodewald, A. Johnston, T. Auer, M. Strimas-Mackey, S. Ligocki, and D. Fink. 2025. A framework for assessing the habitat correlates of spatially explicit population trends. Diversity and Distributions 31:e70030.

*Please contact the first author with any questions about the code or publication: Andrew N. Stillman (ans95[at]cornell.edu)

## Abstract
**Aim.** Halting widespread biodiversity loss will require detailed information on species' trends and the habitat conditions correlated with population declines. However, constraints on conventional monitoring programs and commonplace approaches for trend estimation can make it difficult to obtain such information across species' ranges. Here, we demonstrate how recent developments in machine learning and model interpretation, combined with data sources derived from participatory science, enable landscape-scale inferences on the habitat correlates of population trends across broad spatial extents.

**Location.** Worldwide, with a case study in the western United States.

**Methods.** We used interpretable machine learning to understand the relationships between land cover and spatially explicit bird population trends. Using a case study with three passerine birds in the western U.S. and spatially explicit trends derived from eBird data, we explore the potential impacts of simulated land cover modification while evaluating potential co-benefits among species.

**Results.** Our analysis revealed complex, non-linear relationships between land cover variables and species' population trends as well as substantial interspecific variation in those relationships. Areas with the most positive impacts from a simulated land cover modification overlapped for two species, but these changes had little effect on the third species.

**Main Conclusions.** This framework can help conservation practitioners identify important relationships between species trends and habitat while also highlighting areas where potential modifications to the landscape could bring the biggest benefits. The analysis is transferable to hundreds of species worldwide with spatially explicit trend estimates, allowing inference across multiple species at scales that are tractable for management to combat species declines.

## Repository Directory
This respository is archived on Dyrad and Zenodo: https://doi.org/10.5061/dryad.8pk0p2nzf. 

### Data
All data files necessary to run the analysis in this repository. Files include land cover descriptions for 2007, land cover desriptions for 2021, eBird Trend estimates spanning 2007-2021, and eBird Status estimates of relative abundance for 2021. 

### Code
Three R scripts necessary to complete the case study analysis. Files include a script to run the GAM models, a script to plot the results, and a script to simulate the potential effects of land cover change on bird population trends. 

### Results
This folder structure is necessary to receive and store results from the scripts. Outputs in the ROI_example folder are provided here, and model results can be generated using scripts in the Code folder.

### Acknowledgements
We are grateful to the many thousands of participants, reviewers, and partner organizations around the world who support and contribute to eBird. We thank the eBird and engineering teams for their tireless efforts to grow, support, and improve the project. Likewise, none of this work would be possible without the generous support of the Cornell Lab of Ornithology’s members, donors, and many staff teams. This work was funded by The Leon Levy Foundation, The Wolf Creek Foundation, and the National Science Foundation (ABI sustaining: DBI-1939187). This work used Bridges2 at Pittsburgh Supercomputing Center and Anvil (Song et al. 2022) at the Rosen Center for Advanced Computing at Purdue University through allocation DEB200010 from the Advanced Cyberinfrastructure Coordination Ecosystem: Services & Support (ACCESS) program, which is supported by NSF grants #2138259, #2138286, #2138307, #2137603, and #2138296.
