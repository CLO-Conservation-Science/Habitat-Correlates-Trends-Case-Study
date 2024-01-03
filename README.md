# Habitat-Correlates-Trends-Case-Study
Data and code to run the analysis presented in:
Stillman, A.N., C.L. Davis, K.D. Dunham, V. Ruiz-Gutierrez, A.D. Rodewald, A. Johnston, T. Auer, M. Strimas-Mackey, S. Ligocki, and D. Fink. In review. A framework for assessing the impact of habitat management on wildlife population trends. 

*Please contact the first author with any questions about the code or publication: Andrew N. Stillman (ans95[at]cornell.edu)

## Abstract
Halting or reversing widespread biodiversity loss will require spatially explicit information on species’ trends and the habitat correlates of population change. However, constraints on conventional monitoring programs and commonplace approaches for trend estimation make it difficult to obtain such information across species’ geographic ranges. Here, we demonstrate how recent developments in causal and interpretable machine learning approaches, combined with increased data collection through large-scale participatory science programs, can now enable inferences regarding the habitat correlates of population trends across broad spatial extents. We highlight the utility of our approach using a case study of three songbird species which are strongly associated with the sagebrush ecosystem in North America: sage thrasher (Oreoscoptes montanus), Brewer’s sparrow (Spizella breweri), and sagebrush sparrow (Artemisiospiza nevadensis). Our analysis revealed complex, non-linear associations between different habitat variables and species’ population trends as well as substantial interspecific variation in those associations despite their shared habitats. We then leveraged this information to evaluate the return on investment for targeted habitat management interventions aimed at reducing population declines for a single focal species while also considering potential co-benefits for the other two species. In doing so, we present a framework for assessing wildlife responses to habitat management and environmental change at tractable scales for conservation and decision-making.

## Repository Directory
This respository will be archived on Zenodo after acceptance.

### Data
All data files necessary to run the analysis in this repository. Files include land cover descriptions for 2007, land cover desriptions for 2021, and eBird Trend estimates spanning 2007-2021. 

### Code
Three R scripts necessary to complete the case study analysis. Files include a script to run the GAM models, a script to plot the results, and a script to simulate the potential effects of land cover change on bird population trends. 

### Results
This folder structure is necessary to receive and store results from the scripts. Outputs in .csv, .txt, and .RData formats are provided. 

### Acknowledgements
We are grateful to the many thousands of participants, reviewers, and partner organizations around the world who support and contribute to eBird. We thank the eBird and engineering teams for their tireless efforts to grow, support, and improve the project. Likewise, none of this work would be possible without the generous support of the Cornell Lab of Ornithology’s members, donors, and many staff teams. This work was funded by The Leon Levy Foundation, The Wolf Creek Foundation, and the National Science Foundation (ABI sustaining: DBI-1939187). This work used Bridges2 at Pittsburgh Supercomputing Center and Anvil (Song et al. 2022) at the Rosen Center for Advanced Computing at Purdue University through allocation DEB200010 from the Advanced Cyberinfrastructure Coordination Ecosystem: Services & Support (ACCESS) program, which is supported by NSF grants #2138259, #2138286, #2138307, #2137603, and #2138296.
