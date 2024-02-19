[![DOI](https://zenodo.org/badge/465728800.svg)](https://zenodo.org/badge/latestdoi/465728800)
[![Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/badges/latest/active.svg)

  
# Dorheim et al GMD

The single access point to reproduce the results, experiments, and figures for the “Hector V3: functionality and performance of a reduced-complexity climate model for scientific and policy analyses”, which was submitted to GMD in summer of 2023. This repository status will be listed as "Active" defined as "The project has reached a stable, usable state and is being actively developed"^[https://www.repostatus.org/#active] as we anticipate changes to the repository until the associated manuscript is in its final form. 

## Description of Contents 

* data: data products used in the calibration process or as comparison data. 
* output: calibration results and Hector run results. 
* R: scripts used to calibrate, run Hector, and generate manuscript figures labeled by order the scripts should be run in. 
* Hector-V3-ms-diagrams.pptx.zip: copy of the conceptual diagram included in the manuscript

## How to generate manuscript figures 

All of the figures and tables included in the manuscript are generated from the `R/2.manuscript.Rmd` markdown, knit this document from Rstudio or by running `rmarkdown::render("R/2.manuscript.Rmd")` from your R console. 


## Compelte Workflow  

Install the correct version of Hector! 

```
remotes::install_github("jgcri/hector@v3.1.1")
```

From the R directory run the following. 

| Run Order|Name                              | Description                                              |  
|:---------|-----------------------------------:|---------------------------------------------------------:|
|1         | 0.hectorv3_calibration.R          |Calibration protocol for default Hector five default Hector parameter values. After this script completes all of the Hector packages ini files will need to be updated. |  
|2         | 2.cmip6.R                          |Query and format the global annual temperature values from Leeya Pressburger, & Kalyn R. Dorheim. (2022). JGCRI/hector_cmip6data: v1.0 (v1.0). Zenodo. https://doi.org/10.5281/zenodo.7304553 into a format that is ready to plot and analyze for the 2.manuscript.Rmd.|
|3         | 2.hector_v3_IPCC.R                 |Query the IPCC AR6 repo for the CO2 vs temperature results used to create SM figure 10. Make sure that the LUC emissions are consistent with the AR6 historical scenario and run Hector. Save Hector cumulative CO2 emissions vs temperature results per scenario. | 
|4         | 2.hector_v3.R                      |Run all of the SSP scenarios with Hector V3!| 
|5         | 3.manuscript.Rmd                   |Generate all the manuscript plots and crunch all of the numbers to be included in the main manuscript.| 
|6         | 3.manuscript_misc_info.R           |Scrape the Hector C++ code base and the input tables for information to include in the manuscript (i.e. how many RF agents are modeled by Hector and so on)|
|7         | 3.manuscript_SI.Rmd                |Make the supporting plots that will not end up in the main manuscript text.|


The 0 level scripts, 0.set_up.R and 0.functions.R scripts are used by the L1-3 scripts to set up the same R environment and load functions. 






