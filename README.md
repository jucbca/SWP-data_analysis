# SWP-data_analysis
This process standardizes the phenotyping of wound-induced electric signaling in Arabidopsis thaliana plants. As shown next, the signal parameters calculated are shown as violin plots clustered by sample type (e.g. genotype or treatment). Average traces are computed and shown as mean ± SEM. 
In the ![validation dataset](https://github.com/jucbca/SWP-data_analysis/tree/main/validation%20dataset) is the complete output data structure of a full run of the code on real data.

![Exemplary figure of control and treatment.](Figure_example.png)

The 3D printable parts (![go to 3D model](https://github.com/jucbca/SWP-data_analysis/blob/main/Wounding_Grid.stl)) standardize the wounding stimulus, maximizing the response triggering probability.

The R script (![go to R notebook](https://github.com/jucbca/SWP-data_analysis/blob/main/SWPanalyzer.Rmd) ) is designed for high-throughput importing, managing, plotting and analysing extracellular electric recordings of wound-induced Slow Wave Potentials.

The code is organized as a R notebook, and runs in chunks. 
Open SWPanalyzer.Rmd in RStudio and follow the instructions.

NOTE:
Details on the data structure of input and output is described for every step in the R notebook (SWPanalyzer.Rmd). 
Given the high-throughput nature of the script, and the unknown distribution of your resulting data, final plots and STATISTICS ARE JUST A GUIDE and MUST be doubled checked manually.



