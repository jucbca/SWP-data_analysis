# SWP-data_analysis
This process standardizes the phenotyping of wound-induced electric signaling in Arabidopsis thaliana plants. As shown next, the signal parameters calculated are shown as violin plots clustered by sample type (e.g. genotype or treatment). Average traces are computed and shown as mean ± SEM. 

![Exemplary figure of control and treatment one.]{Figure_example.pdf}

The 3D printable parts (WoundingTeeth.stl) standardize the wounding stimulus, maximizing the response triggering probability.

The R script is designed for high-throughput importing, managing, plotting and analysing extracellular electric recordings of wound-induced Slow Wave Potentials.

The code is organized as a R notebook, and runs in chunks. 
Open SWPanalyzer.Rmd in RStudio and follow the instructions.

NOTE:
Details on the data structure of input and output is described for every step in the R notebook (SWPanalyzer.Rmd). 
Given the high-throughput nature of the script, and the unknown distribution of your resulting data, final plots and STATISTICS ARE JUST A GUIDE and MUST be doubled checked manually.
