# V-Dem Dataset Construction

This repository showcases the V-Dem dataset construction pipeline scripts. 

## Modularized data pipeline
The V-Dem dataset is constructed using 130 modules comprising of 85+ scripts. Many modules get run once per variable resulting in over 7000 individual tasks. These tasks have complex dependencies and we have a system in place that keeps track of the dependencies and calculates or recalculates any number of desired tasks automatically and where possible, in parallel. Unique task_id allow for individual specification of each task, automatically loading dependent files from previous tasks and running through the appropriate script. The data pipeline is integrated with an HPC cluster in that the computationally intensive parts are calculated on the cluster and results are automatically retrieved and based on convergence diagnostics where appropriate automatically recalculated with a higher number of iterations. We keep the log output of every task, also including seeds, in a database. The data pipeline contains all the steps from retrieving the raw data from our database until the final dataset available for download at [https://www.v-dem.net/data](https://www.v-dem.net/data).

The pipeline scripts are divided into two groups: `modules` and `models`. The scripts in `modules` range from data wrangling tasks to the final dataset script. These scripts are designed to be run for many individual variables and all the variables are complied in the final dataset script. Tje `models` scripts relate to the four different models in the V-Dem pipeline that are each run on an HPC cluster. The simulations and other computational tasks were enabled by resources provided by the National Academic Infrastructure for Supercomputing in Sweden (NAISS).

Summary:  
7000+ tasks  
130 modules  
100+ R scripts  
R package vutils  
R package vbase  

The codebase is continuously being developed internally, but we are planning to release the code once a year in tandem with the V-Dem dataset annual release cycle. 

For questions or comments please use contact@v-dem.net.

## Relevant links
Learn more about the V-Dem methodology and our measurement model from the "The V-Dem Measurement Model: Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data" which can be downloaded [here](https://v-dem.net/media/publications/wp21_2026.pdf).

All relevant reference documents for the V-Dem dataset can be found on our website: [www.v-dem.net/data/reference-documents/](https://v-dem.net/data/reference-documents/).

The V-Dem dataset can be downloaded at [www.v-dem.net/data/](https://v-dem.net/data/).

## R Libraries
checkpoint date: 2026-01-03

## Platform
sessionInfo()  
R version 4.2.2 Patched (2022-11-10 r83330)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 12 (bookworm)

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/atlas/libblas.so.3.10.3
LAPACK: /usr/lib/x86_64-linux-gnu/atlas/liblapack.so.3.10.3

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.2.2