The dataset construction code for the V-Dem dataset [https://www.v-dem.net/en/](https://www.v-dem.net/en/).

For questions or comments please use contact@v-dem.net.

## Modularized data pipeline
Summary:  
6718 tasks  
95 modules  
85+ R scripts  
R package vutils  

The V-Dem dataset is constructed using 95 modules comprising of 85+ scripts. Many modules get run once per variable resulting in a total number of 6718 tasks. These tasks have complex dependencies and we have a system in place that keeps track of the dependencies and calculates or recalculates any number of desired tasks automatically and where possible, in parallel. Since a task when it is run knows its own task_id it can load its dependent files from previous tasks automatically. The data pipeline is integrated with an HPC cluster in that the computationally intensive parts are calculated on the cluster and results are automatically retrieved and based on convergence diagnostics where appropriate automatically recalculated with a higher number of iterations. We keep the log output of every task, also including seeds, in a database. The data pipeline contains all the steps from retrieving the raw data from our database until the final dataset available for download at [https://www.v-dem.net/en/](https://www.v-dem.net/en/).

The codebase is continuously being developed internally, but we are planning to release the code once a year in tandem with the V-Dem dataset annual release cycle.

## R Libraries
checkpoint date: 2020-11-11

## Platform
sessionInfo()  
R version 4.0.3 (2020-10-10)  
Platform: x86_64-pc-linux-gnu (64-bit)  
Running under: Debian GNU/Linux bullseye/sid  

Matrix products: default  
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3  
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.13.so  

locale:  
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:  
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:  
[1] colorout_1.2-2

loaded via a namespace (and not attached):  
[1] compiler_4.0.3