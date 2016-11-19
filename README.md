Supplementary material for the article:

Extending R Packages to Support 64-bit Compiled Code:
An Illustration with spam64 and GIMMS NDVI3g Data

Computers & Geoscience 

Authors: Florian Gerber,
         Kaspar Moesinger,
	 Reinhard Furrer (reinhard.furrer@math.uzh.ch)


We provide the packages spam64_2.0-05, spam_2.0-05, and 
dotCall64_0.9-01 together with the R scripts that where 
used to create the figures and tables shown in the 
manuscript. 

This file is available at 

https://github.com/florafauna/CAGEO-spam64-supplement

A current devel version of spam version 2.x is available at

https://git.math.uzh.ch/reinhard.furrer/spam


Notes 
-----

Makefiles are available in all sub-folders to simplify 
the execution of the R scripts. This was used to 
execute scripts in parallel via, e.g.,
"make -j10". 
The entire analysis is not intended to work with one single
call to make. For example, the spam packages in ./spam_libs
need to be installed before the other scripts can be run.

The analysis was performed with
``` r
R version 3.3.1 (2016-06-21)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.5 LTS

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=C               LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=de_CH.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] raster_2.5-8           sp_1.2-3               OpenMPController_0.1-2
 [4] gdata_2.17.0           reshape2_1.4.1         ggplot2_2.1.0         
 [7] fields_8.4-1           maps_3.1.1             foreach_1.4.3         
[10] spam64_2.0-05          spam_2.0-05            dotCall64_0.9-01      

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.6      lattice_0.20-33  codetools_0.2-14 gtools_3.5.0     plyr_1.8.4      
 [6] gtable_0.2.0     magrittr_1.5     scales_0.4.0     stringi_1.1.1    tools_3.3.1     
[11] iterators_1.0.8  stringr_1.1.0    munsell_0.4.3    colorspace_1.2-6
```

Data organization 
-----------------

./spam_libs
The R packages 
spam_1.4-0.tar.gz
spam_2.0-05.tar.gz
spam64_2.0-05.tar.gz
dotCall64_0.9-04.tar.gz
which were used for the performance measurements and the data analysis.

./performance
The performance measurements shown in the paper.

./demos
The R scripts of the demos shown in the paper. 

./NDVI_model
The R scripts for the NDVI model shown in the paper. 
Note that we do not provide the original datasets since 
they are too large (several GBs). Instead, we provide that
preprocessed data in ./NDVI_model/data/europe_asia-785655_v4.RData.
With this the model fit can be done by executing the scripts 
6_preprocess.R, 7_chol.R, 8_fit.R, 8_fit_simple.R, 
9_summary_simple.R, 10_summary.R
The original data is distributed by the organizations indicated
in the manuscript. 
In ./NDVI_model/Rout the .Rout files of the executed R CMD BATCH command are given. 
In ./NDVI_model/figs the figures produces by the R scripts are given. 

./cited_R_source_files
Files from the R source code cited in the manuscript. 
This includes also the mentioned diff files generated with 
'svn diff -r 59004:59009' of the corresponding files in the 
R svn repository (https://svn.r-project.org/R/trunk/R-devel.)