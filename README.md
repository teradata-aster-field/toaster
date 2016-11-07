# toaster

[![Travis-CI Build Status](https://travis-ci.org/teradata-aster-field/toaster.svg?branch=master)](https://travis-ci.org/teradata-aster-field/toaster)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/toaster)](https://CRAN.R-project.org/package=toaster)
[![Downloads_all_badge](http://cranlogs.r-pkg.org/badges/grand-total/toaster?color=orange)](http://www.r-pkg.org/pkg/toaster)
[![Downloads_week_badge](http://cranlogs.r-pkg.org/badges/last-week/tcR?color=orange)](http://www.r-pkg.org/pkg/toaster)
[![License](http://img.shields.io/:license-gpl2-blue.svg)](http://www.gnu.org/licenses/gpl-2.0.html)
[![Coverage Status](https://img.shields.io/codecov/c/github/teradata-aster-field/toaster/master.svg)](https://codecov.io/github/teradata-aster-field/toaster?branch=master)


toaster (to Aster) is a set of tools for computing and analyzing data with [Teradata Aster](http://www.teradata.com/Teradata-Aster/overview/) Big Data database. It brings the power of Teradata Aster's distributed SQL, MapReduce (SQL-MR), and Graph Engine (SQL-GR) to R on desktop and complements analysis of results with a convenient set of plotting functions.

toaster acheives most tasks in 2 distinct steps:

* Compute in Aster using Aster's rich, fully scalable set of analyical functions, transparently running in distributed and parallel environement.

* Deliver and visualize results in R for further exploration and analysis.
 
toaster performs all big data, processing intensive computations in Aster, making results and visualizations available in R. Summary statistics, aggregates, histograms, heatmaps, and coefficients from linear regression models are among results available in R after processing in Aster. Most results have toaster visualization functions to aid further analysis.

You can install:

* the latest released version from CRAN with

    ```R
    install.packages("toaster")
    ````


* the latest development version from github with

    ```R
    devtools::install_github("toaster", "teradata-aster-field")
    ````

* evaluation version of Aster analytic platform - Aster Express - to run on your PC [here](https://aster-community.teradata.com/community/download) and get started with this [Tutorial Series](https://aster-community.teradata.com/community/learn-aster).


If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/teradata-aster-field/toaster/issues).

Attribution:

* Icon: [toAster](http://maytitan.deviantart.com/art/ToAster-468393069) by [Madison Clarke](http://maytitan.deviantart.com/)
