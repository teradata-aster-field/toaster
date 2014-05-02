# toaster

toaster (to Aster) is set of tools for computing and analyzing data with [Teradata Aster](http://www.asterdata.com/) Big Data database. It brings power of Aster's distributed SQL and map-reduce (SQL-MR) to R on desktop and compliments analysis of results with convinient set of plotting functions.

toaster acheives most tasks in 2 distinct steps:

* Compute in Aster using rich set of analyical functions and SQL transparently running in distributed and parallel environement.

* Deliver and visualize results in R for further exploration and analysis.
 
Thus toaster performs all data heavy computations in Aster with results available in R. Summary statistics, aggregates, histograms, heatmaps, linear regression models are among results available from Aster. Most of the results have toaster visualization functions for further analysis.

You can install:

* the latest released version from CRAN with

    ```R
    install.packages("toaster")
    ````


* the latest development version from bitbucket with

    ```R
    devtools::install_bitbucket("toaster", "grigory")
    ````

* evaluation version of Aster analytic platform - Aster Express - to run on your PC [here](http://community.teradata.com/community/teradata-big-data-analytics-community/downloads) and get started with this [Tutorial Series](http://www.asterdata.com/download_aster_express/tutorial.php).


If you encounter a clear bug, please file a minimal reproducible example on [bitbucket](https://bitbucket.org/grigory/toaster/issues).

Attribution:

* Icon: [Toaster Icon by Greg Barnes](http://www.iconarchive.com/show/vintage-kitchen-icons-by-greg-barnes/Toaster-icon.html), [Vintage Kitchen](http://www.iconarchive.com/show/vintage-kitchen-icons-by-greg-barnes.html)

