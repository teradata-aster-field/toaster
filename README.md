# toaster

toaster (to Aster) is set of tools for computing and analyzing data with [Teradata Aster](http://www.asterdata.com/) Big Data database. It brings power of Aster database with its distributed SQL and map-reduce functions to R on desktop. It also makes easier to visualize and explore results with convinient set of visualization functions based on ggplot2 plotting system.
 
toaster offers way to utilize Aster's sQL and rich set of analyical functions transparently running in distributed and
parallel environement. toaster performs all data heavy computations in Aster with results, statistics, and models available in R. 
Summary statistics, aggregates, histograms, linear regression models, etc. are among results. Most of them are avaiable for visualization and further analysis with convinent plotting functions. toaster's goal is to achieve all tasks in 2 distinct steps:

* Compute in Aster and deliver results to R.

* Quickly visualize results for further exploration and analysis.

You can install:
* the latest development version from bitbucket with

    ```R
    devtools::install_bitbucket("toaster", "grigory")
    ````

If you encounter a clear bug, please file a minimal reproducible example on [bitbucket](https://bitbucket.org/grigory/toaster/issues).

Attribution:
Icon: Toaster by Luiza Peixe from The Noun Project

