
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtaptools

<!-- badges: start -->
<!-- badges: end -->

The *gtaptools* package is a set of functions designed for supporting
simulation exercises with CGE (Computable General Equilibrium) models in
R language. The primary goal of this package is to facilitate and
improve file management, increase the analytical potential of the
database, and provide graphical visualizations of simulation results.
The package includes various functions to handle data management tasks,
including data import/export, data manipulation, merging, subsetting,
and cleaning. These functions are designed to streamline the data
preparation process and ensure that data is in a suitable format for use
in the CGE model. In addition to data management, the gtaptools package
includes functions for conducting simulations and analyzing simulation
results. One of the key features of the gtaptools package is its focus
on graphical visualization. The package includes a range of functions
for constructing various types of graphical visualizations, such as
heatmaps, stacked bar charts, and scatterplots. The package is designed
to be user-friendly and comes with detailed documentation and example
code that illustrates how to use its resources. Overall, the gtaptools
package is an interesting tool for researchers, policymakers, and
students who wish to conduct simulation exercises with CGE models and
have access to libraries available in the R language universe. Its focus
on file management, data analysis, and graphical visualization makes it
a powerful tool for building automated pipelines, reducing the chance of
human errors, and increasing productivity.

## Installation

You can install the development version of gtaptools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tsimonato/gtaptools")
```

## Example

This is a basic usage example:

``` r

library(gtaptools)
## basic example code
squeeze(cmf_file = "data/test/teste/termdyn_hou.cmf",
        zip_file = "new_sim.zip")
```