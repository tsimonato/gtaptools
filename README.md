
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gtaptools \[under development\]

<!-- badges: start -->
<!-- badges: end -->

The *gtaptools* is a package <u>under development</u> that aims to offer
a set of functions designed for supporting simulation exercises with CGE
(Computable General Equilibrium) models in R language. The primary goal
of this package is to facilitate and improve file management, increase
the analytical potential of the database, and provide graphical
visualizations of simulation results. The manual with the description of
the functions can be found
[here](https://github.com/tsimonato/gtaptools/tree/master/docs/manual).

The package includes various functions to handle data management tasks,
including data import/export, data manipulation, merging, sub-setting,
and cleaning. These functions are designed to streamline the data
preparation process and ensure that data is in a suitable format for use
in the CGE model. In addition to data management, the gtaptools package
includes functions for conducting simulations and analyzing simulation
results.

One of the key features of the gtaptools package is its focus on
graphical visualization. The package includes a range of functions for
constructing various types of charts, such as heatmaps, stacked bar
charts, scatterplots, and spatial visualizations. The package is
designed to be user-friendly and comes with detailed documentation and
example code that illustrates how to use its resources.

Overall, the gtaptools package is an interesting tool for researchers,
policymakers, and students who wish to conduct simulation exercises with
CGE models and have access to libraries available in the R language
universe. Its focus on file management, data analysis, and graphical
visualization makes it a powerful tool for building automated pipelines,
reducing the chance of human errors, and increasing productivity.

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

path_to_har <- gtaptools::templates("oranig_example.har")

 input_data <- list(
    path_to_har # Path to .har database
 )

new_calculated_vars <- list(
   quote(MARC["COM"] := `1MAR`), # Sum 1MAR to set COM
   quote(MULT[c("REG", "HOU")] := solve(MAKE)), # Solve the MAKE matrix
   quote(NSET := c("Comm1", "Comm2")) # Create sets
)

output_har_r <- 
  gtaptools::har_shape(
    input_data = input_data,
    new_calculated_vars = new_calculated_vars, 
    del_headers = c("1LND"),
    export_sets = "gtaptools_shape_example2_sets.har", 
    output_har_file = "gtaptools_shape_example2.har"
)
#> 1BAS with maxsize 1e+05
#> 3BAS with maxsize 1e+05
#> 4BAS with maxsize 1e+05
#> 5BAS with maxsize 1e+05
#> 6BAS with maxsize 1e+05
#> 1MAR with maxsize 1e+05
#> 3MAR with maxsize 1e+05
#> 4MAR with maxsize 1e+05
#> 5MAR with maxsize 1e+05
#> 1TAX with maxsize 1e+05
#> 3TAX with maxsize 1e+05
#> 4TAX with maxsize 1e+05
#> 5TAX with maxsize 1e+05
#> 1LAB with maxsize 1e+05
#> 1PTX with maxsize 1e+05
#> 1OCT with maxsize 1e+05
#> 0TAR with maxsize 1e+05
#> SLAB with maxsize 1e+05
#> P028 with maxsize 1e+05
#> 1ARM with maxsize 1e+05
#> MAKE with maxsize 1e+05
#> SCET with maxsize 1e+05
#> 2ARM with maxsize 1e+05
#> 3ARM with maxsize 1e+05
#> ITEX with maxsize 1e+05
#> P018 with maxsize 1e+05
#> LCOM with maxsize 1e+05
#> 1CAP with maxsize 1e+05
#> 2BAS with maxsize 1e+05
#> 2MAR with maxsize 1e+05
#> 2TAX with maxsize 1e+05
#> XPLH with maxsize 1e+05
#> 3PUR with maxsize 1e+05
#> P21H with maxsize 1e+05
#> EXNT with maxsize 1e+05
#> MARC with maxsize 1e+05
#> MULT with maxsize 1e+05
```

## References

M. Ivanic, HARr, (2020), GitHub repository:
<https://github.com/USDA-ERS/MTED-HARr>
