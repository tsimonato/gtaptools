% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agg_har.R
\name{agg_har}
\alias{agg_har}
\title{Aggregates headers of data in .har structure.}
\usage{
agg_har(
  input_data,
  model = NULL,
  correspondences,
  vars_weighted_mean,
  output_har_file = NULL
)
}
\arguments{
\item{input_data}{It can indicate a path to a .har file or an existing object in the R environment that has the output structure of the read_har function.}

\item{model}{Indicates the CGE model being worked on (Supports only GTAP, ....). For supported models, this information is sufficient to define the variables that must be aggregated through weighted mean.}

\item{correspondences}{A list indicating the original sets and new aggregated sets that will be exported. It can indicate a path to a .csv file or an existing object in the R environment that has the correspondences between the *input_data* sets and the new aggregated sets. The first line will be considered as header and identifier, and must necessarily contain the same name as the set that must be aggregated from *input_data*.}

\item{vars_weighted_mean}{Vector of characters relating the variables that must be grouped with a weighted average of their respective weight variables, in the format c( "var" = "weight"). Please note the example section. The sets of the variable and its weight will be made compatible through the aggregation by sum of the weight variable, if necessary.}

\item{output_har_file}{Output .har file name.}
}
\description{
It aggregates variables from a .har file on disk or an object with the structure exported by the read_har function. It is possible to adopt customized weights and functions to calculate aggregations. The specification of GTAP models (...) through the *model* parameter is supported so that the respective weight variables are automatically detected according to the model being analyzed.
}
