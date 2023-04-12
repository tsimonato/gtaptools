agg_har <- function(input_data,
                    model = NULL,
                    var,
                    var_weight = NULL,
                    fun = sum,
                    output_har_file = NULL) {
  
  #' @name agg_har
  #' @title Aggregates headers of data in .har structure.
  #'
  #' @description It aggregates variables from a .har file on disk or an object with the structure exported by the read_har function. It is possible to adopt customized weights and functions to calculate aggregations. The specification of GTAP models (...) through the *model* parameter is supported so that the respective weight variables are automatically detected according to the model being analyzed.
  #'
  #' @param input_data It can indicate a path to a .har file or an existing object in the R environment that has the output structure of the read_har function.
  #' @param model Indicates the CGE model being worked on. (Supports only GTAP, ....)
  #' @param var A list object (see example section) with variables names that will be aggregated.
  #' @param var_weight A list object that relates the name of the aggregated variables (*var*) with the name of their respective weights, if any. An aggregation without weights will be performed if the weight variable is not found in the input data (*input_data*).
  #' @param fun Function that will be applied to aggregate (default = sum).
  #' @param output_har_file Output .har file name.
  #'
  #' @import dplyr
  #'
  #'
  #' @export
  

  
  
  
  
  
  
  
  
  
  
}