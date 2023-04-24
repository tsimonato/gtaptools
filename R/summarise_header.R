summarise_header <- function(input_data,
                             sets,
                             col_values = NULL,
                             input_header = NULL,
                             fun = function(x) sum(x, na.rm = T),
                             new_header_name,
                             export_sets = T,
                             output_har_file = NULL,
                             output_csv_file = NULL,
                             ...) {
  #' @name summarise_header
  #' @title Aggregates headers of data in .har structure.
  #'
  #' @description Summarizes a single database to an array compatible format for writing to .har files.
  #'
  #' @param input_data An array that has the output structure of the read_har function or a data.frame.
  #' @param sets Dimensions/columns names that contain the categorical variables for summarizing the output sets. It must be a maximum of 3 characters each.
  #' @param col_values Name of the numerical columns with the values. (only necessary when input_data is a data.frame)
  #' @param input_header Name of the reader to be read. (Only necessary when input_data is an .har file)
  #' @param fun Function used for aggregation in case of non-unique values in sets (default = sum).
  #' @param new_header_name Name of the new header created.
  #' @param export_sets If TRUE, the vectors with the set elements are incorporated with the output. If an output .har file is indicated, it will be created and exported to that .har file. If FALSE, they will not be exported.
  #' @param output_har_file Output .har file name.
  #' @param output_csv_file Output .csv file name.
  #' @param ... Any additional arguments to be used to write the .csv file through data.table::fwrite, such as separator character (sep = ","), the character for decimal points (dec = "."), etc.
  #'
  #' @return An array or vector of characters (sets) structured in a compatible way to compose a .har file.
  #'
  #' @importFrom HARr read_har write_har
  #' @import data.table
  #'
  #' @examples
  #' 
  #'# -Take a data.frame as input 
  #'#   (Could be a path to a .har file, in which case,
  #'#   instead of col_values, input_header should be specified) >
  #'# -Aggregates the numeric variable "Freq" from 4 sets 
  #'#   to 3 sets (COM, SRC, MAR) by simple sum (default) >
  #'# -Write the new header with the name "EXAM" 
  #'#   in a .har file ("gtaptools_summarise_example.har") >
  #'# -Save the sets that compose the new header 
  #'#   in a separate .har file ("gtaptools_summarise_example_sets.har") >
  #'# -Return an array "example_df_CSM".
  #'
  #'example_df_CSM <- gtaptools::summarise_header(
  #'  input_data = gtaptools::example_df,
  #'  sets = c("COM", "SRC", "MAR"),
  #'  col_values = "Freq",
  #'  new_header_name = "EXAM",
  #'  export_sets = "gtaptools_summarise_example_sets.har",
  #'  output_har_file = "gtaptools_summarise_example.har"
  #')  
  #'
  #'
  #'# To explore the object as a data.frame:
  #'as.data.frame.table(example_df_CSM$EXAM)
  #'
  #'#-Get an array as input >
  #'#-Aggregate from the 3 sets of the previous output
  #'#  to 2 sets (SRC, MAR) by simple mean >
  #'#-Save the new header with the name "EXAM" in a .har file
  #'#  ("gtaptools_summarise_example.har") and creates headers for 
  #'   each the set that compose it (export_sets = T, its default)>
  #'#-Export the data as unpivot table to a .csv file ("gtaptools_summarise_example.csv") with 
  #'#  separator "," (default) and decimal point "." (default).
  #'#-Return an array "example_df_SM".
  #'
  #'example_df_SM <- gtaptools::summarise_header(
  #' input_data = example_df_CSM$EXAM,
  #' sets = c("SRC", "MAR"),
  #' #col_values = "Freq",
  #' fun = function(x) mean(x, na.rm = T),
  #' new_header_name = "EXAM",
  #' export_sets = T,
  #' output_har_file = "gtaptools_summarise_example.har",
  #' output_csv_file = "gtaptools_summarise_example.csv",
  #' sep = ",",
  #' dec = "."
  #' ) 
  #' 
  #' # To explore the object as a data.frame:
  #' as.data.frame.table(example_df_SM$EXAM)
  #' 
  #'
  #' @export
  #
  #   path_to_folder <- system.file("extdata", package="gtaptools")
  #
  #   data_example <- mtcars
  #   iris
  #
  #   gtaptools::summarise_header(
  #     input_data = data_example,
  #     sets = c("mpg", "cyl", "am"),
  #     values = "hp",
  #     fun = function(x) sum(x, na.rm = T),
  #     new_header_name = "CARS",
  #     export_sets = T,
  #     output_har_file = "cars_example.har"
  #   )
  #
  #
  #   input_data = data_example
  #   sets = c("mpg", "cyl", "am")
  #   values = "hp"
  #   fun = function(x) sum(x, na.rm = T)
  #   new_header_name = "CARS"
  #   export_sets = T
  #   output_har_file = "cars_example.har"


  #
  # new_var <- NULL
  # input_data_text <- "data/test/teste/input/REG_DYN_HOU.har"
  # input_data <- HARr::read_har(input_data_text,
  #   useCoefficientsAsNames = F,
  #   toLowerCase = F
  # )
  #
  # input_data2 <- input_data1$MAKE |> as.data.frame.table()
  # input_data3 <- input_data1$BSMR |> as.data.frame.table()
  # input_data3 <- input_data1$BSMR
  # input_data4 <- input_data1$HOU
  #
  # input_data <- input_data2
  #
  #
  #
  #
  #
  #
  #
  #   new_calculated_vars <- list(
  #     list(
  #       x = "MAKE",
  #       y = "1CAP",
  #       z = "TFRO",
  #       fun = function(x, y, z) sum(x + y + z),
  #       new_header = "XXXX",
  #       new_dim_cols = c("COM", "IND", "DST")
  #     ),
  #     list(
  #       x = "BSMR",
  #       fun = function(x) log(x),
  #       new_header = "YYYY",
  #       new_dim_cols = c("COM")
  #     ),
  #     list(
  #       x = "BSMR",
  #       fun = function(x) sum(x),
  #       new_header = "MAK4",
  #       new_dim_cols = c("COM", "SRC")
  #     )
  #   )
  #
  #
  #   input_data <- list(
  #     input_data1,
  #     list(
  #       data = input_data2,
  #       dim_cols = c("COM", "IND", "DST"),
  #       values = "Freq",
  #       name_header = "MAKE"
  #     ),
  #     list(
  #       data = input_data3,
  #       dim_cols = c("COM", "SRC", "USR", "DST"),
  #       values = "Freq",
  #       name_header = "MAKE2"
  #     ),
  #     list(
  #       data = input_data4,
  #       name_header = "HOU"
  #     )
  #   )




  # new_derived_vars <- list(
  #   list(
  #     input_header = "MAKE",
  #     new_header = "MAK4",
  #     new_header_dim = c("COM", "IND")
  #   ),
  #   list(
  #     input_header = "BSMR" ,
  #     new_header = "BSM2",
  #     new_header_dim = "COM"
  #   )
  # )

  # df_data = input_data2
  # sets = c("COM", "IND")
  # values = "Freq"
  # new_header_name = "OOOOO"
  #
  #
  # input_data=input_data2
  # sets = c("COM", "IND")
  # values = "Freq"
  # new_header_name = "OOOO"
  # fun = function(x) sum(x, na.rm = T)
  # output_har_file = "Test.har"
  #
  #
  # # input <- input_data1$BSMR
  #
  # df_data <- input$data
  if (!(is.data.frame(input_data) | is.array(input_data) | is.list(input_data))) {
    if (is.character(input_data) & grepl("*.har", input_data)) {
      input_data <- HARr::read_har(input_data, toLowerCase = F)#[[tolower(header)]]
      col_values <- "Freq"
    } else {
      stop("Input_data must be a data.frame, array or a path to a .har file.")
    }
  }

  #sets <- tolower(sets)

  summ_header <- function(input_data,
                          sets,
                          col_values,
                          new_header_name,
                          fun = function(x) sum(x, na.rm = T)) {
    if (!(is.data.frame(input_data) | is.array(input_data) | is.character(input_data))) {
      stop("The input data is not a data.frame, array or a vector of characters (sets).")
    }

    if (nchar(new_header_name) > 4) {
      stop("The name of the new header must have up to 4 characters.")
    }

    if (is.array(input_data)) {
      input_data <- as.data.frame.table(input_data)
    }

    test <- !c(sets, col_values) %in% names(input_data)
    if (any(test)) {
      cols_null <- c(sets, col_values)[test]
      stop(paste0(new_header_name, ": The ", paste0(cols_null, collapse = ", "), " columns indicated in the sets and/or values parameters are not found in the input database."))
    }

    
    col_values <- ifelse(is.null(col_values), "Freq", col_values)

    df_data <- as.data.frame(input_data)
    df_data <- df_data[c(sets, col_values)]
    df_data[[col_values]] <- as.numeric(df_data[[col_values]])
    # df_data <- data.table::as.data.table(df_data)
    df_data <- data.table::setDT(df_data)[, lapply(.SD, fun), by = c(sets)]
    df_data <- as.data.frame(df_data)

    dim_info <- df_data[sets]
    dim <- sapply(dim_info, function(x) length(unique(x)))
    dimnam <- sapply(dim_info, function(x) unique(x))

    if (!is.list(dimnam)) {
      dimnam <- as.list(as.data.frame(dimnam))
    }

    if (any(nchar(names(dim)) > 3)) {
      stop("Set names must be up to 3 characters.")
    }

    har_data <- c()
    har_data[[new_header_name]] <- array(
      data = df_data[[col_values]],
      dim = dim,
      dimnames = dimnam
    )

    return(har_data)
  }


  input_har <- c()
  if (!is.character(input_data)) {
    if (is.array(input_data)) {
      col_values <- ifelse(is.null(col_values), "Freq", col_values)
    }

    input_har <- summ_header(input_data,
      sets = sets,
      col_values = col_values,
      new_header_name = new_header_name,
      fun = fun
    )

    input_har_no_sets <- input_har
    if (export_sets == T) {
      input_har <- c(input_har, dimnames(input_har[[new_header_name]]))
    }
    if (is.character(export_sets)) {
      HARr::write_har(dimnames(input_har[[new_header_name]]),
        filename = export_sets
      )
    }
  } else {
    input_data <- input_data[input_data != "NA"]
    input_data <- input_data[input_data != ""]
    input_data <- input_data[!is.na(input_data)]
    input_data <- na.omit(input_data)
    input_har[[new_header_name]] <- unique(as.character(input_data))
  }


  if (!is.null(output_har_file)) {
    HARr::write_har(input_har, filename = output_har_file)
  }

  if (!is.null(output_csv_file)) {
    data.table::fwrite(
      x = as.data.frame.table(input_har_no_sets[[1]]),
      file = output_csv_file,
      ...
    )
  }

  return(input_har)
}
  
  
