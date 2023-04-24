har_shape <- function(input_data,
                      new_calculated_vars = NULL,
                      del_headers = NULL,
                      output_har_file = NULL) {
  #' @name har_shape
  #' @title Merge and generate new headers.
  #'
  #' @description Allows the combination of different databases in data.frame or array format. Generate new variables flexibly from custom functions. Calculations can be performed between headers/variables of different dimensions/sets.
  #'
  #'
  #'
  #' @param input_data It must consist of one or more input databases, which must be separated from each other by sublists (see example). In the case of multiple databases, all will be combined for the final output.Arrays and data.frames must be inside sublists (list(....)) as indicated in the examples section. Aggregations on input data can only be performed on single array and data.frame inputs.
  #' @param new_calculated_vars New variables resulting from custom calculations between the headers contained in input_data. Each variable's parameters must be informed (it can be *x, y, z ...*), the function *fun* that represents the calculation to be done, the *new_header_name*, and the *sets* for the output structure. The different headers must have at least one similar set so that it is possible to establish correspondence between them. Please note the example section.
  #' @param del_headers Vector of characters with the names of headers that must be excluded from the output.
  #' @param output_har_file Output .har file name.
  #'
  #' @importFrom HARr write_har read_har
  #' @import data.table
  #'
  #' @note
  #' Bear in mind that, for performance reasons, before carrying-out custom calculations between different Headers with different sets, the function aggregates all headers involved in the computation to the output set. That is, in a MAKE(COM, IND)/1CAP(COM) division operation, the first operation is the aggregation for MAKE(COM), and only then is the division calculation processed.
  #' Therefore, for example, a weighted average aggregation cannot be done directly. It is recommended to use the agg_har function in this case.
  #'
  #'
  #'
  #'
  #' @examples
  #' # example code
  #'
  #' # -Reads list_df, a list of input data (data.frame(1), list of arrays(2), array(3)) >
  #' # -Aggregates the input (1) for "MAR1" in 3 sets by simple addition (default) >
  #' # -Aggregates the input(3) for "XPLC" into 1 set per average >
  #' # -Deletes headers "MAR1" and "3pur" >
  #' # -Saves the output in a .har file ("gtaptools_shape_example1.har") >
  #' # -Returns the list of "binded_df" arrays.
  #'
  #' list_df <- list(
  #'  list(
  #'    input_data = gtaptools::example_df, # 1 - data.frame
  #'    sets = c("COM", "SRC", "MAR"), # sum on IND
  #'    col_values = "Freq",
  #'  new_header_name = "MAR1"
  #' ),
  #' gtaptools::example_arrays_har, # 2 - list of arrays
  #'  list(
  #'    input_data = gtaptools::example_arrays_har$xplh, # 3 - array
  #'    sets = c("COM"), # sum in HOU,
  #'    fun = function(x) mean(x),
  #'    new_header_name = "XPLC"
  #' )
  #' )
  #'
  #' binded_df <-
  #'   gtaptools::har_shape(
  #'     input_data = list_df,
  #'     del_headers = c("MAR1", "3pur"),
  #'     output_har_file = "gtaptools_shape_example1.har"
  #'   )
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #'
  #' @export

  #
  #   output_har_file = "test.har"
  #   del_header <- "MAKE"
  #
  #   input_data_text <- "data/test/teste/input/REG_DYN_HOU.har"
  #   input_data1 <- HARr::read_har(input_data_text,
  #     useCoefficientsAsNames = F,
  #     toLowerCase = F
  #   )
  #
  #   input_data2 <- input_data1$MAKE |> as.data.frame.table()
  #   input_data3 <- input_data1$BSMR |> as.data.frame.table()
  #   input_data4 <- input_data1$HOU
  #   input_data5 <- input_data1$MAKE
  #
  #
  #   1.04313517 + 23.37448354
  #
  #   1.04313517 + 23.37448354
  #
  #   log(19078.02545586)
  #
  #
  #   new_calculated_vars <- list(
  #     list(
  #       x = "MAKE",
  #       y = "BSMR",
  #       #z = "TFRO",
  #       fun = function(x, y) x + y,
  #       new_header = "MCID",
  #       new_sets = c("COM", "DST")
  #     ),
  #     list(
  #       x = "BSMR",
  #       fun = function(x) log(x),
  #       new_header = "BSMC",
  #       new_sets = c("COM")
  #     ),
  #     list(
  #       x = "BSMR",
  #       fun = function(x) sum(x),
  #       new_header = "BSCS",
  #       new_sets = c("COM", "SRC")
  #     )
  #   )
  #
  #   input_data <- list(
  #     input_data1,
  # list(
  #   input_data = input_data2,
  #   sets = c("COM", "IND", "DST"),
  #   values = "Freq",
  #   new_header_name = "MAKE"
  # ),
  # list(
  #   input_data = input_data3,
  #   sets = c("COM", "SRC"),
  #   values = "Freq",
  #   new_header_name = "MAKE"
  # ),
  # list(
  #   input_data = input_data4,
  #   new_header_name = "HOU"
  # )
  # )




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


  # l <- 4


  # convert to df_har
  # input <- c()
  
  input_har <- c()
  for (l in 1:length(input_data)) {
    input <- input_data[[l]]

    if (is.array(input)) {
      stop("One or more elements of the input database list (input_data) is an array. Please insert it into a sublist and describe its new_header_name, and sets.")
    }

    if (length(input) == 1) {
      if ((is.character(input[[1]]) & grepl("*.har", input[[1]]))) {
        input <- HARr::read_har(input[[1]], toLowerCase = F, useCoefficientsAsNames = F)

        input_har_diff <- base::setdiff(names(input_har), names(input))
        input_har <- c(input_har[input_har_diff], input)
      }
    } else if (!substr(names(input)[1], 1, 2) == "XX") {
      if (!is.null(input$input_data)) {
        input <- summarise_header(
          input_data = input$input_data,
          sets = input$sets,
          col_values = input$col_values,
          fun = ifelse(is.null(input$fun), function(x) sum(x, na.rm = T), input$fun),
          new_header_name = input$new_header_name
        )
        input_har_diff <- base::setdiff(names(input_har), names(input))
        input_har <- c(input_har[input_har_diff], input)
      } else {
        input_har_diff <- base::setdiff(names(input_har), names(input))
        input_har <- c(input_har[input_har_diff], input)
      }
    }
  }

  
  if (!is.null(new_calculated_vars)) {
    for (v in 1:length(new_calculated_vars)) {
      expr <- new_calculated_vars[[v]][[3]]
      result_calc <- with(input_har, eval(expr))

      if (is.character(result_calc)) {
        new_header_name <- new_calculated_vars[[v]][[2]]
        input_har[[new_header_name]] <- NULL
        input_har[[new_header_name]] <- result_calc
      } else {
        if (length(new_calculated_vars[[v]][[2]]) == 1) {
          new_h <- new_calculated_vars[[v]][[2]]
          stop(paste0(
            "New Header and its sets were not properly described: ",
            new_h,
            '. Please, describe it in a format: new_header_name[c("its sets")].'
          ))
        }

        sets <- new_calculated_vars[[v]][[2]][[3]]


        if (is.null(names(dimnames(result_calc)))) {
          result_calc <- as.array(result_calc)
          names(dimnames(result_calc)) <- eval(sets)
        } else {
          result_calc <- apply(result_calc, eval(sets), sum)
          if (is.null(names(dimnames(result_calc)))) {
            result_calc <- as.array(result_calc)
            names(dimnames(result_calc)) <- eval(sets)
          } 
        }



        new_header_name <- new_calculated_vars[[v]][[2]][[2]]
        input_har[[new_header_name]] <- NULL
        input_har[[new_header_name]] <- result_calc
      }
    }
  }

  if (!is.null(del_headers)) {
    input_har <- input_har[!names(input_har) %in% del_headers]
  }


  if (!is.null(output_har_file)) {
    HARr::write_har(input_har, filename = output_har_file)
  }



  return(input_har)
}


# if (!is.null(new_derived_vars)) {
#   for (l in 1:length(new_derived_vars)) {
#     new_d <- new_derived_vars[[l]]
#
#     new_h <- as.data.frame.table(input_har[[new_d$input_header]])
#     new_h <- new_h[c(new_d$new_header_dim, "Freq")]
#     new_h <- data.table::as.data.table(new_h)
#     new_h <- new_h[, lapply(.SD, base::sum, na.rm = T), by = c(new_d$new_header_dim)]
#     new_h <- as.data.frame(new_h)
#
#     dim_info <- new_h[new_d$new_header_dim]
#     dim <- sapply(dim_info, function(x) length(unique(x)))
#     dimnam <- sapply(dim_info, function(x) unique(x))
#     if (!is.list(dimnam)) {
#       dimnam <- as.list(as.data.frame(dimnam))
#     }
#
#     new_h <- array(
#       data = new_h$Freq,
#       dim = dim,
#       dimnames = dimnam
#     )
#
#     input_har[[new_d$new_header]] <- new_h
#   }
# }




#
#
#
#
#
#
#
#
#     a <- function(x,y,z) sum(x + y - x + z)
#
#     new_c$fun(MAKE, MAK4, TFRO)
#
#
#     fun(new_c$x, new_c$y, new_c$z)
#
#     input_har[[vars_fun[v]]]
#
#     a <- dplyr::full_join( as.data.frame.table(input_har$MAKE), as.data.frame.table(input_har$MAK4))
#     b <-
#       dplyr::full_join( a, as.data.frame.table(input_har$TFRO))
#
#
#
#
#     COM IND DST
#     1168022
#
#
#       new_h[[v+1]] <- data.table::as.data.table(new_h[[v+1]])
#       new_h[["merged"]] = merge(new_h, new_h[[v]])
#       new_h[["merged"]] = data.table::merge.data.table(new_h[v-1], new_h[[v]])
#       new_h[["merged"]] = dplyr::full_join(new_h[[v]], new_h[[v+1]])
#
#
#     }
#
#
#     class(new_h[[v+1]])
#
#
#     new_h <- as.data.frame.table(input_har[[new_d$input_header]])
#     new_h <- new_h[c(new_d$new_header_dim, "Freq")]
#     new_h <- data.table::as.data.table(new_h)
#     new_h <- new_h[, lapply(.SD, base::sum, na.rm = T), by = c(new_d$new_header_dim)]
#     new_h <- as.data.frame(new_h)
#
#     dim_info <- new_h[new_d$new_header_dim]
#     dim <- sapply(dim_info, function(x) length(unique(x)))
#     dimnam <- sapply(dim_info, function(x) unique(x))
#     if (!is.list(dimnam)) {
#       dimnam <- as.list(as.data.frame(dimnam))
#     }
#
#     new_h <- array(
#       data = new_h$Freq,
#       dim = dim,
#       dimnames = dimnam
#     )
#
#     input_har[[new_d$new_header]] <- new_h
#   }
# }
#
#
#
# }
#
#
# }
#
#
#
#
#
#
#
#
#
#
# se tiver mais do q x, merge em todas com loop all
# faz o calculo sumarizando nas dimensoes de saida
#
#
#
#
#
#
#
#
#
#
#
# input_data$
#
#
#
# usethis::use_r("create_header.R")
#
#
# var_custom_agg = list(SLAB = weighted.mean())
#
#
#
# fun = weighted.mean(w = `1LAB`[c("OCC","DST")])
#
# fun = function(x, fun, ...) fun(x, ...)
#
# fun(x= c(2,3,NA), fun = weighted.mean, w=(c(1,4,5)), na.rm=T)
#
#
#
#
# d = list(XX = DDD+FFF)
#
#
# lapply(list, function)
#
# function (X, FUN, ...)
# {
#   FUN <- match.fun(FUN)
#
#   fun <- sum
#   input_data_text <- "data/test/teste/input/REG_DYN_HOU.har"
#   input_data_text2 <- "data/test/teste/output/b37b-r37r-p37p-2021oct.sl4"
#   correspondences <- list(
#     list(
#       state = c("DST", "ORG", "PRD"),
#       input = "inst\\extdata\\example_corresp.csv",
#       sep = ";"
#     ),
#     list(
#       sec_new = c("IND", "COM"),
#       input = "inst\\extdata\\example_corresp.csv",
#       sep = ";"
#     ),
#     list(
#       usr_new = "USR",
#       input = "inst\\extdata\\example_corresp.csv",
#       sep = ";"
#     )
#   )
#
#   var_custom_agg <- list(
#     list(
#       var = c("DST", "ORG", "PRD"),
#       weight = "inst\\extdata\\example_corresp.csv",
#       fun = weighted.mean()
#     ),
#     list(
#       sec_new = c("IND", "COM"),
#       input = "inst\\extdata\\example_corresp.csv",
#       sep = ";"
#     ),
#     list(
#       usr_new = "USR",
#       input = "inst\\extdata\\example_corresp.csv",
#       sep = ";"
#     )
#   )
#
#
#   "inst\\extdata\\example_corresp.csv"
#
#
#
#   #  reading input .har file if input_data is a path
#   if (is.character(input_data_text)) {
#     input_data <- HARr::read_har(input_data_text,
#                                  useCoefficientsAsNames = F,
#                                  toLowerCase = F
#     )
#
#
#     # c <- 1
#     # loop em cada lista de correspondencia
#     agg_csv <- c()
#     for (c in 1:length(correspondences)) {
#       corr <- correspondences[[c]]
#       # Read correspondences if it is a csv file
#       if (is.character(corr$input)) {
#         corr$input <- read.csv(corr$input,
#                                sep = ifelse(!is.null(corr$sep),
#                                             corr$sep,
#                                             ","
#                                )
#         )
#       }
#       set_agg <- names(corr)[!names(corr) %in% c("input", "sep")]
#       set_agg_csv <- c(set_agg, corr[set_agg][[1]])
#       agg_csv[[c]] <- as.data.frame(unique(corr$input[set_agg_csv]))
#       agg_csv[[c]]["number"] <- row.names(agg_csv[[c]])
#       # h <- 18
#       # loop em cada header
#       for (h in 1:length(input_data)) {
#         if (is.array(input_data[[h]])) {
#           header <- as.data.frame.table(input_data[[h]])
#           # header2 <- input_data[[h]]
#           # header2 = input_data[[h]]
#           # set_agg <- names(corr)[!names(corr) %in% c("input", "sep")]
#           test <- corr[[set_agg]] %in% names(header)
#           # test <- corr[[set_agg]] %in% names(dimnames(header2))
#           # testa se o header tem alguma variavel para ser agregada
#           if (any(test)) {
#             col_names <- c(set_agg, corr[[set_agg]][test])
#             corr_to_merge <- unique(corr$input[col_names])
#             corr_to_merge[corr_to_merge == ""] <- NA
#             corr_to_merge <- na.omit(corr_to_merge)
#             # header_agg <- header
#             col <- 2
#             for (col in 2:length(corr_to_merge)) {
#               cols_agg <- c(col_names[1], col_names[col])
#               header <- data.table::as.data.table(header)
#               header <- data.table::merge.data.table(header,
#                                                      unique(corr_to_merge[cols_agg]),
#                                                      by = names(corr_to_merge[col])
#               )
#               header <- as.data.frame(header)
#               header[col_names[col]] <-
#                 as.factor(header[cols_agg[1]][[1]])
#               header[cols_agg[1]] <- NULL
#               # header2 <-
#               #   aggregate(Freq ~ ., data = header, FUN = fun)
#
#               # cols_group = names(header[,names(header)!="Freq"])
#               cols_group <- as.data.frame.table(input_data[[h]])
#               cols_group$Freq <- NULL
#               cols_group <- names(cols_group)
#               header <- data.table::as.data.table(header)
#               header <- header[, lapply(.SD, fun, na.rm = T), by = cols_group]
#               header <- as.data.frame(header)
#             }
#
#             cols_dim_array <- header
#             cols_dim_array$Freq <- NULL
#             dim <- sapply(cols_dim_array, function(x) length(unique(x)))
#             dimnam <- sapply(cols_dim_array, function(x) unique(x))
#             if (!is.list(dimnam)) {
#               dimnam <- as.list(as.data.frame(dimnam))
#             }
#
#             header <- array(
#               data = header$Freq,
#               dim = dim,
#               dimnames = dimnam
#             )
#             #
#             # header4 = array(data = header$Freq,
#             #                 dim = c(126, 27),
#             #                 dimnames = list(
#             #                   IND = unique(header$IND),
#             #                   DST = unique(header$DST)
#             #                 ))
#
#             # class(dimnames)
#
#             input_data[[h]] <- header
#           }
#         } else if (is.character(input_data[[h]])) {
#           set_orig <- corr[!names(corr) %in% c("input", "sep")][[1]]
#           set_char <- names(input_data[h])
#           test <- set_orig %in% names(input_data[h])
#           if (any(test)) {
#             set_new <- names(corr[!names(corr) %in% c("input", "sep")])
#             elem_new <- unique(corr$input[set_new])
#             elem_new[elem_new == ""] <- NA
#             elem_new <- na.omit(elem_new)
#             input_data[[h]] <- elem_new[[1]]
#           }
#         }
#       }
#     }
#
#     agg_csv_df <- c()
#     agg_csv_df$number <- 0
#     for (l in agg_csv) {
#       l$number <- as.numeric(l$number)
#       agg_csv_df <- merge(agg_csv_df, l, by = "number", all = T, sort = T)
#       agg_csv_df[agg_csv_df == "NA"] <- ""
#     }
#     write.csv2(agg_csv_df, file = "gtaptools_agg_har_correspondences.csv")
#
#     if (!is.null(output_har_file)) {
#       HARr::write_har(input_data, filename = output_har_file)
#     }
#
#     return(input_data)
#   }
# }
#
#
#