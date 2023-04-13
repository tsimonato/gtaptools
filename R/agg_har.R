agg_har <- function(input_data,
                    model = NULL,
                    correspondences,
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
  #' @param model Indicates the CGE model being worked on (Supports only GTAP, ....).
  #' @param correspondences A list indicating the original sets and new aggregated sets that will be exported. It can indicate a path to a .csv file or an existing object in the R environment that has the correspondences between the *input_data* sets and the new aggregated sets. The first line will be considered as header and identifier, and must necessarily contain the same name as the set that must be aggregated from *input_data*.
  #' @param var A list object (see example section) with variables names that will be aggregated. If not specified, all variables containing the sets indicated in *sets_to_agg* will be aggregated.
  #' @param var_weight A list object that relates the name of the aggregated variables (*var*) with the name of their respective weights, if any. An aggregation without weights will be performed if the weight variable is not found in the input data (*input_data*).
  #' @param fun Function that will be applied to aggregate (default = sum).
  #' @param output_har_file Output .har file name.
  #'
  #' @import dplyr
  #'
  #'
  #' @export



  # fun <- sum
  # input_data_text <- "data/test/teste/input/REG_DYN_HOU.har"
  # correspondences <- list(
  #   list(
  #     state = c("DST", "ORG", "PRD"),
  #     input = "inst\\extdata\\example_corresp.csv",
  #     sep = ";"
  #   ),
  #   list(
  #     sec_new = c("IND", "COM"),
  #     input = "inst\\extdata\\example_corresp.csv",
  #     sep = ";"
  #   ),
  #   list(
  #     usr_new = "USR",
  #     input = "inst\\extdata\\example_corresp.csv",
  #     sep = ";"
  #   )
  # )
  # 
  # 
  # "inst\\extdata\\example_corresp.csv"



  #  reading input .har file if input_data is a path
  if (is.character(input_data_text)) {
    input_data <- HARr::read_har(input_data_text,
      useCoefficientsAsNames = F,
      toLowerCase = F
    )
  }

  # c <- 1
  # loop em cada lista de correspondencia
  agg_csv <- c()
  for (c in 1:length(correspondences)) {
    corr <- correspondences[[c]]
    # Read correspondences if it is a csv file
    if (is.character(corr$input)) {
      corr$input <- read.csv(corr$input,
        sep = ifelse(!is.null(corr$sep),
          corr$sep,
          ","
        )
      )
    }
    set_agg <- names(corr)[!names(corr) %in% c("input", "sep")]
    set_agg_csv <- c(set_agg, corr[set_agg][[1]])
    agg_csv[[c]] <- as.data.frame(unique(corr$input[set_agg_csv]))
    agg_csv[[c]]["number"] <- row.names(agg_csv[[c]])
    # h <- 18
    # loop em cada header
    for (h in 1:length(input_data)) {
      if (is.array(input_data[[h]])) {
        header <- as.data.frame.table(input_data[[h]])
        # header2 <- input_data[[h]]
        # header2 = input_data[[h]]
        # set_agg <- names(corr)[!names(corr) %in% c("input", "sep")]
        test <- corr[[set_agg]] %in% names(header)
        # test <- corr[[set_agg]] %in% names(dimnames(header2))
        # testa se o header tem alguma variavel para ser agregada
        if (any(test)) {
          col_names <- c(set_agg, corr[[set_agg]][test])
          corr_to_merge <- unique(corr$input[col_names])
          corr_to_merge[corr_to_merge == ""] <- NA
          corr_to_merge <- na.omit(corr_to_merge)
          # header_agg <- header
          col <- 2
          for (col in 2:length(corr_to_merge)) {
            cols_agg <- c(col_names[1], col_names[col])
            header <- data.table::as.data.table(header)
            header <- data.table::merge.data.table(header,
              unique(corr_to_merge[cols_agg]),
              by = names(corr_to_merge[col])
            )
            header <- as.data.frame(header)
            header[col_names[col]] <-
              as.factor(header[cols_agg[1]][[1]])
            header[cols_agg[1]] <- NULL
            # header2 <-
            #   aggregate(Freq ~ ., data = header, FUN = fun)

            # cols_group = names(header[,names(header)!="Freq"])
            cols_group <- as.data.frame.table(input_data[[h]])
            cols_group$Freq <- NULL
            cols_group <- names(cols_group)
            header <- data.table::as.data.table(header)
            header <- header[, lapply(.SD, fun, na.rm = T), by = cols_group]
            header <- as.data.frame(header)
          }

          cols_dim_array <- header
          cols_dim_array$Freq <- NULL
          dim <- sapply(cols_dim_array, function(x) length(unique(x)))
          dimnam <- sapply(cols_dim_array, function(x) unique(x))
          if (!is.list(dimnam)) {
            dimnam <- as.list(as.data.frame(dimnam))
          }

          header <- array(
            data = header$Freq,
            dim = dim,
            dimnames = dimnam
          )
          #
          # header4 = array(data = header$Freq,
          #                 dim = c(126, 27),
          #                 dimnames = list(
          #                   IND = unique(header$IND),
          #                   DST = unique(header$DST)
          #                 ))

          # class(dimnames)

          input_data[[h]] <- header
        }
      } else if (is.character(input_data[[h]])) {
        set_orig <- corr[!names(corr) %in% c("input", "sep")][[1]]
        set_char <- names(input_data[h])
        test <- set_orig %in% names(input_data[h])
        if (any(test)) {
          set_new <- names(corr[!names(corr) %in% c("input", "sep")])
          elem_new <- unique(corr$input[set_new])
          elem_new[elem_new == ""] <- NA
          elem_new <- na.omit(elem_new)
          input_data[[h]] <- elem_new[[1]]
        }
      }
    }
  }

  agg_csv_df <- c()
  agg_csv_df$number <- 0
  for (l in agg_csv) {
    l$number <- as.numeric(l$number)
    agg_csv_df <- merge(agg_csv_df, l, by = "number", all = T, sort = T)
    agg_csv_df[agg_csv_df == "NA"] <- ""
  }
  write.csv2(agg_csv_df, file = "gtaptools_agg_har_correspondences.csv")

  if (!is.null(output_har_file)) {
    HARr::write_har(input_data, filename = output_har_file)
  }

  return(input_data)
}


  
 

