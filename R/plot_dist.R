#' plot_dist <- function(input_data,
#'                       value_var,
#'                       x_label = NULL,
#'                       y_label = NULL,
#'                       fill = NULL,
#'                       facet = NULL,
#'                       palette = NULL,
#'                       legend_title = NULL,
#'                       legend_position = "right",
#'                       hist = T,
#'                       reactive = T,
#'                       gtap_theme = "A") {
#'   #' @name plot_dist
#'   #' @title Plot distributions
#'   #'
#'   #' @description Tool to plot histograms or kernel density distributions.
#'   #'
#'   #' @param input_data A data.frame or array object containing the data to be plotted
#'   #' @param value_var
#'   #' @param x_label the label to be used for the x-axis (default is value_var).
#'   #' @param y_label the label to be used for the y-axis (default is "count").
#'   #' @param fill the name of the variable to be used for stacking the distributions, if any (default is NULL)
#'   #' @param facet the name of the variable to be used for faceting the graph, if any (default is NULL)
#'   #' @param palette the name of the color palette to be used. Can be specified by name or number from 1 to about 15 (default is 1).
#'   #' @param legend_title the title to be used for the legend (default is fill).
#'   #' @param legend_pos the position to be used for the legend ("bottom", "left" or "right"), default is "bottom".
#'   #' @param hist Plot a histogram (TRUE, default) or a density curve (F).
#'   #' @param reactive Plot reactive (default = TRUE).
#'   #' @param gtap_theme the gtap template theme. Can be specified from 1 to about 5 (default is 1).
#'   #'
#'   #'
#'   #' @import ggplot2
#'   #'
#'   #' @importFrom dplyr filter inner_join mutate group_by across all_of summarise
#'   #' @importFrom plotly ggplotly
#'   #' @note
#'   #'
#'   #' Palette options include:
#'   #' Viridis palletes:
#'   #'   "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo".
#'   #' Color Brewer Sequential:
#'   #'   "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
#'   #' Color Brewer Diverging:
#'   #'   "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
#'   #'
#'   #'
#'   #' @export
#' 
#'   # Check input_data class
#'   if (is.array(input_data)) {
#'     input_data <- as.data.frame.table(input_data)
#'     value_var <- "Freq"
#'   } else if (!(is.array(input_data) | is.data.frame(input_data))) {
#'     stop("input_data must be an array or a data.frame object.")
#'   }
#'   
#'   # Check if y and x variables exist in data
#'   if (!(value_var %in% colnames(input_data))) {
#'     stop("value_var variable is not present in the data.")
#'   }
#'   
#'   # x_label arg
#'   if (is.null(x_label)) {
#'     x_label <- value_var
#'   }
#'   
#'   # y_label arg
#'   if (is.null(y_label)) {
#'     y_label <- ggplot2::waiver()
#'   }
#'   
#'   x <- input_data[[value_var]]
#'   # Create the plot object
#'   if (hist) {
#'     # Plot a histogram
#'     plot <- ggplot2::ggplot(data = input_data, ggplot2::aes(x = .data[[value_var]], fill = NULL)) +
#'       ggplot2::geom_histogram(
#'                               binwidth = diff(range(x)) / 30, color = "black") + 
#'       ggplot2::scale_fill_brewer(palette = palette, name = legend_title) +
#'       ggplot2::labs(x = x_label, y = y_label)
#'   } else {
#'     # Plot a kernel density distribution
#'     plot <- ggplot2::ggplot(data = input_data, ggplot2::aes(x = x, fill = fill)) +
#'       ggplot2::geom_density() +
#'       ggplot2::scale_fill_brewer(palette = "Blues", name = legend_title) +
#'       ggplot2::labs(x = x_label, y = y_label)
#'   }
#'   
#'   # Facet arg
#'   if (!is.null(facet)) {
#'     if (!(facet %in% colnames(input_data))) {
#'       stop("`facet` variable is not present in the data.")
#'     }
#'     facet_n_row <- ifelse(!is.null(facet_n_row), facet_n_row, 1)
#'     facet_scales <- ifelse(!is.null(facet_scales), facet_scales, "fixed")
#'     plot <- plot + ggplot2::facet_wrap(facet,
#'                                        scales = facet_scales
#'     )
#'   }
#' 
#'   # Set the legend position
#'   p <- p + ggplot2::theme(legend.position = legend_position)
#' 
#'   # Return the plot object
#'   return(p)
#' }
#' 
#' 
#' 
#' input_data <- gtaptools::templates("oranig")
#' 
#' input_data <- gtaptools::har_shape(input_data)
#' 
#' input_data <- as.data.frame.table(input_data$`1BAS`)
#' 
#' p <- ggplot2::ggplot(data = a, ggplot2::aes(x = Freq))
#' 
#' p <- p + ggplot2::geom_histogram(bins = as.integer(nrow(a)/40))
#' 
#' p
#' 
#' plotly::ggplotly(p)
#' 
#' 
#' 
#' 2178/20
