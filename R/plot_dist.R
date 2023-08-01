plot_dist <- function(input_data,
                      value_var,
                      type = "density",
                      x_label = "",
                      y_label = "",
                      fill = NULL,
                      facet = NULL,
                      linewidth = 0.5,
                      palette = "Pastel1",
                      transparency = 0.7,
                      orientation_bars = "vertical",
                      rotate_x_labels = 0,
                      legend_title = NULL,
                      legend_pos = "right",
                      adjust = 2,
                      reactive = T,
                      gtap_theme = "A") {
  #' @name plot_dist
  #' @title Plot distributions
  #'
  #' @description Tool to plot distribution charts.
  #'
  #' @param input_data A data.frame or array object containing the data to be plotted
  #' @param value_var Numeric variable to plot the distribution.
  #' @param type Type of distribution chart plot such as "density", "histogram", "violin" or "boxplot" (Default = "density").
  #' @param x_label the label to be used for the x-axis.
  #' @param y_label the label to be used for the y-axis.
  #' @param fill the name of the variable to be used for stacking the distributions, if any (default is NULL)
  #' @param facet the name of the variable to be used for faceting the graph, if any (default is NULL)
  #' @param linewidth Line width/thickness (default = 0.5).
  #' @param palette the name of the color palette to be used. Can be specified by name or number from 1 to about 15 (default is 1).
  #' @param transparency degree of color transparency [0-1] (default = 0.7).
  #' @param orientation_bars the orientation of the bars to be plotted, "h" to horizontal or "v" to vertical (default is "v").
  #' @param rotate_x_labels the angle in degrees to rotate the x-axis labels (default is 90)
  #' @param legend_title the title to be used for the legend (default is fill).
  #' @param legend_pos the position to be used for the legend ("bottom", "left" or "right"), default is "bottom".
  #' @param adjust A multiplicate bandwidth adjustment (default = 2).
  #' @param reactive Plot reactive (default = TRUE).
  #' @param gtap_theme the gtap template theme. Can be specified from 1 to about 5 (default is 1).
  #'
  #' @import ggplot2
  #'
  #' @importFrom plotly ggplotly layout
  #'
  #' @export

  # orientation_bars = "H"
  # path_to_har <- gtaptools::templates("simpleg_example.har")
  #
  # input_data <- gtaptools::har_shape(
  #   input_data = path_to_har,
  #   useCoefficientsAsNames = T, # <1>
  #   as_dataframes = T
  # )
  # names(input_data$p_QLANDWTRgl)
  #
  # input_data2 <- cbind(input_data$MP2N, input_data$Latitude) # <2>
  # input_data <- merge(input_data2, input_data$p_QLANDWTRgl)
  # #input_data <- cbind(input_data$Longitude, input_data$Latitude, input_data$MP2N, input_data$p_QLANDg) # <2>
  # input_data$MP2N = input_data$`input_data$MP2N`
  # input_data <- subset(input_data, Latitude != 1) # <3>
  #
  #
  #
  # value_var = "p_QLANDWTRgl"
  # linewidth = 0.5
  # transparency = 0.5
  # fill = "LTYPE"
  # color = NULL
  # type = "density"
  # facet = "MP2N"
  # facet_scales = NULL
  # palette = "Pastel1"
  # orientation_bars = "vertical"
  # rotate_x_labels = NULL
  # adjust = 2
  
  
  # input_data
  # value_var
  # type = "density"
  # x_label = ""
  # y_label = ""
  # fill = NULL
  # facet = NULL
  # palette = "Pastel1"
  # transparency = 0.7
  # orientation_bars = "vertical"
  # rotate_x_labels = 0
  # legend_title = NULL
  # legend_pos = "right"
  # adjust = 2
  # reactive = T
  # gtap_theme = "A"

  # Check input_data class
  if (is.array(input_data)) {
    input_data <- as.data.frame.table(input_data)
  } else if (!(is.array(input_data) | is.data.frame(input_data))) {
    stop("input_data must be an array or a data.frame.")
  }

  # Check if y and x variables exist in data
  if (!(value_var %in% colnames(input_data))) {
    stop("value_var variable is not present in the data.")
  }

  input_data$x <- as.numeric(input_data[[value_var]])

  # Fill arg
  if (!is.null(fill)) {
    if (!(fill %in% colnames(input_data))) {
      stop("`fill` variable is not present in the data.")
    }
    input_data$fill <- as.factor(input_data[[fill]])
  }

  # # Color arg
  # if (!is.null(color)) {
  #   if (!(color %in% colnames(input_data))) {
  #     stop("`color` variable is not present in the data.")
  #   }
  #   input_data$color <- as.factor(input_data[[color]])
  # }

  if (type == "density") {
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = x,
        fill = fill
      )
    ) +
      ggplot2::geom_density(
        adjust = adjust,
        linewidth = linewidth,
        alpha = transparency
      )
  } else if (type == "histogram") {
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = x,
        fill = fill
      )
    ) +
      ggplot2::geom_histogram(
        binwidth = adjust,
        linewidth = linewidth,
        alpha = transparency
      )
  } else if (type == "violin") {
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = fill,
        y = x,
        fill = fill
      )
    ) +
      ggplot2::geom_violin(
        adjust = adjust,
        linewidth = linewidth,
        alpha = transparency
      )
  } else if (type == "boxplot") {
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = fill,
        y = x,
        fill = fill
      )
    ) +
      ggplot2::geom_boxplot(
        linewidth = linewidth,
        alpha = transparency
      )
  }

  # orientation_bars arg
  if (!is.null(orientation_bars)) {
    if (tolower(orientation_bars) %in% c("v", "vertical")) {
      plot <- plot
    } else if (tolower(orientation_bars) %in% c("h", "horizontal")) {
      plot <- plot + ggplot2::coord_flip(expand = T)
    } else {
      stop('"orientation_bars" must be = "H" or "V".')
    }
  }

  # Facet arg
  if (!is.null(facet)) {
    if (!(facet %in% colnames(input_data))) {
      stop("`facet` variable is not present in the data.")
    }
    plot <- plot + ggplot2::facet_wrap(facet)
  }

  # palette arg
  plot <- plot + ggplot2::scale_fill_brewer(palette = palette)

  # x_label arg
  if (!is.null(x_label)) {
    plot <- plot + ggplot2::xlab(x_label)
  }

  # y_label arg
  if (!is.null(y_label)) {
    plot <- plot + ggplot2::ylab(y_label)
  }

  # legend_title arg
  if (!is.null(legend_title)) {
    plot <- plot + ggplot2::labs(fill = legend_title)
  }

  # legend_pos arg
  if (!is.null(legend_pos)) {
    plot <- plot + ggplot2::theme(legend.position = legend_pos)
  }

  # rotate_x_labels arg
  if (is.null(rotate_x_labels)) {
    n_char <- sum(nchar(as.character(unique(input_data["x"])[[1]])))
    n_facet <- ifelse(!is.null(facet), nrow(unique(input_data[facet])), 1)
    n_row_facet <- ifelse(!is.null(facet_n_row), facet_n_row, 1)
    n_col <- n_facet / n_row_facet
    rotate_x_labels <- ifelse(n_char * n_col > 25, 90, 0)
  }
  plot <- plot +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = rotate_x_labels,
      vjust = 0.5,
      hjust = 1
    ))

  # Set font family
  plot <- plot +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"))

  # reactive arg
  if (reactive) {
    if (!is.null(legend_pos)) {
      if (tolower(legend_pos) == "bottom") {
        plot <- plotly::ggplotly(plot) |>
          plotly::layout(legend = list(orientation = "h"))
      } else {
      plot <- plotly::ggplotly(plot)
      }
    }
  }

  return(plot)
}
