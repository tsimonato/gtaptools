plot_bars <- function(input_data,
                      x,
                      x_label = NULL,
                      y,
                      y_label = NULL,
                      fill = NULL,
                      position = "stack",
                      facet = NULL,
                      facet_n_row = NULL,
                      facet_scales = NULL,
                      palette = 1,
                      orientation_bars = "vertical",
                      rotate_x_labels = 90,
                      legend_title = NULL,
                      legend_pos = "bottom",
                      reactive = T,
                      gtap_theme = "A") {
  #' @name plot_bars
  #' @title Bar graphs
  #'
  #' @description Plot static and reactive bar charts.
  #'
  #' @param input_data It must consist of one or more input databases, which must be separated from each other by sublists (see example). In the case of multiple databases, all will be combined for the final output.Arrays and data.frames must be inside sublists (list(....)) as indicated in the examples section. Aggregations on input data can only be performed on single array and data.frame inputs.
  #' @param x the name of the variable to be plotted on the x-axis.
  #' @param x_label the label to be used for the x-axis (default is x).
  #' @param y The name of the variable to be plotted on the y-axis.
  #' @param y_label the label to be used for the x-axis (default is y).
  #' @param fill the name of the variable to be used for stacking the bars, if any (default is NULL)
  #' @param position "stack" to keep the fill categories in the same bar or "dodge" to plot in different bars (default = "stack").
  #' @param facet the name of the variable to be used for faceting the graph, if any (default is NULL)
  #' @param palette the name of the color palette to be used. Can be specified by name or number from 1 to about 15 (default is 1).
  #' @param orientation_bars the orientation of the bars to be plotted, "h" to horizontal or "v" to vertical (default is "v").
  #' @param rotate_x_labels the angle in degrees to rotate the x-axis labels (default is 90)
  #' @param legend_title the title to be used for the legend (default is fill).
  #' @param legend_pos the position to be used for the legend ("bottom", "left" or "right"), default is "bottom".
  #' @param reactive Plot reactive (default = T).
  #' @param gtap_theme the gtap template theme. Can be specified from 1 to about 5 (default is 1).
  #'
  #' @import ggplot2
  #' 
  #' @importFrom plotly ggplotly layout
  #'
  #' @export



  # Check input_data class
  if (is.array(input_data)) {
    input_data <- as.data.frame.table(input_data)
  } else if (!(is.array(input_data) | is.data.frame(input_data))) {
    stop("input_data must be an array or a data.frame.")
  }

  # Check if y and x variables exist in data
  if (!(y %in% colnames(input_data)) | !(x %in% colnames(input_data))) {
    stop("y or x variable is not present in the data.")
  }

  # Fill arg
  if (!is.null(fill)) {
    if (!(fill %in% colnames(input_data))) {
      stop("`fill` variable is not present in the data.")
    }
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        fill = .data[[fill]]
      )
    ) +
      ggplot2::geom_col(position = position)
  } else {
    plot <- ggplot2::ggplot(
      input_data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]]
      )
    ) +
      ggplot2::geom_bar(stat = "identity")
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
    facet_n_row <- ifelse(!is.null(facet_n_row), facet_n_row, 1)
    facet_scales <- ifelse(!is.null(facet_scales), facet_scales, "fixed")
    plot <- plot + ggplot2::facet_wrap(facet,
      nrow = facet_n_row,
      scales = facet_scales
    )
  }

  # palette arg
  if (!is.null(palette)) {
    plot <- plot + ggplot2::scale_fill_brewer(palette = palette)
  }

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
    n_char <- sum(nchar(as.character(unique(input_data[x])[[1]])))
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
    ggplot2::theme(text=ggplot2::element_text(family="serif"))

  # reactive arg
  if (reactive) {
    if (!is.null(legend_pos)) {
      if (tolower(legend_pos) == "bottom") {
        plot <- plotly::ggplotly(plot) |>
          plotly::layout(legend = list(orientation = "h"))
      }
    } else {
      plot <- plotly::ggplotly(plot)
    }
  }

  plot

  return(plot)
}





