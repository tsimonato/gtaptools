plot_map <- function(input_data,
                     var_value,
                     colors,
                     borders_color = ggplot2::alpha("grey", 1 / 8),
                     borders_size = 0.05,
                     legend_labels = NULL,
                     legend_title = NULL,
                     legend_pos = NULL,
                     reactive = T,
                     gtap_theme = "A") {
  #' @name plot_map
  #' @title Plot maps
  #'
  #' @description Plot static and reactive spatial viz.
  #'
  #' @param input_data An input data.frame has at least one numeric column and one region id categorical column. The region id column name must have one of the following (and have content consistent with these formats): iso_a2, iso_a3 and iso_n3.
  #' @param var_value The name of the numeric variable to be plotted on the map.
  #' @param colors The name of the color palette to be adopted (see a list in the section Notes below) or the custom color break vector. See the examples section.
  #' @param borders_color Border line color.
  #' @param borders_size Line border size.
  #' @param legend_labels Legend labels. It must be the same length as the color vector.
  #' @param legend_title Legend title.
  #' @param legend_pos 	The position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector).
  #' @param reactive Plot reactive (default = T).
  #' @param gtap_theme The gtap template theme. Can be specified from 1 to about 5 (default is 1).
  #'
  #' @import ggplot2
  #'
  #' 
  #' @importFrom rnaturalearth ne_countries
  #' @importFrom scales rescale
  #' @importFrom sf st_as_sf
  #' @note
  #'
  #' Palette options include:
  #' Viridis palletes:
  #'   "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D"), "cividis" (or "E"), "rocket" (or "F"), "mako" (or "G"), "turbo" (or "H").
  #' Color Brewer Sequential:
  #'   "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
  #' Color Brewer Diverging:
  #'   "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
  #' 
  #'
  #' @export


  countries_sf <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  )
  countries_sf <- subset(countries_sf, name != "Antarctica")
  
  
  input_data$value <- as.numeric(input_data[, var_value])

  countries_merged <- merge(
    input_data,
    countries_sf,
    all.x = T
  )

  countries_merged <- sf::st_as_sf(countries_merged)

  # Plot the data
  plot <- ggplot2::ggplot(data = countries_merged) +
    ggplot2::geom_sf(ggplot2::aes(fill = value),
      color = ggplot2::alpha("grey", 1 / 8),
      lwd = borders_size
    )
  
  na.value <-
    ifelse(is.null(names(colors[is.na(colors)])),
           "grey50",
           names(colors[is.na(colors)]))
  legend_labels <-
    if (is.null(legend_labels)) {
      ggplot2:::waiver()
    } else {
      c("", legend_labels)
    }
  legend_title <-
    if (is.null(legend_title)) {
      var_value
    } else {
      legend_title
    }
    colors <- colors[!is.na(colors)]
  
    palette <- colors[[1]]
    
    brewer <-
      c(
        "BrBG",
        "PiYG",
        "PRGn",
        "PuOr",
        "RdBu",
        "RdGy",
        "RdYlBu",
        "RdYlGn",
        "Spectral",
        "Blues",
        "BuGn",
        "BuPu",
        "GnBu",
        "Greens",
        "Greys",
        "Oranges",
        "OrRd",
        "PuBu",
        "PuBuGn",
        "PuRd",
        "Purples",
        "RdPu",
        "Reds",
        "YlGn",
        "YlGnBu",
        "YlOrBr",
        "YlOrRd"
      )
    viridis <-
      c(
        "magma",
        "A",
        "inferno",
        "B",
        "plasma",
        "C",
        "viridis",
        "D",
        "cividis",
        "E",
        "rocket",
        "F",
        "mako",
        "G",
        "turbo",
        "H"
      )
    palette_type <- ifelse(palette %in% brewer,
                           "brewer",
                           ifelse(palette %in% viridis,
                                  "viridis",
                                  "custom"))

  if (palette_type == "brewer") {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        palette = colors,
        name = legend_title,
        na.value = na.value,
        labels = c("", legend_labels),
      )
  } else if (palette_type == "viridis") {
    plot <- plot +
      ggplot2::scale_fill_viridis_c(
        option = colors,
        name = legend_title,
        na.value = na.value,
        labels = legend_labels,
      )
  } else if (palette_type == "custom") {
    legend_labels <-
      if (is.null(legend_labels)) {
        colors
      } else {
        c("", legend_labels)
      }
    plot <- plot +
      ggplot2::scale_fill_gradientn(
        colours = names(colors),
        values = c(0, scales::rescale(
          colors,
          from = range(countries_merged$value, na.rm = T)
        ), 1),
        labels = legend_labels,
        na.value = na.value,
        name = legend_title
      )
  }

  if (!is.null(legend_pos)) {
    plot <- plot + ggplot2::theme(legend.position = legend_pos)
  }

  plain <- ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white"),
    plot.title = ggplot2::element_text(hjust = 0.5),
    text = ggplot2::element_text(family = "serif")
  )
  plot <- plot + plain
  
  plot
  return(plot)
}
