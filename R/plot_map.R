plot_map <- function(input_data,
                     value_var,
                     region_var = "name",
                     colors,
                     borders_color = ggplot2::alpha("grey", 1 / 8),
                     borders_size = 0.05,
                     background_color = "transparent",
                     facet = NULL,
                     legend_labels = NULL,
                     legend_title = NULL,
                     legend_pos = NULL,
                     reactive = T,
                     fillOpacity = .7,
                     gtap_theme = "A") {
  #' @name plot_map
  #' @title Plot maps
  #'
  #' @description Plot static and reactive spatial viz.
  #'
  #' @param input_data An input data.frame has at least one numeric column and one region id categorical column. The region id column name must have one of the following (and have content consistent with these formats): iso_a2, iso_a3 and iso_n3.

  #' @param value_var The name of the numeric variable to be plotted on the map.
  #' @param region_var Variable that contains region labels that will be used to aggregate the sf if necessary. (default = "name").
  #' @param colors The name of the color palette to be adopted (see a list in the section Notes below) or the custom color break vector. See the examples section.
  #' @param borders_color Border line color.
  #' @param borders_size Line border size.
  #' @param legend_labels Legend labels. It must be the same length as the color vector.
  #' @param legend_title Legend title.
  #' @param legend_pos 	The position of legends. For reactive = F:"none", "left", "right", "bottom", "top", or two-element numeric vector. For reactive = T: "topright", "bottomright", "bottomleft" and "topleft".
  #' @param reactive Plot reactive (default = T).
  #' @param fillOpacity Color fill layer transparency. Must be between [0, 1] (default = 0.7). Only applied when reactive = T.
  #' @param gtap_theme The gtap template theme. Can be specified from 1 to about 5 (default is 1). Only applied when reactive = F.
  #'
  #'
  #' @import ggplot2
  #' @import leaflet
  #'
  #' @importFrom dplyr filter inner_join mutate group_by across all_of summarise
  #' @importFrom rnaturalearth ne_countries
  #' @importFrom scales rescale number
  #' @importFrom sf st_transform st_union st_as_sf
  #' @note
  #'
  #' Palette options include:
  #' Viridis palletes:

  #'   "magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo".
  #' Color Brewer Sequential:
  #'   "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
  #' Color Brewer Diverging:
  #'   "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral".
  #'
  #'
  #' @export
  #' 
  #' 
  
  
  
  

  # input_data = gtaptools::template_map # <1>
  # value_var = "gdp_pc"# <2>
  # region_var = "name"# <3>
  # colors = "viridis"# <4>
  # legend_title = "GDP per capita 2021, PPP</br>(constant 2017 international $)" # <5>
  
  
  
  
  
  input_data <- input_data[!is.na(input_data[value_var]), ]
  
  countries_merged <- rnaturalearth::ne_countries(
    scale = "large",
    returnclass = "sf"
  )
  countries_merged <- sf::st_transform(countries_merged, "EPSG:3857")
  countries_merged <- dplyr::filter(countries_merged, name != "Antarctica")
  countries_merged <- dplyr::inner_join(countries_merged, input_data)
  countries_merged <- dplyr::mutate(
    countries_merged,
    {{ value_var }} := as.numeric(get(value_var))
  )
  # 
  # countries_merged[[value_var]] = as.numeric(countries_merged[[value_var]])
  # 
  countries_merged <- dplyr::group_by(
    countries_merged,
    dplyr::across(dplyr::all_of(c(
      region_var,
      value_var
    )))
  )
  countries_merged <- dplyr::summarise(countries_merged,
                                       geometry = sf::st_union(geometry)
  )
  countries_merged <- sf::st_as_sf(countries_merged)
  
  
  # countries_sf <- rnaturalearth::ne_countries(
  #   scale = "large",
  #   returnclass = "sf"
  # )
  # countries_sf <- subset(countries_sf, name != "Antarctica")
  #
  #
  # input_data$value <- as.numeric(input_data[, value_var])
  #
  # countries_merged <- merge(input_data,
  #   countries_sf,
  #   all.x = T
  # )

  # countries_merged <- sf::st_as_sf(countries_merged)
  #
  # countries_merged <- sf::st_transform(countries_merged, 'EPSG:3857')
  #
  # #countries_merged <- countries_merged[!is.na(countries_merged[[region_var]]),]
  #
  # countries_merged2 <- aggregate(countries_merged,
  #                                by = list(countries_merged[[region_var]]),
  #                                FUN = function(x) mean(x, na.rm=T))
  #
  # countries_merged2 <-
  #   countries_merged %>%
  #   dplyr::group_by(get(region_var)) %>%
  #   dplyr::summarize(geometry = sf::st_union(geometry))
  #
  # countries_merged <- sf::st_as_sf(countries_merged)


  # countries_merged2 <- data.table::setDT(countries_merged)[,.(geometry=sf::st_union(geometry)), by="iso_a3"]


  if (!reactive) {
    # Plot the data
    plot <- ggplot2::ggplot(data = countries_merged) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = .data[[value_var]]),
        color = borders_color,
        lwd = borders_size
      )

    # na.value <-
    #   ifelse(is.null(names(colors[is.na(colors)])),
    #     "grey50",
    #     names(colors[is.na(colors)])
    #   )
    legend_labels <-
      if (is.null(legend_labels)) {
        scales::number
      } else {
        c("", legend_labels)
      }
    legend_title <-
      if (is.null(legend_title)) {
        value_var
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
        "custom"
      )
    )

    if (palette_type == "brewer") {
      plot <- plot +
        ggplot2::scale_fill_distiller(
          palette = colors,
          name = legend_title,
          # na.value = na.value,
          labels = legend_labels,
        )
    } else if (palette_type == "viridis") {
      plot <- plot +
        ggplot2::scale_fill_viridis_c(
          option = colors,
          name = legend_title,
          # na.value = na.value,
          labels = legend_labels,
        )
    } else if (palette_type == "custom") {
      plot <- plot +
        ggplot2::scale_fill_gradientn(
          colours = names(colors),
          values = c(0, scales::rescale(
            colors,
            from = range(countries_merged[[value_var]], na.rm = T)
          ), 1),
          labels = legend_labels,
          # na.value = na.value,
          name = legend_title
        )
    }

    if (!is.null(legend_pos)) {
      options <- c("none", "left", "right", "bottom", "top")
      if ((!legend_pos %in% options) & !is.numeric(legend_pos)) {
        stop('If reactive = F, the legend_pos must be "none", "left", "right", "bottom", "top", or two-element numeric vector')
      }
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
      text = ggplot2::element_text(family = "serif", size = 12)
    )
    plot <- plot + plain
  }

  if (reactive) {
    # input_data = gtaptools::template_map
    # value_var = "gdp_md"
    # value_var = "N.Amer"
    # colors <- c("red" = 1000, "black" = 2000, "blue" = 5000)
    # breaks = c(0,1e3, 1e6)
    # borders_color = "white"
    # borders_size = 0.5
    # legend_labels = NULL
    # legend_labels = c("dfdfd", "fdfdf")
    # legend_title
    # legend_pos = "bottomright"
    # highlight_label = "iso_a3"

    if (is.null(legend_pos)) {
      legend_pos <- "bottomleft"
    } else {
      options <- c("topright", "bottomright", "bottomleft", "topleft")
      if ((!legend_pos %in% options)) {
        stop('If reactive = T, the legend_pos must be "topright", "bottomright", "bottomleft" or "topleft"')
      }
    }

    if (is.null(legend_labels)) {
      labels <- leaflet::labelFormat(big.mark = " ")
    } else {
      labels <- function(type, cuts, p) {
        paste0(legend_labels)
      }
    }
    # na.value <-
    #   ifelse(is.null(names(colors[is.na(colors)])),
    #     "grey50",
    #     names(colors[is.na(colors)])
    #   )
    # colors <- colors[!is.na(colors)]

    if (length(colors) > 1) {
      ramp <- c()
      for (i in 2:length(colors)) {
        ramp_temp <- colorRampPalette(
          colors = names(colors)[(i - 1):i],
          space = "Lab"
        )(abs(colors[i] - colors[(i - 1)]))
        ramp <- c(ramp, ramp_temp)
      }
      pal <- leaflet::colorNumeric(
        palette = ramp,
        domain = countries_merged[[value_var]] # ,
        # na.color = na.value
      )
    } else {
      pal <- leaflet::colorNumeric(
        palette = colors,
        domain = countries_merged[[value_var]] # ,
        # na.color = na.value
      )
    }

    popup <- paste(
      "<strong>Region: </strong>",
      countries_merged[[region_var]],
      "<br><strong>Value: </strong>",
      countries_merged[[value_var]]
    )

    countries_merged <- sf::st_transform(countries_merged, 4326)

    # Create the map object
    plot <- leaflet::leaflet(data = countries_merged)
    plot <- leaflet::setView(plot, lat = 0, lng = 0, zoom = 1)
    plot <- leaflet::addTiles(plot, group = "Open Street Map")
    plot <- leaflet::addProviderTiles(plot, "Esri.WorldTerrain",
      group = "Esri World Terrain"
    )
    plot <- leaflet::addProviderTiles(plot, "Esri.WorldStreetMap",
      group = "Esri World Street Map"
    )
    plot <- leaflet::addPolygons(
      plot,
      # data = countries_merged$geometry,
      fillColor = ~ pal(countries_merged[[value_var]]),
      fillOpacity = fillOpacity,
      color = borders_color,
      weight = borders_size,
      highlightOptions = leaflet::highlightOptions(
        color = "darkgrey",
        weight = 1,
        bringToFront = TRUE,
        fillOpacity = 1
      ),
      label = ~ paste0(get(region_var), sep = ": ", get(value_var)),
      popup = popup
    )
    plot <-
      leaflet::addLegend(
        map = plot,
        position = legend_pos,
        pal = pal,
        values = ~ countries_merged[[value_var]],
        labFormat = labels,
        opacity = 1,
        title = legend_title
      )
    plot <- leaflet::addLayersControl(
      plot,
      baseGroups = c(
        "Open Street Map",
        "Esri World Terrain",
        "Esri World Street Map"
      ),
      options = leaflet::layersControlOptions(
        collapsed = FALSE,
        position = "topright"
      )
    )
  }

  return(plot)
}
