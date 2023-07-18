plot_map_grid <- function(input_data,
                          value_var,
                          latitude,
                          longitude,
                          crs = 4326,
                          resolution = NULL,
                          interpolate = F,
                          colors,
                          borders_color = "black",
                          borders_size = 3,
                          legend_labels = NULL,
                          legend_title = NULL,
                          legend_pos = NULL,
                          reactive = T,
                          fillOpacity = .8,
                          gtap_theme = "A") {
  #' @name plot_map_grid
  #' @title Plot gridded maps
  #'
  #' @description Plot static and reactive spatial grid viz.
  #'
  #' @param input_data An input data.frame that has geographical coordinates information and at least one numeric column
  #' @param value_var The name of the numeric variable to be plotted on the map.
  #' @param latitude The name of the numeric variable that contains the latitude information of each grid.
  #' @param longitude The name of the numeric variable that contains the longitude information of each grid.
  #' @param crs Coordinate reference system to be assigned to *latitude* and *longitude* (default = 4326, the World Geodetic System 1984 - EPSG:4326).
  #' @param resolution Resolution of the grids. (default = NULL, automatically estimated based on the distance between coordinates.).
  #' @param interpolate A model from gstat::gstat (gstat package) to interpolate values using a fitted model object. If TRUE it applies a simple kriging using the formula z~1. (default = F).
  #' @param colors The name of the color palette to be adopted (see a list in the section Notes below) or the custom color break vector. See the examples section.
  #' @param borders_color Border line color.
  #' @param borders_size Line border size.
  #' @param legend_labels Legend labels. It must be the same length as the color vector.
  #' @param legend_title Legend title.
  #' @param legend_pos 	The position of legends. For reactive = F:"none", "left", "right", "bottom", "top", or two-element numeric vector. For reactive = T: "topright", "bottomright", "bottomleft" and "topleft".
  #' @param reactive Plot reactive (default = T).
  #' @param fillOpacity Color fill layer transparency. Must be between [0, 1] (default = 0.8). Only applied when reactive = T.
  #' @param gtap_theme The gtap template theme. Can be specified from 1 to about 5 (default is 1). Only applied when reactive = F.
  #'
  #'
  #' @import ggplot2
  #' @import leaflet
  #'
  #' @importFrom dplyr filter inner_join mutate group_by across all_of summarise
  #' @importFrom rnaturalearth ne_countries
  #' @importFrom scales rescale number
  #' @importFrom sf st_transform st_as_sf st_intersects st_distance
  #' @importFrom raster projectRaster rasterToPoints interpolate mask values raster extent rasterize
  #' @importFrom gstat gstat
  #' @importFrom leafem addMouseCoordinates addImageQuery
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


  # input_data = gtaptools::template_map
  # value_var = "gdp_md"
  # value_var = "N.Amer"


  # colors <- c(
  #   "blue" = 13,
  #   "white" = 0,
  #   "red" = -13
  # )
  # borders_color <- "black"
  # borders_size <- 3
  # legend_labels <- NULL
  # # legend_labels = c("dfdfd", "fdfdf")
  # legend_title <- "xxxxx"
  # legend_pos <- "right"
  # legend_pos <- "bottomleft"
  # fillOpacity <- 1
  # value_var <- "p_qlandg"
  # crs <- 4326
  # latitude <- "lat"
  # longitude <- "lon"
  # resolution <- NULL
  # interpolate <- F
  # reactive <- T
  
  # input_data = input_data # <1>
  # latitude = "lat"
  # longitude = "lon"
  # value_var = "p_qlandg" # <2>
  # colors = "viridis"


  input_data$x <- input_data[[longitude]]
  input_data$y <- input_data[[latitude]]
  input_data$z <- input_data[[value_var]]
  input_data <- input_data[, c("x", "y", "z", value_var)]

  countries_merged <- rnaturalearth::ne_countries(
    scale = "large",
    returnclass = "sf"
  )
  countries_merged <-
    sf::st_transform(countries_merged, crs = 3857)
  countries_merged <-
    dplyr::filter(countries_merged, name != "Antarctica")

  coordinates_sf <- sf::st_as_sf(
    input_data[, c("x", "y", "z")],
    coords = c("x", "y"),
    crs = crs,
    agr = "constant"
  )
  coordinates_sf <- sf::st_transform(coordinates_sf,
    src = crs,
    crs = 3857
  )

  intersects <-
    sf::st_intersects(countries_merged, coordinates_sf)

  border <-
    countries_merged[lengths(intersects) > 100, "geometry"]


  if ( interpolate != F | reactive) {
    

    distance <- sf::st_distance(coordinates_sf[1:100, ])
    distance <- as.numeric(distance)
    distance <- distance[distance > 0]
    distance <- sort(distance)
    distance <- distance[4] * 1.2


    r <- raster::raster(
      raster::extent(border),
      resolution = ifelse(is.null(resolution), distance, resolution),
      crs = 3857
    )

    r <-
      raster::rasterize(
        coordinates_sf[, "geometry"],
        r,
        field = coordinates_sf$z,
        fun = mean,
        na.rm = TRUE,
        silent = T,
        crs = crs
      )

    input_data <- raster::projectRaster(r, crs = crs)
    input_data <-
      as.data.frame(raster::rasterToPoints(input_data))
    input_data[, value_var] <- input_data$layer
  }

  if (interpolate != F) {
    
    if (interpolate == T){
      model <-
        gstat::gstat(
          id = "z",
          formula = z ~ 1,
          # locations = ~ x + y,
          data = coordinates_sf,
          nmax = 7,
          set = list(idp = .5)
        )
    } else {
      model <- interpolate
    }
    
    r <- raster::interpolate(r, model = model)
    r <- raster::mask(r, border)

    input_data <- raster::projectRaster(r, crs = crs)
    input_data <-
      as.data.frame(raster::rasterToPoints(input_data))
    input_data[, value_var] <- input_data$z.pred
  }


  if (!reactive) {
    plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = border,
        fill = "transparent",
        color = borders_color,
        lwd = borders_size
      ) +
      ggplot2::geom_tile(
        data = input_data,
        ggplot2::aes(
          fill = .data[[value_var]],
          x = x,
          y = y
        )
      ) +
      # ggplot2::coord_fixed() +
      ggplot2::coord_sf(crs = crs)

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
            from = range(input_data[[value_var]], na.rm = T)
          ), 1),
          labels = legend_labels,
          # na.value = na.value,
          name = legend_title
        )
    }

    if (!is.null(legend_pos)) {
      options <- c("none", "left", "right", "bottom", "top")
      if ((!legend_pos %in% options) & !is.numeric(legend_pos)) {
        stop(
          'If reactive = F, the legend_pos must be "none", "left", "right", "bottom", "top", or two-element numeric vector'
        )
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
    if (is.null(legend_pos)) {
      legend_pos <- "bottomleft"
    } else {
      options <- c("topright", "bottomright", "bottomleft", "topleft")
      if ((!legend_pos %in% options)) {
        stop(
          'If reactive = T, the legend_pos must be "topright", "bottomright", "bottomleft" or "topleft"'
        )
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
          space = "rgb"
        )(abs(colors[i] - colors[(i - 1)]))
        ramp <- c(ramp, ramp_temp)
      }
      pal <- leaflet::colorNumeric(
        palette = ramp,
        domain = raster::values(r),
        na.color = "transparent"
      )
    } else {
      pal <- leaflet::colorNumeric(
        palette = colors,
        domain = raster::values(r),
        na.color = "transparent"
      )
    }

    r <-
      raster::projectRaster(r, crs = "+proj=longlat +datum=WGS84")
    border <-
      sf::st_transform(border, crs = "+proj=longlat +datum=WGS84")

    plot <- leaflet::leaflet()
    plot <- leaflet::addTiles(plot, group = "Open Street Map")
    plot <- leaflet::addProviderTiles(plot, "Esri.WorldTerrain",
      group = "Esri World Terrain"
    )
    plot <- leaflet::addProviderTiles(plot, "Esri.WorldStreetMap",
      group = "Esri World Street Map"
    )
    plot <- leaflet::addPolygons(
      plot,
      data = border$geometry,
      fillColor = "transparent",
      color = borders_color,
      weight = borders_size
    )
    plot <-
      leaflet::addRasterImage(
        map = plot,
        x = r,
        layerId = "Value",
        group = "Value",
        colors = pal,
        opacity = fillOpacity
      )
    plot <- leafem::addMouseCoordinates(plot)
    plot <-
      leafem::addImageQuery(
        plot,
        r,
        type = "mousemove",
        layerId = "Value",
        group = "Value",
        prefix = ""
      )
    plot <-
      leaflet::addLegend(
        map = plot,
        position = legend_pos,
        pal = pal,
        values = raster::values(r),
        labFormat = labels,
        opacity = 1,
        title = legend_title
      )
    plot <- leaflet::addLayersControl(
      plot,
      baseGroups = c(
        "Esri World Street Map",
        "Esri World Terrain",
        "Open Street Map"
      ),
      options = leaflet::layersControlOptions(
        collapsed = FALSE,
        position = "topright"
      ),
      overlayGroups = "Value"
    )
  }

  return(plot)
}
