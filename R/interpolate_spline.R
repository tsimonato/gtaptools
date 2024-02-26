interpolate_spline <- function(input_df, groups, year, values, method = "fmm") {
  #' @name interpolate_spline
  #' @title Fill Gaps with Spline Interpolation
  #'
  #' @description This function takes a data frame and performs cubic spline interpolation
  #' to fill in missing year gaps for specified groups.
  #' Groups with fewer than 2 non-NA values are excluded from interpolation.
  #'
  #' @param input_df A data frame containing the data to be processed.
  #' @param groups A vector of column names to group by (to loop applying spline).
  #' @param year The name of the column containing years.
  #' @param values The name of the numeric column containing values for interpolation.
  #' @param method The method of interpolation. Possible values:
  #'      Must be one of "fmm", "natural", "periodic", "monoH.FC" or "hyman".
  #'   - "fmm" (default): Forsythe, Malcolm, and Moler method. An exact cubic spline is fitted through
  #'     the four points at each end of the data. This is used for determining end conditions.
  #'     Not suitable for extrapolation.
  #'   - "natural": Natural spline method. Uses natural splines for interpolation. Linear
  #'     extrapolation outside the range of x using the slope of the interpolating curve at the
  #'     nearest data point.
  #'   - "periodic": Periodic spline method. Suitable for periodic data.
  #'   - "monoH.FC": Monotone Hermite spline according to Fritsch and Carlson. Ensures the spline
  #'     is monotone (increasing or decreasing) if the data are monotone.
  #'   - "hyman": Monotone cubic spline using Hyman filtering of an "fmm" fit for strictly
  #'     monotonic inputs.
  #' @return A data frame with gaps in years filled using spline interpolation.
  #' @import data.table
  #' @importFrom tidyr complete drop_na full_seq
  #' @importFrom dplyr group_by mutate
  #' @importFrom stats spline
  #' @examples
  #' data <- data.frame(
  #'   Scenario = rep(c("Scenario1", "Scenario2"), each = 5),
  #'   Region = rep(c("Region1", "Region2"), each = 5),
  #'   year = rep(c(2000, 2005, 2010, 2015, 2020), 2),
  #'   value = rnorm(10)
  #' )
  #' 
  #' data
  #'
  #' filled_data <- gtaptools::interpolate_spline(
  #'   input_df = data,
  #'   groups = c("Scenario", "Region"),
  #'   year = "year",
  #'   values = "value"
  #' )
  #' 
  #' filled_data
  #'
  #' @export

  # input_df |>
  #   # Remove rows where the value column is NA
  #   tidyr::drop_na(eval(value)) |>
  #   # Convert the year column to numeric (if it's not already)
  #   dplyr::mutate({{ year }} := as.numeric(eval(year))) |>
  #   # Group data by specified columns (e.g., Scenario and Region)
  #   dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
  #   # Keep just groups with at least 2 non-NA values (requirement for Spline interpolation)
  #   dplyr::mutate(non_na_count = sum(!is.na(eval(value)))) |>
  #   dplyr::filter(non_na_count >= 2) |>
  #   # Create a complete sequence of years for each group
  #   tidyr::complete({{ year }} := tidyr::full_seq(eval(year), period = 1)) |>

  valid_methods <- c("fmm", "natural", "periodic", "monoH.FC", "hyman")

  # Check if method is valid
  if (!method %in% valid_methods) {
    stop("Invalid method. Valid methods are 'fmm', 'natural', 'periodic', 'monoH.FC', and 'hyman'.")
  }

  # Check if values column is numeric
  if (!is.numeric(input_df[[values]])) {
    stop("Values column must be numeric.")
  }

  # Convert the input_df to a data.table
  data.table::setDT(input_df)

  # Step 1: Filter out NA values in the value column
  input_df <- input_df[, .SD[!is.na(get(values))]]

  # Step 2: Convert the year column to numeric
  input_df[, year] <- as.numeric(input_df[, year])

  # Step 3: Compute non_na_count and unique_years for each group
  temp <- input_df[, .(non_na_count = .N, unique_years = unique(eval(year))), by = groups]

  # Step 4: Keep groups with at least 2 non-NA values
  valid_groups <- temp[non_na_count >= 2]

  # Step 5: Create a data table of all years for each valid group
  out <- valid_groups[, .(year = seq(min(unique_years), max(unique_years), by = 1)), by = groups]

  # Step 6: Join with the original data table
  out <- input_df[out, on = c(groups, year), nomatch = NA]

  # Select specified columns
  out <- out[, .SD, .SDcols = c(groups, year, values)]

  # Assuming out is your data.table from the previous steps
  out[, (values) := stats::spline(
    x = unique(eval(year)), # Unique years in each group
    y = eval(value), # Corresponding values
    xout = min(eval(year)):max(eval(year)), # Output years for interpolation
    method = eval(method) # Method of interpolation
  )$y, # Extract the 'y' component of the spline result (interpolated values)
  by = groups
  ]

  out <- as.data.frame(out)

  return(out)
}
