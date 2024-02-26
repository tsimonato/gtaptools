interpolate_beers <- function(input_df, groups, year, values, method = "ordinary") {
  #' @name interpolate_beers
  #' @title Fill Gaps Using Beers Method Interpolation
  #'
  #' @description This function applies the Beers method interpolation on a grouped data frame.
  #' It requires each group to have at least 6 non-NA values and assumes data to be in 5-year intervals.
  #' Groups with fewer than 6 non-NA values are excluded from interpolation.
  #'
  #' @param input_df A data frame containing the data to be interpolated.
  #' @param groups A vector of column names to group by.
  #' @param year The name of the column containing year information.
  #' @param values The name of the column containing values for interpolation.
  #' @param method A character string specifying the method to be used.
  #'               Must be one of "ordinary", "modified", "subidvision_ordinary", or "subidvision_modified".
  #'               - "ordinary" (default): In interpolation, include original data points unchanged; in subdivision,
  #'                 each set of 5 subdivided values sums to the original data point.
  #'               - "modified": In interpolation, includes some smoothing with only the first and last points unchanged;
  #'                 in subdivision, similar smoothing occurs.
  #' @return A data frame with interpolated values using the Beers method.
  #' @import data.table
  #' @importFrom tidyr drop_na complete full_seq
  #' @importFrom dplyr group_by mutate filter select
  #' @importFrom stats na.omit
  #' @examples
  #' data <- data.frame(
  #'   Scenario = rep(c("Scenario1", "Scenario2"), each = 6),
  #'   Region = rep(c("Region1", "Region2"), each = 6),
  #'   year = rep(c(2000, 2005, 2010, 2015, 2020, 2025), 2),
  #'   value = rnorm(12)
  #' )
  #' 
  #' data
  #'
  #' filled_data <- gtaptools::interpolate_beers(
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
  #   # Keep just groups with at least 6 non-NA values (requirement for Beers interpolation)
  #   dplyr::mutate(non_na_count = sum(!is.na(eval(value)))) |>
  #   dplyr::filter(non_na_count >= 6) |>
  #   # Create a complete sequence of years for each group
  #   tidyr::complete({{ year }} := tidyr::full_seq(eval(year), period = 1)) |>
  #   # Interpolate values using Beers method for the complete sequence of years
  #   dplyr::mutate(
  #     {{ value }} := calc_beers(stats::na.omit(value))
  #   ) |>
  #   # Select specified columns
  #   dplyr::select(dplyr::all_of(c(groups, year, value))) |>
  #   as.data.frame()

  # Convert the input_df to a data.table
  data.table::setDT(input_df)

  # Step 1: Filter out NA values in the value column
  input_df <- input_df[, .SD[!is.na(get(values))]]

  # Step 2: Convert the year column to numeric
  input_df[, year] <- as.numeric(input_df[, year])

  # Step 3: Compute non_na_count and unique_years for each group
  temp <- input_df[, .(non_na_count = .N, unique_years = unique(eval(year))), by = groups]

  # Step 4: Keep groups with at least 6 non-NA values
  valid_groups <- temp[non_na_count >= 6]

  # Step 5: Create a data table of all years for each valid group
  out <- valid_groups[, .(year = seq(min(unique_years), max(unique_years), by = 1)), by = groups]

  # Step 6: Join with the original data table
  out <- input_df[out, on = c(groups, year), nomatch = NA]

  # Select specified columns
  out <- out[, .SD, .SDcols = c(groups, year, values)]

  # Assuming out is your data.table from the previous steps
  out[, (values) := gtaptools::calc_beers(stats::na.omit(get(values)), method = eval(method)), by = groups]

  out <- as.data.frame(out)

  return(out)
}
