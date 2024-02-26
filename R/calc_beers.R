calc_beers <- function(values, method = "ordinary") {
  #' @name calc_beers
  #' @title Perform Beers Interpolation or Subdivision
  #'
  #' @description This function implements the Beers interpolation or subdivision methods, either in their ordinary or modified forms. It generates interpolated or subdivided points from a given data set. The Beers interpolation method, first introduced in "The Record of the American Institute of Actuaries" (Vol. 34, Part I, 1945), is a six-term formula designed to minimize the fifth differences in interpolated results. More details can be found in "The Methods and Materials of Demography, Volume 2" (page 877).
  #'
  #' @param values A numeric vector of data points for interpolation or subdivision.
  #' @param method A character string specifying the method to be used. (default = "ordinary")
  #'               Must be one of "ordinary", "modified", "subidvision_ordinary", or "subidvision_modified".
  #'               - "ordinary": In interpolation, include original data points unchanged; in subdivision,
  #'                 each set of 5 subdivided values sums to the original data point.
  #'               - "modified": In interpolation, includes some smoothing with only the first and last points unchanged;
  #'                 in subdivision, similar smoothing occurs.
  #' @return A numeric vector with interpolated or subdivided values.
  #' @examples
  #' calc_beers(c(1, 2, 3, 4, 5), "ordinary")
  #' calc_beers(c(1, 2, 3, 4, 5), "modified")
  #' calc_beers(c(1, 2, 3, 4, 5), "subidvision_ordinary")
  #' calc_beers(c(1, 2, 3, 4, 5), "subidvision_modified")
  #' @export

  # Check if 'values' is numeric
  if (!is.numeric(values)) {
    stop("The 'values' parameter must be a numeric vector.")
  }
  # Check if method is valid
  valid_methods <- c("ordinary", "modified", "subidvision_ordinary", "subidvision_modified")
  if (!method %in% valid_methods) {
    stop("Method must be one of 'ordinary', 'modified', 'subidvision_ordinary', or 'subidvision_modified'.")
  }

  beers_int_ord <- aperm(array(c(
    1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
    0.6667, 0.4969, -0.1426, -0.1006, 0.1079, -0.0283,
    0.4072, 0.8344, -0.2336, -0.0976, 0.1224, -0.0328,
    0.2148, 1.0204, -0.2456, -0.0536, 0.0884, -0.0244,
    0.0819, 1.0689, -0.1666, -0.0126, 0.0399, -0.0115,
    0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000,
    -0.0404, 0.8404, 0.2344, -0.0216, -0.0196, 0.0068,
    -0.0497, 0.6229, 0.5014, -0.0646, -0.0181, 0.0081,
    -0.0389, 0.3849, 0.7534, -0.1006, -0.0041, 0.0053,
    -0.0191, 0.1659, 0.9354, -0.0906, 0.0069, 0.0015,
    0.0000, 0.0000, 1.0000, 0.0000, 0.0000, 0.0000,
    0.0117, -0.0921, 0.9234, 0.1854, -0.0311, 0.0027,
    0.0137, -0.1101, 0.7194, 0.4454, -0.0771, 0.0087,
    0.0087, -0.0771, 0.4454, 0.7194, -0.1101, 0.0137,
    0.0027, -0.0311, 0.1854, 0.9234, -0.0921, 0.0117,
    0.0000, 0.0000, 0.0000, 1.0000, 0.0000, 0.0000,
    0.0015, 0.0069, -0.0906, 0.9354, 0.1659, -0.0191,
    0.0053, -0.0041, -0.1006, 0.7534, 0.3849, -0.0389,
    0.0081, -0.0181, -0.0646, 0.5014, 0.6229, -0.0497,
    0.0068, -0.0196, -0.0216, 0.2344, 0.8404, -0.0404,
    0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000,
    -0.0115, 0.0399, -0.0126, -0.1666, 1.0689, 0.0819,
    -0.0244, 0.0884, -0.0536, -0.2456, 1.0204, 0.2148,
    -0.0328, 0.1224, -0.0976, -0.2336, 0.8344, 0.4072,
    -0.0283, 0.1079, -0.1006, -0.1426, 0.4969, 0.6667
  ), dim = c(6, 5, 5)), c(2, 1, 3))

  beers_int_mod <- aperm(array(c(
    1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,
    0.6668, 0.5270, -0.2640, 0.0820, -0.0140, 0.0022,
    0.4099, 0.8592, -0.3598, 0.1052, -0.0173, 0.0028,
    0.2196, 1.0279, -0.3236, 0.0874, -0.0136, 0.0023,
    0.0862, 1.0644, -0.1916, 0.0464, -0.0066, 0.0012,
    0.0000, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000,
    -0.0486, 0.8655, 0.2160, -0.0350, 0.0030, -0.0009,
    -0.0689, 0.6903, 0.4238, -0.0442, 0.0003, -0.0013,
    -0.0697, 0.5018, 0.5938, -0.0152, -0.0097, -0.0010,
    -0.0589, 0.3233, 0.7038, 0.0578, -0.0257, -0.0003,
    -0.0430, 0.1720, 0.7420, 0.1720, -0.0430, 0.0000,
    -0.0270, 0.0587, 0.7072, 0.3162, -0.0538, -0.0013,
    -0.0141, -0.0132, 0.6098, 0.4708, -0.0477, -0.0056,
    -0.0056, -0.0477, 0.4708, 0.6098, -0.0132, -0.0141,
    -0.0013, -0.0538, 0.3162, 0.7072, 0.0587, -0.0270,
    0.0000, -0.0430, 0.1720, 0.7420, 0.1720, -0.0430,
    -0.0003, -0.0257, 0.0578, 0.7038, 0.3233, -0.0589,
    -0.0010, -0.0097, -0.0152, 0.5938, 0.5018, -0.0697,
    -0.0013, 0.0003, -0.0442, 0.4238, 0.6903, -0.0689,
    -0.0009, 0.0030, -0.0350, 0.2160, 0.8655, -0.0486,
    0.0000, 0.0000, 0.0000, 0.0000, 1.0000, 0.0000,
    0.0012, -0.0066, 0.0464, -0.1916, 1.0644, 0.0862,
    0.0023, -0.0136, 0.0874, -0.3236, 1.0279, 0.2196,
    0.0028, -0.0173, 0.1052, -0.3598, 0.8592, 0.4099,
    0.0022, -0.0140, 0.0820, -0.2640, 0.5270, 0.6668
  ), dim = c(6, 5, 5)), c(2, 1, 3))

  beers_ord_sub <- aperm(array(c(
    0.3333, -0.1636, -0.0210, 0.0796, -0.0283,
    0.2595, -0.0780, 0.0130, 0.0100, -0.0045,
    0.1924, 0.0064, 0.0184, -0.0256, 0.0084,
    0.1329, 0.0844, 0.0054, -0.0356, 0.0129,
    0.0819, 0.1508, -0.0158, -0.0284, 0.0115,
    0.0404, 0.2000, -0.0344, -0.0128, 0.0068,
    0.0093, 0.2268, -0.0402, 0.0028, 0.0013,
    -0.0108, 0.2272, -0.0248, 0.0112, -0.0028,
    -0.0198, 0.1992, 0.0172, 0.0072, -0.0038,
    -0.0191, 0.1468, 0.0822, -0.0084, -0.0015,
    -0.0117, 0.0804, 0.1570, -0.0284, 0.0027,
    -0.0020, 0.0160, 0.2200, -0.0400, 0.0060,
    0.0050, -0.0280, 0.2460, -0.0280, 0.0050,
    0.0060, -0.0400, 0.2200, 0.0160, -0.0020,
    0.0027, -0.0284, 0.1570, 0.0804, -0.0117,
    -0.0015, -0.0084, 0.0822, 0.1468, -0.0191,
    -0.0038, 0.0072, 0.0172, 0.1992, -0.0198,
    -0.0028, 0.0112, -0.0248, 0.2272, -0.0108,
    0.0013, 0.0028, -0.0402, 0.2268, 0.0093,
    0.0068, -0.0128, -0.0344, 0.2000, 0.0404,
    0.0115, -0.0284, -0.0158, 0.1508, 0.0819,
    0.0129, -0.0356, 0.0054, 0.0844, 0.1329,
    0.0084, -0.0256, 0.0184, 0.0064, 0.1924,
    -0.0045, 0.0100, 0.0130, -0.0780, 0.2595,
    -0.0283, 0.0796, -0.0210, -0.1636, 0.3333
  ), dim = c(5, 5, 5)), c(2, 1, 3))

  beers_mod_sub <- aperm(array(c(
    0.3332, -0.1938, 0.0702, -0.0118, 0.0022,
    0.2569, -0.0753, 0.0205, -0.0027, 0.0006,
    0.1903, 0.0216, -0.0146, 0.0032, -0.0005,
    0.1334, 0.0969, -0.0351, 0.0059, -0.0011,
    0.0862, 0.1506, -0.0410, 0.0054, -0.0012,
    0.0486, 0.1831, -0.0329, 0.0021, -0.0009,
    0.0203, 0.1955, -0.0123, -0.0031, -0.0004,
    0.0008, 0.1893, 0.0193, -0.0097, 0.0003,
    -0.0108, 0.1677, 0.0577, -0.0153, 0.0007,
    -0.0159, 0.1354, 0.0972, -0.0170, 0.0003,
    -0.0160, 0.0973, 0.1321, -0.0121, -0.0013,
    -0.0129, 0.0590, 0.1564, 0.0018, -0.0043,
    -0.0085, 0.0260, 0.1650, 0.0260, -0.0085,
    -0.0043, 0.0018, 0.1564, 0.0590, -0.0129,
    -0.0013, -0.0121, 0.1321, 0.0973, -0.0160,
    0.0003, -0.0170, 0.0972, 0.1354, -0.0159,
    0.0007, -0.0153, 0.0577, 0.1677, -0.0108,
    0.0003, -0.0097, 0.0193, 0.1893, 0.0008,
    -0.0004, -0.0031, -0.0123, 0.1955, 0.0203,
    -0.0009, 0.0021, -0.0329, 0.1831, 0.0486,
    -0.0012, 0.0054, -0.0410, 0.1506, 0.0862,
    -0.0011, 0.0059, -0.0351, 0.0969, 0.1334,
    -0.0005, 0.0032, -0.0146, 0.0216, 0.1903,
    0.0006, -0.0027, 0.0205, -0.0753, 0.2569,
    0.0022, -0.0118, 0.0702, -0.1938, 0.3332
  ), dim = c(5, 5, 5)), c(2, 1, 3))


  if (method %in% c("ordinary", "modified")) {
    int <- function(values, M1, M2, M3, M4, M5) {
      if (length(values) < 6) {
        stop("Beers interpolation requires at least 6 values")
      }

      num_points <- length(values)
      total_new_points <- (num_points * 5) - 4
      out <- rep(NA, total_new_points)

      # Handle edge cases and middle frames using sliding-window approach
      for (idx in 1:5) {
        out[idx] <- sum(values[1:6] * M1[idx, ])
        out[idx + 5] <- sum(values[1:6] * M2[idx, ])
        out[total_new_points - 11 + idx] <- sum(values[(num_points - 6 + 1):(num_points)] * M4[idx, ])
        out[total_new_points - 6 + idx] <- sum(values[(num_points - 6 + 1):(num_points)] * M5[idx, ])
      }

      for (pos in 3:(num_points - 3)) {
        for (idx in 1:5) {
          out[((pos - 1) * 5) + idx] <- sum(values[(pos - 2):(pos + 3)] * M3[idx, ])
        }
      }

      # Ensure the last data point is included
      out[total_new_points] <- values[num_points]

      out
    }

    if (method == "ordinary") {
      out <-
        int(
          values,
          beers_int_ord[, , 1],
          beers_int_ord[, , 2],
          beers_int_ord[, , 3],
          beers_int_ord[, , 4],
          beers_int_ord[, , 5]
        )
    }

    if (method == "modified") {
      out <-
        int(
          values,
          beers_int_mod[, , 1],
          beers_int_mod[, , 2],
          beers_int_mod[, , 3],
          beers_int_mod[, , 4],
          beers_int_mod[, , 5]
        )
    }
  }

  if (method %in% c("subidvision_ordinary", "subidvision_modified")) {
    int <- function(values, M1, M2, M3, M4, M5) {
      if (length(values) < 5) {
        stop("At least 5 values points are required for subdivision.")
      }

      num_points <- length(values)
      total_new_points <- num_points * 5
      out <- rep(NA, total_new_points)

      # Calculate the outs for the margins
      for (idx in 1:5) {
        out[idx] <- sum(values[1:5] * M1[idx, ])
        out[idx + 5] <- sum(values[1:5] * M2[idx, ])
        out[total_new_points - 10 + idx] <- sum(values[(num_points - 5 + 1):(num_points)] * M4[idx, ])
        out[total_new_points - 5 + idx] <- sum(values[(num_points - 5 + 1):(num_points)] * M5[idx, ])
      }

      # Calculate the outs for the middle points
      for (point in 3:(num_points - 2)) {
        for (idx in 1:5) {
          out[((point - 1) * 5) + idx] <- sum(values[(point - 2):(point + 2)] * M3[idx, ])
        }
      }

      out
    }

    if (method == "subidvision_ordinary") {
      out <-
        int(
          values,
          beers_ord_sub[, , 1],
          beers_ord_sub[, , 2],
          beers_ord_sub[, , 3],
          beers_ord_sub[, , 4],
          beers_ord_sub[, , 5]
        )
    }

    if (method == "subidvision_modified") {
      out <-
        int(
          values,
          beers_mod_sub[, , 1],
          beers_mod_sub[, , 2],
          beers_mod_sub[, , 3],
          beers_mod_sub[, , 4],
          beers_mod_sub[, , 5]
        )
    }
  }

  return(out)
}