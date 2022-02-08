#' Example header data from UPASv2x air sampler.
#'
#' A dataset containing sample information from an example UPASv2x file
#'
#' @format A data frame with 1 row and 57 variables:
#' \describe{
#'   \item{price}{price in US dollars (\$326--\$18,823)}
#'   \item{carat}{weight of the diamond (0.2--5.01)}
#'   \item{cut}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#'   \item{color}{diamond colour, from D (best) to J (worst)}
#'   \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#'     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#'   \item{x}{length in mm (0--10.74)}
#'   \item{y}{width in mm (0--58.9)}
#'   \item{z}{depth in mm (0--31.8)}
#'   \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#'   \item{table}{width of top of diamond relative to widest point (43--95)}
#'   ...
#' }
#' @source PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt
"upasv2x_header"
