#' DRG Stat
#'
#' This function returns statistic values for the Average Medicare Payments
#' in the DRG data set.
#'
#' @param stat Options for calculated statistic. Includes `"mean"`, `"meidan"`, and `"sd"`.
#'
#' @return Returns the statistic value for average medicare payments as per `stat`'s argument.
#' @export
#'
#' @import stats
#' @import utils
#'
#' @examples
#' \dontrun{
#' drg_stat(stat = "sd")
#' }
drg_stat <- function(stat) {

  if (missing(stat)) { stop("Please specify a valid option argument") }

    data("drg") # calls the drg data if the user has not already

    mea <- mean(DRG$Average.Medicare.Payments, na.rm = TRUE) # mean

    med <- median(DRG$Average.Medicare.Payments, na.rm = TRUE) # median

    sdv <- sd(DRG$Average.Medicare.Payments, na.rm = TRUE) # sd

  if (stat == "mean") { print(paste("Mean:", round(mea, 3))) } # user argument conditionals

  else if (stat == "median") { print(paste("Median:", round(med, 3))) }

  else if (stat == "sd") { print(paste("Standard Deviation:", round(sdv, 3))) }

  else { stop("Please specify a valid argument to stat.") } # invalid argument conditionals

}
