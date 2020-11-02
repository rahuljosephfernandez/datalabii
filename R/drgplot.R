#' DRG Plot
#'
#' This function returns a ggplot box-plot for specified payment classes.
#'
#' @param option Options for plot variables. Includes `Average.Medicare.Payments`,
#' `Average.Total.Payments`, and `Average.Covered.Charges`.
#'
#'
#' @return Returns a ggplot box-plot for payments indicated by the `option` argument.
#' @export
#'
#' @import tidyverse
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import ggdark
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data("drg")
#' avgmedi_boxplot <- drg_plot(option = "Average.Medicare.Payments")
#' }
drg_plot <- function(option) {

  if (missing(option)) { stop("Please specify a valid option argument") }

  plt <- DRG %>%

    ggplot(aes(get(option), Code, fill = Code))+ # initialise ggplot

    geom_boxplot(outlier.size = 0.2)+ # outlier point size changes

    dark_theme_minimal()+

    coord_flip()+

    xlab(paste0(str_replace_all(option, "[[:punct:]]", " "), " (USD)"))+ # axis label

    ggtitle(label = paste0("Boxplots for The ", str_replace_all(option, "[[:punct:]]", " "), " (USD) Distribution"))+ # plot title

    theme(plot.title = element_text(face = "bold"), # aesthetic adjusting
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x  = element_text(angle = 90, size = 4.5, hjust = 1),
          panel.grid = element_blank(),
          legend.position = "none")

  plt # returns ggplot

}
