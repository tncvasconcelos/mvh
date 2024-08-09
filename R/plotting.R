#' Plot the Number of Specimens by Institution
#'
#' This function takes metadata containing specimen information and plots the
#' number of specimens for each institution as a bar plot.
#'
#' @param metadata A data frame containing at least the column \code{institutionCode},
#' which lists the institution associated with each specimen.
#'
#' @return A bar plot showing the number of specimens for each institution.
#' @importFrom graphics barplot
#' @export
#'
plot_specimens_by_institution <- function(metadata) {
  sorted_counts <- sort(table(metadata$institutionCode), decreasing = TRUE)
  barplot(sorted_counts, main = "Specimens by Institution",
    xlab = "", ylab = "Number of Specimens",
    col = "lightblue", las = 2)
}
