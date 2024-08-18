#' Plot the Number of Specimens by Institution
#'
#' This function takes metadata containing specimen information and plots the
#' number of specimens for each institution as a bar plot.
#'
#' @param metadata A data frame containing at least the column \code{institutionCode},
#' which lists the institution associated with each specimen.
#'
#' @return A bar plot showing the number of specimens for each institution.
#' @importFrom graphics barplot par text
#' @export
#'
plot_specimens_by_institution <- function(metadata) {
  sorted_counts <- sort(table(metadata$institutionCode), decreasing = TRUE)
  par(mar = c(6, 4, 4, 2) + 0.1)
  bar_positions <- barplot(sorted_counts, main = "Specimens by Institution",
    xlab = "", ylab = "Number of Specimens",
    col = "lightblue", las = 1, names.arg = NA, cex.main = 1, cex.lab = 0.75)
  text(x = bar_positions, y = par("usr")[3] - (par("usr")[4]*.05), labels = names(sorted_counts),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.8)
  par(mar = c(5, 4, 4, 2) + 0.1)
}

#' Plot the Number of Specimens by Country
#'
#' This function takes metadata containing specimen information and plots the
#' number of specimens for each country as a bar plot.
#'
#' @param metadata A data frame containing at least the column \code{country},
#' which lists the country associated with each specimen.
#'
#' @return A bar plot showing the number of specimens for each country.
#' @importFrom graphics barplot par text
#' @export
#'
plot_specimens_by_country <- function(metadata) {
  par(mar = c(6, 4, 4, 2) + 0.1)
  sorted_counts <- sort(table(metadata$country), decreasing = TRUE)
  bar_positions <- barplot(sorted_counts, main = "Specimens by Country",
    xlab = "", ylab = "Number of Specimens",
    col = "orange3", las = 1, names.arg = NA, cex.main = 1, cex.lab = 0.75)
  text(x = bar_positions, y = par("usr")[3] - (par("usr")[4]*.05), labels = names(sorted_counts),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.75)
  par(mar = c(5, 4, 4, 2) + 0.1)
}
