plot.count.specimens <- function(metadata) {
  sorted_counts <- sort(table(metadata$institutionCode), decreasing = TRUE)
  barplot(sorted_counts, main = "Specimens by Institution", 
          xlab = "", ylab = "Number of Specimens",
          col = "lightblue", las = 2)
}
