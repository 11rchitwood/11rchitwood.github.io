# Run from blog/science-sequencing. The default output directory is figures/.
source("R/analysis.R")
source("R/figures.R")
result <- analyse()

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else "figures"
dir.create(output, showWarnings = FALSE, recursive = TRUE)
figures <- list(
  directions = list(plot_directions(result$course_pairs), 6.8, 3.8),
  scores = list(plot_scores(result$scores), 8.3, 3.9),
  missed = list(plot_missed(result$missed, result$scores), 8.3, 5.2),
  sensitivity = list(plot_sensitivity(result$sensitivity, result$scores), 9.1, 5.6)
)
iwalk(figures, function(figure, name) {
  ggsave(file.path(output, paste0(name, ".svg")), plot = figure[[1]],
         width = figure[[2]], height = figure[[3]], device = svglite::svglite, bg = "white")
})

if (length(args) >= 2L) {
  tables <- args[[2]]
  dir.create(tables, showWarnings = FALSE, recursive = TRUE)
  table_names <- c("course_pairs", "strengths", "concepts", "scores", "edge_satisfaction",
                   "missed", "sensitivity", "leave_one_concept_out", "simulation")
  iwalk(c(result[table_names], result$network[c("nodes", "components", "reciprocals")]),
        ~ write_csv(.x, file.path(tables, paste0(.y, ".csv")), na = ""))
  writeLines(capture.output(sessionInfo()), file.path(tables, "session-info.txt"))
}
