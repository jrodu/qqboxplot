"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

has_groups <- function(data) {
  # Check if groups exist
  data$group[1L] != -1L
}
