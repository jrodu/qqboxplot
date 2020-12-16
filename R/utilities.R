"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
