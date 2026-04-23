# List of packages for session
pkgs <- c(
  "data.table",
  "tidyverse",
  "sf",
  "ggrepel",
  "openxlsx"
)

# check availability
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  stop("Missing packages: ", paste(missing, collapse = ", "))
}

# attach only what you really need
invisible(lapply(pkgs, library, character.only = TRUE))

# theme
ggplot2::theme_set(ggplot2::theme_bw())
