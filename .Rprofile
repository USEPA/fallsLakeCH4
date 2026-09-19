source("renv/activate.R")

# load targets
if (interactive()) {
  suppressMessages(library(targets))
}

if (requireNamespace("conflicted", quietly = TRUE)) {
# Set conflict preferences
library(conflicted)
conflicts_prefer(dplyr::filter(), 
                 dplyr::select(),
                 purrr::map(),
                 readxl::read_xlsx(),
                 flextable::align(),
                 flextable::compose()
                 )
}