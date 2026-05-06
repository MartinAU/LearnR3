#' read in one nurses data file
#'
#' @param file_path
#' @param max_rows max rows to read
#'
#' @returns your taxes, fiscal year 1987, in the form of one data frame, tibble

read <- function(file_path, max_rows = 10) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = to_snake_case,
      n_max = max_rows
    )
  return(data)
}

