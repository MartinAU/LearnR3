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

#' Summarise values in a data frame by a rounded datetime
#'
#' @param data A data frame with at least a `collection_datetime` column and
#'  some numeric columns to summarise.
#'
#' @returns A summarised data frame.
#'
summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}
