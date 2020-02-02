#' Get Fred Series
#'
#' This function can pull time series from the FRED database \url{https://fred.stlouisfed.org}.
#'
#' @param series_id FRED time series ID.
#' @param series_name Choose the name for the series column in output dataframe. Default: series_id.
#' @param observation_start Dtae of the first observation in "yyyy-mm-dd" format. Default: earliest observation avaliable.
#' @param observation_end Date of last observation in "yyyy-mm-dd" format. Default: Last observation available.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate_
#' @importFrom dplyr bind_rows
#' @importFrom lubridate as_date
#' @importFrom magrittr %>%
#'
#' @export get_fred_series
#'
#' @examples
#' \dontrun{
#' get_fred_series("INDPRO", "indpro")
#' }
#'
get_fred_series <- function(series_id, series_name = NULL,
                            observation_start = NULL, observation_end = NULL) {


  length_series_id <- nchar(series_id)

  if (is.character(series_id) == FALSE) {
    stop("series_id is always in characters")
  }

  if (is.null(series_name) == TRUE ) {
    series_name <- series_id
  }

  if (is.null(observation_start) == TRUE) {
    observation_start <- "1776-07-04"
  }

  if (is.null(observation_end) == TRUE) {
    observation_end <- "9999-12-31"
  }

  df_series <-
    try({
      fromJSON(
        paste0("https://api.stlouisfed.org/fred/series/observations?series_id=",
               series_id,
               "&observation_start=",
               observation_start,
               "&observation_end=",
               observation_end,
               "&output_type=2",
               "&api_key=98f9f5cad7212e246dc5955e9b744b24&file_type=json")
      )$observations %>%
        mutate_(date = ~as_date(date))
    }, silent = TRUE)

  if (class(df_series) == "try-error") {
    stop("Download of specified time-series failed - did you misspell the identifier?")
  }

  colnames(df_series)[!(colnames(df_series) %in% "date")] <- series_name
  df_series[, 2] <- as.numeric(unlist(df_series[, 2]))
  df_series
}
