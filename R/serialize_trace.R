#' Serialze an rlang error trace as a nested JSON object
#'
#' @param e The rlang enhanced error
#' @param drop When printing a trace rlang usually drops some rows. Should those be dropped here as well?
#'
#' @return
#' @export
#'
#' @examples
serialize_trace <- function(e, drop = T) {
  trace <- as.data.frame(e$trace) |> dplyr::rowwise() |> dplyr::mutate(call = paste(deparse(call), collapse = "\n"))
  if(drop) trace <- trace |> dplyr::filter(visible == T)
  trace <- nest_data(trace) |> jsonlite::toJSON(auto_unbox = TRUE)
}

#' Function to recursively nest data
#'
#' @param data The data to be recursively nested
#' @param parent_id The identity of the parent call
#' @param level_name The name of the current call
#'
#' @return
#' @export
#'
#' @examples
nest_data <- function(data, parent_id = 0, level_name = NULL) {

  subset <- data |> dplyr::filter(parent == parent_id)

  nested_list <- subset$call %>%
    purrr::map(~nest_data(data, match(.x, data$call), .x)) |>
    unlist(recursive = FALSE)

  if (length(nested_list) == 0) {
    return(level_name)
  }

  return(stats::setNames(list(nested_list), level_name))
}
