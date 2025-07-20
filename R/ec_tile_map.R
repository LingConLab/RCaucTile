#' Draw a grid tile map for East Caucasian languages.
#'
#' @returns
#' @export
#'
#' @examples
#' ec_tile_map()
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_void
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate

ec_tile_map <- function(data = NULL) {
  # Check whether data are provided
  if(!is_null(data)){
    # stop if there is no languages or feature columns in ec_languages df
    stopifnot("Data should be a dataframe with columns language and feature" =
                "data.frame" %in% class(data),
              "Data should be a dataframe with columns language and feature" =
                "language" %in% colnames(data),
              "Data should be a dataframe with columns language and feature" =
                "feature" %in% colnames(data))
    ec_languages |>
      dplyr::left_join(data, by = "language") |>
      dplyr::mutate(alpha = if_else(is.na(feature), 0.2, 1)) |>
      ggplot2::ggplot(aes(x, y, fill = lang_col, color = feature, alpha = alpha)) +
      ggplot2::geom_tile(show.legend = FALSE, linewidth = 0) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x-0.5, y=y-0.5, yend=y+0.5),
                            linewidth=2) +
      ggplot2::geom_segment(aes(x=x+0.5, xend=x+0.5, y=y-0.5, yend=y+0.5),
                            linewidth=2) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x+0.5, y=y-0.5, yend=y-0.5),
                            linewidth=2) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x+0.5, y=y+0.5, yend=y+0.5),
                            linewidth=2) +
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$lang_col)+
      ggplot2::guides(alpha="none")
  } else {
    ec_languages |>
      ggplot2::ggplot(aes(x, y, fill = lang_col, color = feature)) +
      ggplot2::geom_tile(show.legend = FALSE, size = 5) +
      ggplot2::theme_void()
  }
}
