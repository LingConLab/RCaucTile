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
      dplyr::mutate(alpha = if_else(is.na(feature), 0.2, 1),
                    lang_col = fct_inorder(lang_col)) |>
      ggplot2::ggplot(aes(x, y, fill = lang_col, color = feature, alpha = alpha)) +
      ggplot2::geom_tile(show.legend = FALSE, linewidth = 0) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x-0.5, y=y-0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::geom_segment(aes(x=x+0.5, xend=x+0.5, y=y-0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x+0.5, y=y-0.5, yend=y-0.5),
                            linewidth=3) +
      ggplot2::geom_segment(aes(x=x-0.5, xend=x+0.5, y=y+0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::geom_text(aes(label = language), color = "black") +
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$lang_col)+
      ggplot2::scale_colour_discrete(na.translate = FALSE)+
      ggplot2::guides(alpha="none")+
      ggplot2::labs(color = NULL)+
      ggplot2::theme(legend.position = "bottom")
  } else {
    ec_languages |>
      mutate(lang_col = fct_inorder(lang_col)) |>
      ggplot2::ggplot(aes(x, y)) +
      ggplot2::geom_tile(aes(fill = lang_col), show.legend = FALSE, size = 5) +
      ggplot2::geom_label(aes(label = language))+
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$lang_col)
  }
}
