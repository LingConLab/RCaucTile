#' Draw a grid tile map for East Caucasian languages.
#'
#' @param data Could be NULL, then it will print the language template. Otherwise should be a dataframe with language column annotated with some feature. Default value is NULL.
#' @param feature_column Character vector of length 1 which specifies the column in dataframe that contains annotation for the feature to color the language template with.
#'
#' @returns a `ggplot2` object
#' @export
#'
#' @examples
#' ec_tile_map()
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_colour_discrete
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 guides
#' @importFrom dplyr left_join
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom dplyr rename
#' @importFrom forcats fct_inorder
#' @importFrom grDevices col2rgb

ec_tile_map <- function(data = NULL, feature_column = "feature") {
  # Check whether data are provided
  if(!is_null(data)){
    # Arguments check
    stopifnot("Data should be a dataframe" =
                "data.frame" %in% class(data),
              "Data should contain column 'language'" =
                "language" %in% colnames(data),
              "Data should contain column 'feature'. If you have a different
              name, please, use the argument 'feature_column' to provide it." =
                feature_column %in% colnames(data),
              "The argument 'feature_column' should be a character vector with
              one value" =
                length(feature_column) == 1)

    ec_languages |>
      dplyr::left_join(data, by = "language") |>
      dplyr::rename(feature = {{feature}}) |>
      dplyr::rowwise() |>
      dplyr::mutate(red = grDevices::col2rgb(lang_col)[1],
                    green = grDevices::col2rgb(lang_col)[2],
                    blue = grDevices::col2rgb(lang_col)[3],
                    text_color = dplyr::if_else(
                      (red*0.299 + green*0.587 + blue*0.114) > 186,
                      "#000000",
                      "#ffffff"),
                    alpha = if_else(is.na(feature), 0.2, 1),
                    text_color = dplyr::if_else(alpha == 0.2, "grey70", text_color),
                    lang_col = forcats::fct_inorder(lang_col)) ->
      for_plot

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y, fill = lang_col,
                                   color = feature, alpha = alpha)) +
      ggplot2::geom_tile(show.legend = FALSE, linewidth = 0) +
      ggplot2::geom_segment(ggplot2::aes(x=x-0.5, xend=x-0.5,
                                         y=y-0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::geom_segment(ggplot2::aes(x=x+0.5, xend=x+0.5,
                                         y=y-0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::geom_segment(ggplot2::aes(x=x-0.5, xend=x+0.5,
                                         y=y-0.5, yend=y-0.5),
                            linewidth=3) +
      ggplot2::geom_segment(ggplot2::aes(x=x-0.5, xend=x+0.5,
                                         y=y+0.5, yend=y+0.5),
                            linewidth=3) +
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "#000000",]$x,
                        y = for_plot[for_plot$text_color == "#000000",]$y,
                        label = for_plot[for_plot$text_color == "#000000",]$language,
                        color = "#000000")+
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "#ffffff",]$x,
                        y = for_plot[for_plot$text_color == "#ffffff",]$y,
                        label = for_plot[for_plot$text_color == "#ffffff",]$language,
                        color = "#ffffff")+
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "grey70",]$x,
                        y = for_plot[for_plot$text_color == "grey70",]$y,
                        label = for_plot[for_plot$text_color == "grey70",]$language,
                        color = "grey70")+
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$lang_col)+
      ggplot2::scale_colour_discrete(na.translate = FALSE)+
      ggplot2::guides(alpha="none")+
      ggplot2::labs(color = NULL)+
      ggplot2::theme(legend.position = "bottom")
  } else {
    ec_languages |>
      dplyr::rowwise() |>
      dplyr::mutate(red = grDevices::col2rgb(lang_col)[1],
                    green = grDevices::col2rgb(lang_col)[2],
                    blue = grDevices::col2rgb(lang_col)[3],
                    text_color = dplyr::if_else(
                      (red*0.299 + green*0.587 + blue*0.114) > 186,
                      "#000000",
                      "#ffffff"),
                    lang_col = forcats::fct_inorder(lang_col)) |>
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = lang_col), show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = language, color = text_color),
                         show.legend = FALSE)+
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$lang_col)+
      ggplot2::scale_color_manual(values = c("black", "white"))
  }
}
