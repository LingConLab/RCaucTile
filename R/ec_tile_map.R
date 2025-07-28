#' Draw a grid tile map for East Caucasian languages.
#'
#' @param data Could be NULL, then it will print the language template. Otherwise should be a dataframe with language column annotated with some feature. Default value is NULL.
#' @param feature_column Character vector of length 1 which specifies the column in dataframe that contains annotation for the feature to color the language template with.
#' @param abbreviation logical variable that specifies, whether use abbreviations for languages specified in the package.
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
#' @importFrom grDevices col2rgb

ec_tile_map <- function(data = NULL,
                        feature_column = "feature",
                        abbreviation = TRUE) {

# fake variables for R CMD check to be succeedded -------------------------

  x <- NULL
  y <- NULL
  language_color <- NULL
  feature <- NULL
  alpha <- NULL
  language <- NULL
  text_color <- NULL

# load data ---------------------------------------------------------------

ec_languages <- RCaucTile::ec_languages

# Check whether user provided some data -----------------------------------

  if(!is.null(data)){

# Arguments check ---------------------------------------------------------

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

# merge EC dataset with data provided by a user ---------------------------

    for_plot <- merge(ec_languages, data, all.x = TRUE)

# create a column with the name feature if there is no one ----------------

    names(for_plot)[names(for_plot) == feature_column] <- "feature"

# add a 'text_color' column for the text colors ---------------------------

    grDevices::col2rgb(for_plot$language_color) |>
      t() |>
      as.data.frame() ->
      colors_for_text

    for_plot$text_color <- ifelse((colors_for_text$red*0.299 +
                                     colors_for_text$green*0.587 +
                                     colors_for_text$blue*0.114) > 160,
                                         "#000000",
                                         "#ffffff")

    for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                  "grey70",
                                  for_plot$text_color)

# add an 'alpha' column for the cases when there are NAs in data ----------

    for_plot$alpha <- ifelse(is.na(for_plot$feature), 0.2, 1)

# create a factor for correct coloring in ggplot --------------------------

    for_plot$language_color <- factor(for_plot$language_color,
                                      levels = for_plot$language_color)

# change labels to abbreviations ------------------------------------------

    if(isTRUE(abbreviation)){
      for_plot$language <- ifelse(is.na(for_plot$abbreviation),
                                  for_plot$language,
                                  for_plot$abbreviation)
    }

# create a map ------------------------------------------------------------

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y, fill = language_color,
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
      ggplot2::scale_fill_manual(values = ec_languages$language_color)+
      ggplot2::scale_colour_discrete(na.translate = FALSE)+
      ggplot2::guides(alpha="none")+
      ggplot2::labs(color = NULL)+
      ggplot2::theme(legend.position = "bottom")

  } else {

# if there is no data print language template -----------------------------

    for_plot <- ec_languages

# add a 'text_color' column for the text colors ---------------------------

    grDevices::col2rgb(for_plot$language_color) |>
      t() |>
      as.data.frame() ->
      colors_for_text

    for_plot$text_color <- ifelse((colors_for_text$red*0.299 +
                                     colors_for_text$green*0.587 +
                                     colors_for_text$blue*0.114) > 160,
                                  "#000000",
                                  "#ffffff")

# create a factor for correct coloring in ggplot --------------------------

    for_plot$language_color <- factor(for_plot$language_color,
                                      levels = for_plot$language_color)

# change labels to abbreviations ------------------------------------------

    if(isTRUE(abbreviation)){
      for_plot$language <- ifelse(is.na(for_plot$abbreviation),
                                  for_plot$language,
                                  for_plot$abbreviation)
    }
# create a map ------------------------------------------------------------

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y)) +
      ggplot2::geom_tile(ggplot2::aes(fill = language_color), show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = language, color = text_color),
                         show.legend = FALSE)+
      ggplot2::theme_void()+
      ggplot2::scale_fill_manual(values = ec_languages$language_color)+
      ggplot2::scale_color_manual(values = c("black", "white"))
  }
}
