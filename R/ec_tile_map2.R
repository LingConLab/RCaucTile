#' Draw a grid tile map for East Caucasian languages.
#'
#' @param data Could be \code{NULL}, then it will print the language template. Otherwise should be a dataframe with language column annotated with some feature. Default value is \code{NULL}.
#' @param feature_column Character vector of length 1 which specifies the column in dataframe that contains annotation for the feature to color the language template with.
#' @param title Character vector of length 1, which specifies the title of the plot.
#' @param title_position Character vector of length 1, which specifies the title's position. Possible values are \code{left}, \code{center}, and \code{right}. Default value is \code{left}.
#' @param fill_by Character vector of length 1 which specifies, whether internal part of the rectangular should be colored by the language (value \code{language}) or by the feature (value \code{feature}). Default value is \code{feature}.
#' @param annotate_feature Logical variable that specifies, whether to add feature values on the tile. This especially make sense in case of numeric features. Default value is \code{FALSE}.
#' @param abbreviation Logical variable that specifies, whether use abbreviations for languages specified in the package. Default value is \code{TRUE}.
#'
#' @returns a `ggplot2` object
#' @export
#'
#' @examples
#' ec_tile_map()
#' ec_tile_map(ec_languages,
#'             feature_column = "morning_greetings",
#'             title = "Morning greetings (Naccarato, Verhees 2021)")
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_colour_discrete
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 guides
#' @importFrom grDevices col2rgb
#' @export

ec_tile_map2 <- function(data = NULL,
                         feature_column = "feature",
                         title = NULL,
                         title_position = "left",
                         fill_by = "feature",
                         annotate_feature = FALSE,
                         abbreviation = TRUE) {

  # arguments check ---------------------------------------------------------

  stopifnot("The argument 'title' should be a character vector with one value" =
              length(title) <= 1,
            "The argument 'title' should be a character vector with one value" =
              is.character(title) | is.null(title),
            "The argument 'title_position' should be a character vector with one of the following values: 'left', 'center', or 'right'" =
              length(title_position) <= 1,
            "The argument 'title_position' should be a character vector with one of the following values: 'left', 'center', or 'right'" =
              title_position %in% c('left', 'center', 'right'),
            "The argument 'annotate_feature' should be a logical vector with one value" =
              length(annotate_feature) == 1,
            "The argument 'annotate_feature' should be a logical vector with one value" =
              is.logical(annotate_feature),
            "The argument 'abbreviation' should be a logical vector with one value" =
              length(abbreviation) == 1,
            "The argument 'abbreviation' should be a logical vector with one value" =
              is.logical(abbreviation))

  # redefine title_position -------------------------------------------------

  if(title_position == "left") {
    title_position <- 0
  } else if(title_position == "center") {
    title_position <- 0.5
  } else if(title_position == "right") {
    title_position <- 1
  }

  # ec_template() assignment ------------------------------------------------

  if(is.null(data)){
    ec_template(title = title,
                title_position = title_position,
                abbreviation = abbreviation)
  } else {

    # arguments check ---------------------------------------------------------

    stopifnot(
      "Data should be a dataframe" =
        "data.frame" %in% class(data),
      "Data should contain column 'language'" =
        "language" %in% colnames(data),
      "Data should contain column 'feature'. If you have a column with a different name, please, use the argument 'feature_column' to provide it." =
        feature_column %in% colnames(data),
      "The argument 'feature_column' should be a character vector with one value" =
        length(feature_column) == 1,
      "The argument 'fill_by' should be a character vector with
              two possible values: 'language' or 'feature'" =
        length(fill_by) == 1,
      "The argument 'fill_by' should be a character vector with
              two possible values: 'language' or 'feature'" =
        fill_by %in% c("language", "feature"))

    # merge EC dataset with data provided by a user ---------------------------

    for_plot <- merge(RCaucTile::ec_languages, data, all.x = TRUE)

    # create a column with the name feature if there is no one ----------------

    names(for_plot)[names(for_plot) == feature_column] <- "feature"

    # ec_tile_numeric() or ec_tile_categorical() ------------------------------

    if(is.numeric(for_plot$feature)){
      ec_tile_numeric(data = for_plot,
                      title = title,
                      title_position = title_position,
                      fill_by = fill_by,
                      annotate_feature = annotate_feature,
                      abbreviation = abbreviation)
    } else {
      ec_tile_categorical(data = for_plot,
                          title = title,
                          title_position = title_position,
                          fill_by = fill_by,
                          annotate_feature = annotate_feature,
                          abbreviation = abbreviation)
    }
  }
}

ec_template <- function(title,
                        title_position,
                        abbreviation){

  # fake variables for R CMD check to be succeedded -------------------------

  x <- NULL
  y <- NULL
  language_color <- NULL
  feature <- NULL
  alpha <- NULL
  language <- NULL
  text_color <- NULL

  # load data ---------------------------------------------------------------

  for_plot <- RCaucTile::ec_languages

  # add a 'text_color' column for the text colors ---------------------------

  for_plot$text_color <- define_annotation_color(for_plot$language_color)

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
                       show.legend = FALSE) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = ec_languages$language_color) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::labs(color = NULL, title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = title_position))
}

ec_tile_numeric <- function(data,
                            title,
                            title_position,
                            fill_by,
                            annotate_feature,
                            abbreviation){

  for_plot <- data

  if(fill_by == "feature"){

    for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                  "grey70",
                                  "#ffffff")

    for_plot$alpha <- ifelse(is.na(for_plot$feature), 0.2, 1)

    if(isTRUE(abbreviation)){
      for_plot$language <- ifelse(is.na(for_plot$abbreviation),
                                  for_plot$language,
                                  for_plot$abbreviation)
    }

    if(isTRUE(annotate_feature)){
      for_plot$language <- ifelse(is.na(for_plot$feature),
                                  for_plot$language,
                                  paste0(for_plot$language, "\n", for_plot$feature))
    }

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y,
                                   fill = feature,
                                   alpha = alpha)) +
      ggplot2::geom_tile(linewidth = 0) +
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "#ffffff",]$x,
                        y = for_plot[for_plot$text_color == "#ffffff",]$y,
                        label = for_plot[for_plot$text_color == "#ffffff",]$language,
                        color = "#ffffff") +
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "grey70",]$x,
                        y = for_plot[for_plot$text_color == "grey70",]$y,
                        label = for_plot[for_plot$text_color == "grey70",]$language,
                        color = "grey70") +
      ggplot2::theme_void() +
      ggplot2::labs(fill = NULL, color = NULL, title = title) +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = title_position)) +
      ggplot2::guides(alpha="none")

  } else if(fill_by == "language"){
    "ec_tile_numeric_language"
  }
}

ec_tile_categorical <- function(data,
                                title,
                                title_position,
                                fill_by,
                                annotate_feature,
                                abbreviation){
  for_plot <- data

  if(fill_by == "feature"){

    for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                  "grey70",
                                  "#000000")

    for_plot$alpha <- ifelse(is.na(for_plot$feature), 0.2, 1)

    if(isTRUE(abbreviation)){
      for_plot$language <- ifelse(is.na(for_plot$abbreviation),
                                  for_plot$language,
                                  for_plot$abbreviation)
    }

    if(isTRUE(annotate_feature)){
      for_plot$language <- ifelse(is.na(for_plot$feature),
                                  for_plot$language,
                                  paste0(for_plot$language, "\n", for_plot$feature))
    }

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y,
                                   fill = feature,
                                   alpha = alpha)) +
      ggplot2::geom_tile(linewidth = 0) +
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "#000000",]$x,
                        y = for_plot[for_plot$text_color == "#000000",]$y,
                        label = for_plot[for_plot$text_color == "#000000",]$language,
                        color = "#000000") +
      ggplot2::annotate(geom = "text",
                        x = for_plot[for_plot$text_color == "grey70",]$x,
                        y = for_plot[for_plot$text_color == "grey70",]$y,
                        label = for_plot[for_plot$text_color == "grey70",]$language,
                        color = "grey70") +
      ggplot2::theme_void() +
      ggplot2::labs(fill = NULL, color = NULL, title = title) +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = title_position)) +
      ggplot2::guides(alpha="none") +
      ggplot2::scale_fill_discrete(na.translate = FALSE)

  } else if(fill_by == "language"){
    "ec_tile_categorical_language"
  }

}

define_annotation_color <- function(colors){

  grDevices::col2rgb(colors) |>
    t() |>
    as.data.frame() ->
    colors_for_text

  ifelse((colors_for_text$red*0.299 +
            colors_for_text$green*0.587 +
            colors_for_text$blue*0.114) > 160,
         "#000000",
         "#ffffff")
}
