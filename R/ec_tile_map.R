#' Draw a grid tile map for East Caucasian languages.
#'
#' @param data Could be NULL, then it will print the language template. Otherwise should be a dataframe with language column annotated with some feature. Default value is NULL.
#' @param feature_column Character vector of length 1 which specifies the column in dataframe that contains annotation for the feature to color the language template with.
#' @param title Character vector of length 1 which specifies the title of the plot.
#' @param fill_by Character vector of length 1 which specifies, whether internal part of the rectangular should be colored by the language (value "language") or by the feature (value "feature"). Default value is "feature".
#' @param abbreviation Logical variable that specifies, whether use abbreviations for languages specified in the package.
#'
#' @returns a `ggplot2` object
#' @export
#'
#' @examples
#' ec_tile_map()
#' ec_tile_map(ec_languages,
#'             feature_column = "morning_greetings",
#'             fill_by = "feature",
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
#' @importFrom ggplot2 guides
#' @importFrom grDevices col2rgb

ec_tile_map <- function(data = NULL,
                        feature_column = "feature",
                        title = NULL,
                        fill_by = "feature",
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
                length(feature_column) == 1,
              "The argument 'title' should be a character vector with
              one value" =
                length(title) <= 1,
              "The argument 'abbreviation' should be a logical vector with
              one value" =
                length(abbreviation) == 1,
              "The argument 'fill_by' should be a character vector with
              two possible values: 'language' or 'feature'" =
                length(fill_by) == 1,
              "The argument 'fill_by' should be a character vector with
              two possible values: 'language' or 'feature'" =
                fill_by %in% c("language", "feature"))

    # merge EC dataset with data provided by a user ---------------------------

    for_plot <- merge(ec_languages, data, all.x = TRUE)

    # create a column with the name feature if there is no one ----------------

    names(for_plot)[names(for_plot) == feature_column] <- "feature"

    # add a 'text_color' column for the text colors ---------------------------

    if(fill_by == "language"){
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
    } else if(fill_by == "feature") {

      for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                    "grey70",
                                    "#000000")
    }

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

    # change fill according to fill_by argument -------------------------------

    if(fill_by == "language"){
      for_plot$fill_by <- for_plot$language_color
    } else if(fill_by == "feature"){
      for_plot$fill_by <- for_plot$feature
    }

    # create a map ------------------------------------------------------------

    for_plot |>
      ggplot2::ggplot(ggplot2::aes(x, y, fill = fill_by,
                                   color = feature, alpha = alpha)) +
      ggplot2::geom_tile(show.legend = FALSE, linewidth = 0) +
      ggplot2::geom_tile(alpha = 0, linewidth = 3, height = 0.92, width = 0.92)+
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
      ggplot2::scale_colour_discrete(na.translate = FALSE)+
      ggplot2::guides(alpha="none", fill = "none")+
      ggplot2::labs(color = NULL, title = title)+
      ggplot2::theme(legend.position = "bottom") ->
      p

    if(fill_by == "language"){
      p <- p + ggplot2::scale_fill_manual(values = ec_languages$language_color)
    }

    return(p)

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
      ggplot2::scale_color_manual(values = c("black", "white"))+
      ggplot2::labs(color = NULL, title = title)
  }
}
