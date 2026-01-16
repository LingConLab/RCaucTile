#' Draw a grid tile map for East Caucasian languages.
#'
#' @param data Could be \code{NULL}, then it will print the language template. Otherwise should be a dataframe with language column annotated with some feature. Default value is \code{NULL}.
#' @param feature_column Character vector of length 1 which specifies the column in dataframe that contains annotation for the feature to color the language template with.
#' @param title Character vector of length 1, which specifies the title of the plot.
#' @param title_position Character vector of length 1, which specifies the title's position. Possible values are \code{left}, \code{center}, and \code{right}. Default value is \code{left}.
#' @param annotate_feature Logical variable that specifies, whether to add feature values on the tile. This especially make sense in case of numeric features. Default value is \code{FALSE}.
#' @param abbreviation Logical variable that specifies, whether use abbreviations for languages specified in the package. Default value is \code{TRUE}.
#' @param hide_languages Character variable that specifies, which languages should be removed from the template.
#' @param rename_languages This variable maps old language names to their corresponding new names. It can be represented as:
#' \itemize{
#' \item{either a named vector, where names are the old language names and values are the corresponding new language names.}
#' \item{or a data frame with two columns: \code{language} (the old language names) and \code{new_language_name} (the corresponding new language names).}}
#' @param tile_colors Character variable that specifies the tile color based on variable levels. Behavior differs depending on the type of the feature variable.
#' \itemize{
#' \item{For numeric variables, it can be filled with a vector of 2 or 3 colors defining the lower, upper, and middle segments of the palette.}
#' \item{For categorical variables, it can be filled with a vector of colors with the length equal to the number of possible values in the feature variable.}
#' \item{The variable can also use palettes from the \code{RColorBrewer} and \code{viridis} packages.}}
#' @param palette_reverse Logical variable that specifies, whether the colors in palettes from the \code{RColorBrewer} and \code{viridis} packages should be used in reverse order.
#'
#' @returns a `ggplot2` object
#' @export
#'
#' @examples
#' ec_tile_map()
#'
#' ec_tile_map(ec_languages,
#'             feature_column = "morning_greetings",
#'             title = "Morning greetings (Naccarato, Verhees 2021)")
#'
#' ec_tile_map(ec_languages,
#'             feature_column = "consonant_inventory_size",
#'             title = "Consonant inventory size (Moroz 2021)",
#'             annotate_feature = TRUE)
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
#' @importFrom scales hue_pal
#' @importFrom scales col_factor
#' @importFrom scales col_numeric
#' @export

ec_tile_map <- function(data = NULL,
                        feature_column = "feature",
                        title = NULL,
                        title_position = "left",
                        annotate_feature = FALSE,
                        abbreviation = TRUE,
                        hide_languages = NULL,
                        rename_languages = NULL,
                        tile_colors = NULL,
                        palette_reverse = FALSE) {

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
              is.logical(abbreviation),
            "The argument 'hide_languages' should be a character vector with languages, see 'ec_languages$language' for the possible values" =
              is.character(hide_languages) | is.null(hide_languages),
            "The argument 'hide_languages' should be a character vector with languages, see 'ec_languages$language' for the possible values" =
              is.null(hide_languages) |
              hide_languages %in% RCaucTile::ec_languages$language,
            "The argument 'rename_languages' should be either a named character vector with languages as a name or dataframe with columns 'language' and 'new_language_name', see 'ec_languages$language' for the possible values" =
              is.null(rename_languages) | "character" %in% class(rename_languages) | "data.frame" %in% class(rename_languages))

  if("data.frame" %in% class(rename_languages)){
    stopifnot(
      "The argument 'rename_languages' should be either a named character vector with languages as a name or dataframe with columns 'language' and 'new_language_name', see 'ec_languages$language' for the possible values" =
        "language" %in% colnames(rename_languages),
      "The argument 'rename_languages' should be either a named character vector with languages as a name or dataframe with columns 'language' and 'new_language_name', see 'ec_languages$language' for the possible values" =
        "new_language_name" %in% colnames(rename_languages),
      "The 'language' column in 'rename_languages' contains unexpected values, see 'ec_languages$language' for the possible values" =
        rename_languages$language %in% RCaucTile::ec_languages$language)
  } else if("character" %in% class(rename_languages)){
    stopifnot(
      "The names in 'rename_languages' contain unexpected values, see 'ec_languages$language' for the possible values" =
        names(rename_languages) %in% RCaucTile::ec_languages$language)
  }

  # restructure rename_languages --------------------------------------------

  if("character" %in% class(rename_languages)){
    rename_languages <- as.data.frame(rename_languages)
    colnames(rename_languages) <- "new_language_name"
    rename_languages$language <- rownames(rename_languages)
  }

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
        length(feature_column) == 1)

    # merge EC dataset with data provided by a user ---------------------------

    for_plot <- merge(RCaucTile::ec_languages, data, all.x = TRUE)

    # rename languages --------------------------------------------------------

    if(!is.null(rename_languages)){
      for_plot <- merge(for_plot, rename_languages, all.x = TRUE)
      for_plot$language <- ifelse(is.na(for_plot$new_language_name),
                                  for_plot$language,
                                  for_plot$new_language_name)
      for_plot$abbreviation <- ifelse(is.na(for_plot$new_language_name),
                                      for_plot$abbreviation,
                                      for_plot$new_language_name)
    }

    # hide languages ----------------------------------------------------------

    if(!is.null(hide_languages)){
      for_plot <- for_plot[!(for_plot$language %in% hide_languages), ]
    }

    # create a column with the name feature if there is no one ----------------

    names(for_plot)[names(for_plot) == feature_column] <- "feature"

    # change labels to abbreviations ------------------------------------------

    if(isTRUE(abbreviation)){
      for_plot$language <- ifelse(is.na(for_plot$abbreviation),
                                  for_plot$language,
                                  for_plot$abbreviation)
    }

    # add feature values to the language names --------------------------------

    if(isTRUE(annotate_feature)){
      for_plot$language <- ifelse(is.na(for_plot$feature),
                                  for_plot$language,
                                  paste0(for_plot$language, "\n", for_plot$feature))
    }

    # ec_tile_numeric() or ec_tile_categorical() ------------------------------

    if(is.numeric(for_plot$feature)){
      ec_tile_numeric(data = for_plot,
                      title = title,
                      title_position = title_position,
                      annotate_feature = annotate_feature,
                      abbreviation = abbreviation,
                      tile_colors = tile_colors,
                      palette_reverse = palette_reverse)
    } else {
      ec_tile_categorical(data = for_plot,
                          title = title,
                          title_position = title_position,
                          annotate_feature = annotate_feature,
                          abbreviation = abbreviation,
                          tile_colors = tile_colors,
                          palette_reverse = palette_reverse)
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
    ggplot2::scale_fill_manual(values = RCaucTile::ec_languages$language_color) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    ggplot2::labs(color = NULL, title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = title_position))
}

ec_tile_numeric <- function(data,
                            title,
                            title_position,
                            annotate_feature,
                            abbreviation,
                            tile_colors,
                            palette_reverse){

  # fake variables for R CMD check to be succeedded -------------------------

  x <- NULL
  y <- NULL
  feature <- NULL
  alpha <- NULL
  text_color <- NULL
  language <- NULL

  # load data ---------------------------------------------------------------

  for_plot <- data

  # add colors for tiles and text -------------------------------------------

  transform <- ifelse(palette_reverse, "reverse", "identity")

  possible_palettes <- c("viridis", "inferno", "magma", "plasma", "BrBG",
                         "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
                         "RdYlGn", "Spectral", "Accent", "Dark2", "Paired",
                         "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues",
                         "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                         "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu",
                         "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")

  if(length(tile_colors) <= 1){

    if(is.null(tile_colors)){
      scales::col_numeric(palette = "Blues",
                          na.color = "grey95",
                          reverse = palette_reverse,
                          domain = c(min(for_plot$feature,
                                         na.rm = TRUE),
                                     max(for_plot$feature,
                                         na.rm = TRUE)))(for_plot$feature) ->
        for_plot$tile_color
    } else if(tile_colors %in% possible_palettes){
      scales::col_numeric(palette = tile_colors,
                          na.color = "grey95",
                          reverse = palette_reverse,
                          domain = c(min(for_plot$feature,
                                         na.rm = TRUE),
                                     max(for_plot$feature,
                                         na.rm = TRUE)))(for_plot$feature) ->
        for_plot$tile_color
    } else {
      stop("We expect that if the variable 'tile_colors' is of length of one it is the name of 'ColorBrewer' or 'viridis' palettes")
    }
  } else if(length(tile_colors) > 3){

    stop("We expect that the variable 'tile_colors' is either  a vector of length of 2 or 3 or the name of 'ColorBrewer' or 'viridis' palettes")

  } else if(length(tile_colors) %in% c(2, 3)){

    for_color_check <- check_colors(tile_colors)
    for_color_check[for_color_check %in% FALSE] |>
      names() ->
      wrong_color_names

    if(length(wrong_color_names) > 0){
      paste("We found some cases with the wrong color names:",
            paste(wrong_color_names, collapse = ", ")) |>
        stop()
    }

    suppressWarnings(scales::gradient_n_pal(colours = tile_colors,
                                            values = for_plot$feature)(for_plot$feature) ->
                       for_plot$tile_color)

  }

  for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                "grey80",
                                define_annotation_color(for_plot$tile_color))


  # create a map ------------------------------------------------------------

  for_plot |>
    ggplot2::ggplot(ggplot2::aes(x, y,
                                 fill = feature,
                                 alpha = alpha)) +
    ggplot2::geom_tile(linewidth = 0) +
    ggplot2::geom_text(ggplot2::aes(color = text_color, label = language)) +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, fill = NULL) +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = title_position)) +
    ggplot2::scale_fill_continuous(palette = tile_colors, na.value = "grey95", trans = transform)+
    ggplot2::scale_color_manual(values = for_plot$text_color |> unique() |> sort())+
    ggplot2::guides(alpha="none", color = "none")
}

ec_tile_categorical <- function(data,
                                title,
                                title_position,
                                fill_by,
                                annotate_feature,
                                abbreviation,
                                tile_colors,
                                palette_reverse){

  # fake variables for R CMD check to be succeedded -------------------------

  x <- NULL
  y <- NULL
  feature <- NULL
  alpha <- NULL
  text_color <- NULL
  language <- NULL

  # load data ---------------------------------------------------------------

  for_plot <- data

  # factor feature column ---------------------------------------------------

  if(!is.factor(for_plot$feature)){
    for_plot$feature <- factor(for_plot$feature)
  }

  # add colors for tiles and text -------------------------------------------
  transform <- ifelse(palette_reverse, "reverse", "identity")

  possible_palettes <- c("viridis", "inferno", "magma", "plasma", "BrBG",
                         "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu",
                         "RdYlGn", "Spectral", "Accent", "Dark2", "Paired",
                         "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues",
                         "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                         "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu",
                         "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")

  if(length(tile_colors) <= 1){

    if(is.null(tile_colors)){
      scales::col_factor(palette = scales::hue_pal()(length(levels(for_plot$feature))),
                         na.color = "grey95",
                         domain = levels(for_plot$feature))(for_plot$feature) ->
        for_plot$tile_color
      tile_colors <- scales::hue_pal()(length(levels(for_plot$feature)))

    } else if(tile_colors %in% possible_palettes){
      scales::col_factor(palette = tile_colors,
                         na.color = "grey95",
                         reverse = palette_reverse,
                         domain = for_plot$feature)(for_plot$feature) ->
        for_plot$tile_color
    } else {
      stop("We expect that if the variable 'tile_colors' is of length of one it is the name of 'ColorBrewer' or 'viridis' palettes")
    }

    legend_colors <- scales::col_factor(palette = tile_colors,
                                        na.color = "grey95",
                                        reverse = palette_reverse,
                                        domain = for_plot$feature)(levels(for_plot$feature))

  } else if(length(tile_colors) > 1){
    for_color_check <- check_colors(tile_colors)
    for_color_check[for_color_check %in% FALSE] |>
      names() ->
      wrong_color_names

    if(length(wrong_color_names) > 0){
      paste("We found some cases with the wrong color names:",
            paste(wrong_color_names, collapse = ", ")) |>
        stop()
    }
    stopifnot("Argument 'tile_colors' should be either a vector of length equal to the number of levels in the 'feature' column, or a vector with ColorBrewer or viridis palettes" =
                length(levels(for_plot$feature)) == length(tile_colors))

        for_plot <- merge(for_plot,
                          data.frame(feature = levels(for_plot$feature),
                                     tile_color = tile_colors),
                          all.x = TRUE)
        legend_colors <- tile_colors
  }

  for_plot$text_color <- ifelse(is.na(for_plot$feature),
                                "grey80",
                                define_annotation_color(for_plot$tile_color))

  # create a map ------------------------------------------------------------

  for_plot |>
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_tile(fill = "grey95", linewidth = 0) +
    ggplot2::geom_tile(ggplot2::aes(fill = feature), linewidth = 0) +
    ggplot2::geom_text(ggplot2::aes(color = text_color, label = language)) +
    ggplot2::theme_void() +
    ggplot2::labs(fill = NULL, color = NULL, title = title) +
    ggplot2::theme(legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = title_position)) +
    ggplot2::scale_fill_manual(values = legend_colors, na.translate = FALSE)+
    ggplot2::scale_color_manual(values = for_plot$text_color |> unique() |> sort())+
    ggplot2::guides(alpha="none", color = "none")
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
         "white")
}

check_colors <- function(x) {
  sapply(x, function(color) {
    tryCatch(is.matrix(grDevices::col2rgb(color)),
             error = function(e) FALSE)
  })
}
