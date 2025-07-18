# Draw a grid tile map for East Caucasian languages.
#

ec_tile_map <- function() {
  ec_languages |>
    mutate(feature = rbernoulli(n = 56)) |>
    ggplot2::ggplot(aes(x, y, fill = lang_col, color = feature))+
    ggplot2::geom_tile(show.legend = FALSE, linejoin = "mitre", size = 5)+
    scale_color_manual(values = c("red", "black"))+
    ggplot2::theme_void()
}
