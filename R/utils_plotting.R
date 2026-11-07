
#' the void theme
#'
#'@export
theme_void_pango <- function(){
  ggplot2::theme(
    plot.margin = ggplot2::margin(0,0,0,0),
    plot.background = ggplot2::element_rect(fill = "white",
                                            colour = "white"),
    panel.background = ggplot2::element_rect(fill = "white",
                                             colour = "white"),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "None",
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
}

#' plotting point plot
#'
point_plot_pango <- function(plotting_dataset,
                             x="x",
                             y="y",
                             colour_key="leiden",
                             show_colour=character(),
                             facet_key=character(),
                             size=0.5){

  n_levels <- plotting_dataset[[colour_key]] |>
    as.character() |>
    unique() |>
    length()

  if (length(show_colour) == 0) {
    if (n_levels > 20) {
      show_colour <- plotting_dataset |>
        dplyr::count(.data[[colour_key]], sort = TRUE) |>
        dplyr::slice_head(n = 20) |>
        dplyr::pull(.data[[colour_key]]) |>
        as.character()
    }else{
      show_colour <- plotting_dataset[[colour_key]] |>
        as.character() |>
        unique()
    }
  }

  plotting_dataset <- plotting_dataset |>
    dplyr::mutate(
      .colour_group = ifelse(
        as.character(.data[[colour_key]]) %in% show_colour,
        as.character(.data[[colour_key]]),
        NA_character_
      )
    )

  if (length(facet_key)==0) {
    plotting_dataset <- plotting_dataset |>
      dplyr::mutate(
        .facet_title = "title")
  }else{
    plotting_dataset <- plotting_dataset |>
      dplyr::mutate(
        .facet_title = as.character(.data[[facet_key]])
        )
  }

  pt_plt <- ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = .data[[x]],
                                               y = .data[[y]],
                                               colour = as.factor(.colour_group)),
                        data = plotting_dataset,
                        size = size) +
    ggthemes::scale_colour_tableau(palette = "Tableau 20",
                                   na.value = "grey50") +
    ggplot2::facet_wrap(~.facet_title,
                        scales = "free")+
    theme_void_pango()

}

#' save ggplot as png
#' 
#' @param gplot_obj the ggplot object
#' 
#' @export 
#' 

ggsave_pango <- function(png_path,
                         gplot_obj,
                         width,
                         height,
                         dpi=300) {

  tmp_file <- tempfile(fileext = ".pdf")
  ggplot2::ggsave(filename = tmp_file,
                  plot=gplot_obj,
                  width=width,
                  height=height,
                  units="mm")
  reticulate::py$sampling_pdf_to_png(pdf_path=tmp_file,
                                     png_path=png_path,
                                     dpi=dpi)

  return(png_path)

}

