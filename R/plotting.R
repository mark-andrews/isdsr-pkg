#' Interaction plot using expected marginal means
#'
#' @param model A model such as afex::aov_car of a two-way interaction.
#' @param iv1 One of the two independent variables (variable name must be quoted)
#' @param iv2 The other of the two independent variables (variable name must be quoted)
#' @param ylab The y-axis label
#' @param text_size The size of the font in the legend
#'
#' @return A `patchwork` object
#' @export
#'
#' @examples
#' M <- afex::aov_car(nc_erp ~ category + Error(id/change), data = catknowledge)
#' twoway_interaction_plot(M, 'category', 'change', ylab = 'Nc ERP amplitude')
twoway_interaction_plot <- function(model, iv1, iv2, ylab, text_size = 8){
  
  IV1 <- rlang::sym(iv1)
  IV2 <- rlang::sym(iv2)
  
  formula_1 <- rlang::expr(!!IV1 ~ !!IV2)
  formula_2 <- rlang::expr(!!IV2 ~ !!IV1)
  
  G <-
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = 'bottom',
                   legend.text = element_text(size = text_size),
                   legend.title = element_text(size = text_size)
    )
  
  p1 <- emmeans::emmip(model, formula_1, ylab = ylab) +
    ggplot2::scale_color_brewer(palette = 'Set1') +
    G
  
  p2 <- emmeans::emmip(model, formula_2, ylab = ylab) +
    ggplot2::scale_color_brewer(palette = 'Set1') +
    G
  
  p1 + p2 + patchwork::plot_layout(ncol = 2) + patchwork::plot_annotation(tag_levels = 'A')
  
}