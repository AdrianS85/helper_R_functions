generate_alluvial <- function(
    dataset,
    x_, # vector annotating values to a given x axis tick, e.g. round
    stratum_, # vector annotating values to a given color, e.g. accommodation
    alluvium_, # vector annotating values to a given wave/line, e.g. LONGIT_ID
    x_level_order = NULL, 
    y_breaks = NULL,
    transparent = F,
    color_labels = NULL,
    x_name = NULL,
    y_name = NULL,
    stratum_name = NULL,
    plot_name = "alluvial_plot")
{
  library(ggplot2)
  library(ggalluvial)
  library(scales)
  
  
  bg_ <- "white"
  
  if (is.null(x_level_order)) { x_level_order <- unique(x_) }
  if (is.null(color_labels)) { color_labels <- stratum_ }
  if (is.null(x_name)) { x_name <- "x axis" }
  if (is.null(y_name)) { y_name <- "y axis" }
  if (is.null(stratum_name)) { stratum_name <- "color" }
  
  p_ <- ggplot(df, aes(x = x_, stratum = stratum_, alluvium = alluvium_, fill = stratum_, label = stratum_)) +
    scale_fill_brewer(labels = color_labels, type = "qual", palette = "Set2") +
    scale_x_discrete(expand = c(.1, .1), limits = x_level_order) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum",
              aes(label = percent(after_stat(prop), accuracy = .1)))+
    labs(title = plot_name, x = x_name, y = y_name, fill = stratum_name)

  
  if (!is.null(y_breaks)) { p_ <- p_ + scale_y_continuous(breaks = y_breaks) }
  
  if (transparent) {
    
    p_ <- p_ + theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent', colour = 'transparent'))
    
    bg_ <- "transparent"
    
  }
  
  ggsave(
    filename = paste0(plot_name, ".png"),
    plot = p_,
    device = "png",
    bg = bg_,
    width = 297,
    height = 210,
    units = "mm")
  
}
