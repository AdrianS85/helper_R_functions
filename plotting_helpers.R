generate_alluvial <- function(
    dataset,
    x_, # vector annotating values to a given x axis tick, e.g. round
    stratum_, # vector annotating values to a given color, e.g. accommodation
    alluvium_, # vector annotating values to a given wave/line, e.g. LONGIT_ID
    x_level_order = NULL, 
    y_breaks = NULL,
    background_color = NULL, # Can be set to "transparent" or "white" e.g.
    color_labels = NULL,
    x_name = NULL,
    y_name = NULL,
    text_size = NULL,
    stratum_name = NULL,
    plot_name = "alluvial_plot",
    output_dir = "" # do not end this with "/"
) 
{
  library(ggplot2)
  library(ggalluvial)
  library(scales)
  
  
  bg_ <- "white"
  
  if (is.null(x_level_order)) { x_level_order <- unique(x_) }
  if (is.null(color_labels)) { color_labels <- unique(stratum_) }
  if (is.null(x_name)) { x_name <- "x axis" }
  if (is.null(y_name)) { y_name <- "y axis" }
  if (is.null(stratum_name)) { stratum_name <- "color" }
  
  p_ <- ggplot(dataset, aes(x = x_, stratum = stratum_, alluvium = alluvium_, fill = stratum_, label = stratum_)) +
    scale_fill_brewer(labels = color_labels, type = "qual", palette = "Set2") +
    scale_x_discrete(expand = c(.1, .1), limits = x_level_order) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum",
              aes(label = percent(after_stat(prop), accuracy = .1)))+
    labs(title = plot_name, x = x_name, y = y_name, fill = stratum_name)

  
  if (!is.null(y_breaks)) { p_ <- p_ + scale_y_continuous(breaks = y_breaks) }
  
  if (!is.null(background_color)) {

    p_ <- p_ + theme(
      panel.background = element_rect(fill=background_color),
      plot.background = element_rect(fill=background_color, color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill=background_color),
      legend.box.background = element_rect(fill=background_color))  # legend.box.background = element_rect(fill=background_color, colour = 'transparent') 
  }

  if (!is.null(text_size)) { p_ <- p_ + theme(text = element_text(size = text_size)) }

      
  filename <- paste0(stringr::str_replace_all(plot_name, " ", "_") ".png")
    
  ggsave(
    filename = filename,
    plot = p_,
    device = "png",
    bg = background_color,
    path = "",
    width = 297,
    height = 210,
    units = "mm")
  
}













count_occurances_of_disaggregated_multiple_answer_questions <- function(
    df_, # entire dataset
    binary_recognition_regex, # to get only disaggregated columns for given variable
    presence_indicator, # usually 1 or T
    absence_indicator) # usually 0 or F
{
  colnames_used <- colnames(df_)[stringr::str_detect(colnames(df_), binary_recognition_regex)]
  
  df_only_disagg_cols <- subset(
    df_,
    select = colnames_used )
  
  df_only_disagg_cols_ <- purrr::map_dfc(
    .x = df_only_disagg_cols,
    .f = function(col_){
      
      col_[col_ %in% presence_indicator] <- T
      
      col_[col_ %in% absence_indicator] <- F
      col_
    })
  
  output <- tibble::enframe( purrr::map_int(df_only_disagg_cols_, sum, na.rm =T), name = "this_value", value = "occurs_this_many_times" )
  
  output <- output[order(output$occurs_this_many_times, decreasing = T),]
  
  return( list(colnames_used = colnames_used, output = output) ) 
}
