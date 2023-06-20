generate_alluvial <- function(
    dataset,
    x_, # vector annotating values to a given x axis tick, e.g. round
    stratum_, # vector annotating values to a given color, e.g. accommodation
    alluvium_, # vector annotating values to a given wave/line, e.g. LONGIT_ID
    x_level_order = NULL, 
    y_breaks = NULL,
    theme_ = NULL,
    #background_color = NULL, # Can be set to "transparent" or "white" e.g.
    color_labels = NULL,
    x_name = NULL,
    y_name = NULL,
    stratum_name = NULL, # this is the color legend name
    plot_name = "alluvial_plot",
    subtitle_name = NULL,
    output_file_name = "alluvial_plot"
) ### Change it so it takes theme instead of all the theme elements
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
  if (is.null(subtitle_name)) { subtitle_name <- "" }  
  
  p_ <- ggplot(dataset, aes(x = x_, stratum = stratum_, alluvium = alluvium_, fill = stratum_, label = stratum_)) +
    scale_fill_brewer(labels = color_labels, type = "qual", palette = "Set2") +
    scale_x_discrete(expand = c(.1, .1), limits = x_level_order) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum",
              aes(label = percent(after_stat(prop), accuracy = .1)))+
    labs(title = plot_name, x = x_name, y = y_name, fill = stratum_name, subtitle = subtitle_name)

  
  if (!is.null(y_breaks)) { p_ <- p_ + scale_y_continuous(breaks = y_breaks) }

  if (!is.null(theme_)) { p_ <- p_ + theme_ }

    
  ggsave(
    filename = paste0(output_file_name, ".png"),
    plot = p_,
    device = "png",
    bg = background_color,
    width = 297,
    height = 210,
    units = "mm")
  
}













count_occurances_of_disaggregated_multiple_answer_questions <- function(
    df_, # entire dataset
    binary_recognition_regex, # to get only disaggregated columns for given variable
    presence_indicator, # of the original data, usually 1 or T. will be force-changed to T
    absence_indicator) # of the original data, usually 0 or F. will be force-changed to T
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








convert_columns_to_given_types_using_vector_dicts <- function(
    df_to_convert,
    col_names, # vector of column names
    col_types # vector of type names 'character', 'numeric', 'Date' or 'factor', corresponding to given column names. date actually doesnt work very well with our current format
)
{
  
  dict_ <- as.list(col_types)
  names(dict_) <- col_names
  
  cols_in_df_absent_in_col_names <- subset(df_to_convert, select = !(colnames(df_to_convert) %in% col_names) )

  
  if (ncol(cols_in_df_absent_in_col_names) != 0) { warning("BEWARE: some columns names in the data frame do not have their analouges in the col_names vector") }

  
  
  converted_df_ <- purrr::imap_dfc(
    .x = dict_,
    .f = function(type_, column_){
      
      type_ <- tolower(type_)
      
      assertthat::assert_that(type_ %in% c("character", "numeric", "date", "factor"), msg = "only recognized values are 'character', 'numeric' or 'date', 'factor'")
      ### !!! not really, but ok for now
      
      if (type_ == "date") { type_ <- "Date" }
      
      convert_function <- match.fun(paste0("as.", type_)) 
      
      if ( !(column_ %in% colnames(df_to_convert)) ) { 
        df_to_convert[[column_]] <- NA
        warning("BEWARE: column '", column_, "' provided in col_names is missing in the dataset")
      }
      
      convert_function(df_to_convert[[column_]])
    })
  
  converted_df_ <- subset(converted_df_, select = colnames(converted_df_) %in% colnames(df_to_convert))
  
  converted_df_ <- cbind(converted_df_, cols_in_df_absent_in_col_names)
  
  converted_df_[,colnames(df_to_convert)]

}



