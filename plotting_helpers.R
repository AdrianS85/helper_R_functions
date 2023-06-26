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
    pallette_ = "Set2",
    x_name = NULL,
    y_name = NULL,
    stratum_name = NULL, # this is the color legend name
    plot_name = "alluvial_plot",
    subtitle_name = NULL,
    output_file_name = "alluvial_plot",
    autosave = T
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
    scale_fill_brewer(labels = color_labels, type = "qual", palette = pallette_) +
    scale_x_discrete(expand = c(.1, .1), limits = x_level_order) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum",
              aes(label = percent(after_stat(prop), accuracy = .1)))+
    labs(title = plot_name, x = x_name, y = y_name, fill = stratum_name, subtitle = subtitle_name)
  
  
  if (!is.null(y_breaks)) { p_ <- p_ + scale_y_continuous(breaks = y_breaks) }
  
  if (!is.null(theme_)) { p_ <- p_ + theme_ }
  
  if (autosave) {
    ggsave(
      filename = paste0(output_file_name, ".png"),
      plot = p_,
      device = "png",
      bg = background_color,
      width = 297,
      height = 210,
      units = "mm")
  }

  return(p_)
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
    col_types, # vector of type names 'character', 'numeric', 'Date' or 'factor', corresponding to given column names. date actually doesnt work very well with our current format
    order_numericlike_factors_ = T,
    order_numericlike_factors_decreasing_ = F
)
{
  
  assertthat::assert_that("data.frame" %in% class(df_to_convert), msg = "ERROR: df_to_convert is not data frame")
  assertthat::assert_that("character" %in% class(col_names), msg = "ERROR: col_names is not of class character")
  assertthat::assert_that("character" %in% class(col_types), msg = "ERROR: col_types is not of class character")
  assertthat::assert_that(length(col_names) == length(col_types), msg = "ERROR: col_names and col_types have different lenghts")
  
  colnames(df_to_convert)
  
  cols_in_df_absent_in_col_names <- subset(df_to_convert, select = !(colnames(df_to_convert) %in% col_names) )
  
  if (nrow(cols_in_df_absent_in_col_names) != 0) { 
    
    warning(paste0("BEWARE: some columns names in the data frame do not have their analouges in the col_names vector:\n"), paste(colnames(cols_in_df_absent_in_col_names), collapse = ", "))
  }
  
  
  
  dict_ <- as.list(col_types)
  names(dict_) <- col_names
  
  
  
  converted_df_ <- purrr::imap_dfc(
    .x = dict_,
    .f = function(type_, column_){
      
      type_ <- tolower(type_)
      
      assertthat::assert_that(type_ %in% c("character", "numeric", "date", "factor", "logical"), msg = "only recognized values are 'character', 'numeric' or 'date', 'factor'")
      ### !!! not really, but ok for now
      
      if (type_ == "date") { type_ <- "Date" }
      
      convert_function <- match.fun(paste0("as.", type_)) 
      
      if ( !(column_ %in% colnames(df_to_convert)) ) { 
        df_to_convert[[column_]] <- NA
        warning("BEWARE: column '", column_, "' provided in col_names is missing in the dataset")
      }
      
      return_ <- convert_function(df_to_convert[[column_]])
      
      shall_i_order_numericlike_factors_ <- ( order_numericlike_factors_ &&  type_ == "factor" && all(!is.na(as.numeric(levels(return_)))) )
      
      
      if (shall_i_order_numericlike_factors_) {
        
        return_ <- order_numericlike_factors(
          factor_vec = return_,
          decreasing_ = order_numericlike_factors_decreasing_)
        
      }
      
      return(return_)
      
    })
  
  converted_df_ <- subset(converted_df_, select = colnames(converted_df_) %in% colnames(df_to_convert))
  
  converted_df_ <- cbind(converted_df_, cols_in_df_absent_in_col_names)
  
  converted_df_[,colnames(df_to_convert)]
  
}












### !!! Here we can input functions which will take a specific group of df columns and do something for each subsequent pair of them. So sliding window basically
generate_new_columns_with_differences_between_subsequent_columns_in_df <- function(
    working_wide_df,
    ordered_levels_colnames, # ORDERED names of columns in which subsequent values are present.
    yesno_to_yesno = F # just one of the functions to be used. Perhaps replace this system with function taken as an argument?
    )
{
  
  seq_of_levels_to_calculate_values_over <- seq(2, length(ordered_levels_colnames))
  
  
  
  for (iter_ in seq_of_levels_to_calculate_values_over ) {
    
    previous_colname <- ordered_levels_colnames[iter_-1]
    
    current_colname <- ordered_levels_colnames[iter_]
    
    transition_name <- paste0(previous_colname, "_to_", current_colname)
    
    
    if (yesno_to_yesno) {
      
      working_wide_df[[transition_name]] <- dplyr::case_when(
        working_wide_df[[previous_colname]] %in% T & working_wide_df[[current_colname]] %in% T ~ "yes_to_yes",
        working_wide_df[[previous_colname]] %in% T & working_wide_df[[current_colname]] %in% F ~ "yes_to_no",
        working_wide_df[[previous_colname]] %in% F & working_wide_df[[current_colname]] %in% T ~ "no_to_yes",
        working_wide_df[[previous_colname]] %in% F & working_wide_df[[current_colname]] %in% F ~ "no_to_no")
    }
  }
  
  return(working_wide_df)
}










get_xml_to_preety_dict_from_tool <- function(
    tool_adress,
    xml_variable_name,
    variable_name_colname = "list_name",
    tool_sheet_with_dict = "choices",
    xml_colname = "name",
    preety_colname = "label::English"
    )
{
  tool_ <- readxl::read_excel(
    path = tool_adress,
    sheet = tool_sheet_with_dict,
    col_types = "text")
  
  tool_vars <- subset(
    x = tool_,
    subset = tool_[[variable_name_colname]] %in% xml_variable_name,
    select = c(xml_colname, preety_colname))
  
  ### !!! here xml_colnames need to be treated in the same way as variables during cleaning process to be able to be matched.
  
  tool_vars
  
  
  
}











order_numericlike_factors <- function(
    factor_vec,
    decreasing_ = F)
{
  assertthat::assert_that(class(factor_vec) == "factor", msg = "factor_vec is not a factor")
  assertthat::assert_that(all(!is.na(as.numeric(levels(factor_vec)))), msg = "factor_vec levels dont translate to numerics")

  fac_levels_as_num <- as.numeric(levels(factor_vec))
  
  fac_levels_as_num_sorted <- sort(x = fac_levels_as_num, decreasing = decreasing_)

  forcats::fct_relevel(.f = factor_vec, as.character(fac_levels_as_num_sorted))
}













add_binaries_from_any_column_to_dataset <- function(
    dataset,
    column_name,
    sep_for_binaries,
    sep_between_values_present_within_one_cell = NULL # if NULL, that means that column has only 1 possible value in any cell
)
{
  temp <- subset(dataset, select = column_name)

  temp[[1]] <- as.character(temp[[1]])
  
  ### !!! beware of escape symbols such as \t - its not tested against it
  all_levels <- purrr::map(
    .x = temp[[1]],
    .f = function(x){
      
      if (!is.null(sep_between_values_present_within_one_cell)) {
        
        if (!is.na(x)) { stringr::str_split_1(string = x, pattern = sep_between_values_present_within_one_cell) } else NA 
        
      } else x
      
    })
  
  all_levels_vars <- na.omit( unique( rlist::list.ungroup(all_levels) ) )
  
  all_levels_names <- stringr::str_c(column_name, sep_for_binaries, all_levels_vars)
  
  
  for (lev_nb in seq(1,length(all_levels_vars))) {
    
    temp[[ all_levels_names[lev_nb] ]] <- ifelse(
      test = stringr::str_detect(string = temp[[1]], pattern = all_levels_vars[lev_nb]),
      yes = T,
      no = F)
  }
  
  cbind(dataset, temp[,-1])
}




















get_top_freq_categories_from_vector_of_categories <- function(
    vector_,
    return_top_n,
    decreasing_ = T,
    remove_na = T,
    categories_to_remove = NULL # this needs to be character vector
    )
{
  tabyl_ <- janitor::tabyl(vector_)
  
  return_ <- list(raw_table = tabyl_)
  
  if (remove_na) { tabyl_ <- subset(tabyl_, !is.na(tabyl_[[1]]) ) }
  
  if ( !is.null(categories_to_remove) ) { tabyl_ <-  subset(tabyl_, !(tmp_general_counts[[1]] %in% categories_to_remove) ) }
  
  tabyl_ <- tabyl_[order(tabyl_$n, decreasing = decreasing_),]
  
  if (return_top_n > nrow(tabyl_)) { return_top_n <- nrow(tabyl_) }
  
  top_values <- tabyl_[[1]][1:return_top_n]

  return_$top_values <- top_values
  
  return(return_)
}



















get_complete_case_individuals_that_are_also_present_in_all_rounds <- function(
    ds_,
    nb_of_rounds,
    unique_id_col_
  )
{
  
  ds_ <- ds_[complete.cases(ds_),]
  
  count_nb_of_times_given_longit_occurs <- janitor::tabyl( ds_[[unique_id_col_]] )
  
  resp_present_all_rounds <- ifelse(
    test = count_nb_of_times_given_longit_occurs$n == nb_of_rounds,
    yes = count_nb_of_times_given_longit_occurs[[1]],
    no = NA)
  
  resp_present_all_rounds <- na.omit(resp_present_all_rounds)
  
  resp_present_all_rounds_logical <- ds_[[unique_id_col_]] %in% resp_present_all_rounds
  
  ds_ <- subset(ds_, subset = resp_present_all_rounds_logical)
  
  removed_ds <- subset(ds_, subset = !resp_present_all_rounds_logical)
  
  return( list(updated_ds = ds_, removed_ds = removed_ds) )
}
