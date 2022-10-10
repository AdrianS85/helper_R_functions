# rm(list = ls(pattern = '(.*)(temp)|(test)(.*)'))
# options(scipen = 999)
# cluster xxx.tsv -g 4 -m a -u xxx

split_and_measure_length <- function(df_, split_by_str, write_file = F, file_name = '')
{
  df_list <- split(df_, f = df_[[split_by_str]])
  
  names_vector <- names(df_list)
  lenght_vector <- as.character(lapply(X = df_list, FUN = function(x) { length(x[[1]]) } ))
  
  df_output <- data.frame(split_by_str = names_vector, 'number_of_values' = lenght_vector)
  
  if (file_name == '' && write_file == T) {
    write.table(x = df_output, file = paste0(deparse(substitute(df_)), '_splited_by_', split_by_str, '_column.tsv'), row.names = F, sep = '\t', quote = F)
  }
  else if (file_name != '' && write_file == T) {
    write.table(x = df_output, file = file_name, row.names = F, sep = '\t', quote = F)
  }  
  
  return(df_output)
}



# Compares lenght of original df to child dfs, that were split from original df by filtering. INPUT: str_what_was_splited - name for a file in which the result will be written to. OUTPUT: bool_ - This is true when lendght of og_df == summed lenghts of child dfs, written file
check_was_the_spliting_of_df_by_filtering_ok <- function(str_what_was_splited = '', df_original, list_df_splited)
{
  lenght_of_original_df <- length(df_original[[1]])
  lenght_of_child_df <- sum( as.integer(lapply(X = list_df_splited, FUN = function(x) { length(x[[1]]) } )) )

  was_spliting_good <- lenght_of_original_df == lenght_of_child_df

  df_to_write_was_spliting_good <- as.data.frame(was_spliting_good)
  colnames(df_to_write_was_spliting_good) <- str_what_was_splited

  return(was_spliting_good)
}



# For extracting part of a string, for which we know beggining or end. Function first captures this known beggining or end of string, and then trims it from the other side using known value for trimming. In other words, function cuts the string from larger string using two border regexes. INPUT: Depending on wether string You want starts with known value or ends with known value, first use regex_one_, than regex_two_ for cutting string from the other side
extract_from_string <- function(chr_vec, regex_one = '', regex_two = '')
{
  chr_vec2 <- stringr::str_extract(string = chr_vec, pattern = regex_one)
  chr_vec3 <- stringr::str_remove(string = chr_vec2, pattern = regex_two)
  
  return(chr_vec3)
}



# INPUT: changes values in to_recode_chrvec based on key values in replace_this_chrvec into with_this_chrvec values; OUTPUT: chrvec where to_recode_chrvec are replaced with approprate value from replace_this_chrvec / with_this_chrvec key/value pairs
recode_values_based_on_key <- function(to_recode_chrvec, replace_this_chrvec, with_this_chrvec)
{
  assertthat::assert_that(length(replace_this_chrvec) == length(with_this_chrvec), msg = 'replace_this_chrvec and with_this_chrvec arguments need to be the same lenght. This is because every element in first vector will be recoded as corresponding element in the second vector')
  assertthat::assert_that(!any(duplicated(replace_this_chrvec)), msg = 'replace_this_chrvec argument includes duplicated values. This cannot be, because we use one specific value to be changed into another specific value')
  
  replace_this_chrvec <- as.character(replace_this_chrvec)
  with_this_chrvec <- as.character(with_this_chrvec)
  to_recode_chrvec <- as.character(to_recode_chrvec)
  
  key_value_pairs <-
    data.frame(replace_this_chrvec, with_this_chrvec)
  
  to_recode_chrvec <- data.frame(to_recode_chrvec)
  
  to_recode_chrvec$order <- as.integer(rownames(to_recode_chrvec))

  result_chrvec <-
    merge(
      x = to_recode_chrvec,
      y = key_value_pairs,
      by.x = 'to_recode_chrvec',
      by.y = 'replace_this_chrvec',
      all.x = T
    )
  
  result_chrvec <- result_chrvec[order(result_chrvec$order),]
  
  return(as.character(result_chrvec$with_this_chrvec)) ### !!! ADDED LATER - as.character()
}



get_all_symbols_in_chrvec <- function(chrvec)
{
  chrvec <- as.character(chrvec)
  symbols <-
    lapply(
      X = chrvec,
      FUN = function(x) {
        paste(strsplit(x, '')[[1]])
      }
    )
  
  symbols <- unlist(symbols, use.names=FALSE)
  
  return(sort(unique(symbols)))
}



get_all_symbols_in_df_per_column <- function(df_)
{
  
  all_symbols_ <- purrr::map(
    .x = df_, 
    .f = function(x){
      get_all_symbols_in_chrvec(x)
    })
  
  return(all_symbols_)
}



#This checks if in each position all values in each vector are the same by checking if unique'ing them gives char vector of lenght 1. IGNORES NA OBJECTS
are_vectors_the_same <- function(chr_vec_list)
{
  temp <- rlist::list.rbind(chr_vec_list)
  temp <- data.frame(temp, stringsAsFactors=FALSE)
  temp <- as.list(temp)
  
  temp2 <- sapply(
    X = temp,
    FUN = function(x) {
      u_x <- unique(x)
      length(u_x)
    }
  )
  
  if(min(temp2) == 1 & max(temp2) == 1){ return(T)} else {return(F)}
}



read_excel_all_sheets <- function(directory, file_name, colNames_)
{
  directory_and_file <- paste0(directory, '/', file_name)
  
  sheet_names <- openxlsx::getSheetNames(directory_and_file)
  
  list <- purrr::map(.x = seq_along(sheet_names), .f = function(x) { openxlsx::read.xlsx(xlsxFile = directory_and_file, sheet = x, colNames = colNames_) })
  
  names(list) <- sheet_names
  
  return(list)
}



remove_corrupting_symbols_from_chrvec <- function(chr_vec, repeated_spaces, trailing_spaces, character_NAs, change_to_lower, to_ascii, empty_strings)
{
  if(class(chr_vec) == 'character'){
    if (repeated_spaces == T) {
      chr_vec <- stringr::str_replace_all(string = chr_vec, pattern = ' {2,}', replacement = ' ')}
    
    if (trailing_spaces == T) {
      chr_vec <- stringr::str_trim(string = chr_vec, side = 'both')}
    
    if (character_NAs == T) {
      chr_vec[chr_vec == 'NA'] <- NA}
    
    if (change_to_lower == T) {
      chr_vec <- tolower(chr_vec)}
    
    if (to_ascii == T) {
      chr_vec <- stringi::stri_enc_toascii(chr_vec)}
    
    if (empty_strings == T) {
      chr_vec[chr_vec == ''] <- NA}
    
    return(chr_vec)
  } else { 
    warning('Supplied vector is not of class character!')
    return(chr_vec) 
  }
}


#Add - check if null, check is lenght ==0
verify_df <- function(df_, only_qa = F, sort_by_col = NA, repeated_spaces_ = T, trailing_spaces_ = T, character_NAs_ = T, empty_strings_ = T, change_to_lower_ = T, to_ascii_ = T, possible_na_values = c("na", "nan", "none", "no", "empty", "missing", "bad"), clean_colnames = T, rm_empty = T)
{
  if (only_qa == F) {
    df_ <- purrr::map_dfc(.x = df_, .f = function(x) {remove_corrupting_symbols_from_chrvec(chr_vec = x, repeated_spaces = repeated_spaces_, trailing_spaces = trailing_spaces_, character_NAs = character_NAs_, empty_strings = empty_strings_, change_to_lower = change_to_lower_, to_ascii = to_ascii_)})
  }
  
  if (!is.na(sort_by_col)) {
    df_ <- df_[order(df_[[sort_by_col]]),]
  }
  
  if (clean_colnames == T) {
    df_ <- janitor::clean_names(df_) 
  }
  
  if (rm_empty == T) {
    df_ <- janitor::remove_empty(df_, which = c("rows", "cols"), quiet = TRUE)
  }
  
  
  qa <- list(
    'col_types' = purrr::map(.x = df_, .f = class),
    
    'symbols' = get_all_symbols_in_df_per_column(df_),
    
    'unique' = purrr::map(.x = df_, .f = function(x){ sort(unique(x)) }),
    
    'duplicates' = purrr::map(.x = df_, .f = function(x){
      x_log <- duplicated(x)
      unique(x[x_log])
    }),
    
    'na_perc' = purrr::map(.x = df_, .f = function(x) { (length(subset(x, (is.na(x))))/length(x))*100 }),
    
    'possible_nas' = purrr::map(.x = df_, .f = function(x) {
      if (class(x) == 'character') {
        res_ <- purrr::map(
          .x = possible_na_values,
          .f = function(possible_na_value) {
            any(ifelse(x[!is.na(x)] == possible_na_value, T, F))
          })
        names(res_) <- possible_na_values
        res_
      } else return('Vector of class different than character')
    }),
    
    "max_string_lenght" = purrr::map(.x = df_, .f = function(x){ max(stringr::str_length(as.character(x)), na.rm = T) }),
    
    "not_only_lower_symbols" = purrr::map(.x = df_, .f = function(x){ 
      any(ifelse(x == tolower(x), F, T), na.rm = T)
    }),
    
    'min-max' = purrr::map(.x = df_, .f = function(x) {
      if (class(x) == 'numeric' || class(x) == 'integer' || class(x) == 'double') {
        temp <- subset(x, !is.na(x))
        return(paste0('From ', min(temp), ' to ', max(temp)))
      } else return('Vector of class different than numeric, integer or double')
    })
  )
  
  
  
  if (only_qa == F) {
    return(list('df' = df_, 'qa' = qa))
  } else return(qa)
}



# OUTPUT: [[1]] - actual prepared vector, [[2]] - dataframe for validation
cleanup_differing_units <- function(charvec_, unit_identifier_regexList, unit_multiplier_list)
{
  temp <- data.frame(charvec_)
  
  temp$order <- c(1:length(charvec_))
  
  temp$number <- stringr::str_extract(string = temp$charvec_, pattern = '[0-9]{1,}')
  
  temp$number <- sapply(X = temp$number, FUN = function(x) {paste0(x, collapse = '') } )
  
  for (unit_nb in seq_along(unit_identifier_regexList)) {
    temp2 <- subset(x = temp , subset = stringr::str_detect(string = charvec_, pattern = unit_identifier_regexList[[unit_nb]]))
    
    temp2$temp <- as.numeric(temp2$number) * unit_multiplier_list[[unit_nb]]
    
    if (unit_nb == 1) {
      ret_urn <- temp2
    } else if (unit_nb > 1) {
      ret_urn <- rbind(ret_urn, temp2)
    }
  }
  
  ret_urn <- merge(temp, ret_urn, by = 'order', all.x = T)
  
  if(length(charvec_) != length(ret_urn[[1]])){
    stop('Output differs from input. Probably You have an entry which matches to two or more of the unit_identifier_regexList')
  }
  
  ret_urn <- dplyr::select(.data = ret_urn, order, charvec_.x, temp)
  
  ret_urn <- ret_urn[order(ret_urn$order),]
  
  return(list(ret_urn$temp, ret_urn))
}



validate_col_types <- function(df_, col_names_list, col_types_list)
{
  purrr::walk2(
    .x = col_names_list, 
    .y = col_types_list, 
    .f = function(x,y){
      if (class(df_[[x]]) != y) {
        stop(paste0('column ', x, ' from provided df is not of required type ', y))
      }
    })
  
  print('GOOD! all columns are of proper type')
  
  return(T)
}





paste_strings_consisting_of_series_of_values <- function(strings_to_add_list, divider_of_values_in_serie_str, uniqualize = F)
{
  combined_string <- strings_to_add_list[[1]]
  
  for (char_vec in strings_to_add_list[-1]) {
    combined_string <- paste(combined_string, char_vec, sep = divider_of_values_in_serie_str)
  }

  combined_string <- stringr::str_remove_all(string = combined_string, pattern = paste0('NA', divider_of_values_in_serie_str))
  combined_string <- stringr::str_remove(string = combined_string, pattern = paste0(divider_of_values_in_serie_str, 'NA$'))
  
  if (uniqualize == T) {
    combined_string <- remove_repeated_values_from_string_series_from_final_column(column_to_process = combined_string, sep_ = divider_of_values_in_serie_str)
  }
  
  combined_string[combined_string == 'NA'] <- NA
  combined_string[combined_string == ''] <- NA

  return(combined_string)
}





remove_repeated_values_from_string_series_from_final_column <- function(column_to_process, sep_)
{
  temp <- as.data.frame(stringr::str_split_fixed(string = column_to_process, pattern = sep_, n = Inf))
  
  temp[temp == ''] <- NA
  
  temp$xxx <- NA
  
  for (row_nb in seq(length(temp[[1]]))) {
    
    row <- unique(t(temp[row_nb,]))
    
    row <- subset(row, !is.na(row))
    
    temp$xxx[[row_nb]] <- paste0(row, collapse = sep_)
  }
  
  return(temp$xxx)
}




split_string_by_pattern_and_extract_vec_of_unique_values <- function(chr_vec, pattern_to_split_individual_strings_with, rm_repeated_spaces = T, rm_trailing_spaces = T, rm_character_NAs = T)
{
  split_ <- stringr::str_split_fixed(string = chr_vec, pattern = pattern_to_split_individual_strings_with, n = Inf)
  
  and_get_unique_values <- unique(purrr::map_chr(.x = split_, .f = function(x) {x}))
  
  clean_unique_values <- remove_corrupting_symbols_from_chrvec(chr_vec = and_get_unique_values, repeated_spaces = T, trailing_spaces = T, character_NAs = T, change_to_lower = F, to_ascii = F, empty_strings = T)
  
  return(clean_unique_values)
}





split_string_by_pattern_and_replace_values_according_to_key <- function(string_to_split, pattern_to_split_with, key_replace_this_chrvec, key_with_this_chrvec, return_unique_values)
{
  child_strings <- split_string_by_pattern_and_extract_vec_of_unique_values(string_to_split, pattern = pattern_to_split_with)
  
  recoded <- recode_values_based_on_key(to_recode_chrvec = child_strings, replace_this_chrvec = key_replace_this_chrvec, with_this_chrvec = key_with_this_chrvec)
  
  recoded <- recoded[!is.na(recoded)]
  
  if (length(recoded) == 0) {
    return(NA)
  } else if (return_unique_values == T) {
      recoded <- unique(recoded)
      recoded <- paste(recoded, collapse = pattern_to_split_with)
      return(recoded)
    } else if (return_unique_values == F) {
      recoded <- paste(recoded, collapse = pattern_to_split_with)
      return(recoded)
    }
}





compare_sample_rows_in_i_vs_o <- function(in_df, out_df, analysis_name, sample_size = 100, should_i_order_dfs_before_sampling = F, order_by_in_col = NA, order_by_out_col = NA, write_to_folder = NA, drop_i_cols = NA, drop_o_cols = NA, sep_for_write = '\t', dec_for_write = ',')
{
  if (length(in_df[[1]]) != length(out_df[[2]])) { 
    warning(paste0('input has different lenght than output. this function compares. ', analysis_name, ' was not performed'))
    return(list('in' = NA, 'out' = NA))}
  
  if (all(rownames(in_df) == seq_along(rownames(in_df))) & all(rownames(out_df) == seq_along(rownames(out_df)))) {
    in_df <- in_df[order(row.names(in_df)),]
    out_df <- out_df[order(row.names(out_df)),]
  } else {
    warning(paste0('rownames of input or output are not sequence of numbers ', analysis_name, ' was not performed'))
    return(list('in' = NA, 'out' = NA))}

  sample_ <- sample(x = seq_along(in_df[[1]]), size = sample_size)

  input <- subset(x = in_df, subset = rownames(in_df) %in% sample_)
  output <- subset(x = out_df, subset = rownames(out_df) %in% sample_)

  if (!is.na(drop_i_cols)) {
    input <- dplyr::select(.data = input, -drop_i_cols) }
  if (!is.na(drop_o_cols)) {
    output <- dplyr::select(.data = output, -drop_o_cols) }

  if (!is.na(write_to_folder)) {
    i_file_name <- paste0(write_to_folder, '/test_input_', analysis_name, '.tsv')
    o_file_name <- paste0(write_to_folder, '/test_output_', analysis_name, '.tsv')
  } else {
    i_file_name <- paste0('test_input_', analysis_name, '.tsv')
    o_file_name <- paste0('test_output_', analysis_name, '.tsv')
  }

  write.table(x = input, file = i_file_name, sep = sep_for_write, dec = dec_for_write, row.names = F)
  write.table(x = output, file = o_file_name, sep = sep_for_write, dec = dec_for_write, row.names = F)

  return(list('in' = input, 'out' = output))
}






group_and_then_summarize_all_cols_in_df <- function(df_, group_by_col, summarize_method = 'median')
{
  df_nested <- dplyr::nest_by(.data = df_, eval(parse(text = group_by_col)))
  
  md <- purrr::map_df(
    .x = df_nested$data, 
    .f = function(group_){
      
      purrr:: map_df(.x = group_, .f = function(column){
        
        if (is.numeric(column)) {
          eval(parse(text = summarize_method))(column)
        } else { paste(unique(column), collapse = '____')}
        
      })
  })
  return(md)
}













### From: https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
catchWarningsAndErrors <- function(expr) {
  val <- NULL
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  myError <- NULL
  eHandler <- function(e) {
    myError <<- e$message
    NULL
  }
  val <- tryCatch(withCallingHandlers(expr, warning = wHandler), error = eHandler)
  list(warnings = myWarnings, error=myError)
} 
