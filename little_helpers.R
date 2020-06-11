###### FUNCTION FOR GETTING ENRICHMENTS ######
### tibble_list - tibble list with tibbles for all 5 comparisons, 
### Currently I work with tibbles with 3 columns, with the first one being gene names and the third one being comparison_id. We should be able to use different ones
function_enrich_list <- function(tibble_list = enrich_overview_list, gene_names_column = 1){
  ### Create list of interesting databases
  dbs_Onto_Path <- c("GO_Molecular_Function_2018", "GO_Cellular_Component_2018", "GO_Biological_Process_2018", "MGI_Mammalian_Phenotype_2017", "Human_Phenotype_Ontology", "KEGG_2016", "WikiPathways_2016", "Panther_2016", "Reactome_2016", "BioCarta_2016", "NCI-Nature_2016", "ARCHS4_Kinases_Coexp", "HumanCyc_2016", "BioPlex_2017", "SILAC_Phosphoproteomics")
  dbs_Regul <- c("Genome_Browser_PWMs", "TRANSFAC_and_JASPAR_PWMs", "Transcription_Factor_PPIs", "ChEA_2016",  "TF-LOF_Expression_from_GEO", "PPI_Hub_Proteins", "ENCODE_TF_ChIP-seq_2015", "ENCODE_Histone_Modifications_2015", "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "CORUM", "Pfam_InterPro_Domains", "Phosphatase_Substrates_from_DEPOD", "TF_Perturbations_Followed_by_Expression", "ARCHS4_TFs_Coexp", "miRTarBase_2017", "TargetScan_microRNA_2017", "Enrichr_Submissions_TF-Gene_Coocurrence", "Epigenomics_Roadmap_HM_ChIP-seq")
  dbs_Drug_Tissue_Other <- c("ARCHS4_IDG_Coexp", "DrugMatrix", "RNA-Seq_Disease_Gene_and_Drug_Signatures_from_GEO", "OMIM_Disease", "Jensen_DISEASES", "DSigDB",  "Jensen_COMPARTMENTS", "ARCHS4_Tissues", "Tissue_Protein_Expression_from_Human_Proteome_Map", "Tissue_Protein_Expression_from_ProteomicsDB", "Mouse_Gene_Atlas", "ESCAPE", "Chromosome_Location", "MSigDB_Computational")
  dbs <- c(dbs_Onto_Path, dbs_Regul, dbs_Drug_Tissue_Other)
  
  ##  dbs_Removed <- c("Jensen_TISSUES", "dbGaP", "Genes_Associated_with_NIH_Grants", "GeneSigDB")
  ##  Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  : 
  ##  line 1 did not have 2 elements OR line 109 did not have 9 elements
  ##  https://www.biostars.org/p/349240/
  
  ### Initialize list, so I can work on it
  enriched <- list()

  for(n in seq(from = 1, to = length(comparisons_ids))){
    enriched[[n]] <- enrichR::enrichr(tibble_list[[n]][[gene_names_column]], dbs) %>%
      map(filter, P.value <= 0.05) %>%
      map(as.tibble)
  }
  
  ### Initialize list, so I can work on it
  enriched_names <- list()
  ### Initialize list counter for the loop
  merged_number_2 <- 1
  
  ### Add information on databaseses and comparisons used
  for(n in seq(from = 1, to = length(comparisons_ids))){
    for(d in seq(from = 1, to = length(dbs))){
      enriched_names[[merged_number_2]] <- enriched[[n]][[d]]%>%
        mutate(Database = dbs[d], Comparison = comparisons_ids[n])
      merged_number_2 <- merged_number_2 + 1  
    }
  }
  
  merged_enriched_names <- rlist::list.rbind(enriched_names)

  ### Create list of tibbles based on comparison names
  final_list_2 <- list()
  for (c in seq(from = 1, to = length(comparisons_ids))){
    final_list_2[[c]] <- merged_enriched_names %>%
      filter(Comparison == comparisons_ids[c])
  }
  
  return(final_list_2)
}



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
  assertthat::assert_that(!any(duplicated(replace_this_chrvec)), !any(duplicated(with_this_chrvec)), msg = 'replace_this_chrvec or with_this_chrvec arguments include duplicated values. This cannot be, because we use one specific value to be changed into another specific value')
  
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
  
  return(result_chrvec$with_this_chrvec)
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
