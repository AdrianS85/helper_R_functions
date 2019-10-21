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
  
  if (file_name == '' && write_file = T) {
    write.table(x = df_output, file = paste0(deparse(substitute(df_)), '_splited_by_', split_by_str, '_column.tsv'), row.names = F, sep = '\t', quote = F)
  }
  else if (file_name != '' && write_file = T) {
    write.table(x = df_output, file = file_name, row.names = F, sep = '\t', quote = F)
  }  
  
  return(df_output)
}
