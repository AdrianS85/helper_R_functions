# INPUT: raw_online_txt - needs to be coercable to data.frame
get_random_genes <- function(sample_size_int, species_str = '', df_with_full_list_of_gene_names = 'https://www.genenames.org/cgi-bin/download/custom?col=gd_app_sym&col=gd_pub_eg_id&status=Approved&status=Entry%20Withdrawn&hgnc_dbtag=on&order_by=gd_app_sym_sort&format=text&submit=submit', delim_ = '\t', col_types_ = 'cc')
{
  species_str = tolower(species_str)
  species_list <-
    list(human = c('homo', 'homo sapiens', 'human', 'humans'))
  
  if (species_str %in% species_list$human)
  {
    genes <-
      readr::read_delim(df_with_full_list_of_gene_names,
                        delim = delim_,
                        col_types = col_types_)
  }
  else
  {
    stop(
      'Species name either not recognized or species not supported.\nSupported species are: ',
      paste(names(species_list), collapse = ' ')
    )
  }
  
  gene_sample_rowNumbers <-
    sample(x = seq_along(genes[[1]]), size = sample_size_int)
  
  gene_sample <- genes[gene_sample_rowNumbers, ]
  
  return(gene_sample)
}
