ls *.tsv | sed 's/.tsv//' > input

parallel "~/cluster-1.57/src/cluster -f {}.tsv -m c -g 3 -e 3 -u {}_cn_link__abs_uncent_pear_cor" :::: input
