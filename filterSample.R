library('openxlsx')
library('readxl')
library('dplyr')
library('tibble')
library('tidyr')
library('arsenal')
library('naniar')
library('ggplot2')
library('gridExtra')
library('grid')
library('stringr')
#input:  .pheno or .fam file + file[s] with sample list as first column 
args = commandArgs(trailingOnly=TRUE)
if (length(args) > 1) {
  sample_take = vector()
  input_pheno_file = args[1]
 
  lapply(args[-1], function(file) {take = read.table(file); sample_take <<- c(take$V1, sample_take)})
  print(sample_take)
  
  if (str_detect(input_pheno_file, ".pheno", negate = FALSE)){
    output_pheno_file =  paste0(sub(".pheno", "",  input_pheno_file), "_filtered.pheno")
    phenotypes = read.table(input_pheno_file, na.strings = "NA", header = T)
    phenotypes <- phenotypes[(phenotypes$IID %in% sample_take),]
    write.table(phenotypes, file = output_pheno_file, quote = F, row.names = F)
  }
  else if (str_detect(input_pheno_file, ".fam", negate = FALSE)){
    output_pheno_file =  paste0(sub(".fam", "",  input_pheno_file), "_filtered.fam")
    fam = read.table(input_pheno_file, na.strings = "NA", header = F)
    phenotypes <- phenotypes[(fam$V2 %in% sample_take),]
    write.table(phenotypes, file = output_pheno_file, quote = F, row.names = F)
  }
  
}

