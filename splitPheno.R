# split phenofile to quant, bin and covar 
library('dplyr')
library('stringr')
args <- commandArgs(trailingOnly = T)
if (length(args) >= 1) {
  input_pheno_file = args[1]
  input <- read.table(input_pheno_file, na.strings = "NA", header = T)
  output_bin_file =  paste0(sub(".pheno", "",  input_pheno_file), "_bin.pheno")
  output_quant_file =  paste0(sub(".pheno", "",  input_pheno_file), "_quant.pheno")
  output_covar_file =  paste0(sub(".pheno", "",  input_pheno_file), "_covar.pheno")
  t <- lapply(input[, c(-1,-2)], FUN = function(x){ ifelse(all(x %in% c(NA, 1, 2)) | all(x %in% c(NA, 0, 1)), "bin", ifelse(is.numeric(x), "quant", "out"))}) %>% as.vector()
  
  quant <- names(t[t == "quant"])
  
  quant_covar <- quant[str_detect(quant, 'Birth')]
  quant <- quant[!str_detect(quant, 'Birth')]
  
  bin <- names(t[t == "bin"])
  #covar <- bin[str_detect(bin, 'smok') | str_detect(bin, 'sex')]
  covar <- bin[str_detect(bin, 'sex')]
  bin <- bin[!str_detect(bin, 'smok') & !str_detect(bin, 'sex')]
  
  write.table(input[, c('FID', 'IID', bin)], file = output_bin_file, quote = F, row.names = F)
  write.table(input[, c('FID', 'IID', quant)], file = output_quant_file, quote = F, row.names = F)
  write.table(input[,  c('FID', 'IID', covar)], file = output_covar_file, quote = F, row.names = F)
  print(output_quant_file)
}