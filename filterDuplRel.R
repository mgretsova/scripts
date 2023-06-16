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
## as input expect pheno file FID, IID + phenotypes and similarity file
## if sex and yearOfBirth are included, we can find duplicated
## phenotypes
phenotypes_priority = c('PSC' = 1, 'UC' = 2, 'AD' = 3, 'PSOR' = 4, 'SARC' = 5, 'yearOfBirth' = 6, 'ageAtIBD' = 7, 'ageAtPSOR' = 8, 'ageAtSARC' = 9)
p = max(phenotypes_priority) + 1
phenotypes_value = 2**(p - phenotypes_priority)
code_phenotype <- function(x){
  if (all(x %in% c(0, 1, NA))){
    x[is.na(x)] <- 0
    x
  }
  else
    if (all(x %in% c(2, 1, NA))){
      x[is.na(x) | x == 1] <- 0
      x[x==2] <- 1
      x
    }
  else{
    as.numeric(!is.na(x))
  }
}

priority <- function(m){
  names_m <- names(m)
  names_p <- names(phenotypes_priority)
  names_i <- intersect(names_m, names_p)
  priority_val <- phenotypes_value[names_i]
  priority_i <- m[names_i]
  sum(priority_val * priority_i)
}

duplicates_merge <- function(phenotypes){
  names_m <- names(phenotypes)
  if (setequal(intersect(c('sex', 'yearOfBirth'), names_m), c('sex', 'yearOfBirth'))){
    columns_to_merge = setdiff(names_m, c('sex', 'yearOfBirth', 'FID', 'IID' ))
    columns_to_merge_x = paste(columns_to_merge, 'x', sep = '.')
    columns_to_merge_y = paste(columns_to_merge, 'y', sep = '.')
    names(columns_to_merge_x) <- columns_to_merge
    names(columns_to_merge_y) <- columns_to_merge
    duplicates <- phenotypes %>% left_join(similarity, by = c('IID' = 'IID1')) %>% left_join(phenotypes, by = c('IID2' = 'IID')) %>% 
      filter(PI_HAT > 0.8, sex.x == sex.y, yearOfBirth.x == yearOfBirth.y) # %>% select(c(columns_to_merge_x, columns_to_merge_y))
    new_values <- lapply(columns_to_merge, function(x){ z = duplicates[c(columns_to_merge_x[x], columns_to_merge_y[x])]; apply(z, MARGIN = 1, function(y){max(na.omit(y))})}) %>% as.data.frame()
    colnames(new_values) <- columns_to_merge
    new_values[new_values==-Inf] <- NA
    rownames(phenotypes) <- phenotypes$IID 
    phenotypes[duplicates$IID, columns_to_merge] <- new_values
    phenotypes
  }
  else{
    print('No sex and yearOfBirth fields')
    phenotypes
  }
}

args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
  input_pheno_file = args[1]
  similarity_file = args[2]
  
  # input files
  # input_pheno_file = "~/IdeaProjects/scripts/IBD_PSC_full.pheno"
  # similarity_file = "~/IdeaProjects/scripts/Regeneron_SampleQCI_pruned_IBD_PSC_gsa.genome"
  
  # output files
  output_pheno_file =  paste0(sub(".pheno", "",  input_pheno_file), "_genome_filtered.pheno")
  output_remove_file =  paste0(sub(".pheno", "",  input_pheno_file), "_genome_filtered.remove")
  output_cases_file =  paste0(sub(".pheno", "",  input_pheno_file), "_genome_filtered_cases.txt")
  output_controls_file =  paste0(sub(".pheno", "",  input_pheno_file), "_genome_filtered_controls.txt")
  
  phenotypes = read.table(input_pheno_file, na.strings = "NA", header = T)
  similarity = read.table(similarity_file, na.strings = "NA", header = T) %>% select(IID1, IID2, PI_HAT)
  
  # find duplicates
  phenotypes <- duplicates_merge(phenotypes)
  # calculate priority value
  res <- lapply(phenotypes %>% select(-FID, -IID), FUN = code_phenotype) %>% as.data.frame()
  priority_val <- apply(res, MARGIN = 1, priority)
  res <- cbind(phenotypes %>% select(FID, IID), res, priority_val = priority_val)
  # choose duplicates/relatives to remove 
  remove <- res %>% left_join(similarity, by = c('IID' = 'IID1')) %>% left_join(res, by = c('IID2' = 'IID')) %>% 
     mutate(removeId = ifelse(is.na(priority_val.y), NA, ifelse(priority_val.x < priority_val.y, IID, IID2))) %>% filter(!is.na(removeId))
  removeIds <- remove$removeId
  write.table(remove, file = output_remove_file, quote = F, row.names = F)
  # remove duplicates/relatives/miss/het
  phenotypes <- phenotypes[!(phenotypes$IID %in% removeIds),]
  write.table(phenotypes %>% select(-sex), file = output_pheno_file, quote = F, row.names = F)
  
  # cases / controls
  cases <- phenotypes %>% filter(IBD == 2 | PSC == 2)
  cases_table <- phenotypes %>% filter(IID %in% cases$IID) %>% select(FID, IID)
  controls_table <- phenotypes %>% filter(!(IID %in% cases$IID)) %>% select(FID, IID)
  
  write.table(cases_table, output_cases_file, quote = F, row.names = F, col.names = F)
  write.table(controls_table, output_controls_file, quote = F, row.names = F, col.names = F)
}

