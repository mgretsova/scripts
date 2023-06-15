
# input phenotype json + pruned bim
library('stringr')
library('dplyr')
library('data.table')
library('jsonlite')
args = commandArgs(trailingOnly=TRUE)

process_regenie_results <- function(st){
  address <- st[1] %>% unlist()
  name <- st[2] %>% unlist()
  print(address)
  print(name)
  #chr6: 29691116–33054976
  m2 <- fread(address, select = c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1', 'BETA', 'SE', 'PVAL')) 
  m2 <- m2[SE <= 0.2 & PVAL <= 0.001 & (CHROM != 6 | GENPOS > 33054976 | GENPOS < 29691116 ), c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1', 'BETA')]
  setkey(m2, 'ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1')
  print(head(m))
  colnames(m2)[colnames(m2) == 'BETA'] <- name 
  if (length(m) > 0){ 
    m <<- merge(m, m2, by = c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1'), all = T) #%>% head()
  }
  else {
    m <<- m2
  }
}


if (length(args) > 0) {
  sample_remove = vector()
  json <- read_json(args[1], simplifyVector = TRUE) %>% as.data.frame()
  str(json)
  print(json$assoc_files)
  print(json$phenocode)
  print(head(json))
  as.list(json)
  m <- data.frame()
  apply(json, MARGIN = 1, FUN = process_regenie_results)
  head(m)
  m[is.na(m)] <- 0
  write.csv(m, 'result_.csv')
  mm <- m[, -c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1')]
  mm <- scale(mm)
  mm <- cbind(m[, c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1')], mm)
  write.csv(mm, 'result_mm.csv')
}

if (length(args) > 1) {
  pruned_bim = args[2]
  prune <- fread(pruned_bim, header = F, select = c('V2'))
  colnames(prune) <- c('ID')
  m_pruned <- merge(prune, m, by = c('ID'), all.y = F)
  m_pruned[is.na(m_pruned)] <- 0
  write.csv(m_pruned, 'result_pruned.csv')
  mm <- m_pruned[, -c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1')]
  mm <- scale(mm)
  mm <- cbind(m_pruned[, c('ID', 'CHROM', 'GENPOS', 'ALLELE0', 'ALLELE1')], mm)
  write.csv(mm, 'result_mm_pruned.csv')
}