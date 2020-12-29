library(limma)
library(dplyr)

analyse.de <- function(df) {
  
  log.intensity.matrix <- as.matrix(select(df, -Protein))
  rownames(log.intensity.matrix) <- pull(df, Protein)
  
  sample.info <- as.data.frame(strsplit2(colnames(log.intensity.matrix),"_"))
  colnames(sample.info) <- c('Condition', 'Sample')
  
  # Make Condition into a factor
  sample.info <- mutate(sample.info, Condition = factor(sample.info$Condition))
  
  design <- model.matrix(~0 + Condition, sample.info)
  colnames(design) <- gsub("Condition", "", colnames(design)) # Remove the word 'Condition' from the column names to tidy.
  
  contrasts <- c('KO.PMCAI.MI-KO.PMCAI.Sham',
                 'KO.PMCAI.MI-WT.MI',
                 'KO.PMCAI.MI-WT.Sham',
                 'KO.PMCAI.Sham-WT.MI',
                 'KO.PMCAI.Sham-WT.Sham',
                 'WT.MI-WT.Sham')
  cont.matrix <- makeContrasts(contrasts=contrasts,
                               levels=levels(sample.info$Condition))
  fit <- lmFit(log.intensity.matrix,design)
  fit <- contrasts.fit(fit, cont.matrix)
  fit <- eBayes(fit)
  list('contrasts' = contrasts, 'fit' = fit)
}
