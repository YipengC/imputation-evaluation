library(stringr)
library(dplyr)

df <- read.table(file = 'Manchester_mice_proteins.txt', header = TRUE, sep = '\t')
original.df <- df
cols <- colnames(df)

# Save 'Protein' column
protein.column <- df['Protein']

# Extract unique identifier
cols <- lapply(cols, function(column.name) {
  column.name <- str_extract(column.name, '_\\d{3,5}') # Extract biological replicate identifier
  column.name <- substring(column.name, 2) # Remove underscore prefix
  column.name
})

# Extract unique column names (excluding 'Protein')
cols <- unique(unlist(cols)[-length(cols)])

cols.before.technical.replicates.average <- cols

# Average replicates
df <- lapply(cols, function(sample.name) {
  samples <- select(df, contains(sample.name))
  # Return average intensity across technical replicates
  rowMeans(samples)
})

# Convert IDs to conditions
# Associative array for mapping replicate identifiers to conditions
id.to.condition <- list('979' = 'KO.PMCAI.Sham', '985' = 'KO.PMCAI.Sham', '1128' = 'KO.PMCAI.Sham',
                        '1017' = 'KO.PMCAI.MI', '65104' = 'KO.PMCAI.MI', '60349' = 'KO.PMCAI.MI', '61389' = 'KO.PMCAI.MI',
                        '1133' = 'WT.Sham', '1020' = 'WT.Sham', '984' = 'WT.Sham', '60348' = 'WT.Sham',
                        '1131' = 'WT.MI', '65103' = 'WT.MI', '60347' = 'WT.MI', '60346' = 'WT.MI')

cols <- lapply(cols, function(identifier) {
  id.to.condition[[identifier]]
})

# Make columns unique by adding a suffix to duplicates (technical replicates)
cols <- make.unique(unlist(cols), sep = '_')

# Add '_0' suffix to column names with no suffixes from make.unique
cols <- lapply(cols, function(column.name) {
  if (!grepl('_', column.name)) {
    paste(column.name, '0', sep = '_')
  } else {
    column.name
  }
})

# We want suffixes to start from 1 so increment each by 1
cols <- lapply(cols, function(column.name) {
  paste(str_sub(column.name, 1, -2), strtoi(str_sub(column.name, -1)) + 1, sep = '')
})

df <- as.data.frame(df, col.names = cols)

# Restore last column (Protein)
df <- cbind(df, protein.column)
cols <- colnames(df)

df
cols

# Sort columns keeping Protein column at the end
cols <- c(sort(cols[-length(cols)]), 'Protein')
df <- df[cols]

write.table(df, file = 'manchester_mice_proteins_preprocessed.txt', sep = '\t')