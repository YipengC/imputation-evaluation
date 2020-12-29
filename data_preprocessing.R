# For datasets where 0s are to be treated as missing values
zeros.to.NAs <- function(df, data.column.names) {
  df[data.column.names][df[data.column.names] == 0] <- NA
  return(df)
}

# Convert all NAs to 0
NAs.to.zeros <- function(df, data.column.names) {
  df[data.column.names][is.na(df[data.column.names])] <- 0
  return(df)
}

# Returns a dataframe of the same shape as the subset corresponding to data.column.names.
# Each element is TRUE if the corresponding element in the input is NA, else FALSE
get.missing.data.mask <- function(df, data.column.names) {
  is.na(df[data.column.names])
}

# Perform normalization per column. Take log and subtract the median.
subtract.median <- function(df) {
  column.medians <- apply(df, 2, median, na.rm = TRUE)
  df <- sweep(df, 2, column.medians)
  list(df = df, column.medians = column.medians)
}

# Replace Majority.protein.ID with extracted gene product ID in original data frames
extract.protein.ID <- function(id.string) {
  if (startsWith(id.string, 'CON__')) {
    split.id <- strsplit(id.string, '__')[[1]]
    substr(split.id[[2]], 1, 6)
  } else {
    split.id <- strsplit(id.string, '\\|')[[1]]
    if (length(split.id) > 1) {
      if (split.id[[1]] == 'tr' | split.id[[1]] == 'REV__tr' | split.id[[1]] == 'sp' | split.id[[1]] == 'REV__sp') {
        split.id[[2]]
      } else if (startsWith(split.id[[1]], 'CON__')) {
        split.id.1.by.double.underscore <- strsplit(split.id[[1]], '__')[[1]]
        substr(split.id.1.by.double.underscore[[2]], 1, 6)
      } else {
        print("extract.protein.ID failed after testing split.id[[1]] == 'tr'")
        id.string
      }
    } else {
      print("extract.protein.ID failed after testing length(split.id) > 1")
      id.string
    }
  }
}

extract.gene.name.from.fasta.headers.entry <- function(x) {
  substring(str_extract(x, 'GN=[A-Za-z0-9]*'), 4)
}