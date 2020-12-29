
impute.combined <- function(pre.imputation.df, mar.imputed.df, lod.imputed.df, condition.factors) {
  threshold <- min(pre.imputation.df, na.rm = TRUE) + 0.5
  
  # Compute selection dataframe consisting of 'mar', 'lod', or 'original' corresponding to if MAR imputed, LOD imputed or original values is to be used for each element.
  selection.df <- matrix(nrow = nrow(pre.imputation.df), ncol = ncol(pre.imputation.df))
  for (i in 1:nrow(selection.df)) {
    condition.means <- lapply(levels(condition.factors), function(condition.name) {
      mean(unlist(pre.imputation.df[i,][condition.factors == condition.name]), na.rm = TRUE)
    })
    names(condition.means) <- levels(condition.factors)
    for (j in 1:ncol(selection.df)) {
      if (is.na(pre.imputation.df[i,j])) {
        # If element is missing, use condition mean to decide whether to use mcar or lod
        condition.mean <- condition.means[[condition.factors[[j]]]]
        if (is.nan(condition.mean)) {
          # Use lod by default if no other values exist in the row for the same condition.
          selection.df[i,j] <- 'lod'
        } else {
          if (condition.mean > threshold) {
            selection.df[i,j] <- 'mar'
          } else {
            selection.df[i,j] <- 'lod'
          }
        }
      } else {
        # Data is not missing so simply use original value
        selection.df[i,j] <- 'original'
      }
    }
  }
  # Make copy of mar.imputed.df and replace values with lod.imputed.df value if lod is to be used
  imputed.df <- mar.imputed.df
  for (i in 1:nrow(imputed.df)) {
    for (j in 1:ncol(imputed.df)) {
      if (selection.df[i, j] == 'lod') {
        imputed.df[i, j] <- lod.imputed.df[i, j]
      }
    }
  }
  list(imputed.df = imputed.df, selection.df = selection.df)
}