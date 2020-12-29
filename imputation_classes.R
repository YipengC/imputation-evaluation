library(impute)
library(missForest)
library(mice)
library(imputeLCMD)

source('combined_imputation.R')

options(stringsAsFactors = F)

# Reference class for imputation techniques
imputation.technique <- setRefClass("imputation.technique",
                                    fields = list(name = "character", apply = "function"))

# Zero Imputation
zero.imputation.technique <- imputation.technique(name = 'zero', apply = function(df) {
  NAs.to.zeros(df, colnames(df))
})

# Deterministic Minimal Value
min.det.imputation.technique <- imputation.technique(name = 'deterministic minimal value', apply = function(df) {
  impute.MinDet(dataSet.mvs = df)
})

# From http://www-stat.stanford.edu/~hastie/Papers/missing.pdf
knn.imputation.technique <- imputation.technique(name = "knn", apply = function(df) {
  # Apply knn imputation and return only the imputed dataset
  as.data.frame(impute.knn(data = as.matrix(df), k = 20, rowmax = 1, colmax = 1, maxp = nrow(df))$data)
})

# Daniel J. Stekhoven, Peter B?hlmann, MissForest-non-parametric missing value imputation for mixed-type data,
# Bioinformatics, Volume 28, Issue 1, 1 January 2012, Pages 112-118, https://doi.org/10.1093/bioinformatics/btr597
random.forest.imputation.technique <- imputation.technique(name = 'random forest', apply = function(df) {
  # Apply missForest imputation and return only the imputed dataset
  missForest(df)$ximp
})

# QRILC
qrilc.imputation.technique <- imputation.technique(name = "qrilc", apply = function(df) {
  qrilc.result <- impute.QRILC(df, tune.sigma = 0.2)
  qrilc.result[[1]]
})

# Combined imputation
combined.imputation.technique <- imputation.technique(name = 'combined', apply = function(df, mar.imputed.df, lod.imputed.df, condition.factors) {
  impute.combined(df, mar.imputed.df, lod.imputed.df, condition.factors)
})

# Zero imputation with median shift
zero.with.median.shift.imputation.technique <- imputation.technique(name = 'zero with median shift', apply = function(df, median.shift.values = NULL) {
  if (is.null(median.shift.values)) {
    NAs.to.zeros(df, colnames(df))
  } else if (length(median.shift.values) != ncol(df)) {
    print('length(median.shift.values) must be equal to ncol(df)')
  } else {
    for (i in 1:length(median.shift.values)) {
      df[,i][is.na(df[,i])] <- median.shift.values[[i]] * (-1)
    }
    df
  }
})