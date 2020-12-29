library(ggplot2)

actual.vs.predicted.single.variable <- function(original.data, imputed.data, missing.data.mask, column.name, imputation.technique.name) {
  original.data.selected.variable <- original.data[, column.name]
  imputed.data.selected.variable <- imputed.data[, column.name]
  missing.data.mask.selected.variable <- missing.data.mask[, column.name]
  original.not.involved.in.imputation <- original.data.selected.variable[!missing.data.mask.selected.variable]
  original.involved.in.imputation <- original.data.selected.variable[missing.data.mask.selected.variable]
  imputed <- imputed.data.selected.variable[missing.data.mask.selected.variable]
  
  # Create scatterplot including original.not.involved.in.imputation vs original.not.involved.in.imputation
  # as well as original.involved.in.imputation vs imputed
  
  # Dataframe for original.not.involved.in.imputation vs original.not.involved.in.imputation
  original.non.imputation.vs.original.non.imputation.dataframe <- data.frame(y = original.not.involved.in.imputation,
                                                                             x = original.not.involved.in.imputation,
                                                                             type = replicate(length(original.not.involved.in.imputation), 'not in imputation'))
  
  # Dataframe for original.involved.in.imputation vs imputed
  original.involved.in.imputation.vs.imputed.dataframe <- data.frame(y = original.involved.in.imputation,
                                                                     x = imputed,
                                                                     type = replicate(length(imputed), 'in imputation'))
  
  # Concatenate dataframes for plotting
  data.to.plot <- rbind(original.non.imputation.vs.original.non.imputation.dataframe, original.involved.in.imputation.vs.imputed.dataframe)
  
  # Plot
  ggplot(data.to.plot, aes(x=x, y=y, shape=type, color=type)) +
    geom_point() + ggtitle(imputation.technique.name) + xlab('predicted value') + ylab('actual value') +
    theme_grey(base_size = 36) + theme(legend.title = element_blank()) + guides(shape = guide_legend(override.aes = list(size = 8)))
}
