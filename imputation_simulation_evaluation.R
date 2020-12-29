library(Metrics)

calculate.rmse <- function(original.data, imputed.data, missing.data.mask) {
  simulation.elements.original <- original.data[missing.data.mask]
  simulation.elements.imputed <- imputed.data[missing.data.mask]
  rmse(simulation.elements.original, simulation.elements.imputed)
}
