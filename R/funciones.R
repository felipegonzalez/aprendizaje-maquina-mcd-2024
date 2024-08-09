

ratio_estimate <- function(sample, sampling_frame) {
  # calculate ratio estimate for stratified random
  # sampling
  # sample: a data frame with columns stratum and y
  # y: the column name of numerator variable
  # x: column name of denominator variable
  # returns: a data frame with columns stratum and
  #   ratio_estimate weighted by stratification
  n_table <- sampling_frame %>%
    group_by(stratum) %>%
    summarise(N = n())
  sample %>%
    group_by(stratum) %>%
    summarise(ratio_estimate = sum(y) / sum(x), n = n()) %>%
    left_join(n_table, by = "stratum") %>%
    mutate(ratio_estimate = ratio_estimate * N / n) %>%
    summarise(ratio_estimate = sum(ratio_estimate))


  }

tomar_raiz <- function(x, a){
  # x: un número
  # a: un número
  # returns: la raíz a-ésima de x

  # checar que a es positiva
  if (a <= 0) {
    stop("a debe ser positiva")
  }
  # checar que x es positiva si a es par
  if (a %% 2 == 0 & x < 0) {
    stop("x debe ser positiva si a es par")
  }

  x^(1/a)
}
