compilestan_plan <- drake_plan(
  binom.model =
      stan_model(file_in("Stan/bin-regression-model.stan"),
                 model_name = "Binomial regression"),
  betab.model =
      stan_model(file_in("Stan/bb-regression-model.stan"),
                 model_name = "BB regression"),
  eibin.model =
      stan_model(file_in("Stan/eibi-regression-model.stan"),
                 model_name = "EIBi regression"),
  eibeb.model =
      stan_model(file_in("Stan/eibb-regression-model.stan"),
                 model_name = "EIBB regression")
)
