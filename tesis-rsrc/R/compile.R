compilestan_plan <- drake_plan(
  binom.model =
      stan_model(file_in("Stan/bin-regression-model.stan"),
                 model_name = "Binomial regression"),
  betab.model =
      stan_model(file_in("Stan/bb-regression-model.stan"),
                 model_name = "BB regression"),
  binRE.model =
      stan_model(file_in("Stan/binRE-regression-model.stan"),
                 model_name = "Binomial regression with RE"),
  binRE_zero.model =
      stan_model(file_in("Stan/binRE_zero-regression-model.stan"),
                 model_name = "Binomial regression with zero-mean RE"),
  eibin.model =
      stan_model(file_in("Stan/eibi-regression-model.stan"),
                 model_name = "EIBi regression"),
  eibeb.model =
      stan_model(file_in("Stan/eibb-regression-model.stan"),
                 model_name = "EIBB regression"),
  eibiP.model =
      stan_model(file_in("Stan/eibi-regression-model-P.stan"),
                 model_name = "EIBi regression - P parametrization")
)