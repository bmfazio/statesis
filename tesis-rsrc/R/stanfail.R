compilestan_plan <- drake_plan(
  binom.model =
      stan_model(file_in("Stan/bin-regression-model.stan"),
                 model_name = "Binomial regression")
)

simulation_plan <- drake_plan(
simu.binom.data1 =
  eibb.sim(N = 10**1, n = 10,
           bx = c(0, 0.5, -0.5)),
simu.binom.data2 =
  eibb.sim(N = 10**2, n = 10,
           bx = c(0, 0.5, -0.5)),
simu.binom.data3 =
  eibb.sim(N = 10**3, n = 10,
           bx = c(0, 0.5, -0.5)),
simu.binom.data4 =
  eibb.sim(N = 10**4, n = 10,
           bx = c(0, 0.5, -0.5)),
simu.binom.data5 =
  eibb.sim(N = 10**5, n = 10,
           bx = c(0, 0.5, -0.5))
)

fit_attempts <- drake_plan(
  simu.binom.fit =
    sampling(binom.model,
             data = data__,
             chains = 2,
             iter = 1000)
)

simfull_plan <- evaluate_plan(
  plan = fit_attempts,
  wildcard = "data__",
  values = simulation_plan$target
)

whole_plan <- bind_plans(
  compilestan_plan,
  simulation_plan,
  simfull_plan
)