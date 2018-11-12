seeds_plan <- data.frame(
  target =
    paste0("seed", sprintf("%03d", 1:100)),
  command = 
    as.character(1:100),
  stringsAsFactors = FALSE
  )

simu_betab_pars <- drake_plan(
  simu.betab.data_0.05_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.05,
             seed = seed__),
  simu.betab.data_0.05_n500 =
    eibb.sim(N = 5*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.05,
             seed = seed__),
  simu.betab.data_0.05_n1k =
    eibb.sim(N = 10**3, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.05,
             seed = seed__),
  simu.betab.data_0.1_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.1,
             seed = seed__),
  simu.betab.data_0.1_n500 =
    eibb.sim(N = 5*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.1,
             seed = seed__),
  simu.betab.data_0.2_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.2,
             seed = seed__)
)

simu_betab_plan <- evaluate_plan(
  plan = simu_betab_pars,
  wildcard = "seed__",
  values = seeds_plan$target
)

simu_betab_fits <- drake_plan(
  sampling(binom.model,
           data = data__,
           chains = 1, iter = 2000),
  sampling(betab.model,
           data = data__,
           chains = 1, iter = 2000,
            control =
             list(adapt_delta = 0.9,
                  max_treedepth = 15
             ))
)

simu_fits_plan <- evaluate_plan(
  plan = simu_betab_fits,
  wildcard = "data__",
  values = simu_betab_plan$target
)

