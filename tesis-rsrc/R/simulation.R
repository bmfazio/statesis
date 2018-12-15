seeds_plan <- data.frame(
  target =
    paste0("seed", sprintf("%03d", 1:100)),
  command = 
    as.character(1:100),
  stringsAsFactors = FALSE
  )

# Binomial vs Beta-Binomial simulations
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
             seed = seed__),
  # Evaluar sesgo en coeficientes
  simu.betab.data_0.5_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.5,
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

simu_betab.fits_plan <- evaluate_plan(
  plan = simu_betab_fits,
  wildcard = "data__",
  values = simu_betab_plan$target
)

# Binomial vs Endpoint-Inflated Binomial simulations
simu_eibin_pars <- drake_plan(
  simu.eibin.data_p0.05_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.3039784,
             seed = seed__),
  simu.eibin.data_p0.05_n250 =
    eibb.sim(N = 2.5*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.3039784,
             seed = seed__),
  simu.eibin.data_p0.05_n400 =
    eibb.sim(N = 4*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.3039784,
             seed = seed__),
  simu.eibin.data_p0.1_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.3901521,
             seed = seed__),
  simu.eibin.data_p0.1_n200 =
    eibb.sim(N = 2*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.3901521,
             seed = seed__),
  simu.eibin.data_p0.125_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.4346506,
             seed = seed__),
  simu.eibin.data_p0.125_n150 =
    eibb.sim(N = 1.5*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.4346506,
             seed = seed__),
  simu.eibin.data_p0.15_n100 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             s = 0.4824237,
             seed = seed__)
)

simu_eibin_plan <- evaluate_plan(
  plan = simu_eibin_pars,
  wildcard = "seed__",
  values = seeds_plan$target
)

simu_eibin_fits <- drake_plan(
  sampling(binom.model,
           data = data__,
           chains = 1, iter = 2000),
  sampling(eibin.model,
           data = data__,
           chains = 1, iter = 2000,
            control =
             list(adapt_delta = 0.9,
                  max_treedepth = 15
             ))
)

simu_eibin.fits_plan <- evaluate_plan(
  plan = simu_eibin_fits,
  wildcard = "data__",
  values = simu_eibin_plan$target
)

simu_binRE_fits <- drake_plan(
  binREfit =
    sampling(binRE.model, data = data__, chains = 1, iter = 2000,
             control = list(adapt_delta = 0.9, max_treedepth = 15))
)

simu_binRE.fits_plan <- evaluate_plan(
  plan = simu_binRE_fits,
  wildcard = "data__",
  values = simu_betab_plan$target
)

simu_eibiP_fits <- drake_plan(
  eibiPfit =
    sampling(eibiP.model, data = data__, chains = 1, iter = 2000,
             control = list(adapt_delta = 0.9, max_treedepth = 15))
)

simu_eibiP.fits_plan <- evaluate_plan(
  plan = simu_eibiP_fits,
  wildcard = "data__",
  values = simu_eibin_plan$target[c(201:300,401:500,601:700,701:800)]
)


# All sims
simu_plan <- bind_plans(
  simu_betab_plan,
  simu_betab.fits_plan,
  simu_eibin_plan,
  simu_eibin.fits_plan,
  simu_eibiP.fits_plan,
  simu_binRE.fits_plan
)

betab_dnumbs <- tibble(
  target = paste0(simu_betab.fits_plan$target,"_divergences"),
  command = paste("get_num_divergent(",simu_betab.fits_plan$target,")")
)

betab_divergences <- gather_plan(
  betab_dnumbs,
  target = "betab_ndiv",
  gather = "diverg.tb"
)

eibin_dnumbs <- tibble(
  target = paste0(simu_eibin.fits_plan$target,"_divergences"),
  command = paste("get_num_divergent(",simu_eibin.fits_plan$target,")")
)

eibin_divergences <- gather_plan(
  eibin_dnumbs,
  target = "eibin_ndiv",
  gather = "diverg.tb"
)

eibiP_dnumbs <- tibble(
  target = paste0(simu_eibiP.fits_plan$target,"_divergences"),
  command = paste("get_num_divergent(",simu_eibiP.fits_plan$target,")")
)

eibiP_divergences <- gather_plan(
  eibiP_dnumbs,
  target = "eibiP_ndiv",
  gather = "diverg.tb2"
)

divergent_plan <- bind_plans(
  betab_dnumbs,
  betab_divergences,
  eibin_dnumbs,
  eibin_divergences,
  eibiP_dnumbs,
  eibiP_divergences
)

# Listo con la tabla de divergencias
# loadd(eibin_ndiv)
# loadd(eibiP_ndiv)
# num_divergences %>% group_by(model, data, pars, n) %>% summarise(divergent = max(divergent))

# hazte unas divergencias de los simufits tmb

# times_plan <- gather_plan( ### VER CODIGO DE ABAJO PARA EXTRAER TIEMPOS:
#   divergence_numbs,
#   target = "num_divergences",
#   gather = "diverg.tb"
# )
# 
# # busk forma de contar divergentes para saber a cuales incrementar n
# # get_num_divergent(drake_target_2_simu.eibin.data_p0.025_n100_seed001)
# loadd(num_divergences)
# num_divergences %>%
#   filter(model == 2) %>%
#   group_by(pars, n) %>%
#   summarize(divs = sum(divergent))
# 
# loadd(simu_eibin.fits_plan$target[1])
# drake_target_1_simu.eibin.data_p0.025_n100_seed001
# drake_target_1_simu.eibin.data_p0.025_n100_seed001 %>% attr("sim") %>% attributes
# (drake_target_1_simu.eibin.data_p0.025_n100_seed001 %>% attr("sim"))$samples[[1]] %>% attr("elapsed_time")
# 
# loadd(simu_eibin.fits_plan$target[1400])
# drake_target_2_simu.eibin.data_p0.15_n100_seed100
# (drake_target_2_simu.eibin.data_p0.15_n100_seed100 %>% attr("sim"))$samples[[1]] %>% attr("elapsed_time")
# 
# #
# loadd(simu_eibin.fits_plan$target[701])
# ((drake_target_2_simu.eibin.data_p0.025_n100_seed001 %>% attr("sim"))$samples[[1]] %>% attr("elapsed_time"))
