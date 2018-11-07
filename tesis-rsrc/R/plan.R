# VARIABLES PA AANHaaDIR:
#   - educacion
#   - consumo de cigarro, alcohol

# hacer mil simuaciones?

# Las simulaciones deberian tener casos "extremos" (?)
# > No existe nada "extremo" para coeficientes lineales per se, pero quiero evaluar interaccion
# > En el caso de la dispersion beta, seria bueno evaluar en casi binomial y en que momento se bifurca
# > En el caso de la media y dispersion latente normal, evaluar:
#   > ausencia
#   > solo un lado presente <- probablemente sea problematico
#   > los tres con mayoria central
#   > los tres con minoria central

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

seeds_plan <- drake_plan(
  seed0 = sample.int(.Machine$integer.max, 1),
  seed1 = sample.int(.Machine$integer.max, 1),
  seed2 = sample.int(.Machine$integer.max, 1),
  seed3 = sample.int(.Machine$integer.max, 1),
  seed4 = sample.int(.Machine$integer.max, 1)#,
  # seed5 = sample.int(.Machine$integer.max, 1),
  # seed6 = sample.int(.Machine$integer.max, 1),
  # seed7 = sample.int(.Machine$integer.max, 1),
  # seed8 = sample.int(.Machine$integer.max, 1),
  # seed9 = sample.int(.Machine$integer.max, 1)
)

simulation_betab_pars <- drake_plan(
  simu.betab.data_0.05 =
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
  simu.betab.data_0.1 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.1,
             seed = seed__),
  simu.betab.data_0.1_n500 =
    eibb.sim(N = 5*10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.1,
             seed = seed__),
  simu.betab.data_0.2 =
    eibb.sim(N = 10**2, n = 7,
             bx = c(0, 0.5, -0.5),
             rho = 0.2,
             seed = seed__)
)

simu_betab_plan <- evaluate_plan(
  plan = simulation_betab_pars,
  wildcard = "seed__",
  values = seeds_plan$target
)

simu_fits <- drake_plan(
  simu.binom.fit =
    sampling(binom.model,
             data = data__,
             chains = 1, iter = 2000),
  simu.betab.fit =
    sampling(betab.model,
             data = data__,
             chains = 1, iter = 2000)
)

simu_fits_plan <- evaluate_plan(
  plan = simu_fits,
  wildcard = "data__",
  values = simu_betab_plan$target
)

commands <- paste0("tab_looic_divergent(", simu_fits_plan$target, ")")
targets <- paste0("looic_", simu_fits_plan$target)
simu_looic <- data.frame(target = targets, command = commands)

loo_tab_plan <- gather_plan(
  plan = simu_looic,
  target = "looic_tab",
  gather = "rbind"
)

loo_reshape_plan <- drake_plan(
  looic_tab2 = cbind(looic_tab[1:(nrow(looic_tab)/2),],looic_tab[-(1:(nrow(looic_tab)/2)),])
)

endes.load_plan <- drake_plan(
    endesdir =
    read.table(file_in("config.txt"), header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    filter(id == Sys.info()[c(1,4:6)] %>% paste(collapse = "|")) %>%
    select(path) %>% unlist %>% file.path("inei","endes"),
  CSALUD01 =
    file.path(endesdir,"2017","Modulo414","CSALUD01.sav") %>%
    haven::read_sav(encoding = "latin1") %>%
    select(
      !!!c(id.household = "HHID",
           id.hh.person = "QSNUMERO",
           survey.weight = "PESO15_AMAS",
           sex = "QSSEXO",
           age = "QS23",
           month = "QSINTM",
           had.fruit  = "QS213U",
           days.fruit = "QS213C",
           had.juice  = "QS215U",
           days.juice = "QS215C",
           had.fsalad  = "QS217U",
           days.fsalad = "QS217C",
           had.vsalad  = "QS219U",
           days.vsalad = "QS219C")
      ),
  REC0111 =
    file.path(endesdir,"2017","Modulo66","REC0111.SAV") %>%
    haven::read_sav(encoding = "latin1") %>%
    select(
      !!!c(id.household = "hhid",
           id.hh.person = "V003",
           residence.childhood = "V103",
           residence.time      = "V104",
           residence.last      = "V105")
      ),
  RECH1 =
    file.path(endesdir,"2017","Modulo64","RECH1.SAV") %>%
    haven::read_sav(encoding = "latin1") %>%
    select(
      !!!c(id.household = "HHID",
           id.hh.person = "HVIDX",
           education = "HV106")
      ),
  RECH0 =
    file.path(endesdir,"2017","Modulo64","RECH0.SAV") %>%
    haven::read_sav(encoding = "latin1") %>%
    select(
      !!!c(id.household = "HHID",
           loc.region = "HV023",
           stratum.area = "HV022",
           psu = "HV021")
      ),
  RECH23 =
    file.path(endesdir,"2017","Modulo65","RECH23.SAV") %>%
    haven::read_sav(encoding = "latin1") %>%
    select(
      !!!c(id.household = "HHID",
           loc.natural = "SHREGION",
           loc.province = "SHPROVIN",
           loc.district = "SHDISTRI",
           wealth.quintile = "HV270",
           wealth.index = "HV271")
      ),
  endes.merged =
    CSALUD01 %>%
    left_join(REC0111, by = c("id.household", "id.hh.person")) %>%
    left_join(RECH1, by = c("id.household", "id.hh.person")) %>%
    left_join(RECH0, by = "id.household") %>%
    left_join(RECH23, by ="id.household") %>%
    mutate(survey.weight = replace(survey.weight, TRUE, survey.weight/10**6),
           days.fruit = replace(days.fruit, had.fruit == 3, 0),
           days.juice = replace(days.juice, had.juice == 3, 0),
           days.fsalad = replace(days.fsalad, had.fsalad == 3, 0),
           days.vsalad = replace(days.vsalad, had.vsalad == 3, 0)) %>%
    select(-c(had.fruit, had.juice, had.fsalad, had.vsalad)),
  endes.subset0 = endes.merged %>%
                     subset(!is.na(days.vsalad)|education==8),
  endes.subset = endes.subset0,
  endes.formula =
    days.vsalad ~
    as.factor(sex) +
    as.factor(education) +
    as.factor(loc.region) +
    I(scale(age)) + I(scale(age)**2) +
    I(scale(wealth.index)) + I(scale(wealth.index)**2),
  endes.frame = model.frame(endes.formula, data = endes.subset),
  endes.matrix = model.matrix(endes.formula, data = endes.subset) %>% as.matrix,
  endes.data =
    list(
      N = nrow(endes.frame),
      Kx = ncol(endes.matrix),
      Kz = ncol(endes.matrix),
      n = rep(7, nrow(endes.frame)),
      y = model.response(endes.frame),
      x = endes.matrix,
      z = endes.matrix
    )
)

endes.fit_plan <- drake_plan(
    endes.betab.fit =
    sampling(betab.model,
             data = endes.data,
             chains = 1, iter = 2000),
    endes.eibin.fit =
    sampling(eibin.model,
             data = endes.data,
             chains = 1, iter = 2000),
    endes.eibeb.fit =
    sampling(eibeb.model,
             data = endes.data,
             chains = 1, iter = 2000)
)

endes.loo_plan <- drake_plan(
  betab.loo = tab_looic_divergent(endes.betab.fit),
  eibin.loo = tab_looic_divergent(endes.eibin.fit),
  eibeb.loo = tab_looic_divergent(endes.eibeb.fit),
  endes.loo.tab = rbind(betab.loo, eibin.loo, eibeb.loo)
)

whole_plan <- bind_plans(
  compilestan_plan,
  seeds_plan,
  simu_betab_plan,
  simu_fits_plan,
  loo_tab_plan,
  loo_reshape_plan,
  endes.load_plan,
  endes.fit_plan,
  endes.loo_plan
)
