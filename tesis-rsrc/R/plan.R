# VARIABLES PA AANHaaDIR:
#   - educacion
#   - consumo de cigarro, alcohol

# The workflow plan data frame outlines what you are going to do.
# seed_plan <- drake_plan(
#   seed1 = 2**0,
#   seed2 = 2**1,
#   seed3 = 2**2,
#   seed4 = 2**3,
#   seed5 = 2**4
# )

# Las simulaciones deberian tener casos "extremos"
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
  eibeb.model =
      stan_model(file_in("Stan/eibb-regression-model.stan"),
                 model_name = "EIBB regression")
)

simulation_plan <- drake_plan(
simu.binom.data =
  eibb.sim(N = 10**3, n = 10,
           bx = c(0, 0.5, -0.5)),
simu.betab.data =
  eibb.sim(N = 10**3, n = 10,
           bx = c(0, 0.5, -0.5),
           rho = 0.2),
simu.eibeb.data =
  eibb.sim(N = 10**3, n = 10,
           bx = c(0, 0.5, -0.5),
           rho = 0.2, s = 0.5)
)

fit_attempts <- drake_plan(
  simu.binom.fit =
    sampling(binom.model,
             data = data__,
             chains = 2,
             iter = 1000),
  simu.betab.fit =
    sampling(betab.model,
             data = data__,
             chains = 2,
             iter = 1000),
  simu.eibeb.fit =
    sampling(eibeb.model,
             data = data__,
             chains = 2,
             iter = 1000)
)

simfull_plan <- evaluate_plan(
  plan = fit_attempts,
  wildcard = "data__",
  values = simulation_plan$target
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
  endes.subset0 = (endes.merged %>% subset(!is.na(days.vsalad))),
  endes.subset = endes.subset0,#(function(){set.seed(1);endes.subset0[sample(nrow(endes.subset0), 5000),]})(),
  endes.formula =
    days.vsalad ~
    as.factor(sex) +
    as.factor(stratum.area) +
    I(scale(age)) + I(scale(age)**2) +
    I(scale(wealth.index)),
  endes.frame = model.frame(endes.formula, data = endes.subset),
  endes.matrix = model.matrix(endes.formula, data = endes.subset) %>% as.matrix,
  endes.data = # seria ideal armar una funcion que me construya esto
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
    # endes.binom.fit =
    # sampling(binom.model,
    #          data = endes.data,
    #          chains = 2,
    #          iter = 1000),
    # endes.betab.fit =
    # sampling(betab.model,
    #          data = endes.data,
    #          chains = 2,
    #          iter = 1000),
    endes.eibeb.fit =
    sampling(eibeb.model,
             data = endes.data,
             chains = 1,
             iter = 2000)
)

whole_plan <- bind_plans(
  compilestan_plan,
  simulation_plan,
  simfull_plan,
  endes.load_plan,
  endes.fit_plan
)