# # The workflow plan data frame outlines what you are going to do.
plan <- drake_plan(
  endesdir =
    read.table(file_in("config.txt"), header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    filter(id == Sys.info()[c(1,4:6)] %>% paste(collapse = "|")) %>%
    select(path) %>% unlist %>% file.path("inei","endes"),
  # ineidir = file.path(datadir, "inei"),
  # endesdir = file.path(ineidir, "endes"),
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
      )
  ,
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
  simu.data =
    eibb.sim(N = 10**3, n = 50,
             bx = c(0.1, 0.7, -0.5), bz = c(0.5,0.67,-1), rho = 0.1, s = 0.7, sx = 0.3, sz = 0.2, seed = 1, fullinfo = F)
)

  #
  # data = raw_data %>%
  #   mutate(Species = forcats::fct_inorder(Species)) %>%
  #   select(-X__1),
  # hist = create_plot(data),
  # fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  # report = rmarkdown::render(
  #   knitr_in("report.Rmd"),
  #   output_file = file_out("report.html"),
  #   quiet = TRUE
  # )