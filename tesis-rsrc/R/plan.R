# # The workflow plan data frame outlines what you are going to do.
plan <- drake_plan(
  datadir =
    read.table(file_in("config.txt"), header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
    filter(id == Sys.info()[c(1,4:6)] %>% paste(collapse = "|")) %>%
    select(path) %>% unlist  %>% file.path,
  ineidir = file.path(datadir, "inei"),
  endesdir = file.path(ineidir, "endes"),
  CSALUD01 =
    file.path(endesdir,"2017","Modulo414","CSALUD01.sav") %>%
    haven::read_sav(encoding = "latin1"),
  REC0111 =
    file.path(endesdir,"2017","Modulo66","REC0111.SAV") %>%
    haven::read_sav(encoding = "latin1"),
  RECH1 =
    file.path(endesdir,"2017","Modulo64","RECH1.SAV") %>%
    haven::read_sav(encoding = "latin1"),
  RECH0 =
    file.path(endesdir,"2017","Modulo64","RECH0.SAV") %>%
    haven::read_sav(encoding = "latin1"),
  RECH23 =
    file.path(endesdir,"2017","Modulo65","RECH23.SAV") %>%
    haven::read_sav(encoding = "latin1"),
  
    rio::import("2017/Modulo66/REC0111.SAV" %>% paste0(datadir,.)
  
)

# (data.table(haven::read_sav("2017/Modulo414/CSALUD01.sav" %>% paste0(datadir,.), encoding = "latin1")) %>%
#    merge(rio::import("2017/Modulo66/REC0111.SAV" %>% paste0(datadir,.), setclass = "data.table"),
#          by.x = c("HHID", "QSNUMERO"), by.y = c("hhid","V003"), all.x = T) %>%
    merge(rio::import("2017/Modulo64/RECH1.SAV" %>% paste0(datadir,.), setclass = "data.table"),
          by.x = c("HHID", "QSNUMERO"), by.y = c("HHID","HVIDX"), all.x = T) %>%
    merge(rio::import("2017/Modulo64/RECH0.SAV" %>% paste0(datadir,.), setclass = "data.table"), by = "HHID", all.x = TRUE) %>%
    merge(rio::import("2017/Modulo65/RECH23.SAV" %>% paste0(datadir,.), setclass = "data.table"), by = "HHID", all.x = TRUE))[,.(
      peso=PESO15_AMAS/10**6, estrato.region=putlabel(HV023), estrato.urbrur=HV022, psuid=HV021,
      region.natural=putlabel(SHREGION), ubigeo=paste0(sprintf("%02d",HV023),sprintf("%02d",SHPROVIN),sprintf("%02d",SHDISTRI)),
      residencia.infancia = putlabel(V103), tiempo.resactual = V104 %>% ifelse(. > 95, NA, .) %>% ifelse(. == 95, QS23, .),
      residencia.previa = putlabel(V105),
      sexo=putlabel(QSSEXO), edad=as.vector(QS23), mes.encuesta = putlabel(QSINTM),
      escolaridad=HV106 %>% ifelse(.==8,NA,.) %>% factor(.,labels=names(HV106 %>% attr("labels"))[-5]),
      wealth.cat = HV270, wealth.index = HV271,
      consume.frut = case_when(QS213U == 3 ~ 0, QS213U == 1 ~ QS213C, TRUE ~ NA_real_),
      cant.frut = case_when(QS214U == 1 & QS214C != 9.9 ~ QS214C, TRUE ~ NA_real_),
      consume.jugo = case_when(QS215U == 3 ~ 0, QS215U == 1 ~ QS215C, TRUE ~ NA_real_),
      cant.jugo = case_when(QS216U == 1 & QS216C != 9.9 ~ QS216C, TRUE ~ NA_real_),
      consume.enfr = case_when(QS217U == 3 ~ 0, QS217U == 1 ~ QS217C, TRUE ~ NA_real_),
      cant.enfr = case_when(QS216U == 1 & QS216C != 9.9 ~ QS216C, TRUE ~ NA_real_),
      consume.enve = case_when(QS219U == 3 ~ 0, QS219U == 1 ~ QS219C, TRUE ~ NA_real_),
      cant.enve = case_when(QS220U == 1 ~ QS220CV, QS220U == 2 ~ QS220CC, TRUE ~ NA_real_)
      )]

  
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