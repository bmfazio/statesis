# commands <- paste0("tab_looic_divergent(", simu_fits_plan$target, ")")
# targets <- paste0("looic_", simu_fits_plan$target)
# simu_looic <- data.frame(target = targets, command = commands)
# 
# loo_tab_plan <- gather_plan(
#   plan = simu_looic,
#   target = "looic_tab",
#   gather = "rbind"
# )
#  
# # loo_reshape_plan <- drake_plan(
# #   looic_tab2 = cbind(looic_tab[1:(nrow(looic_tab)/2),],looic_tab[-(1:(nrow(looic_tab)/2)),])
# # )
# 
# 
# 
# endes.load_plan <- drake_plan(
#     endesdir =
#     read.table(file_in("config.txt"), header = TRUE, sep = ";", stringsAsFactors = FALSE) %>%
#     filter(id == Sys.info()[c(1,4:6)] %>% paste(collapse = "|")) %>%
#     select(path) %>% unlist %>% file.path("inei","endes"),
#   CSALUD01 =
#     file.path(endesdir,"2017","Modulo414","CSALUD01.sav") %>%
#     haven::read_sav(encoding = "latin1") %>%
#     select(
#       !!!c(id.household = "HHID",
#            id.hh.person = "QSNUMERO",
#            survey.weight = "PESO15_AMAS",
#            sex = "QSSEXO",
#            age = "QS23",
#            month = "QSINTM",
#            had.fruit  = "QS213U",
#            days.fruit = "QS213C",
#            had.juice  = "QS215U",
#            days.juice = "QS215C",
#            had.fsalad  = "QS217U",
#            days.fsalad = "QS217C",
#            had.vsalad  = "QS219U",
#            days.vsalad = "QS219C")
#       ),
#   REC0111 =
#     file.path(endesdir,"2017","Modulo66","REC0111.SAV") %>%
#     haven::read_sav(encoding = "latin1") %>%
#     select(
#       !!!c(id.household = "hhid",
#            id.hh.person = "V003",
#            residence.childhood = "V103",
#            residence.time      = "V104",
#            residence.last      = "V105")
#       ),
#   RECH1 =
#     file.path(endesdir,"2017","Modulo64","RECH1.SAV") %>%
#     haven::read_sav(encoding = "latin1") %>%
#     select(
#       !!!c(id.household = "HHID",
#            id.hh.person = "HVIDX",
#            education = "HV106")
#       ),
#   RECH0 =
#     file.path(endesdir,"2017","Modulo64","RECH0.SAV") %>%
#     haven::read_sav(encoding = "latin1") %>%
#     select(
#       !!!c(id.household = "HHID",
#            loc.region = "HV023",
#            stratum.area = "HV022",
#            psu = "HV021")
#       ),
#   RECH23 =
#     file.path(endesdir,"2017","Modulo65","RECH23.SAV") %>%
#     haven::read_sav(encoding = "latin1") %>%
#     select(
#       !!!c(id.household = "HHID",
#            loc.natural = "SHREGION",
#            loc.province = "SHPROVIN",
#            loc.district = "SHDISTRI",
#            wealth.quintile = "HV270",
#            wealth.index = "HV271")
#       ),
#   endes.merged =
#     CSALUD01 %>%
#     left_join(REC0111, by = c("id.household", "id.hh.person")) %>%
#     left_join(RECH1, by = c("id.household", "id.hh.person")) %>%
#     left_join(RECH0, by = "id.household") %>%
#     left_join(RECH23, by ="id.household") %>%
#     mutate(survey.weight = replace(survey.weight, TRUE, survey.weight/10**6),
#            days.fruit = replace(days.fruit, had.fruit == 3, 0),
#            days.juice = replace(days.juice, had.juice == 3, 0),
#            days.fsalad = replace(days.fsalad, had.fsalad == 3, 0),
#            days.vsalad = replace(days.vsalad, had.vsalad == 3, 0)) %>%
#     select(-c(had.fruit, had.juice, had.fsalad, had.vsalad)),
#   endes.subset0 = endes.merged %>%
#                      subset(!(is.na(days.vsalad)|education==8)),
#   endes.subset = endes.subset0,
#   endes.formula =
#     days.vsalad ~
#     as.factor(sex) +
#     as.factor(education) +
#     as.factor(loc.region) +
#     as.factor(wealth.quintile) +
#     as.factor(month) +
#     I(scale(age)) + I(scale(age)**2),
#   endes.frame = model.frame(endes.formula, data = endes.subset),
#   endes.matrix = model.matrix(endes.formula, data = endes.subset) %>% as.matrix,
#   endes.data =
#     list(
#       N = nrow(endes.frame),
#       Kx = ncol(endes.matrix),
#       Kz = ncol(endes.matrix),
#       n = rep(7, nrow(endes.frame)),
#       y = model.response(endes.frame),
#       x = endes.matrix,
#       z = endes.matrix
#     ),

#   bx_names = c(
#     "Intercept", "Sex: Female",
#     "Education: Primary", "Education: Secondary", "Education: Higher", # ref none
#     "Ancash", "Apurimac", "Arequipa", "Ayacucho", "Cajamarca", "Callao", "Cusco", # ref amazonas
#     "Huancavelica", "Huanuco", "Ica", "Junin", "La Libertad", "Lambayeque", "Lima", "Loreto",
#     "Madre de Dios", "Moquegua", "Pasco", "Piura", "Puno", "San Martin", "Tacna", "Tumbes", "Ucayali",
#     "Wealth 2", "Wealth 3", "Wealth 4", "Wealth 5", # ref Wealth 1 (mas pobre)
#     "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Noviembre", "Diciembre", # ref enero
#     "Age", "Age^2"
#     )
# )
# 
# endes.fit_plan <- drake_plan(
#     endes.betab.fit =
#     sampling(betab.model,
#              data = endes.data,
#              chains = 1, iter = 2000),
#     endes.eibin.fit =
#     sampling(eibin.model,
#              data = endes.data,
#              chains = 1, iter = 2000),
#     endes.eibeb.fit = ########### SACA ESTO, DEMORA MUCHO - Idea alternativa, pasalo por optimizing
#     sampling(eibeb.model,
#              data = endes.data,
#              chains = 1, iter = 2000)
# )
# 
# endes.loo_plan <- drake_plan(
#   betab.loo = tab_looic_divergent(endes.betab.fit),
#   eibin.loo = tab_looic_divergent(endes.eibin.fit),
#   endes.loo.tab = rbind(betab.loo, eibin.loo)
# )
# 
# plots_plan <- drake_plan(
#   Q219 =
#     (function(x){
#       png(filename=x, width = 600, height = 400)
#       endes.data$y %>%
#         table %>%
#         prop.table %>%
#         barplot(xlab = "Days", ylab = "Proportion",
#                 main = "Ate vegetable salad on how many days out of last seven? (n = 33 206)")
#       dev.off()
#       })(file_out("output/images/Q219.png")),
#   betab_post =
#     (function(x){
#       post_ <- do.call(cbind, extract(endes.betab.fit, pars = c("bx", "rho")))
#       colnames(post_) <- c(bx_names, "rho")
#       mcmc_areas(post_[,-6]) + coord_cartesian(xlim = c(-1.5, 1))
#       ggsave(x, width = 8, height = 6)
#       })(file_out("output/images/betab_post.png")),
#   eibin_post =
#     (function(x){
#       post_ <- do.call(cbind, extract(endes.eibin.fit, pars = c("bx", "sigma")))
#       colnames(post_) <- c(bx_names, "sigma")
#       mcmc_areas(post_[,-6]) + coord_cartesian(xlim = c(-1.5, 1))
#       ggsave(x, width = 8, height = 6)
#       })(file_out("output/images/eibin_post.png")),
#   eibeb_post =
#     (function(x){
#       post_ <- do.call(cbind, extract(endes.eibeb.fit, pars = c("bx", "rho")))
#       colnames(post_) <- c(bx_names, "rho")
#       mcmc_areas(post_[,-6]) + coord_cartesian(xlim = c(-1.5, 1))
#       ggsave(x, width = 8, height = 6)
#       })(file_out("output/images/eibeb_post.png"))
# )
# 
# 
# tables_plan <- drake_plan(
#   betab.endes.tab = (function(){
#     post_pars <- do.call(cbind, extract(endes.betab.fit, pars = c("bx", "rho")))
#     colnames(post_pars) <- c(bx_names, "rho")
#     par_tab <- round(as.vector(apply(post_pars[,-6], 2, mean)),3)
#     names(par_tab) <- colnames(post_pars[,-6])
#     return(t(t(par_tab)))
#   })(),
#   eibin.endes.tab = (function(){
#     post_pars <- do.call(cbind, extract(endes.eibin.fit, pars = c("bx", "bz", "sigma")))
#     colnames(post_pars) <- c(bx_names, bx_names, "sigma")
#     par_tab <- round((apply(post_pars[,-c(6,40)], 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))),3)
#     return(t(par_tab))
#   })(),
#   eibeb.endes.tab = (function(){
#     post_pars <- do.call(cbind, extract(endes.eibeb.fit, pars = c("bx", "bz", "sigma","rho")))
#     colnames(post_pars) <- c(bx_names, bx_names, "sigma","rho")
#     par_tab <- round((apply(post_pars[,-c(6,40)], 2, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))),3)
#     return(t(par_tab))
#   })()
# )

plan_final <- bind_plans(
  compilestan_plan,
  seeds_plan,
  simu_plan,
  divergent_plan
#   loo_tab_plan,
# #  loo_reshape_plan,
#   endes.load_plan,
#   endes.fit_plan,
#   endes.loo_plan,
#   plots_plan,
#   tables_plan
)


## SIGUE: comparar tiempos
## Sesgo/prediccion bajo diferentes condiciones de generacion de data
## Efectos aleatorios
## Incorporar a ENDES (haz parametrizacion no centrada, no la cagues)
## Dos secciones de priors: as model descriptors / as computational tools