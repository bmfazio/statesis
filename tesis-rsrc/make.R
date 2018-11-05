HOLA HEY HEY:
  Ya tengo el "simulador" funcional, aunque le faltan los efectos aleatorios.
  Me estaría faltando:
    -Implementar efectos aleatorios
    -Implementar los criterios de comparacion
    -Ajustar con datos de la ENDES

# drake makefile

# Load our packages and supporting functions into our session.
source(file.path("R", "setup.R"))

# Create the `drake` plan that outlines the work we are going to do.
source(file.path("R", "plan.R"))
#source(file.path("R", "stanfail.R"))

# Run your work with make().
make(whole_plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config().
config <- drake_config(whole_plan)
vis_drake_graph(config)

#loadd(endes.merged)
# loadd(simu.data)
# hist(simu.data$y)

#loadd(simu.bin.fit);simu.bin.fit
#loadd(simu.bin.data);hist(simu.bin.data$y)
#loadd(simu.bb.fit);simu.bb.fit

# loadd(simu.binom.fit_simu.binom.data);simu.binom.fit_simu.binom.data
# loadd(simu.binom.fit_simu.betab.data);simu.binom.fit_simu.betab.data
# loadd(simu.binom.fit_simu.eibeb.data);simu.binom.fit_simu.eibeb.data
# # 
# loadd(simu.betab.fit_simu.binom.data);simu.betab.fit_simu.binom.data
# loadd(simu.betab.fit_simu.betab.data);simu.betab.fit_simu.betab.data
# loadd(simu.betab.fit_simu.eibeb.data);simu.betab.fit_simu.eibeb.data
# # 
# loadd(simu.eibeb.fit_simu.binom.data);simu.eibeb.fit_simu.binom.data
# loadd(simu.eibeb.fit_simu.betab.data);simu.eibeb.fit_simu.betab.data
# loadd(simu.eibeb.fit_simu.eibeb.data);simu.eibeb.fit_simu.eibeb.data
loadd(endes.eibeb.fit);endes.eibeb.fit
loadd(endes.betab.fit);endes.betab.fit
loadd(endes.binom.fit);endes.binom.fit


shinystan::launch_shinystan(endes.eibeb.fit)
shinystan::launch_shinystan(simu.eibeb.fit_simu.eibeb.data)
shinystan::launch_shinystan(simu.eibeb.fit_simu.betab.data)
shinystan::launch_shinystan(simu.eibeb.fit_simu.binom.data)


# lapply(endes.data, function(x){
#   if(is.matrix(x)){
#     a<-dim(x);return(matrix(as.vector(x), nrow=a[1]))
#   } else {
#     return(as.vector(x))
#   }
# }) -> d
# 
# with(d, stan_rdump(names(d), file = "endes_dump.R"))

# make /home/bmfazio/git-repos/tesis-pucpstat/tesis-rsrc/Stan/eibb-regression-model
# /home/bmfazio/git-repos/tesis-pucpstat/tesis-rsrc/Stan/eibb-regression-model sample data file=/home/bmfazio/git-repos/tesis-pucpstat/tesis-rsrc/endes_dump.R

read.csv("/home/bmfazio/git-repos/tesis-pucpstat/output.csv", comment.char="#") -> hmm
library(bayesplot)
mcmc_areas(hmm[-(1:7)])
mcmc_dens(hmm[-(1:7)])