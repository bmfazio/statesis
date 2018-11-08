# HOLA HEY HEY:
#   Ya tengo el "simulador" funcional, aunque le faltan los efectos aleatorios.
#   Me estaría faltando:
#     -Implementar efectos aleatorios
#     -Implementar los criterios de comparacion
#     -Ajustar con datos de la ENDES

# drake makefile

# Load our packages and supporting functions into our session.
source(file.path("R", "setup.R"))

# Create the `drake` plan that outlines the work we are going to do.
source(file.path("R", "plan.R"))

# Run your work with make().
make(whole_plan, seed = 1991)

# See also loadd(), readd(), vis_drake_graph(), and drake_config().
config <- drake_config(whole_plan)
vis_drake_graph(config)

### outputs
loadd(endes.betab.fit)

loadd(bx_names)
ble <- do.call(cbind, extract(endes.betab.fit, pars = c("bx", "rho")))
colnames(ble) <- c(bx_names, "rho")
mcmc_areas(ble[,-6]) 