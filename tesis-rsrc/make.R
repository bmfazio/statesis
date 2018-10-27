# drake makefile

# Load our packages and supporting functions into our session.
source(file.path("R", "setup.R"))

# Create the `drake` plan that outlines the work we are going to do.
source(file.path("R", "plan.R"))

# Run your work with make().
make(plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config().
config <- drake_config(plan)
vis_drake_graph(config)

#loadd(endes.merged)
loadd(simu.data)
hist(simu.data$y)
# #### PLAN: fijate que la carga de datos (en mi caso tmb simulacion) aparece como parte del plan
# # The workflow plan data frame outlines what you are going to do.
# plan <- drake_plan(
#   raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
#   data = raw_data %>%
#     mutate(Species = forcats::fct_inorder(Species)) %>%
#     select(-X__1),
#   hist = create_plot(data),
#   fit = lm(Sepal.Width ~ Petal.Width + Species, data),
#   report = rmarkdown::render(
#     knitr_in("report.Rmd"),
#     output_file = file_out("report.html"),
#     quiet = TRUE
#   )
# )
