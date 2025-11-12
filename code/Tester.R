# multiple models combined tester

#Run Locally?
cluster = TRUE
# cluster = FALSE


#Package Library Location
if (cluster) {
  .libPaths("~/Rlibs")
} else {
  library(here) # File Locations
}

# model_num <- commandArgs(trailingOnly = TRUE)[[1]]
model_num <- 1


if (cluster) {
  params_list <- read_csv("/home/amoor53/APSVE_cluster/cleandata/2021_age_5_11_TND_params.csv")
} else {
  params_list <- read_csv(file = here("cleandata", "2021_age_5_11_TND_params.csv"))
}

params <- params_list[model_num,]

print(params$data_file)
print(params$model_formula)
print(params$method)