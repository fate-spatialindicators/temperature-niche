# quad_prior branch
devtools::install_github("pbs-assess/sdmTMB", "quad-priors")

# load west coast model 
fit = readRDS("output/wc/model_7.rds")
# check it's dover sole:
fit$data$scientific_name[1]

sd_report <- summary(fit$sd_report)
params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])

Estimate Std. Error
quadratic_low       -2.7929702 0.14411492
quadratic_hi         2.2636048 0.06683277

# The b_js here are in order of temp + temp2 + as.factor(year) + depth + depth2

b_j = fit$sd_report$par.fixed[grep("b_j", names(fit$sd_report$par.fixed))]
nb = length(b_j)
bs_to_save = c(1,2,nb-1,nb)

b_mean = b_j[bs_to_save]
b_cov = fit$sd_report$cov.fixed[bs_to_save,bs_to_save]

# load in the Dover sole GOA model
fit_goa = readRDS("output/goa/model_15.rds")
# check it's dover sole:
fit_goa$data$scientific_name[1]

sd_report <- summary(fit_goa$sd_report)
params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])

Estimate  Std. Error
quadratic_low       -9.2242030 3.227650843
quadratic_hi         7.1561681 1.787235144

# figure out dimension of fixed effects
nb = length(grep("b_j", names(fit_goa$sd_report$par.fixed)))
# bring in priors from west coast
prior_mean = rep(0, nb)
prior_mean[1:4] = b_mean
prior_cov = diag(nb)*10
prior_cov[1:4,1:4] = b_cov

# now re-fit the GOA model with the informative prior
fit_goa_informative = sdmTMB(
  formula = cpue_kg_km2 ~ -1 + enviro + I(enviro^2) + depth + 
    I(depth^2) + as.factor(year),
  time_varying = fit_goa$time_varying,
  spde = fit_goa$spde,
  time = fit_goa$time,
  family = tweedie(link = "log"),
  data = fit_goa$data,
  spatial_only = FALSE,
  quadratic_roots = TRUE,
  prior_mean = prior_mean,
  prior_cov = prior_cov
)

sd_report <- summary(fit_goa_informative$sd_report)

params <- as.data.frame(sd_report[grep("quadratic", row.names(sd_report)), ])


Estimate  Std. Error
quadratic_low       -2.281848 0.066752691
quadratic_hi         2.867270 0.066732202


enviro                  0.26    0.04
I(enviro^2)            -0.45    0.02
depth                   1.77    0.04
I(depth^2)             -0.58    0.02