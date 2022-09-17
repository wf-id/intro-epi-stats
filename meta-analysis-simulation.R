
generate_studies <- function(mu_true = .15, n_study = 3, tau = .7){
  tau_use <- rnorm(n = 1, mean = tau, sd = .01)
  ran_effect <- rnorm(n_study, 0, tau_use)
  obs_prop <- arm::invlogit(arm::logit(mu_true) + ran_effect)
  sample_size <- sample(x = c(10:50, 100:200), size = n_study)

  cases <- rbinom(n = n_study, size = sample_size, prob = obs_prop)

  data.frame(study = letters[1:n_study],
             true_mu = mu_true,
             cases = cases, sample_size = sample_size)


}


library(metafor)
run_study <- function(x){
  d <- generate_studies(n_study = x)
  base <- rma.glmm(xi = cases , ni = sample_size ,
                   data = d,
                   slab = paste(study),
                   measure = "PLO")

  o <- dplyr::mutate_all(as.data.frame(predict(base)), function(x) arm::invlogit(x))
  cbind(data.frame( I2 = base$I2, times = x),o)
}


z <- purrr::map_dfr(2:20, ~purrr::rerun(50, run_study(.x)))

library(ggplot2)

z |>
  ggplot(aes(factor(times), I2))+
  geom_boxplot()

z |>
  ggplot(aes(factor(times), pred))+
  geom_boxplot()+
  geom_hline(yintercept = .15)

forest(base,
       xlim=c(-0.6,1.2),
       ilab=cbind(d$cases, d$sample_size), ilab.xpos=c(-0.2,-0.1),
       transf = transf.ilogit,
       cex=.75, header="Author(s) and Year", mlab="",)
