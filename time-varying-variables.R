library(survival)

# Source: https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf
vignette("timedep", "survival")

set.seed(1953)  # a good year

nvisit <- floor(pmin(lung$time/30.5, 12))

# 5% chance with each visit that they've "responded",
# the more visits, the more chance for response, simple
# as that. Another way to put it, dying off lessens
# your chance to respond.
response <- rbinom(nrow(lung), nvisit, .05) > 0

badfit <- survfit(
  Surv(time / 365.25, status) ~ response,
  data = lung)

plot(badfit, mark.time = FALSE, lty = 1:2,
     xlab = "Years post diagnosis", ylab = "Survival")

legend(1.5, .85, c("Responders", "Non-responders"),
       lty=2:1, bty='n')