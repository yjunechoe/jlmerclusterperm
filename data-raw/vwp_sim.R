# Simulated visual world paradigm (VWP) eyetracking experiment with known effects
#
# Adults and children hear a word while viewing a display with the named (target)
# object and either a similar-sounding competitor or only unrelated objects. The
# response is looks to the target over time.
#
# 2 x 2 design: `Condition` (Related vs. Unrelated competitor; within participants
# and items, Latin square) x `Age` (Adult vs. Child; between participants).
#   - Target looks rise after word onset, earlier and higher for adults
#   - A related competitor temporarily draws looks away from the target while the
#     word is still ambiguous, then target looks catch up
#   - Competition is larger and lasts longer for children
#
# See `?vwp_sim` (R/data.R) for the user-facing description.
#
# To regenerate, run from the package root: `source("data-raw/vwp_sim.R")`.
# If you change any parameters, also update the documentation in R/data.R and
# re-render README.Rmd, which reports results on this data.

# Arguments:
#   n_subjects        Number of participants, split evenly into adults and children
#   n_items           Number of items (each participant sees each item once)
#   time              Time bins, in ms from word onset
#   n_samples         Number of eyetracking samples per time bin
#   baseline          Log-odds of looking at the target before word onset
#   target_rise       Asymptotic rise in log-odds of looks to the target, by age group
#   rise_onset        Time (ms) at which the rise is halfway to its asymptote, by age
#   rise_scale        Scale (ms) of the logistic rise; the rise takes about
#                     4 * rise_scale to go from 12% to 88%
#   competition       Proportion of the target rise suppressed at the peak of
#                     competition on Related trials, by age group. Competition slows
#                     the rise but never pushes target looks below baseline.
#   competition_peak  Time (ms) of peak competition, by age group
#   competition_sd    Width (ms) of competition as the SD of a Gaussian curve, by age
#   sd_subject        SDs of by-participant random intercepts (log-odds) and
#                     competition slopes (log-odds of `competition`)
#   sd_item           SDs of by-item random intercepts (log-odds) and competition
#                     slopes (log-odds of `competition`)
#   ar_rho            Autocorrelation of trial-level AR(1) noise across time bins
#   ar_sd             SD of trial-level AR(1) noise
#   seed              Random seed
simulate_vwp <- function(n_subjects = 40, n_items = 16,
                         time = seq(0L, 2000L, by = 50L), n_samples = 25L,
                         baseline = stats::qlogis(0.25),
                         target_rise = c(Adult = 3, Child = 2),
                         rise_onset = c(Adult = 400, Child = 600), rise_scale = 80,
                         competition = c(Adult = 0.15, Child = 0.65),
                         competition_peak = c(Adult = 550, Child = 800),
                         competition_sd = c(Adult = 150, Child = 250),
                         sd_subject = c(intercept = 0.5, competition = 0.4),
                         sd_item = c(intercept = 0.3, competition = 0.3),
                         ar_rho = 0.9, ar_sd = 0.8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_time <- length(time)

  subjects <- data.frame(
    Subject = sprintf("S%02d", seq_len(n_subjects)),
    # Age groups are the first and second half of participants, so that
    # counterbalancing lists (alternating participants, below) are crossed with Age
    Age = rep(c("Adult", "Child"), each = n_subjects / 2),
    subj_intercept = stats::rnorm(n_subjects, 0, sd_subject[["intercept"]]),
    subj_competition = stats::rnorm(n_subjects, 0, sd_subject[["competition"]])
  )
  items <- data.frame(
    Item = sprintf("I%02d", seq_len(n_items)),
    item_intercept = stats::rnorm(n_items, 0, sd_item[["intercept"]]),
    item_competition = stats::rnorm(n_items, 0, sd_item[["competition"]])
  )

  # One trial per subject x item, with Condition counterbalanced (Latin square)
  trials <- merge(subjects, items, by = NULL)
  trials$Condition <- ifelse(
    (match(trials$Subject, subjects$Subject) + match(trials$Item, items$Item)) %% 2 == 0,
    "Related", "Unrelated"
  )
  n_trials <- nrow(trials)

  # Trial-level AR(1) noise on the logit scale (gaze is autocorrelated over time)
  noise <- matrix(0, n_trials, n_time)
  noise[, 1] <- stats::rnorm(n_trials, 0, ar_sd)
  for (j in seq_len(n_time)[-1]) {
    noise[, j] <- ar_rho * noise[, j - 1] + stats::rnorm(n_trials, 0, ar_sd * sqrt(1 - ar_rho^2))
  }

  age <- trials$Age
  related <- as.numeric(trials$Condition == "Related")
  rise <- target_rise[age] * stats::plogis(outer(-rise_onset[age], time, "+") / rise_scale)
  # Proportion of the rise suppressed on Related trials, varying by participant and item
  suppression <- stats::plogis(
    stats::qlogis(competition[age]) + trials$subj_competition + trials$item_competition
  ) * exp(-outer(-competition_peak[age], time, "+")^2 / (2 * competition_sd[age]^2))
  eta <- baseline + trials$subj_intercept + trials$item_intercept +
    rise * (1 - related * suppression) +
    noise

  out <- data.frame(
    Subject = rep(trials$Subject, times = n_time),
    Age = factor(rep(trials$Age, times = n_time), levels = c("Adult", "Child")),
    Item = rep(trials$Item, times = n_time),
    Condition = factor(rep(trials$Condition, times = n_time), levels = c("Related", "Unrelated")),
    Time = rep(time, each = n_trials),
    Samples = n_samples,
    Fixations = stats::rbinom(n_trials * n_time, n_samples, stats::plogis(as.vector(eta)))
  )
  out$elog <- log((out$Fixations + 0.5) / (out$Samples - out$Fixations + 0.5))
  contrasts(out$Age) <- contr.sum(2)
  contrasts(out$Condition) <- contr.sum(2)
  out <- out[order(out$Subject, out$Item, out$Time), ]
  rownames(out) <- NULL
  out
}

vwp_sim <- simulate_vwp(seed = 1)

# Sanity check: mean empirical logit of looks to the target by Age and Condition
matplot(
  unique(vwp_sim$Time), matrix(tapply(vwp_sim$elog, vwp_sim[c("Time", "Age", "Condition")], mean), ncol = 4),
  type = "l", col = 1:2, lty = rep(1:2, each = 2), lwd = 3,
  xlab = "Time (ms)", ylab = "Looks to target (empirical logit)"
)
legend("topleft", c("Adult Related", "Child Related", "Adult Unrelated", "Child Unrelated"),
       col = 1:2, lty = rep(1:2, each = 2), lwd = 3, bty = "n")

usethis::use_data(vwp_sim, overwrite = TRUE)
