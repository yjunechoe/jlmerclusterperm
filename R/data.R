#' Simulated visual world eyetracking data
#'
#' A simulated visual world paradigm experiment on lexical competition in adults
#' and children, where the true time course of each effect is known.
#'
#' The data simulates an experiment where adults and children hear a word while
#' viewing a display that contains the named (target) object and either a
#' competitor whose name sounds similar (`Condition == "Related"`) or only unrelated
#' objects (`Condition == "Unrelated"`). The response is how often participants look
#' at the target over time.
#'
#' The prediction, which also bears out clearly from looking at the data, is that:
#'
#' * Looks to the target rise after word onset, earlier and higher for adults than
#'   for children (a main effect of `Age` from around 200ms onward).
#' * A related competitor temporarily draws looks away from the target while the
#'   word is still ambiguous, and target looks catch up afterwards (a transient
#'   main effect of `Condition`, from around 400ms to 1300ms).
#' * This competition is larger and lasts longer for children than for adults (an
#'   interaction of `Condition` and `Age`, from around 450ms to 1300ms).
#'
#' See `data-raw/vwp_sim.R` in the package source on GitHub for the simulation code.
#'
#' @format A data frame with 26,240 rows and 7 columns:
#' \describe{
#'   \item{Subject}{Participant ID (40 participants)}
#'   \item{Age}{Age group of the participant (`"Adult"` or `"Child"`), between participants}
#'   \item{Item}{Item ID (16 items)}
#'   \item{Condition}{Whether the display contains a related competitor (`"Related"`) or only
#'     unrelated objects (`"Unrelated"`), within participants and items. Each participant
#'     sees each item once, counterbalanced across participants.}
#'   \item{Time}{Time from word onset in milliseconds, in 50ms bins from 0 to 2000}
#'   \item{Samples}{Number of eyetracking samples in the time bin}
#'   \item{Fixations}{Number of samples on the target}
#' }
#'
#' @examples
#' # Empirical logit of looks to the target
#' vwp <- vwp_sim
#' vwp$elog <- with(vwp, log((Fixations + 0.5) / (Samples - Fixations + 0.5)))
#'
#' # Mean time course by Age and Condition
#' means <- tapply(vwp$elog, vwp[c("Time", "Age", "Condition")], mean)
#' matplot(
#'   as.integer(rownames(means)), matrix(means, nrow = nrow(means)),
#'   type = "l", lty = rep(1:2, each = 2), col = 1:2, lwd = 3,
#'   xlab = "Time (ms)", ylab = "Looks to target (empirical logit)"
#' )
#' legend("topleft",
#'   c("Adult Related", "Child Related", "Adult Unrelated", "Child Unrelated"),
#'   lty = rep(1:2, each = 2), col = 1:2, lwd = 3, bty = "n"
#' )
"vwp_sim"
