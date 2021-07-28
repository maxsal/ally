#' Simply likelihood ratio test for comparing two models
#' @param m1 a model of class "`glm`", "`lm`", "`nls`", or "`Arima`"
#' @param m2 a model of class "`glm`", "`lm`", "`nls`", or "`Arima`". *Assumed to be nested in `m1`.*
#' @param df degrees of freedom between the two models for testing the test statistic. Default is 1.
#' @return Returns the log-likelihoods from the input models, along with the Chi-sq statistic and p-value from the likelihood ratio test
#' @importFrom stats logLik
#' @importFrom stats pchisq
#' @export
#' @examples
#' \dontrun{
#' model1 <- lm(mpg ~ wt + hp, data = mtcars)
#' model2 <- lm(mpg ~ wt, data = mtcars)
#'
#' lrt_test(m1 = model1, m2 = model2)
#'
#' }

lrt_test <- function(m1, m2, df = 1) {

  tmp_a <- stats::logLik(m1)
  tmp_b <- stats::logLik(m2)

  tmp_stat <- -2 * (as.numeric(tmp_a) - as.numeric(tmp_b))

  tmp_p <- stats::pchisq(tmp_stat, df = df, lower.tail = FALSE)

  list(
    m1_logLik = tmp_a,
    m2_logLik = tmp_b,
    test_stat = tmp_stat,
    p_value   = tmp_p
  )

}
