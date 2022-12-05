#' @title calc_personal_income_tax
#'
#' @description Calculate marginal rates
#'
#' @details Calculates marginal rates based on a vector of breaks and rates.
#'
#' @param total_income Numeric. Total income.
#' @param brackets Vector (numeric). Tax brackets e.g., c(50198, 100393, 155626).
#' @param rates Vector (numeric). Rates associated with each bracket. Vector length should be longer than the brackets vector by a unit of one. Final rate value will be applied to amounts beyond max bracket value
#'
#' @returns Total tax payable based on marginal rate summary
#'
#'@examples
#'\dontrun{
#' library(TaxCanada)
#' }
#'
#' @export
calc_marginal_rate <- function(total_income = 50000,
                               brackets = c(50198, 100393, 155626, 221708),
                               rates = c(0.15, 0.205, 0.26, 0.29, 0.33)) {

  # Perform checks
  if((length(brackets) + 1) != length(rates)) {
    stop("length(rate) vector should be length(brackets) + 1")
  }
  if(max(rates) > 1) {
    stop("Tax rates should be fractions and not exceed 1")
  }
  if(min(rates) < 0) {
    stop("Tax rates should be fractions and not be below 0")
  }

  # Income below lowest bracket
  if(total_income <= brackets[1]) {
    # Simple single rate calculate
    tax <- total_income * rates[1]
    return(tax)
  }


  total_tax <- 0
  # Step through brackets
  for(i in 1:length(brackets)) {

    this_bracket <- brackets[i]
    this_rate <- rates[i]

    if((total_income - this_bracket) > 0) {
      # Calculate tax within window
      if(i == 1) {
        window <- brackets[i]
      } else {
        window <- brackets[i] - brackets[i - 1]
      }

      window_tax <- window * this_rate
      total_tax <- total_tax + window_tax
      next
    }
    if((total_income - this_bracket) <= 0) {
      # Calculate marginal tax
      margin <- total_income - brackets[i - 1]
      margin_tax <- margin * this_rate
      total_tax <- total_tax + margin_tax
      break
    }
  }

  # Final value - beyond max
  if(total_income > brackets[length(brackets)]) {
    max_margin <- total_income - brackets[length(brackets)]
    max_rate <- rates[length(rates)]
    total_tax <- total_tax + (max_margin * max_rate)
  }

  return(total_tax)


}
