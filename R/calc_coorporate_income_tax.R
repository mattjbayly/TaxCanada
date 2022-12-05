#' @title calc_corporate_income_tax
#'
#' @description Calculate corporate income tax deductions
#'
#' @details Calculates corporate income (CIT) tax deductions.
#'
#'
#' @param province_code Character. Must be set to `"BC"` for British Columbia.
#' @param tax_year Numeric. Must be set to `2022` for the current tax year.
#' @param employment_income Numeric. Employment income and other taxable benefits.
#' @param other_income Numeric. All other income including rental fees, side-hustles, interest, EI and CPP.
#' @param rrsp_contributions Numeric. RRSP Contributions.
#' @param capital_gains_losses Numeric. Capital gains. Growth of value of investments (outside of a TFSA).
#' @param eligable_dividends Numeric. Eligible Dividends.
#' @param income_taxes_paid Numeric. Income taxes paid.
#'
#'
#'@examples
#'\dontrun{
#' library(TaxCanada)
#' }
#'
#' @export
calc_corporate_income_tax <- function(province_code = "BC",
                                     tax_year = 2022,
                                     gross_income = 50000,
                                     capital_gains_losses = 0,

                                     other_income = 0) {


  if(capital_gains_losses != 0) {
    stop("Not developed...")
  }


  # General Guides:

  # https://accountor.ca/blog/taxation/corporate-tax-rate.html

  # https://taxsummaries.pwc.com/canada/corporate/taxes-on-corporate-income



  # 2020
  federal    <- 0.09
  provincial <- 0.02



  ret_obj <- list()
  ret_obj$total_income        <- total_income
  ret_obj$net_income          <- total_income - total_taxes
  ret_obj$average_tax_rate    <- total_income_tax/total_income
  ret_obj$marginal_tax_rate   <- margin_fed + margin_prov
  ret_obj$total_taxes         <- total_taxes
  ret_obj$total_income_tax    <- total_provincial_tax + total_federal_tax
  ret_obj$total_federal_tax   <- total_federal_tax
  ret_obj$total_provincial_tax<- total_provincial_tax
  ret_obj$total_employee_cpp  <- tax_cpp
  ret_obj$total_employee_ei   <- e_gcr
  ret_obj$total_employer_cpp  <- NA
  ret_obj$total_employee_ei   <- e_ecr

  return(ret_obj)

}
