#' @title calc_personal_income_tax
#'
#' @description Calculate personal income tax deductions
#'
#' @details Calculates personal income tax deductions including CPP, EI,
#' federal income tax and provincial income tax (in BC).
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
calc_personal_income_tax <- function(province_code = "BC",
                                     tax_year = 2022,
                                     employment_income = 50000,
                                     other_income = 0,
                                     rrsp_contributions = 0,
                                     capital_gains_losses = 0,
                                     eligable_dividends = 0,
                                     income_taxes_paid = 0) {

  # General Guides:

  # https://www.canada.ca/content/dam/cra-arc/migration/cra-arc/tx/bsnss/tpcs/pyrll/t4032/2022/t4032-bc-1-22eng.pdf

  # https://www.canada.ca/en/revenue-agency/services/tax/businesses/topics/payroll/payroll-deductions-contributions/methods-calculating-deductions-cpp-ei-income-tax.html#cppmpnsc

  # https://www.cibc.com/content/dam/personal_banking/advice_centre/tax-savings/do-you-know-your-tax-rate-en.pdf

  # ----------------------------------------------------------------------------------
  # QA Checks - 1
  # https://turbotax.intuit.ca/tax-resources/british-columbia-income-tax-calculator.jsp

  # QA Checks - 2
  # https://www.wealthsimple.com/en-ca/tool/tax-calculator/british-columbia

  # QA Checks - 3
  # https://www.careerbeacon.com/en/income-tax-calculator/2022/bc?salary=90000

  # QA Checks - 4
  # https://wowa.ca/bc-tax-calculator

  # QA Checks - 5
  # https://www.avanti.ca/resources/income-tax-calculator

  # QA Checks - 6
  # https://www.calculconversion.com/income-tax-calculator-bc.html

  # QA Checks - 7
  # https://ca.talent.com/tax-calculator

  # QA Checks - 8
  # https://www.thomsonreuters.ca/en/dtprofessionalsuite/support/taxratecalculator.html


  if(FALSE) {
    province_code = "BC"
     tax_year = 2022
     employment_income = 10000
     other_income = 0
     rrsp_contributions = 0
     capital_gains_losses = 0
     eligable_dividends = 0
     income_taxes_paid = 0
  }


  if (province_code != "BC") {
    stop("Function not yet parameterized for other provinces...")
  }


  if (tax_year != 2022) {
    stop("Function not parameterized for other tax years...")
  }


  total_income <- employment_income + other_income


  # --------------------------------------------------------
  # Calculate CPP - Canada Pension Plan
  # General Rate 0.057
  # (minimum) - basic exemption: $3500
  # (maximum) - wage cap: $64900
  # Determine the taxable gross pay (all pay types except reimbursement
  # Registered Retirement Savings Plan (RRSP) company contributions).

  cpp1 <- total_income
  value_max <- ifelse(cpp1 > 64900, 64900, cpp1)
  value_min <- value_max - 3500
  tax_cpp <- value_min * 0.057
  tax_cpp <- ifelse(tax_cpp < 0, 0, tax_cpp)



  # --------------------------------------------------------
  # EI Calculations
  # Calculation of employee employment insurance (EI) premiums
  # Annual insurable earnings for the year
  # maximum annual amount of $60,300 for 2022.
  # $952.74
  ei1 <- total_income
  ei1 <- ifelse(ei1 > 60300, 60300, ei1)
  # General contribution rate (self-employed or employed): 1.58%
  e_gcr <- ei1 * 0.0158
  # Employer contribution rate: 2.212% (general rate x 1.4)
  e_ecr <- ei1 * 0.02212



  # --------------------------------------------------------
  # Calculate the federal tax rate for the tax year
  # https://www.canada.ca/en/financial-consumer-agency/services/financial-toolkit/taxes/taxes-2/5.html
  # Total income
  brackets <- c(50198, 100393, 155626, 221708)
  rates <- c(0.15, 0.205, 0.26, 0.29, 0.33)

  # Subtract the basic personal amount - federal
  total_income_bpa <- total_income - 14398

  # Calculate provincial marginal rate
  if(total_income_bpa >= 50198) {
    idx <- max(which(total_income_bpa >= brackets)) + 1
    margin_fed <- rates[idx]
  } else {
    margin_fed <- rates[1]
  }
  if(total_income_bpa < 14398) {
    margin_fed <- 0
  }


  total_federal_tax <-
    calc_marginal_rate(total_income = total_income_bpa,
                       brackets = brackets,
                       rates = rates)

  total_federal_tax <- ifelse(total_federal_tax < 0, 0, total_federal_tax)


  # --------------------------------------------------------
  # Calculate the provincial tax rate for the tax year
  # https://www2.gov.bc.ca/gov/content/taxes/income-taxes/personal/tax-rates
  brackets <-
    c(43070.00, 86141.00, 98901.00, 120094.00, 162832.00, 227091)
  rates <- c(0.0506, 0.077, 0.105, 0.1229, 0.1470, 0.1680, 0.205)

  # Subtract the basic personal amount - provincial
  total_income_bpa <- total_income - 11302 # bpa provincial

  # Calculate provincial marginal rate
  if(total_income_bpa >= 43070.00) {
    idx <- max(which(total_income_bpa >= brackets)) + 1
    margin_prov <- rates[idx]
  } else {
    margin_prov <- rates[1]
  }
  if(total_income_bpa < 11302) {
    margin_prov <- 0
  }

  total_provincial_tax <-
    calc_marginal_rate(total_income = total_income_bpa,
                       brackets = brackets,
                       rates = rates)

  total_provincial_tax <- ifelse(total_provincial_tax < 0, 0, total_provincial_tax)



  # --------------------------------------------------------
  # The Net Income Threshold Adjustment
  # for Basic Personal Amount



  # --------------------------------------------------------
  # Return Object:
  # Total Income
  # Taxes
    # Total Tax
    # Federal Tax
    # Provincial Tax
    # CPP
    # EI Premiums
  # Net After Tax Income
  # Other
    # Marginal Tax Rate
    # Average Tax Rate

  total_taxes <- total_provincial_tax + total_federal_tax + e_gcr + tax_cpp
  total_income_tax <- total_provincial_tax + total_federal_tax


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
