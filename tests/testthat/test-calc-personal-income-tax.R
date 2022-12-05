test_that("calc_personal_income_tax works", {

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


  v1 <- calc_personal_income_tax(
    province_code = "BC",
    tax_year = 2022,
    employment_income = 40000
  )

  # Federal -
  mv <- median(c(3347, 3240, 3281.44, 3840.30, 5400))
  check1 <- abs(v1$total_federal_tax - mv)
  expect_true(check1 < 500)

  # Provincial -
  mv <- median(c(1331, 1315, 1314.87, 1452.12, 1887))
  check1 <- abs(v1$total_provincial_tax - mv)
  expect_true(check1 < 200)

  # Income Tax
  mv <- median(c(5292.39, 5292))
  check1 <- abs(v1$total_income_tax - mv)
  expect_true(check1 < 200)

  # EI and CPP
  check <- v1$total_employee_cpp + v1$total_employee_ei
  mv <- median(c(2621, 2713, (2080.50 + 632.00), (2080.50 + 632.00), (2080.50 + 632.00), (2080.50 + 632.00)))
  check1 <- abs(check - mv)
  expect_true(check1 < 300)

  # Average tax rate -
  mv <- median(c(0.036, 0.25, 0.1368, 0.20, 0.1323, 0.1827, 0.1817, 0.1175))
  check1 <- abs(v1$average_tax_rate - mv)
  expect_true(check1 < 0.04)







  v1 <- calc_personal_income_tax(
    province_code = "BC",
    tax_year = 2022,
    employment_income = 90000
  )

  # Federal -
  mv <- median(c(12870, 12643, 12737, 14828))
  check1 <- abs(v1$total_federal_tax - mv)
  expect_true(check1 < 100)

  # Provincial -
  mv <- median(c(5193, 5079, 5103.88, 5676))
  check1 <- abs(v1$total_provincial_tax - mv)
  expect_true(check1 < 300)

  # Income Tax
  mv <- median(c(18858))
  check1 <- abs(v1$total_income_tax/18858 - 1)
  expect_true(check1 < 0.1)

  # EI and CPP
  check <- v1$total_employee_cpp + v1$total_employee_ei
  mv <- median(c(4056, 4453, (3499.8 + 952.74), (3499.8 + 952.74), (3500 + 953)))
  check1 <- abs(check - mv)
  expect_true(check1 < 400)

  # Average tax rate -
  mv <- median(c(0.2095, 0.277, 0.2115, 0.2095, 0.2477, 0.2464, 0.2013))
  check1 <- abs(v1$average_tax_rate - mv)
  expect_true(check1 < 0.03)






  v1 <- calc_personal_income_tax(
    province_code = "BC",
    tax_year = 2022,
    employment_income = 900000
  )

  # Average tax rate -
  mv <- median(c(0.4858, 0.4893, 0.4893, 0.49))
  check1 <- abs(v1$average_tax_rate - mv)
  expect_true(check1 < 0.03)



})
