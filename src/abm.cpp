#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' geo_sum_c
//'
//' The geometric sum d+d^2 + .. d^term
//'
//' @param d First term
//' @param term number of terms
//' @return the sum d+d^2 + .. d^term
// [[Rcpp::export]]
double geo_sum_c(double d, int term) {
  double res;
  if (d != 1) {
    res = d * (pow(d, term) - 1) / (d - 1);
  } else {
    res = term;
  }
  return res;
}

//' hello 2 x world
//'
//' @param params_vec First term
//' @return 2 standing charge
//' @export
// [[Rcpp::export]]
double test_c(NumericVector params_vec) {
   double res = as<double>(params_vec["standing_charge"]);
   return 2*res;
}



//' seai_grant_c
//'
//' seai grant for domestic solar installation (2022 structure)
//' fast c++ implementation
//'
//' @param params_vec scenario parameter vector
//' @param s solar capacity in kW
//' @param b battery capacity in kWh
//' @return grant amount in euros
//' @export
// [[Rcpp::export]]
double seai_grant_c(NumericVector params_vec, double s, double b) {
  double sol_lower_threshold = as<double>(params_vec["sol_lower_threshold"]);
  double sol_upper_threshold = as<double>(params_vec["sol_upper_threshold"]);
  double sol_lower_grant = as<double>(params_vec["sol_lower_grant"]);
  double sol_upper_grant = as<double>(params_vec["sol_upper_grant"]);
  double bat_threshold = as<double>(params_vec["bat_threshold"]);
  double bat_grant = as<double>(params_vec["bat_grant"]);
  double grant_introduction_date = as<double>(params_vec["grant_introduction_date"]);
  double grant_removal_date = as<double>(params_vec["grant_removal_date"]);
  double yeartime = as<double>(params_vec["yeartime"]);

  double max_sol_grant = sol_lower_threshold * sol_lower_grant +
    (sol_upper_threshold - sol_lower_threshold) * sol_upper_grant;

  double grant = 0;

  if (yeartime >= grant_introduction_date && yeartime <= grant_removal_date) {
    if (s <= sol_lower_threshold) {
      grant = sol_lower_grant * s;
    } else if (s >= sol_upper_threshold && b < bat_threshold) {
      grant = max_sol_grant;
    } else if (s >= sol_upper_threshold && b >= bat_threshold) {
      grant = max_sol_grant + bat_grant;
    } else if (s > sol_lower_threshold && s < sol_upper_threshold) {
      grant = sol_lower_threshold * sol_lower_grant + (s - sol_lower_threshold) * sol_upper_grant;
    }
  }

  return grant;
}

//' amort
//'
//' amortisation or annuity payment for unit principal
//'
//' @param r decimal interest rate
//' @param term term of loan or investment
//'
//' @return real value
//' @export
// [[Rcpp::export]]
double amort_c(double r, int term) {
  double res;
  if (r != 0) {
    res = (r * pow(1 + r, term)) / ((pow(1 + r, term)) - 1);
  } else {
    res = 1.0 / term;
  }
  return res;
}

//' get_sys_util_c
//'
//' fast implementation of the financial utility of investment in a household solar PV system. This is defined as the fractional reduction in net present value of all associated cashflows(NPV_0-NPV)/NPV_0 where NPV_0 is the net present value
//' of expected future bills (including inflation expectations, discounted out to a term) and NPV is the net present value of all future cashflows
//' when a solar PV investment is made (amortisation, feedin tariff revenues, reduced electricity bills, tax). The financial model assumes a 2-component roof with incremental pv investments
//' d_solar1 and d_solar2 and incremental battery installation d_battery.
//' This function calculates the financial partial utility for initial adoption (old_solar1=0 and old_solar2=0) or for augmentation of an existing installation.
//' It is assumed that no grant is available for augmenting an existing system.
//'
//' @param params_vec vector of scenario parameters extracted from R params environment using "mget"
//' @param demand annual household electrical energy demand (kWh) (from smart meter dataset).
//' @param old_imports annual energy (kWh) imports with existing system (old_solar1, old_solar2, old_battery)
//' @param old_exports annual energy exports (kWh) with existing system (old_solar1, old_solar2, old_battery)
//' @param old_solar1 existing installed solar capacity on half roof1
//' @param old_solar2 existing installed solar capacity on half roof2
//' @param old_battery currently installed battery
//' @param new_imports annual energy (kWh) import with new (proposed) system (old_solar1 + d_solar1, old_solar2 + d_solar2, old_battery+d_battery)
//' @param new_exports annual energy exports (kWh) with new (proposed) system (old_solar1 + d_solar1, old_solar2 + d_solar2, old_battery+d_battery)
//' @param d_solar1 additional solar capacity on roof1
//' @param d_solar2 additional solar capacity on roof2
//' @param d_battery kWh battery size increment in steps of 2.5 kWh
//' @param include_grant True or False
//' @param customer "domestic" is only option at the moment
//'
//' @return unscaled financial partial utility
//' @export
// [[Rcpp::export]]
double get_sys_util_c(NumericVector params_vec, double demand, double old_imports, double old_exports,
                             double old_solar1, double old_solar2, double old_battery, double new_imports,
                             double new_exports, double d_solar1, double d_solar2, double d_battery,
                             bool include_grant = true, const char* customer = "domestic") {


  double d_solar = d_solar1 + d_solar2;
  double old_solar = old_solar1 + old_solar2;

  double install_cost_pv = (d_solar > 0 && old_solar == 0) ? as<double>(params_vec["pv_install_cost"]) : (d_solar > 0 && old_solar > 0) ? as<double>(params_vec["pv_install_cost"]) / 2 : 0;

  double install_cost_bat = (d_battery > 0 && old_battery == 0) ? as<double>(params_vec["battery_install_cost"]) : (d_battery > 0 && old_battery > 0) ? 0 : 0;

  double install_cost = install_cost_pv + install_cost_bat;

  int term_of_loan = (d_solar == 0 && d_battery == 0) ? 0 : as<double>(params_vec["term_of_loan"]);
  int system_lifetime = as<double>(params_vec["system_lifetime"]);

  double grant = (!include_grant) ? 0 : (old_solar > 0) ? 0 : seai_grant_c(params_vec, d_solar, d_battery);
  double fit = as<double>(params_vec["ceg"]);
  double discount_rate = as<double>(params_vec["discount_rate"]);
  double e_price_inflation = as<double>(params_vec["e_price_inflation"]);
  double ceg_price_inflation = as<double>(params_vec["ceg_price_inflation"]);
  double marginal_rax_rate = as<double>(params_vec["marginal_tax_rate"]);
  double fit_tax_threshold = as<double>(params_vec["ceg_tax_threshold"]);
  double e_price = as<double>(params_vec["e_price"]);

  double capex = install_cost + as<double>(params_vec["pv_cost"]) * d_solar + as<double>(params_vec["battery_cost"]) * d_battery - grant;
  double amort_payment = amort_c(discount_rate, term_of_loan) * capex;

  double first_year_bill_old = e_price * old_imports + as<double>(params_vec["standing_charge"]);
  double first_year_bill_new = e_price * new_imports + as<double>(params_vec["standing_charge"]);
  //double bill_saving = params_vec["e_price"] * (new_imports - old_imports);

  double first_year_mss_revenue_old = (fit * old_exports < fit_tax_threshold) ?
  fit * old_exports : (fit * old_exports * (1 - marginal_rax_rate) + fit_tax_threshold * marginal_rax_rate);

  double first_year_mss_revenue_new = (fit * new_exports < fit_tax_threshold) ?
  fit * new_exports : (fit * new_exports * (1 - marginal_rax_rate) + fit_tax_threshold * marginal_rax_rate);


  double npv_loan = geo_sum_c(1 / (1 + discount_rate), term_of_loan) * amort_payment;
  double npv_bills = geo_sum_c((1 + e_price_inflation) / (1 + discount_rate), system_lifetime) * first_year_bill_new;
  double npv_mss = 0.0-geo_sum_c((1 + ceg_price_inflation) / (1 + discount_rate), system_lifetime) * first_year_mss_revenue_new;

  npv_loan = (term_of_loan == 0) ? 0 : npv_loan;

  double npv_noinvest = geo_sum_c((1 + e_price_inflation) / (1 + discount_rate), system_lifetime) * (first_year_bill_old - first_year_mss_revenue_old);

  double npv = npv_loan + npv_bills + npv_mss;

  // Positive is good
  return (npv_noinvest - npv) / npv_noinvest;
}


  // You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

