
library(tidyverse)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(purrr)
library(decisionSupport)

make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(input)
make_variables(estimate_read_csv(paste("C:/Users/Mein PC/OneDrive/Brown/Research project/Decision analysis/labour_optimization_DA/cooperative_interventions.csv",sep="")))

# Coding the mathematical model with the name labour optimization model

labour_optimization_model <- function(x,varname)
{
  # The interventions include:
  # Intervention 1: Cooperative-Driven Mechanization
  # Intervention 2: Cooperative with Gender Balance
  
  #Define each variable as vector of 15 values corresponding to 15 years of simulation
  
  labour_cost_baseline <- rep(0,n_years)
  machine_purchase_cost <- rep(0,n_years)
  fuel_cost <- rep(0,n_years)
  membership_fee <- rep(0,n_years)
  maintenance_cost <- rep(0,n_years)
  establishment_cost <- rep(0,n_years)
  Fuel_cost <- rep(0,n_years)
  training_cost <- rep(0,n_years)
  mechanization_benefits <- rep(0,n_years)
  Labour_pool_benefits <- rep(0,n_years)
  baseline_benefits <- rep(0,n_years)
  yield_increase <- rep(0,n_years)
  labour_saving <- rep(0,n_years)
  income_increase <- rep(0,n_years)
  yield_baseline <- rep(0,n_years)
  
  # Calculating the chance event of risks and uncertainties
  
  training_risk <- chance_event(training_hours_risk_i12,value_if = 1,
                                      n=n_years)
  investment_risk_i1 <- chance_event(investment_risk_i1,value_if = 1,
                                     n=n_years)
  machine_risk_i1 <- chance_event(machine_risk_i1,value_if = 1,
                                  n=n_years)
  fuel_risk <- chance_event(fuel_risk, value_if = 1,
                            n=n_years)
  skill_risk_i12 <- chance_event(skill_risk_i12,value_if = 1,
                                 n=n_years)
  cultural_risk_i2 <- chance_event(cultural_risk_i2, value_if = 1,
                                   n=n_years)
  poor_coordination_risk_i12 <- chance_event(poor_coordination_risk_i12, 
                                             value_if = 1,
                                             n=n_years)
  free_riding_risk_i2 <- chance_event(free_riding_risk_i2,value_if = 1,
                                      n=n_years)
  
  # Risk multipliers (average of multiple binary risks)
  
  # Mechanization risk
  
  mechanization_risk <- 1 - (
    (investment_risk_i1 + machine_risk_i1 + fuel_risk +
                               poor_coordination_risk_i12 +  skill_risk_i12 + 
                               training_risk)/6
    )

  
  mechanization_risk[mechanization_risk < 0] <- 0
  
  # Labour pooling risk
  
  labour_pooling_risk <-  1 - (
    (free_riding_risk_i2 + cultural_risk_i2 + 
                                 training_risk + skill_risk_i12 + 
                                 poor_coordination_risk_i12)/5
  )

   
  labour_pooling_risk[labour_pooling_risk < 0] <- 0
  
  
  # Cost calculation
  
  labour_cost_baseline[1:15] <- labour_baseline
  
  machine_purchase_cost[1] <- machine_purchase_cost_i1
  machine_purchase_cost[2:15] <- 0
  
  membership_fee[1:15] <- membership_fee_i12
  
  establishment_cost[1] <- vv((planning_redesign_i1 + execution_redesign_i1) *
                                labour_cost + redesign_input_cost_i1, 
                              var_CV,
                              n_years)
  establishment_cost[2:15] <- 0
  
  training_cost[1:15] <- travel_cost_for_trainings
  
  maintenance_cost [1:15] <- vv(maintenance_cost_i1 + onsite_repairs_cost,CV_cost, n_years) 
  
  fuel_cost [1:15] <- vv(fuel_cost_i1, CV_fuel_price, n_years)
  
  # Benefit Calculations
  
  # Mechcanization related benefits
  
  yield_increase_mech <- yield_increase_i1*yield_baseline
  income_mech <- yield_increase_mech*price_per_kg
  labour_saving_mech <- labour_saving_rate_i1*labour_cost_baseline
  
  total_benefit_mech <- (income_mech + labour_saving_mech)*mechanization_risk
  net_benefit_mech <- total_benefit_mech - ( machine_purchase_cost + 
                                               fuel_cost + maintenance_cost + 
                                               establishment_cost)
  
  # Labour pooling related benefits
  
  yield_increase_gender <- yield_increase_i2 * yield_baseline
  income_gender <- yield_increase_gender * price_per_kg
  labour_saving_gender <- labour_saving_rate_i2 * labour_cost_baseline
  
  total_benefit_gender <- (income_gender + labour_saving_gender)*labour_pooling_risk
  net_benefit_gender <- total_benefit_gender - membership_fee
  
  # Baseline benefit
  
  total_benefit_baseline <- yield_baseline * price_per_kg
  net_benefit_baseline <- total_benefit_baseline - labour_cost_baseline
  
  
  # Discount NPV
  
  NPV_mech <- discount(net_benefit_mech, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_gender <- discount(net_benefit_gender, discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_baseline <- discount(net_benefit_baseline, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  # Compare NPVs
  return(list(
    NPV_cooperative_driven_mechanization = NPV_mech,
    NPV_cooperative_with_gender_balance = NPV_gender,
    NPV_baseline = NPV_baseline,
    NPV_decision_mech = NPV_mech - NPV_baseline,
    NPV_decision_gender = NPV_gender - NPV_baseline,
    cashflow_decision_mech = net_benefit_mech - net_benefit_baseline,
    cashflow_decision_gender = net_benefit_gender - net_benefit_baseline
    
  ))
  
}


# lets run this using mcSimulation function 
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("C:/Users/Mein PC/OneDrive/Brown/Research project/Decision analysis/labour_optimization_DA/cooperative_interventions.csv"),
  model_function = labour_optimization_model,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

# plotting the results to compare NPVs

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_cooperative_driven_mechanization",
                                             "NPV_cooperative_with_gender_balance",
                                             "NPV_baseline"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_cooperative_driven_mechanization",
                                             "NPV_cooperative_with_gender_balance",
                                             "NPV_baseline"),
                                    method = 'boxplot')
# NPV decision 
# 1: NPV decision for mechanization

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_mech",
                                    method = 'boxplot_density')


# 2: NPV decision for gender balance

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_gender",
                                    method = 'boxplot_density')


# Cashflow Analysis

#Here we plot the distribution of annual cashflow over the entire simulated period 
#for both interventions. For this we use the plot_cashflow() function which uses the 
#specified cashflow outputs from the mcSimulation() function (in our case 
#Cashflow_decision_do) to show cashflow over time.

# 1: Cashflow for cooperative driven mechanization

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "cashflow_decision_mech")

# 2: Cashflow for cooperative with gender balance

plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "cashflow_decision_gender")

# Projection to latent Structures (PLS) analysis

#We apply a post-hoc analysis to the mcSimulation() outputs with plsr.mcSimulation() 
#to determine the Variable Importance in the Projection (VIP) score and coefficients 
#of a Projection to Latent Structures (PLS) regression model. This function uses 
#the outputs of the mcSimulation() selecting all the input variables from the decision 
#analysis function in the parameter object and then runs a PLS regression with an 
#outcome variable defined in the parameter resultName. We use the code names
#(mcSimulation_results$y)[4] and [5](for both interventions)to select the outcome 
#variable NPV_decision_mech and NPV_decision_gender, which are the third and forth 
#element of the list y in our mcSimulation_results outputs (this mustbe a character element).

# 1. Cooperative-driven mechanization decision

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[4], ncomp = 1)
input_table <- read.csv("C:/Users/Mein PC/OneDrive/Brown/Research project/Decision analysis/labour_optimization_DA/cooperative_interventions.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0)

# 2. Cooperative with gender balance decision

pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[5], ncomp = 1)

plot_pls(pls_result, input_table = input_table, threshold = 0)

#Value of Information (VoI) analysis

mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:5])

evpi_intervention_1 <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_cooperative_driven_mechanization")
evpi_intervention_2 <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_cooperative_with_gender_balance")



plot_evpi(evpi_intervention_1, decision_vars = "NPV_decision_mech")
plot_evpi(evpi_intervention_2, decision_vars = "NPV_decision_gender")


