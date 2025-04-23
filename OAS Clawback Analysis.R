# Check and install required packages
required_packages <- c("dplyr", "tidyr", "ggplot2", "scales")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

set.seed(123)

# --- Constants ---
n_sim <- 2000
years <- 40
retirement_age <- 65
initial_balance <- 1000000
oas_base_65_74 <- 727.67 * 12  # $8,732.04/year (65-74, 2025 base)
oas_base_75_plus <- 800.44 * 12  # $9,605.28/year (75+, 2025 base)
clawback_threshold <- 93454  # 2025 estimate = $93,454
clawback_rate <- 0.15
income_cutoff_65_74 <- 142609  # 2025 estimate = $151,668
income_cutoff_75_plus <- 148179  # 2025 estimate = $157,490
start_ages <- 65:70
deferral_rate <- 0.072  # 7.2% per year --> 36% for 5 years
discount_rate <- 0.03   # 3% discount rate to PV at 65
stock_weight <- 0.5
bond_weight <- 0.5

# RRIF Minimum Withdrawal Factors (simplified for ages 71-110, 2025 rules)
rrif_factors <- c(
  0.0540, 0.0553, 0.0567, 0.0582, 0.0598, 0.0617, 0.0636, 0.0658, 0.0682, 0.0708,  # 71-80
  0.0738, 0.0771, 0.0809, 0.0851, 0.0899, 0.0955, 0.1020, 0.1096, 0.1187, 0.1300,  # 81-90
  0.1448, 0.1642, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000,  # 91-100
  0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000, 0.2000   # 101-110
)

# p_x data (Ontario 2023)
px_both <- c(0.99133, 0.99051, 0.98959, 0.98857, 0.98744, 0.98618, 0.98478, 0.98321, 0.98147, 0.97952,
             0.97734, 0.97490, 0.97217, 0.96911, 0.96568, 0.96182, 0.95748, 0.95259, 0.94709, 0.94088,
             0.93387, 0.92595, 0.91699, 0.90685, 0.89535, 0.88230, 0.86783, 0.85223, 0.83549, 0.81765,
             0.79573, 0.77557, 0.75449, 0.73262, 0.71014, 0.68725, 0.66417, 0.64111, 0.61829, 0.59593,
             0.57422, 0.55332, 0.53338, 0.51452, 0.49681, 0.00000)
px_male <- c(0.98928, 0.98828, 0.98717, 0.98594, 0.98458, 0.98307, 0.98139, 0.97953, 0.97744, 0.97512,
             0.97253, 0.96964, 0.96640, 0.96278, 0.95872, 0.95417, 0.94906, 0.94332, 0.93686, 0.92958,
             0.92138, 0.91213, 0.90168, 0.88986, 0.87649, 0.86134, 0.84461, 0.82669, 0.80762, 0.78746,
             0.76253, 0.74048, 0.71772, 0.69447, 0.67095, 0.64738, 0.62400, 0.60104, 0.57871, 0.55720,
             0.53667, 0.51723, 0.49899, 0.48200, 0.46630, 0.00000)
px_female <- c(0.99331, 0.99264, 0.99189, 0.99105, 0.99011, 0.98907, 0.98789, 0.98658, 0.98510, 0.98345,
               0.98158, 0.97949, 0.97712, 0.97446, 0.97144, 0.96803, 0.96418, 0.95981, 0.95485, 0.94921,
               0.94281, 0.93552, 0.92721, 0.91774, 0.90692, 0.89454, 0.88072, 0.86569, 0.84944, 0.83198,
               0.81034, 0.79014, 0.76883, 0.74655, 0.72349, 0.69986, 0.67589, 0.65182, 0.62790, 0.60438,
               0.58149, 0.55943, 0.53836, 0.51844, 0.49975, 0.00000)

# --- Market Returns ---
regime_stock_means <- c(0.09, -0.15, 0.03) # Bull, Bear, Stagnant
regime_stock_sds <- c(0.16, 0.25, 0.10) # Bull, Bear, Stagnant
regime_bond_means <- c(0.01, 0.04, 0.02) # Bull, Bear, Stagnant
regime_bond_sds <- c(0.04, 0.06, 0.03) # Bull, Bear, Stagnant
transition_matrix <- matrix(c(0.75, 0.1, 0.15, 0.2, 0.65, 0.15, 0.15, 0.15, 0.7), nrow = 3, byrow = TRUE)
inflation_matrix <- matrix(rnorm(years * n_sim, mean = 0.033, sd = 0.02), nrow = years, ncol = n_sim)

stock_returns <- matrix(0, years, n_sim)
bond_returns <- matrix(0, years, n_sim)
regime_matrix <- matrix(1, years, n_sim)

for (sim in 1:n_sim) {
  for (year in 1:years) {
    current_state <- regime_matrix[year, sim]
    stock_returns[year, sim] <- rnorm(1, regime_stock_means[current_state], regime_stock_sds[current_state])
    bond_returns[year, sim] <- rnorm(1, regime_bond_means[current_state], regime_bond_sds[current_state])
    if (year < years) {
      regime_matrix[year + 1, sim] <- sample(1:3, 1, prob = transition_matrix[current_state, ])
    }
  }
}

portfolio_returns <- stock_weight * stock_returns + bond_weight * bond_returns

# --- Mortality Simulation ---
simulate_deaths <- function(px_adj, n_sim) {
  death_ages <- numeric(n_sim)
  for (i in 1:n_sim) {
    age <- 65
    while (age <= 110) {
      if (runif(1) < px_adj[age - 64]) age <- age + 1 else break
    }
    death_ages[i] <- age
  }
  death_ages
}

# --- Taxation Code ---
calculate_total_tax <- function(income) {
  tax <- numeric(length(income))
  federal_brackets <- c(0, 55867, 111733, 173205, 246752)  # 2025 estimate
  federal_rates <- c(0.15, 0.205, 0.26, 0.29, 0.33)
  ontario_brackets <- c(0, 51446, 102894, 150000, 220000)  # 2025 estimate
  ontario_rates <- c(0.0505, 0.0915, 0.1116, 0.1216, 0.1316)
  
  for (i in seq_along(income)) {
    inc <- income[i]
    fed_tax <- 0
    for (j in 1:length(federal_brackets)) {
      if (inc > federal_brackets[j]) {
        upper <- ifelse(j < length(federal_brackets), min(inc, federal_brackets[j + 1]), inc)
        fed_tax <- fed_tax + (upper - federal_brackets[j]) * federal_rates[j]
      }
    }
    ont_tax <- 0
    for (j in 1:length(ontario_brackets)) {
      if (inc > ontario_brackets[j]) {
        upper <- ifelse(j < length(ontario_brackets), min(inc, ontario_brackets[j + 1]), inc)
        ont_tax <- ont_tax + (upper - ontario_brackets[j]) * ontario_rates[j]
      }
    }
    tax[i] <- fed_tax + ont_tax
  }
  return(tax)
}

# --- Combined Simulation ---
sexes <- c("Both", "Male", "Female")
account_types <- c("RRSP", "TFSA")
results <- list()
death_ages_by_sex <- list()

for (sex in sexes) {
  px <- if (sex == "Both") px_both else if (sex == "Male") px_male else px_female
  death_ages <- simulate_deaths(px, n_sim)
  death_ages_by_sex[[sex]] <- death_ages
  
  for (acct in account_types) {
    for (start_age in start_ages) {
      portfolio <- matrix(initial_balance, years + 1, n_sim)
      withdrawals <- matrix(0, years, n_sim)
      oas_received <- matrix(0, years, n_sim)
      taxable_income <- matrix(0, years, n_sim)
      clawback_amount <- matrix(0, years, n_sim)
      total_tax <- matrix(0, years, n_sim)
      net_income <- matrix(0, years, n_sim)
      pv_oas <- numeric(n_sim)
      
      oas_adj_65_74 <- oas_base_65_74 * (1 + deferral_rate * (start_age - 65))
      oas_adj_75_plus <- oas_base_75_plus * (1 + deferral_rate * (start_age - 65))
      
      for (t in 1:years) {
        age <- retirement_age + t - 1
        alive <- age < death_ages
        remaining <- pmax(1, death_ages - age)
        
        # Waring-Siegel withdrawal
        ws_withdrawal <- ifelse(alive, portfolio[t, ] / remaining, 0)
        
        # RRIF minimum withdrawal (for RRSP only, age 71+)
        rrif_factor <- ifelse(age >= 71 & age <= 110, rrif_factors[age - 70], 0)
        rrif_withdrawal <- ifelse(acct == "RRSP" & alive, portfolio[t, ] * rrif_factor, 0)
        
        # Withdrawal is max of Waring-Siegel and RRIF (if applicable)
        withdrawals[t, ] <- ifelse(acct == "RRSP" & age >= 71 & alive,
                                   pmax(ws_withdrawal, rrif_withdrawal),
                                   ws_withdrawal)
        withdrawals[t, ] <- pmin(withdrawals[t, ], portfolio[t, ])
        
        # OAS as additional revenue (before clawback)
        gross_oas <- ifelse(age >= start_age & alive,
                            ifelse(age < 75, oas_adj_65_74, oas_adj_75_plus), 0)
        
        # Income for clawback (withdrawals for RRSP + OAS for both)
        income_for_clawback <- ifelse(acct == "RRSP" & alive, withdrawals[t, ], 0) + gross_oas
        
        # Clawback calculation
        excess <- pmax(0, income_for_clawback - clawback_threshold)
        clawback_amount[t, ] <- excess * clawback_rate
        
        # Final OAS after clawback
        income_cutoff <- ifelse(age < 75, income_cutoff_65_74, income_cutoff_75_plus)
        oas_received[t, ] <- ifelse(age >= start_age & alive & income_for_clawback < income_cutoff,
                                    pmax(0, gross_oas - clawback_amount[t, ]), 0)
        
        # Taxable income (withdrawals + OAS for RRSP)
        taxable_income[t, ] <- ifelse(acct == "RRSP" & alive, withdrawals[t, ] + oas_received[t, ], 0)
        
        # Tax
        total_tax[t, ] <- ifelse(acct == "RRSP" & alive, calculate_total_tax(taxable_income[t, ]), 0)
        
        # Net income (withdrawals + OAS - tax - clawback)
        net_income[t, ] <- withdrawals[t, ] + oas_received[t, ] - total_tax[t, ] - clawback_amount[t, ]
        
        # Update portfolio
        portfolio[t + 1, ] <- pmax(0, (portfolio[t, ] - withdrawals[t, ]) * (1 + portfolio_returns[t, ]))
        
        # PV of OAS (3% discount)
        pv_oas <- pv_oas + oas_received[t, ] / (1 + discount_rate)^(t - 1)
      }
      
      key <- paste(sex, acct, start_age, sep = "_")
      results[[key]] <- list(
        portfolio = portfolio,
        withdrawals = withdrawals,
        oas_received = oas_received,
        taxable_income = taxable_income,
        clawback_amount = clawback_amount,
        total_tax = total_tax,
        net_income = net_income,
        death_ages = death_ages,
        pv_oas = pv_oas
      )
    }
  }
}

# --- Data Processing ---
income_data <- data.frame()
for (key in names(results)) {
  res <- results[[key]]
  temp <- data.frame(
    Year = rep(1:years, n_sim),
    Simulation = rep(1:n_sim, each = years),
    Sex = strsplit(key, "_")[[1]][1],
    AccountType = strsplit(key, "_")[[1]][2],
    StartAge = as.integer(strsplit(key, "_")[[1]][3]),
    Age = rep(retirement_age:(retirement_age + years - 1), n_sim),
    Portfolio = as.vector(res$portfolio[1:years, ]),
    Withdrawal = as.vector(res$withdrawals),
    OAS = as.vector(res$oas_received),
    NetIncome = as.vector(res$net_income),
    Clawback = as.vector(res$clawback_amount),
    DeathAge = rep(res$death_ages, each = years),
    PV_OAS = rep(res$pv_oas, each = years)
  )
  income_data <- rbind(income_data, temp)
}

# --- Summary Statistics ---
oas_summary <- income_data %>%
  group_by(Sex, AccountType, StartAge, Simulation) %>%
  summarise(Total_OAS = sum(OAS), Total_PV_OAS = mean(PV_OAS), .groups = "drop") %>%
  group_by(Sex, AccountType, StartAge) %>%
  summarise(
    Mean_Total_OAS = mean(Total_OAS),
    Mean_PV_OAS = mean(Total_PV_OAS),
    .groups = "drop"
  )

# Calculate early return per simulation (first 10 years of portfolio returns)
early_returns <- apply(portfolio_returns[1:10, ], 2, function(x) prod(1 + x) - 1)

# Create summary data per simulation
sim_summary <- income_data %>%
  filter(AccountType == "RRSP") %>%
  group_by(Simulation, StartAge, Sex, AccountType) %>%
  summarise(
    total_net_oas = sum(OAS, na.rm = TRUE),
    total_clawback = sum(Clawback, na.rm = TRUE),
    death_age = max(DeathAge),
    .groups = "drop"
  ) %>%
  mutate(
    early_return = early_returns[Simulation],
    return_quartile = ntile(early_return, 4)
  )

# --- Visualizations (16 Graphs Total) ---

# --- Supplementary Graphs ---
# S1. Distribution of Death Ages
death_ages_data <- data.frame(
  Sex = rep(c("Both", "Male", "Female"), each = n_sim),
  DeathAge = c(death_ages_by_sex[["Both"]], death_ages_by_sex[["Male"]], death_ages_by_sex[["Female"]])
)

ps1 <- ggplot(death_ages_data, aes(x = DeathAge, fill = Sex)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(title = "S1: Distribution of Death Ages", x = "Age at Death", y = "Count", fill = "Sex") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(ps1)

# S2. Distribution of Portfolio Returns
ps2 <- ggplot(data.frame(Returns = as.vector(portfolio_returns)), aes(x = Returns)) +
  geom_histogram(binwidth = 0.01, fill = "blue", alpha = 0.7) +
  labs(title = "S2: Distribution of Annual Portfolio Returns", x = "Return", y = "Count") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(ps2)

# --- Main Graphs ---

# Chart 1: Mean Net OAS Received by Start Age: RRSP vs TFSA
account_stats <- oas_summary %>%
  group_by(AccountType, StartAge) %>%
  summarize(mean_net_oas = mean(Mean_Total_OAS), .groups = "drop")

p1 <- ggplot(account_stats, aes(x = StartAge, y = mean_net_oas, color = AccountType)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("darkgreen", "orange")) +
  labs(
    title = "Chart 1: Mean Net OAS Received by Start Age: RRSP vs TFSA",
    x = "OAS Start Age",
    y = "Mean Lifetime Net OAS ($)",
    color = "Account Type"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p1)

# Chart 2: OAS Clawback Probability by Start Age and Account Type
clawback_summary <- income_data %>%
  group_by(AccountType, StartAge, Simulation) %>%
  summarise(Clawback_Occurred = any(Clawback > 0), .groups = "drop") %>%
  group_by(AccountType, StartAge) %>%
  summarise(Prob_Clawback = mean(Clawback_Occurred), .groups = "drop")

p2 <- ggplot(clawback_summary, aes(x = factor(StartAge), y = Prob_Clawback, fill = AccountType)) +
  geom_col(position = "dodge") +
  labs(title = "Chart 2: OAS Clawback Probability by Start Age and Account Type",
       x = "OAS Start Age", y = "Probability of Clawback", fill = "Account Type") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p2)

# Chart 3: Mean Net OAS by Start Age for Long- vs Short-Lived Groups (RRSP)
longevity_net_oas <- sim_summary %>%
  mutate(Longevity_Group = ifelse(death_age >= 90, "Long-lived (>=90)", "Short-lived (<90)")) %>%
  group_by(Longevity_Group, StartAge) %>%
  summarise(Mean_Net_OAS = mean(total_net_oas), .groups = "drop")

p3 <- ggplot(longevity_net_oas, aes(x = StartAge, y = Mean_Net_OAS, color = Longevity_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Chart 3: Net OAS Received vs Start Age: Long-Lived vs Short-Lived (RRSP)",
    x = "OAS Start Age",
    y = "Mean Lifetime Net OAS ($)",
    color = "Longevity Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))
print(p3)

# Chart 4: Mean Total Clawback by Start Age for Long- vs Short-Lived Groups (RRSP)
longevity_claw <- sim_summary %>%
  mutate(Longevity_Group = ifelse(death_age >= 90, "Long-lived (>=90)", "Short-lived (<90)")) %>%
  group_by(Longevity_Group, StartAge) %>%
  summarize(mean_clawback = mean(total_clawback), .groups = "drop")

p4 <- ggplot(longevity_claw, aes(x = StartAge, y = mean_clawback, color = Longevity_Group)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Chart 4: Total OAS Clawed Back vs Start Age: Long-Lived vs Short-Lived (RRSP)",
    x = "OAS Start Age",
    y = "Mean Lifetime OAS Clawed Back ($)",
    color = "Longevity Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))
print(p4)

# Chart 5: OAS Received Based on Longevity
oas_longevity <- income_data %>%
  mutate(Longevity_Group = ifelse(DeathAge >= 90, "90+", "Under 90")) %>%
  group_by(Longevity_Group, StartAge, AccountType) %>%
  summarise(Mean_OAS = mean(OAS), .groups = "drop")

p5 <- ggplot(oas_longevity, aes(x = factor(StartAge), y = Mean_OAS, fill = AccountType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Longevity_Group) +
  labs(title = "Chart 5: OAS Received Based on Longevity",
       x = "OAS Start Age", y = "Mean Annual OAS ($)", fill = "Account Type") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p5)

# Chart 6: Optimal Start Age by Lifespan Heatmap
optimal_age_df <- income_data %>%
  group_by(Simulation, StartAge, Sex, AccountType) %>%
  summarise(Total_OAS = sum(OAS), DeathAge = max(DeathAge), .groups = "drop") %>%
  group_by(Simulation, Sex, AccountType) %>%
  mutate(Best_Start = StartAge[which.max(Total_OAS)],
         DeathAge = mean(DeathAge)) %>%
  distinct(Simulation, Sex, AccountType, Best_Start, DeathAge)

p6 <- ggplot(optimal_age_df, aes(x = DeathAge, fill = factor(Best_Start))) +
  geom_histogram(binwidth = 1, position = "fill") +
  facet_wrap(~AccountType + Sex, scales = "free_y") +
  labs(title = "Chart 6: Optimal OAS Start Age by Longevity",
       x = "Age at Death", y = "Proportion", fill = "Best Start Age") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))
print(p6)

# Chart 7: Clawback Exposure Over Time by Early Returns Quartile
# Compute early 10-year return per simulation
early_returns <- apply(portfolio_returns[1:10, ], 2, function(x) prod(1 + x) - 1)

# Assign Early Return Quartile to each simulation
early_return_df <- data.frame(Simulation = 1:n_sim, EarlyReturn = early_returns) %>%
  mutate(ReturnQuartile = ntile(EarlyReturn, 4))

# Merge quartiles into income_data
income_with_quartile <- income_data %>%
  filter(AccountType == "RRSP", StartAge == 65) %>%
  left_join(early_return_df, by = "Simulation")

# Compute % of retirees with clawback by age and return quartile
clawback_by_age_q <- income_with_quartile %>%
  group_by(ReturnQuartile, Age) %>%
  summarise(ClawbackRate = mean(Clawback > 0), .groups = "drop")

p7 <- ggplot(clawback_by_age_q, aes(x = Age, y = ClawbackRate, color = factor(ReturnQuartile))) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("red", "orange", "blue", "green"),
    labels = c("Q1 (Worst)", "Q2", "Q3", "Q4 (Best)")
  ) +
  labs(
    title = "Chart 7: Clawback Exposure Over Time by Early Returns Quartile (RRSP, OAS @65)",
    x = "Age",
    y = "Fraction with OAS Clawback",
    color = "Return Quartile"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p7)

# Chart 8: Mean Net OAS Received by Start Age, by Early Return Quartile (RRSP)
netoas_by_start_q <- sim_summary %>%
  group_by(return_quartile, StartAge) %>%
  summarise(mean_net_oas = mean(total_net_oas), .groups = "drop") %>%
  mutate(Quartile_Label = factor(return_quartile,
                                 levels = c(1, 2, 3, 4),
                                 labels = c("Q1 (Worst)", "Q2", "Q3", "Q4 (Best)")))

quartile_colors <- c("Q1 (Worst)" = "red",
                     "Q2" = "orange",
                     "Q3" = "blue",
                     "Q4 (Best)" = "darkgreen")

p8 <- ggplot(netoas_by_start_q, aes(x = StartAge, y = mean_net_oas, color = Quartile_Label)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = quartile_colors) +
  labs(
    title = "Chart 8: Mean Net OAS Received by Start Age, by Early Return Quartile (RRSP)",
    x = "OAS Start Age",
    y = "Mean Lifetime Net OAS ($)",
    color = "Early Return Quartile"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p8)

# --- Sensitivity Analysis Section ---

# Function to run a simplified simulation focusing on OAS outcomes
simulate_sensitivity_OAS <- function(deferral_rate_val, discount_rate_val, 
                                     stock_weight_val, account_type = "RRSP", 
                                     sex = "Both", start_age = 70) {
  px <- if (sex == "Both") px_both else if (sex == "Male") px_male else px_female
  death_ages <- simulate_deaths(px, n_sim)
  
  # Adjust portfolio returns for stock weight
  bond_weight_val <- 1 - stock_weight_val
  portfolio_returns <- stock_weight_val * stock_returns + bond_weight_val * bond_returns
  
  portfolio <- matrix(initial_balance, years + 1, n_sim)
  oas_received <- matrix(0, years, n_sim)
  pv_oas <- numeric(n_sim)
  
  # Adjust OAS based on deferral rate and start age
  oas_adj_65_74 <- oas_base_65_74 * (1 + deferral_rate_val * (start_age - 65))
  oas_adj_75_plus <- oas_base_75_plus * (1 + deferral_rate_val * (start_age - 65))
  
  for (t in 1:years) {
    age <- retirement_age + t - 1
    alive <- age < death_ages
    remaining <- pmax(1, death_ages - age)
    ws_withdrawal <- ifelse(alive, portfolio[t, ] / remaining, 0)
    withdrawals <- pmin(ws_withdrawal, portfolio[t, ])
    
    gross_oas <- ifelse(age >= start_age & alive,
                        ifelse(age < 75, oas_adj_65_74, oas_adj_75_plus), 0)
    income_for_clawback <- ifelse(account_type == "RRSP" & alive, withdrawals, 0) + gross_oas
    excess <- pmax(0, income_for_clawback - clawback_threshold)
    clawback_amount <- excess * clawback_rate
    income_cutoff <- ifelse(age < 75, income_cutoff_65_74, income_cutoff_75_plus)
    net_oas <- ifelse(age >= start_age & alive & income_for_clawback < income_cutoff,
                      pmax(0, gross_oas - clawback_amount), 0)
    oas_received[t, ] <- net_oas
    
    portfolio[t + 1, ] <- pmax(0, (portfolio[t, ] - withdrawals) * (1 + portfolio_returns[t, ]))
    pv_oas <- pv_oas + net_oas / (1 + discount_rate_val)^(t - 1)
  }
  return(data.frame(PV_OAS = pv_oas))
}

# Graph A1: Sensitivity of Mean PV OAS to Stock Allocation
stock_weights <- seq(0.3, 0.7, by = 0.1)
sens_alloc <- data.frame(stock_weight = stock_weights, mean_PV_OAS = NA)
for (i in seq_along(stock_weights)) {
  res <- simulate_sensitivity_OAS(deferral_rate_val = 0.072, 
                                  discount_rate_val = 0.03,
                                  stock_weight_val = stock_weights[i],
                                  account_type = "RRSP", sex = "Both", start_age = 70)
  sens_alloc$mean_PV_OAS[i] <- mean(res$PV_OAS)
}
p9 <- ggplot(sens_alloc, aes(x = stock_weight, y = mean_PV_OAS)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point(size = 2) +
  labs(title = "Chart 9: Sensitivity of Mean PV OAS to Stock Allocation",
       x = "Stock Weight in Portfolio", y = "Mean Present Value of OAS ($)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p9)

# Graph A2: Sensitivity of Mean PV OAS to Deferral Rate
deferral_vals <- seq(0.04, 0.10, by = 0.01)
sens_def <- data.frame(deferral_rate = deferral_vals, mean_PV_OAS = NA)
for (i in seq_along(deferral_vals)) {
  res <- simulate_sensitivity_OAS(deferral_rate_val = deferral_vals[i], 
                                  discount_rate_val = 0.03,
                                  stock_weight_val = 0.5,
                                  account_type = "RRSP", sex = "Both", start_age = 70)
  sens_def$mean_PV_OAS[i] <- mean(res$PV_OAS)
}
p10 <- ggplot(sens_def, aes(x = deferral_rate, y = mean_PV_OAS)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point(size = 2) +
  labs(title = "Chart 10: Sensitivity of Mean PV OAS to Deferral Rate",
       x = "Deferral Rate", y = "Mean Present Value of OAS ($)") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(p10)

# Function to run simulation with adjustable parameters for initial balance and clawback threshold
run_simulation <- function(initial_balance, clawback_threshold, stock_weight_val = 0.5) {
  bond_weight_val <- 1 - stock_weight_val
  portfolio_returns <- stock_weight_val * stock_returns + bond_weight_val * bond_returns
  
  px <- px_both
  death_ages <- simulate_deaths(px, n_sim)
  portfolio <- matrix(initial_balance, years + 1, n_sim)
  withdrawals <- matrix(0, years, n_sim)
  oas_received <- matrix(0, years, n_sim)
  clawback_amount <- matrix(0, years, n_sim)
  pv_oas <- numeric(n_sim)
  start_age <- 65
  
  oas_adj_65_74 <- oas_base_65_74 * (1 + deferral_rate * (start_age - 65))
  oas_adj_75_plus <- oas_base_75_plus * (1 + deferral_rate * (start_age - 65))
  
  for (t in 1:years) {
    age <- retirement_age + t - 1
    alive <- age < death_ages
    remaining <- pmax(1, death_ages - age)
    ws_withdrawal <- ifelse(alive, portfolio[t, ] / remaining, 0)
    rrif_factor <- ifelse(age >= 71 & age <= 110, rrif_factors[age - 70], 0)
    rrif_withdrawal <- ifelse(alive, portfolio[t, ] * rrif_factor, 0)
    withdrawals[t, ] <- ifelse(age >= 71 & alive, pmax(ws_withdrawal, rrif_withdrawal), ws_withdrawal)
    withdrawals[t, ] <- pmin(withdrawals[t, ], portfolio[t, ])
    
    gross_oas <- ifelse(age >= start_age & alive, ifelse(age < 75, oas_adj_65_74, oas_adj_75_plus), 0)
    income_for_clawback <- ifelse(alive, withdrawals[t, ], 0) + gross_oas
    excess <- pmax(0, income_for_clawback - clawback_threshold)
    clawback_amount[t, ] <- excess * clawback_rate
    income_cutoff <- ifelse(age < 75, income_cutoff_65_74, income_cutoff_75_plus)
    oas_received[t, ] <- ifelse(age >= start_age & alive & income_for_clawback < income_cutoff,
                                pmax(0, gross_oas - clawback_amount[t, ]), 0)
    portfolio[t + 1, ] <- pmax(0, (portfolio[t, ] - withdrawals[t, ]) * (1 + portfolio_returns[t, ]))
    pv_oas <- pv_oas + oas_received[t, ] / (1 + discount_rate)^(t - 1)
  }
  
  list(
    total_oas = colSums(oas_received),
    clawback_prob = mean(apply(clawback_amount > 0, 2, any)),
    pv_oas = pv_oas
  )
}

# Define scenarios for Initial Balance and Clawback Threshold
initial_balances <- c(500000, 1000000, 1500000)
clawback_thresholds <- c(86912 * 0.9, 86912, 86912 * 1.1)

results_ib <- lapply(initial_balances, function(ib) {
  run_simulation(ib, 86912)
})
results_ct <- lapply(clawback_thresholds, function(ct) {
  run_simulation(1000000, ct)
})

sensitivity_ib <- data.frame(
  Initial_Balance = initial_balances,
  Mean_Total_OAS = sapply(results_ib, function(x) mean(x$total_oas)),
  Clawback_Probability = sapply(results_ib, function(x) x$clawback_prob),
  Mean_PV_OAS = sapply(results_ib, function(x) mean(x$pv_oas))
)

sensitivity_ct <- data.frame(
  Clawback_Threshold = clawback_thresholds,
  Mean_Total_OAS = sapply(results_ct, function(x) mean(x$total_oas)),
  Clawback_Probability = sapply(results_ct, function(x) x$clawback_prob),
  Mean_PV_OAS = sapply(results_ct, function(x) mean(x$pv_oas))
)

# Graph B1: Impact of Initial Balance on Mean Total OAS
p11 <- ggplot(sensitivity_ib, aes(x = Initial_Balance, y = Mean_Total_OAS)) +
  geom_line(color = "blue", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 11: Impact of Initial Balance on Mean Total OAS",
       x = "Initial Balance ($)", y = "Mean Total OAS ($)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p11)

# Graph B2: Impact of Initial Balance on Clawback Probability
p12 <- ggplot(sensitivity_ib, aes(x = Initial_Balance, y = Clawback_Probability)) +
  geom_line(color = "red", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 12: Impact of Initial Balance on Clawback Probability",
       x = "Initial Balance ($)", y = "Probability of Clawback") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p12)

# Graph B3: Impact of Clawback Threshold on Mean Total OAS
p13 <- ggplot(sensitivity_ct, aes(x = Clawback_Threshold, y = Mean_Total_OAS)) +
  geom_line(color = "blue", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 13: Impact of Clawback Threshold on Mean Total OAS",
       x = "Clawback Threshold ($)", y = "Mean Total OAS ($)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p13)

# Graph B4: Impact of Clawback Threshold on Clawback Probability
p14 <- ggplot(sensitivity_ct, aes(x = Clawback_Threshold, y = Clawback_Probability)) +
  geom_line(color = "red", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 14: Impact of Clawback Threshold on Clawback Probability",
       x = "Clawback Threshold ($)", y = "Probability of Clawback") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p14)

# --- Additional Graphs for Sensitivity Analysis (Stock Market Returns) ---

# Function to run simulation with adjustable stock market returns
run_simulation_stock_returns <- function(regime_stock_means_val) {
  stock_returns <- matrix(0, years, n_sim)
  bond_returns <- matrix(0, years, n_sim)
  regime_matrix <- matrix(1, years, n_sim)
  for (sim in 1:n_sim) {
    for (year in 1:years) {
      current_state <- regime_matrix[year, sim]
      stock_returns[year, sim] <- rnorm(1, regime_stock_means_val[current_state], regime_stock_sds[current_state])
      bond_returns[year, sim] <- rnorm(1, regime_bond_means[current_state], regime_bond_sds[current_state])
      if (year < years) {
        regime_matrix[year + 1, sim] <- sample(1:3, 1, prob = transition_matrix[current_state, ])
      }
    }
  }
  portfolio_returns <- stock_weight * stock_returns + bond_weight * bond_returns
  
  px <- px_both
  death_ages <- simulate_deaths(px, n_sim)
  portfolio <- matrix(initial_balance, years + 1, n_sim)
  withdrawals <- matrix(0, years, n_sim)
  oas_received <- matrix(0, years, n_sim)
  clawback_amount <- matrix(0, years, n_sim)
  pv_oas <- numeric(n_sim)
  start_age <- 65
  
  oas_adj_65_74 <- oas_base_65_74 * (1 + deferral_rate * (start_age - 65))
  oas_adj_75_plus <- oas_base_75_plus * (1 + deferral_rate * (start_age - 65))
  
  for (t in 1:years) {
    age <- retirement_age + t - 1
    alive <- age < death_ages
    remaining <- pmax(1, death_ages - age)
    ws_withdrawal <- ifelse(alive, portfolio[t, ] / remaining, 0)
    rrif_factor <- ifelse(age >= 71 & age <= 110, rrif_factors[age - 70], 0)
    rrif_withdrawal <- ifelse(alive, portfolio[t, ] * rrif_factor, 0)
    withdrawals[t, ] <- ifelse(age >= 71 & alive, pmax(ws_withdrawal, rrif_withdrawal), ws_withdrawal)
    withdrawals[t, ] <- pmin(withdrawals[t, ], portfolio[t, ])
    
    gross_oas <- ifelse(age >= start_age & alive, ifelse(age < 75, oas_adj_65_74, oas_adj_75_plus), 0)
    income_for_clawback <- ifelse(alive, withdrawals[t, ], 0) + gross_oas
    excess <- pmax(0, income_for_clawback - clawback_threshold)
    clawback_amount[t, ] <- excess * clawback_rate
    income_cutoff <- ifelse(age < 75, income_cutoff_65_74, income_cutoff_75_plus)
    oas_received[t, ] <- ifelse(age >= start_age & alive & income_for_clawback < income_cutoff,
                                pmax(0, gross_oas - clawback_amount[t, ]), 0)
    portfolio[t + 1, ] <- pmax(0, (portfolio[t, ] - withdrawals[t, ]) * (1 + portfolio_returns[t, ]))
    pv_oas <- pv_oas + oas_received[t, ] / (1 + discount_rate)^(t - 1)
  }
  
  list(
    total_oas = colSums(oas_received),
    clawback_prob = mean(apply(clawback_amount > 0, 2, any)),
    pv_oas = pv_oas
  )
}

# Define scenarios for stock market returns (adjusting Bull regime mean)
stock_return_scenarios <- list(
  c(0.07, -0.15, 0.03),  # Lower Bull returns
  c(0.09, -0.15, 0.03),  # Base case
  c(0.11, -0.15, 0.03)   # Higher Bull returns
)

results_sr <- lapply(stock_return_scenarios, function(sr) {
  run_simulation_stock_returns(sr)
})

sensitivity_sr <- data.frame(
  Bull_Return = c(0.07, 0.09, 0.11),
  Mean_Total_OAS = sapply(results_sr, function(x) mean(x$total_oas)),
  Clawback_Probability = sapply(results_sr, function(x) x$clawback_prob),
  Mean_PV_OAS = sapply(results_sr, function(x) mean(x$pv_oas))
)

# Graph C1: Impact of Stock Market Returns on Mean Total OAS
p15 <- ggplot(sensitivity_sr, aes(x = Bull_Return, y = Mean_Total_OAS)) +
  geom_line(color = "blue", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 15: Impact of Stock Market Returns on Mean Total OAS",
       x = "Bull Market Stock Return", y = "Mean Total OAS ($)") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p15)

# Graph C2: Impact of Stock Market Returns on Clawback Probability
p16 <- ggplot(sensitivity_sr, aes(x = Bull_Return, y = Clawback_Probability)) +
  geom_line(color = "red", size = 1.2) + 
  geom_point(size = 3) +
  labs(title = "Chart 16: Impact of Stock Market Returns on Clawback Probability",
       x = "Bull Market Stock Return", y = "Probability of Clawback") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))
print(p16)