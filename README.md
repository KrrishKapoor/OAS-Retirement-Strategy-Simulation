# 📊 OAS Strategy Simulation – Retirement Planning in R

This project models optimal Old Age Security (OAS) withdrawal strategies for Canadian retirees using **72,000 Monte Carlo simulations**. It examines how account type, longevity, and market conditions influence the trade-off between **early OAS access** and **maximizing lifetime benefits** by deferring to age 70.

The simulation incorporates:
- Dynamic mortality risk
- Canadian tax regulations
- OAS clawback mechanisms
- Sequence of returns risk
- Regime-switching market returns
- Both RRSP and TFSA withdrawal frameworks

> ✅ Built entirely in **R**, with personalized strategy recommendations based on demographic and portfolio conditions.

---

## 🧠 Key Questions Explored

- When should retirees claim OAS — at age 65 or defer to age 70?
- How do RRSP vs. TFSA withdrawals impact tax burden and OAS clawbacks?
- How do market sequences and lifespan affect portfolio longevity?
- What are the optimal OAS start ages for different demographic profiles?

---

## 🔧 Methodology

- **Simulation engine:** R-based Monte Carlo with 2,000 simulations per condition
- **Parameters:**  
  - Start age: 65–70  
  - Account type: RRSP vs. TFSA  
  - Gender: Male / Female / Combined  
  - Market returns: Modeled via 3-regime Markov switching (bull, bear, neutral)  
  - Mortality: Ontario survival probabilities (age 65–110)

- **Withdrawals:** Waring-Siegel method  
- **Taxation:** Full provincial + federal tax rates applied to RRSP  
- **OAS clawback:** Tracked annually based on taxable income vs. clawback threshold

---

## 📈 Key Findings

### 🏦 Account Type
- **TFSA**: Zero clawback risk. Delaying OAS to 70 always increases lifetime benefits.
- **RRSP**: 80%+ of retirees face clawbacks due to higher taxable income. Early withdrawals may reduce exposure.

### 👵 Longevity Effects
- Long-lived retirees benefit more from deferral (esp. women).
- Shorter-lived retirees see minimal gain from deferring due to limited years to enjoy higher payments.

### 📉 Sequence of Returns
- **Negative early returns**: Lower taxable withdrawals → less clawback → deferral to 70 is ideal.
- **Positive early returns**: Larger RRSP growth → higher mandatory withdrawals → clawbacks increase → deferring may backfire.

---

## 📂 Project Files

| File | Description |
|------|-------------|
| `OAS Clawback Analysis.R` | Full R simulation code with all logic and outputs |
| `Balancing Benefits and Clawbacks: A Retirement Strategy Model.pdf` | Final report summarizing methodology and findings |
| `README.md` | This documentation file |

---

## 🛠 Tools & Technologies

- R (base R, tidyverse, dplyr, ggplot2)
- Statistical simulation & data visualization
- Financial modeling
- Tax policy integration

---

## 📌 Takeaway

> "The optimal OAS strategy isn’t one-size-fits-all. Longevity, account type, and market timing all matter. This model helps retirees make personalized, tax-efficient decisions."

---
