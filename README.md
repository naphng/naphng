# Asset Pricing and Idiosyncratic Momentum (iMOM) Anomaly in the Vietnamese Stock Market

## Project Overview
This project investigates asset pricing models and the extended Fama-French framework, with a specific focus on testing the idiosyncratic momentum (iMOM) anomaly within the Vietnamese stock market. The analysis aims to provide empirical evidence on risk factors and momentum signals for non-financial firms, serving as a quantitative basis for investment strategies.

## Tech Stack & Methodologies
- **Language:** R
- **Key Libraries:** `dplyr`, `lmtest`, `sandwich`, `car`, `tseries`, `ggplot2`
- **Methodologies:** - Data wrangling and cleaning for Vietnamese non-financial corporate datasets.
  - Risk factor calculation including Size (SMB), Value (HML), Standard Momentum (MOM), and Idiosyncratic Momentum (iMOM) via 11-month rolling regressions.
  - Multicollinearity checks (Condition Number, VIF) and Newey-West robust standard errors.
  - Portfolio sorting (by size, B/M, and iMOM signals) and GRS (Gibbons, Ross, Shanken) statistical testing.

## Key Empirical Findings

**1. iMOM Outperforms Standard Factors**
Descriptive statistics reveal that the Idiosyncratic Momentum (iMOM) factor generated the highest mean monthly return (**1.34%**) and the highest Sharpe ratio (**0.804**) among all tested factors, significantly outperforming the standard Market (Sharpe = 0.426) and traditional Momentum (Sharpe = 0.366) factors.

**2. Significant Alpha Generation**
When regressing the iMOM factor against the traditional Carhart 4-factor model, iMOM still yields a statistically significant positive intercept (Alpha = **0.0103**, p < 0.05). This confirms that idiosyncratic momentum captures a unique premium that cannot be explained by standard Market, Size, Value, and Momentum risks.

**3. Superior Pricing Model (GRS Test)**
A GRS test was conducted on 5 portfolios sorted by iMOM signals to evaluate model efficiency. The proposed model incorporating iMOM leaves the least unexplained pricing error, proving to be the most robust model for these test assets.

| Asset Pricing Model | GRS Statistic | P-Value | Conclusion |
| :--- | :---: | :---: | :--- |
| **Fama-French 3** | 1.2195 | 0.3087 | Acceptable fit |
| **Carhart 4 (w/ MOM)** | 1.9380 | 0.0985 | Marginal fit (weakest) |
| **Proposed (w/ iMOM)** | **0.9613** | **0.4473** | **Best fit** (Lowest GRS, Highest p-value) |
*(Note: A smaller GRS Statistic and a higher P-value indicate a better pricing model that fully captures expected returns).*

## Visualizations
<img width="547" height="501" alt="image" src="https://github.com/user-attachments/assets/5c9a34e7-efd6-4031-82f8-4a529ae8719f" />

## Financial & Business Implications
The empirical results provide highly actionable insights for quantitative portfolio management in the Vietnamese equity market. 
- **Factor-Based Screening:** Standard momentum (MOM) often suffers from high volatility and crashes. Filtering stocks based on firm-specific (idiosyncratic) momentum yields a cleaner, more robust signal with superior risk-adjusted returns.
- **Strategy Development:** Quantitative analysts can leverage the iMOM premium by systematically going long on high iMOM quintiles and shorting low iMOM quintiles, as this anomaly persists even after adjusting for traditional Fama-French risk factors.
