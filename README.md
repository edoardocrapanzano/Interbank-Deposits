# The Architecture of Liquidity in the Italian Electronic Market for Interbank Deposits

## 📌 Project Overview
This study presents a longitudinal structural analysis of the Italian electronic Interbank Market (e-Mid) over a decade spanning the introduction of the Euro and the 2008 financial crisis. By integrating advanced graph theory visualization with Linear Mixed Model (LMM) econometrics, I document the existence of a hierarchical, scale-free network characterized by a "Robust-yet-Fragile" architecture. My analysis reveals a significant asymmetry in market integration: while domestic small banks are deepl embedded in the core, foreign institutions remain peripheral appendages. Econometric analysis using LMM reveals a dichotomy in pricing mechanisms: borrowing costs are driven by reputational factors (Eigenvector Centrality), whereas lending yields are determined by intermediation power (Betweenness Centrality). Furthermore, Principal Component Analysis (PCA) identifies a regime shift post-Euribor manipulation (started in 2005), where the structural profit advantages of core banks were eroded by a non-linear explosion in borrowing spreads, signalling a breakdown in the market’s arbitrage mechanism
during the financial crisis.

## 🎯 Objectives
Specifically, I investigate how the "Core-Periphery" architecture creates a tiered system of access, how this structure creates asymmetric pricing power in borrowing versus lending, and how the network's structural integrity degraded during the transition from the stability of the early 2000s to the turmoil of the Euribor manipulation period (2005-2008), followed by the 2008 financial crisis.

## 🛠️ Tech Stack & Methodology
* **Languages:** R version 4.4.1;
* **Libraries/Packages:** tidyverse, igraph, scales, plotly, mgcv, lubridate, lme4, lmerTest;  
* **Methodologies:** graph theory (diameter, incoming/outgoing degree, strength, clustering coefficient), centrality measures (total degree, closeness, betweenness, eigenvector), regression analysis, Principal Component Analysis.

## 📊 Data
The study relies on monthly transaction data from Italian electronic Market for Interbank Deposits. For each bank I have its ID, the size, the number of lending and borrowing transactions, the traded volume, the lending and the borrowing spreads. Also, I have all the links between the banks (e.g. a link between bank 122 and bank 7 exist if a transaction occurred) and they are pointing from the lender to the borrower to follow the flow of liquidity. To handle the complexity of this dataset, I employ a multi-layered methodological approach: I utilize four distinct layouts (Fruchterman-Reingold, Kamada-Kawai, Circle, Sugiyama) to visualize the geometry of liquidity flows and assess spatial topology. Then, I display the degree distributions (Kin, Kout), node strength distribution (Si) and the probability distribution of total degree with regards to a single snapshot of the network in April 2004 (pre-Euribor manipulation and before 2008 financial crisis). I analyse the probability distributions of clustering and diameter to quantify the network's heterogeneity along all the periods (1999-2009). I use Linear Mixed Models (LMM) to estimate borrowing and lending spreads, controlling for bank size groups, temporal trends, standardized centrality measures and unobserved bank heterogeneity via random effects. Finally, I employ Principal Component Analysis (PCA) to reduce the multidimensionality of network metrics into
interpretable components (e.g. Network Structure vs. Systemic Importance), in the first instance, to track bank profitability profiles at the single snapshot in April 2004 and in the second instance, to visualise temporal shifts in network structure from January 1999 to December 2009.

## 💡 Key Findings
* The market is structurally "Robust-yet-Fragile”: the scale-free degree distribution and the dependence on a few "Very Large" domestic hubs created a system resilient to random peripheral failures but extremely vulnerable to the crisis of core nodes.
* The pricing power is structurally determined and asymmetric. Borrowing is a reputation game where "who you know" (Eigenvector) reduces costs, while lending is a power game where "where you sit" (Betweenness) increases yields. This confirms that the interbank market is a hierarchy where topology dictates terms.
* Since most nodes are peripheral (low degree), a random shock is statistically likely to hit a non-systemic bank, causing negligible damage to the overall connectivity.Conversely, the network is extremely fragile to shocks targeting the Hubs (the tail nodes). This confirms that the "Too Big to Fail" problem is a structural feature of the network topology itself.

## 🚀 How to run the code
The main analysis can be found in the `interbank_deposits.R` notebook.

[📄 Read the full report (PDF)](./report_interbank_deposits.pdf)]
