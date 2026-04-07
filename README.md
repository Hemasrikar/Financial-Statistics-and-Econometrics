# Asset Pricing Factor Analysis
 
Two Jupyter notebooks implementing and comparing factor models for cross-sectional asset pricing using the **Fama-Macbeth regression** framework on 25 Fama-French size/BEME portfolios.
 
## Notebooks
 
**`forming_some_factors.ipynb`** - Factor construction
- Computes excess returns (25 portfolios minus RF)
- Builds **in-sample PCA factors**: eigendecomposition of the full-sample covariance matrix, top 3 PCs
- Builds **out-of-sample PCA factors**: eigenvectors estimated on odd months, applied to even months (and vice versa)
- Visualises covariance matrices and cross-factor correlation heatmap
 
**`asset_pricing_tests.ipynb`** - Model testing & comparison
- Runs Fama-Macbeth two-pass regressions for four factor sets:
  - Fama-French 3 factors (Mkt-RF, SMB, HML)
  - Macroeconomic factors (Div growth, DEF, TERM)
  - In-sample PC factors
  - Out-of-sample PC factors
- Compares models on time-series R², cross-sectional R², and pricing errors (α)
- Checks multicollinearity via VIF
- Exports betas, gammas, and R² tables to `asset_pricing_results/`
 
## Data
 
Place `Data_FE.xlsx` in a `data/` folder. Required sheets:
- `25 Size and BEME portfolios`
- `Fama-French factors`
- `Macroeconomic factors`
 
### Dependencies
 
```bash
pip install pandas numpy matplotlib seaborn statsmodels
```
---
The Final report can be found [here](/report.pdf)