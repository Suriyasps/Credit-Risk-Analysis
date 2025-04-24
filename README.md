# Predictive Analytics and Customer Insights: Advanced Machine Learning for Loan Default Risk

**Project By**: Suriya Subbiah Perumal  

---

## 📌 Abstract
Traditional credit scoring models often fail in today's dynamic financial landscape. This research improves upon them by integrating advanced machine learning models — Logistic Regression, Random Forest, XGBoost, SVM, and ANN — with optimization and interpretability techniques. Using the "Bank Loan Defaulter Prediction Hackathon" dataset, models outperformed baseline accuracy by at least 20%. Interpretability tools like SHAP, ALE, and CIME ensured ethical transparency.

---

## 🧠 Methodology (CRISP-DM Framework)
### 1. Business & Data Understanding
- Dataset includes borrower demographics, financial status, and loan attributes.
- Key variables: loan amount, interest rate, credit score, home ownership, inquiries, and public records.

### 2. Data Preparation
- **Cleaning**: Removed NAs, handled outliers using Z-scores.
- **Feature Selection**: Lasso regression to identify top predictors.
- **Balancing**: SMOTE for class imbalance.

### 3. Modelling
- Logistic Regression, Random Forest, XGBoost, SVM, ANN, Naive Bayes.
- Tuned using **Bayesian Optimization** and validated via **5-fold Cross-Validation**.

### 4. Evaluation Metrics
- Accuracy, Precision, Recall, F1 Score, AUC.

### 5. Interpretability Tools
- **SHAP**: Explains individual predictions using game theory.
- **ALE**: Visualizes effects of features locally.
- **CIME**: Measures contextual importance and utility.

### 6. Ethical Considerations
- Compliance with **GDPR** and fairness audits to mitigate bias.

---

## 📊 Exploratory Data Analysis
- **Loan Amount Distribution**: Multimodal peaks suggest clustering.
- **Interest Rate vs Loan Amount**: Shows spread across credit grades.
- **Sunburst Plot**: Correlates ownership type, credit grade, and loan default.
- **Scaled Metric Trends**: Visual comparison across interest rate bins.
- **Recoveries vs Loan Amount**: High recoveries in high-risk loans.

---

## ⚙️ Model Performance Summary
| Model               | Accuracy | Sensitivity | Specificity | AUC  |
|--------------------|----------|-------------|-------------|------|
| XGBoost            | **93.1%**| 90%         | 85.5%       | 0.94 |
| Random Forest      | 91.2%    | 88%         | 83.2%       | 0.92 |
| ANN                | 90.5%    | 85%         | 82.7%       | 0.91 |
| SVM                | 88.3%    | 83%         | 80.4%       | 0.89 |
| Logistic Regression| 85.6%    | 80%         | 78.5%       | 0.86 |
| Naive Bayes        | 83.1%    | 76%         | 77.2%       | 0.84 |

---

## 🔍 Interpretability Results
- **SHAP**: Top features include loan amount, interest rate, credit score.
- **ALE**: Interest rate has strong non-linear effects.
- **CIME**: Highlights contextual drivers like home ownership and loan term.

---

## 🧪 Tools Used
- **Languages**: R, Python
- **Libraries**: `caret`, `xgboost`, `randomForest`, `e1071`, `keras`, `pROC`, `ggplot2`, `SHAPforxgboost`

---

## 💡 Key Innovations
- Integration of **Bayesian Optimization** for hyperparameter tuning.
- Use of **SHAP, ALE, and CIME** for explainability.
- Rigorous CRISP-DM-driven methodology.

---

## 🧭 Technical Execution Logbook
- **Week 1–2**: Data cleaning and acquisition
- **Week 3**: Business problem framing
- **Week 4–5**: Literature review, feature selection
- **Week 6–8**: Model development and tuning
- **Week 9–11**: Validation and testing
- **Week 12–14**: Compilation, review, and editing

---

## 📌 Conclusion
- **XGBoost** was the most effective model with high accuracy and AUC.
- SHAP and ALE made complex models interpretable, supporting ethical deployment.
- Demonstrated potential for ML to revolutionize credit scoring.

---

## 🔮 Future Work
- Incorporate alternative data (e.g., psychometrics, mobile usage).
- Explore quantum computing for model training.
- Extend explainability with LLM-powered natural language explanations.

---






