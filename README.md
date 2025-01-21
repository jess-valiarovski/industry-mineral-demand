## 📊 **Industry Mineral Demand Analysis**  

### 🚀 **Project Title:**  
**Statistical Analysis of Critical Minerals Demand in Clean Energy Industries**  

📅 **Timeframe:** September 2024 - December 2024  
📍 **Author:** **Jess Valiarovski**  
📚 **Course:** **Applied Biostatistics BIOL 3272**  
🎓 **Acknowledgements:** Thank you, **Yaniv Brandvain** and **Abigail Meyer**!  

---

## **1. Project Overview**  
The transition from **fossil fuels** to **low or zero-carbon energy** is a major challenge for **clean energy industries**.  
This project examines how **mineral demand** varies across industries using **linear regression, bootstrapping**, and **pairwise statistical tests** to provide robust insights into demand patterns using **R-programming**.  

### 🔍 **Objectives**  
- **Test Hypothesis:** Does mineral demand significantly differ across industries?  
- **Compare Statistical Methods:** Linear modeling vs. resampling techniques.  
- **Apply Statistical Modeling:** **ANOVA, bootstrapping, and multiple comparisons** to detect industry-specific trends.  

### 🔬 **Dataset Details**  
- **Source:** International Energy Agency (IEA) Critical Minerals Survey 2023  
- **Data Type:** Mineral demand in **kilotons (kT)** per industry  

---

## 🛠️ **2. Methods & Tools**  

### 🔍 **Statistical Methods Applied**  
- **Data Cleaning & Transformation:** `dplyr`, `tidyverse`, `janitor`  
- **Statistical Modeling:** `lm()` for linear modeling and bootstrapping  
- **Multiple Comparisons:** Holm’s correction  
- **Effect Size Analysis:** Cohen’s d  

### 📊 **Data Visualization (`ggplot2`)**  
- Violin plots for demand distributions  
- Bootstrapped confidence intervals to highlight sampling variability  
- Pairwise comparisons with **Holm correction** to identify significant differences  

---

## **3. Key Findings & Visuals**  

### 🛠 **Key Results from Statistical Analysis**  
📌 **Industry sector influences mineral demand significantly** (**p = 0.0395 < 0.05, F = 2.9391**).  
📌 **Electric Vehicles (EVs)** have the highest demand, while **Hydrogen Technologies** have the lowest.  
📌 **Bootstrapped confidence intervals** show that all industries significantly differ.  
📌 **Without bootstrapping**, industry demand appears **similar**, likely due to **low sample sizes**.  

### 📊 **Key Figures**  
1️⃣ **Log-Transformed Demand Visualization**  
*To reduce residual variance & improve linear model assumptions.*  

2️⃣ **Industry Demand Variability & Confidence Intervals**  
*ANOVA post-hoc comparisons using Holm’s correction to identify significant differences in demand.*  

---

🛠️ **Technical Skills Demonstrated**

- **Data Wrangling:** `dplyr`, `tidyverse`, `janitor`  
- **Statistical Modeling:** `lm()`, `anova()`, `emmeans()`  
- **Resampling Techniques:** `boot`, `infer`  
- **Data Visualization:** `ggplot2`, **violin plots**, **bootstrapped CI**  
- **Effect Size & Significance Testing:** **Cohen’s d**, **Holm’s correction**  
- **Hypothesis Testing:** **Type II ANOVA, multiple comparisons**  

---

## 📢 **How to View the Analysis**  
📌 **GitHub Pages Link:**  
📎 **[View the Full Report Here](https://jess-valiarovski.github.io/industry-mineral-demand/)**  

---

## **5. Next Steps!**  

🔹 **Identify which minerals** each industry uses the most (e.g., **Copper, Selenium**) and compare the data to **global resource scarcity** and **the mining benefits and risks/costs**.  
🔹 **Try out Bayesian methods** to quantify uncertainty in demand estimates.  

---

👩‍💻 **Connect with Me!**  
📬 Feel free to reach out for any questions, feedback, or collaborations.  
