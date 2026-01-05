# 🏙️ Urbanization and Housing Affordability — Data Science Project  

---

## 🪜 **Step 1: Data Importing**
The dataset titled **[Global Housing Market Analysis (2015–2024)](https://www.kaggle.com/datasets/atharvasoundankar/global-housing-market-analysis-2015-2024)** was obtained from **Kaggle**.  
It covers global housing and economic indicators from 2015 to 2024 for multiple countries, focusing on housing affordability, prices, and urban development.

The data was imported into R and prepared for analysis. Initial inspection (using `head()` and `View()`) confirmed proper structure and variable names.

---

## 🪜 **Step 2: Feature Selection**
The dataset originally contained 11 variables, including housing, rent, inflation, and construction indexes.  
For this study, only the variables relevant to **urbanization and housing affordability** were retained:

| Variable | Description |
|-----------|--------------|
| **Country** | Name of the country |
| **Year** | Year of observation |
| **House Price Index** | Relative measure of housing prices (base = 100, in USD) |
| **Affordability Ratio** | Median house price divided by median household income |
| **Urbanization Rate (%)** | Percentage of population living in urban areas |
| **Population Growth (%)** | Annual population growth rate |

These features were selected because they best capture the relationship between **urbanization** and **housing affordability** trends globally.

---

## 🪜 **Step 3: Data Cleaning**
A complete data cleaning process was performed to ensure high data quality and accuracy.

### 🔹 Missing and Duplicate Values
- The dataset was thoroughly checked for **missing** and **duplicate** records using R validation functions.  
- No missing values were found in any variable.  
- No duplicate records were detected across observations.  
- Therefore, the dataset is **complete**, **clean**, and required **no imputation or record removal**.

### 🔹 Outlier Detection
- Outliers were inspected visually using **boxplots** for key variables: *House Price Index*, *Affordability Ratio*, and *Urbanization Rate (%)*.  
- The analysis revealed **no extreme outliers** or unusual data points.  
- All observations were within a reasonable range, ensuring **data consistency** and **reliability**.  
- The dataset was confirmed to be **stable and ready** for further transformation and analysis.



### 🔹 Text Cleaning
- Extra spaces (leading, trailing, and multiple spaces) were removed from country names.  
- This ensured consistency when grouping or merging records by country.

### 🔹 Data Type Standardization
- All numeric fields (e.g., Year, Price Index, Urbanization Rate) were converted to proper numeric formats.  
- The dataset structure was verified and standardized for analysis.
- No need to standardize currency for house price as the House Price Index column as a whole in USD currency.

---

## 🪜 **Step 4: Data Transformation**
After cleaning, several transformations were applied to enhance analytical depth.

### 🔹 Computed a Derived Variable — *Price-to-Income Ratio*
A new metric was created by dividing the **House Price Index** by the **Affordability Ratio**.  
This provides a clearer affordability measure, showing the real relationship between house prices and income.

### 🔹 Urban Growth Categorization
The **Urbanization Rate (%)** was classified into three categories:
- **Low Urban Growth**: Urbanization rate below 65%  
- **Medium Urban Growth**: Urbanization rate between 65% and 80%  
- **High Urban Growth**: Urbanization rate above 80%  

This categorization helps compare housing affordability across countries with different levels of urban development.

### 🔹 Country-Level Summary Dataset
Data was aggregated at the **country level** to analyze overall trends.  
For each country, the following averages were calculated:
- Average House Price Index  
- Average Affordability Ratio  
- Average Price-to-Income Ratio  
- Average Urbanization Rate  
- Average Population Growth  
- Urban Growth Category  

The resulting summarized dataset provides a clearer view of **how urbanization intensity affects housing affordability** globally.

---

## 🪜 **Overview of Results**
- The final dataset consists of **200 records (year-level)** and a **summarized country-level dataset**.  
- All variables were cleaned, standardized, and transformed for analysis.  
- The data is now ready for **exploratory data analysis (EDA)** and Visualization to uncover meaningful patterns between **urban growth** and **housing affordability**.

---


**Group members:** Maryam Tahir,Yashfeen Fatima,Esha Ashfaq  
**Tools Used:** R (readr, dplyr)  
**Data Source:** [Kaggle – Global Housing Market Analysis (2015–2024)](https://www.kaggle.com/datasets/atharvasoundankar/global-housing-market-analysis-2015-2024)  

---
