# Time-Series-Analysis-TruckSales

## 📈 Overview
This project forecasts truck sales from 2003–2014 to predict sales for 2015 and 2016. We applied multiple time series models to capture seasonal trends and identify the most accurate forecasting method.

## 📘 Course Info
- **Course**: BAN 673 – Time Series Analytics  
- **Instructor**: Prof. Zinovy Radovilsky  
- **Team Members**: Krutarth Sompura, Manan Upadhyay, Hemanth Varma Pericherla, Priyadarshini Madhusudanan, Vaishnavi Karingala  
- **Date**: March 10, 2024

## 🧾 Dataset
- **Source**: [Kaggle - Dummy Truck Sales](https://www.kaggle.com/datasets/ddosad/dummy-truck-sales-for-time-series/data)
- **Duration**: Monthly sales data from Jan 2003 – Dec 2014 (144 observations)

## 🧠 Models Used
1. Two-level Regression + Moving Average  
2. Holt-Winter’s (ETS) – 🏆 Best Performer  
3. Quadratic Trend + Seasonality  
4. Auto ARIMA  

## ✅ Key Findings
- Holt-Winter’s model delivered the lowest RMSE & MAPE, making it the most accurate model for 2015–2016 sales forecasts.
- Seasonality: Peak sales mid-year, drop toward year-end.
- High autocorrelation across lags confirmed strong monthly patterns.

## 🔧 How to Run
1. Install R and required packages: `forecast`, `zoo`, `TTR`
2. Run `Time_Series_Analysis_Code.R` in RStudio

## ✍️ Acknowledgments
Thanks to Prof. Radovilsky for his guidance and feedback.
