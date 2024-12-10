# 📊 Mashable Article Popularity Predictor

A machine learning project analyzing and predicting article popularity on Mashable using various statistical and ML models. The project compares different approaches to determine the most effective method for predicting article shares.

![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
![scikit-learn](https://img.shields.io/badge/scikit--learn-%23F7931E.svg?style=for-the-badge&logo=scikit-learn&logoColor=white)
![Pandas](https://img.shields.io/badge/pandas-%23150458.svg?style=for-the-badge&logo=pandas&logoColor=white)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
## 🎯 Project Overview

This study compares six different models to predict article popularity based on shares:

| Model | RMSE | Key Features |
|-------|------|--------------|
| Random Forest | 0.863 | Best overall performance, ensemble learning |
| GAM | 0.864 | Non-parametric, captures non-linear relationships |
| Linear Regression | 0.884 | 41 variables selected via backward stepwise |
| PCA Regression | 0.889 | 24 principal components, 94% variance explained |
| Lasso | 0.912 | Selected 15 key variables |
| Ridge | 0.939 | Prevents overfitting via regularization |

## 🔍 Key Findings

- **Best Models**: Random Forest and GAM showed superior performance
- **Feature Selection**: Backward stepwise selection identified 41 significant variables
- **Dimensionality Reduction**: PCA effectively reduced features while maintaining performance
- **Model Complexity**: Simpler models (Linear Regression) performed comparably to complex ones

## 🛠️ Methods Used

- Linear Regression (Forward/Backward/Autometrics selection)
- Ridge Regression (with cross-validation)
- Lasso Regression (with cross-validation)
- Principal Component Analysis (PCA)
- General Additive Model (GAM)
- Random Forest

## 📈 Results

The study revealed that article popularity can be effectively predicted using machine learning models, with Random Forest and GAM showing the best performance (RMSE: 0.863 and 0.864 respectively).

## 📝 Conclusion

While Random Forest and GAM models showed marginally better performance, the PCA regression offers a robust alternative when dealing with many variables. The study demonstrates that article popularity depends on multiple factors including content characteristics and publication timing.

## 📚 References

- Mashable dataset
- Related research papers and methodologies

## 🤝 Contributing

Feel free to fork this repository and submit pull requests. For major changes, please open an issue first to discuss the proposed changes.

This project was developed using an R version older than 4. With R version 4 or newer, you may encounter difficulties running the code, particularly when installing certain packages.
