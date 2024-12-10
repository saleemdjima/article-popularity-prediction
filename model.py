# Import required libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn import linear_model
from sklearn.decomposition import PCA



# Ordinary Least Squares (OLS) Regression
class OLS:
    def __init__(self, fit_intercept=True):
        """
        Ordinary Least Squares Regression
        """
        self.fit_intercept = fit_intercept
        self.coefficients = []
    
    def fit(self, X, Y):
        X = np.array(X)
        if self.fit_intercept:
            X = np.concatenate((np.ones((X.shape[0], 1)), X), axis=1)
        Y = np.array(Y)
        
        self.coefficients = np.linalg.inv(X.T @ X) @ X.T @ Y
        self.residuals = Y - X @ self.coefficients
        self.std_errors = np.sqrt(np.diag(np.var(self.residuals) * np.linalg.inv(X.T @ X)))
        self.t_values = self.coefficients / self.std_errors
        self.r_squared = 1 - np.sum(self.residuals**2) / np.sum((Y - np.mean(Y))**2)
        return self



# Ridge Regression
class RidgeRegression:
    def __init__(self, alpha=1, fit_intercept=True):
        """
        Ridge Regression with L2 Regularization
        """
        self.alpha = alpha
        self.fit_intercept = fit_intercept
    
    def fit(self, X, Y):
        X = np.array(X)
        if self.fit_intercept:
            X = np.concatenate((np.ones((X.shape[0], 1)), X), axis=1)
        Y = np.array(Y)
        
        self.coefficients = np.linalg.inv(X.T @ X + self.alpha * np.identity(X.shape[1])) @ X.T @ Y
        return self



# Principal Component Analysis (PCA)
class PCAImplementation:
    def __init__(self):
        """
        Principal Component Analysis
        """
        self.components = []
        self.eigen_values = []
        self.eigen_vectors = []
    
    def fit(self, X):
        _X = np.array(X)
        X = _X - np.mean(_X, axis=0)
        
        covariance_matrix = np.cov(X, rowvar=False)
        eigen_values, eigen_vectors = np.linalg.eig(covariance_matrix)
        
        # Sort eigenvalues and eigenvectors
        indices = np.argsort(eigen_values)[::-1]
        self.eigen_values = eigen_values[indices]
        self.eigen_vectors = eigen_vectors[:, indices]
        
        self.components = X @ self.eigen_vectors
        self.explained_variance_ratio = self.eigen_values / np.sum(self.eigen_values)
        return self
    
    def plot_explained_variance(self):
        plt.bar(range(1, len(self.explained_variance_ratio) + 1), self.explained_variance_ratio)
        plt.xlabel("Principal Components")
        plt.ylabel("Explained Variance Ratio")
        plt.show()

