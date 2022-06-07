# Preprocessing

Steps in Exploratory Data Analysis

# Visualising spectral data

Import the data  using the correct delimiters.This dataset is separated by semicolons and whitespaces have been stripped.
### Steps

1. Split the data according to _Freshness_. Therefore, the data is divided into two parts: **Fresh** and **Thawed**.

2. Extract wavelength number from column features. Save the numbers as a list named *wavelengths*

3. Create more splits according to *Scan_Type* and transpose the data. The first column for each of these transposed datasets is the wavelength number.

4. Convert the datasets into spectral objects using the **pavo** package in R.
5. Plot the spectral objects and rename the axes.


# Linear Discriminant Analysis

LDA on the is tested on the entire dataset by splitting a 70-30 ratio of train and test set. The datasets used for the split are the entire chicken dataset, subset of fresh fillets, and subset of thawed fillets.
It separates the group means of *Scan-Type* quite well and produced a good train accuracy of 0.915 on the entire dataset (even higher for subsets).

# PCA
Principal Component Analysis aims to describe maximum variation in the data. When conducted for the entire dataset, PC1 explains 62.4% of the variance, whereas PC2 explains 36.6% of the total variance. **However, the data seems more separable in LDA.**
We also plot the `cos2`, which is a quality of representation of the variables.
* A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle.
* The `contribution` of the top 50 wavelengths are also plotted for Principal Components 1 and 2.