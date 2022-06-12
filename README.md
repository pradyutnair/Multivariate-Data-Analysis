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


