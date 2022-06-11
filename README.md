# Model Building

# Linear Discriminant Analysis
LDA on the is tested on the entire dataset by splitting a 70-30 ratio of train and test set. We want to analyse whether there is a discrepancy between class separation of _Scan Type_ when the data is smoothed or not.
For the smoothing, a Savitsky-Golay algorithm was applied to the NIR data with `p=2 and n=3`, indicating that the where `p` is the filter order and `n` is the filter length.
This hypothesis doesn't lead to conclusive results as the LDA model performs exactly the same for both datasets. The onyl difference is the slight variance in the visualisation of the linear discriminants of the smoothed data which have lower variance.
# PCA
Principal Component Analysis aims to describe maximum variation in the data. When conducted for the entire dataset, PC1 explains 62.4% of the variance, whereas PC2 explains 36.6% of the total variance. However, the data seems more separable in LDA. We also plot the cos2, which is a quality of representation of the variables.
A high cos2 indicates a good representation of the variable on the principal component. In this case the variable is positioned close to the circumference of the correlation circle.
The contribution of the top 50 wavelengths are also plotted for Principal Components 1 and 2.

# PCA with RandomForestClassifier
A RandomForestClassifier was used to train the model on the entire dataset. This was taken as the baseline performance. The number of trees was set to 2 and the baseline performance was quite good with 92.4% and 80% accuracy on the train and test sets respectively.
However, with by using the first two principal components as the training data, we get a lower train accuracy of 84.6% but much better generalisation with a test accuracy of 85.3%.