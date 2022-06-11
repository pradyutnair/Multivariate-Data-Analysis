# Model Building

# Linear Discriminant Analysis
LDA on the is tested on the entire dataset by splitting a 70-30 ratio of train and test set. We want to analyse whether there is a discrepancy between class separation of _Scan Type_ when the data is smoothed or not.
For the smoothing, a Savitsky-Golay algorithm was applied to the NIR data with `p=2 and n=3`, indicating that the where `p` is the filter order and `n` is the filter length.
This hypothesis doesn't lead to conclusive results as the LDA model performs exactly the same for both datasets. The onyl difference is the slight variance in the visualisation of the linear discriminants of the smoothed data which have lower variance.