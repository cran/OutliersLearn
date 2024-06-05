## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(OutliersLearn);

## ----echo=TRUE----------------------------------------------------------------
inputData = t(matrix(c(3,2,3.5,12,4.7,4.1,5.2,4.9,7.1,6.1,6.2,5.2,14,5.3),2,7,dimnames=list(c("r","d"))));
inputData = data.frame(inputData);
print(inputData);

## ----echo=TRUE----------------------------------------------------------------
plot(inputData);

## ----echo=TRUE----------------------------------------------------------------
point1 = inputData[1,];
point2 = inputData[4,];
distance = euclidean_distance(point1, point2);
print(distance);

## ----echo=TRUE----------------------------------------------------------------
inputDataMatrix = as.matrix(inputData); #Required conversion for this function
sampleMeans = c();
#Calculate the mean for each column
for(i in 1:ncol(inputDataMatrix)){
  column = inputDataMatrix[,i];
  calculatedMean = sum(column)/length(column);
  sampleMeans = c(sampleMeans, calculatedMean);
}
#Calculate the covariance matrix
covariance_matrix = cov(inputDataMatrix);

distance = mahalanobis_distance(inputDataMatrix[3,], sampleMeans, covariance_matrix);
print(distance)


## ----echo=TRUE----------------------------------------------------------------
distance = manhattan_dist(c(1,2), c(3,4));
print(distance);

## ----echo=TRUE----------------------------------------------------------------
mean = mean_outliersLearn(inputData[,1]);
print(mean);

## ----echo=TRUE----------------------------------------------------------------
sd = sd_outliersLearn(inputData[,1], mean);
print(sd); 

## ----echo=TRUE----------------------------------------------------------------
q = quantile_outliersLearn(c(12,2,3,4,1,13), 0.60); 
print(q);

## ----echo=TRUE----------------------------------------------------------------
numeric_data = c(1, 2, 3)
character_data = c("a", "b", "c")
logical_data = c(TRUE, FALSE, TRUE)
factor_data = factor(c("A", "B", "A"))
integer_data = as.integer(c(1, 2, 3))
complex_data = complex(real = c(1, 2, 3), imaginary = c(4, 5, 6))
list_data = list(1, "apple", TRUE)
data_frame_data = data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))

transformed_numeric = transform_to_vector(numeric_data);
print(transformed_numeric);
transformed_character = transform_to_vector(character_data);
print(transformed_character);
transformed_logical = transform_to_vector(logical_data);
print(transformed_logical);
transformed_factor = transform_to_vector(factor_data);
print(transformed_factor);
transformed_integer = transform_to_vector(integer_data);
print(transformed_integer);
transformed_complex = transform_to_vector(complex_data);
print(transformed_complex);
transformed_list = transform_to_vector(list_data);
print(transformed_list);
transformed_data_frame = transform_to_vector(data_frame_data);
print(transformed_data_frame);


## ----echo=TRUE----------------------------------------------------------------
boxandwhiskers(inputData,2,FALSE)

## ----echo=TRUE----------------------------------------------------------------
boxandwhiskers(inputData,2,TRUE)

## ----echo=TRUE----------------------------------------------------------------
eps = 4;
min_pts = 3;
DBSCAN_method(inputData, eps, min_pts, FALSE);

## ----echo=TRUE----------------------------------------------------------------
eps = 4;
min_pts = 3;
DBSCAN_method(inputData, eps, min_pts, TRUE);

## ----echo=TRUE----------------------------------------------------------------
knn(inputData,3,2,FALSE)

## ----echo=TRUE----------------------------------------------------------------
knn(inputData,3,2,TRUE)

## ----echo=TRUE----------------------------------------------------------------
lof(inputData, 3, 0.5, FALSE);

## ----echo=TRUE----------------------------------------------------------------
lof(inputData, 3, 0.5, TRUE);

## ----echo=TRUE----------------------------------------------------------------
mahalanobis_method(inputData, 0.7, FALSE);

## ----echo=TRUE----------------------------------------------------------------
mahalanobis_method(inputData, 0.7, TRUE);

## ----echo=TRUE----------------------------------------------------------------
z_score_method(inputData,2,FALSE);

## ----echo=TRUE----------------------------------------------------------------
z_score_method(inputData,2,TRUE);

