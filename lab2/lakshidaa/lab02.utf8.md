---
title: "Multivariate Statistical Methods - Lab 02"
author: "Lakshidaa Saigiridharan (laksa656)"
date: "11/30/2019"
output: pdf_document
---



# Question 1: Test of outliers

## Consider again the data set from the T1-9.dat file, National track records for women. In the first assignment we studied different distance measures between an observation and the sample average vector. The most common multivariate residual is the Mahalanobis distance and we computed this distance for all observations.


```r
# Loading required data
track_data <- read.table("T1-9.dat")
colnames(track_data) = c("country", "100m", "200m", "400m",
                   "800m", "1500m", "3000m", "marathon")
track_data
```

```
##    country  100m  200m  400m 800m 1500m 3000m marathon
## 1      ARG 11.57 22.94 52.50 2.05  4.25  9.19   150.32
## 2      AUS 11.12 22.23 48.63 1.98  4.02  8.63   143.51
## 3      AUT 11.15 22.70 50.62 1.94  4.05  8.78   154.35
## 4      BEL 11.14 22.48 51.45 1.97  4.08  8.82   143.05
## 5      BER 11.46 23.05 53.30 2.07  4.29  9.81   174.18
## 6      BRA 11.17 22.60 50.62 1.97  4.17  9.04   147.41
## 7      CAN 10.98 22.62 49.91 1.97  4.00  8.54   148.36
## 8      CHI 11.65 23.84 53.68 2.00  4.22  9.26   152.23
## 9      CHN 10.79 22.01 49.81 1.93  3.84  8.10   139.39
## 10     COL 11.31 22.92 49.64 2.04  4.34  9.37   155.19
## 11     COK 12.52 25.91 61.65 2.28  4.82 11.10   212.33
## 12     CRC 11.72 23.92 52.57 2.10  4.52  9.84   164.33
## 13     CZE 11.09 21.97 47.99 1.89  4.03  8.87   145.19
## 14     DEN 11.42 23.36 52.92 2.02  4.12  8.71   149.34
## 15     DOM 11.63 23.91 53.02 2.09  4.54  9.89   166.46
## 16     FIN 11.13 22.39 50.14 2.01  4.10  8.69   148.00
## 17     FRA 10.73 21.99 48.25 1.94  4.03  8.64   148.27
## 18     GER 10.81 21.71 47.60 1.92  3.96  8.51   141.45
## 19     GBR 11.10 22.10 49.43 1.94  3.97  8.37   135.25
## 20     GRE 10.83 22.67 50.56 2.00  4.09  8.96   153.40
## 21     GUA 11.92 24.50 55.64 2.15  4.48  9.71   171.33
## 22     HUN 11.41 23.06 51.50 1.99  4.02  8.55   148.50
## 23     INA 11.56 23.86 55.08 2.10  4.36  9.50   154.29
## 24     IND 11.38 22.82 51.05 2.00  4.10  9.11   158.10
## 25     IRL 11.43 23.02 51.07 2.01  3.98  8.36   142.23
## 26     ISR 11.45 23.15 52.06 2.07  4.24  9.33   156.36
## 27     ITA 11.14 22.60 51.31 1.96  3.98  8.59   143.47
## 28     JPN 11.36 23.33 51.93 2.01  4.16  8.74   139.41
## 29     KEN 11.62 23.37 51.56 1.97  3.96  8.39   138.47
## 30    KORS 11.49 23.80 53.67 2.09  4.24  9.01   146.12
## 31    KORN 11.80 25.10 56.23 1.97  4.25  8.96   145.31
## 32     LUX 11.76 23.96 56.07 2.07  4.35  9.21   149.23
## 33     MAS 11.50 23.37 52.56 2.12  4.39  9.31   169.28
## 34     MRI 11.72 23.83 54.62 2.06  4.33  9.24   167.09
## 35     MEX 11.09 23.13 48.89 2.02  4.19  8.89   144.06
## 36     MYA 11.66 23.69 52.96 2.03  4.20  9.08   158.42
## 37     NED 11.08 22.81 51.35 1.93  4.06  8.57   143.43
## 38     NZL 11.32 23.13 51.60 1.97  4.10  8.76   146.46
## 39     NOR 11.41 23.31 52.45 2.03  4.01  8.53   141.06
## 40     PNG 11.96 24.68 55.18 2.24  4.62 10.21   221.14
## 41     PHI 11.28 23.35 54.75 2.12  4.41  9.81   165.48
## 42     POL 10.93 22.13 49.28 1.95  3.99  8.53   144.18
## 43     POR 11.30 22.88 51.92 1.98  3.96  8.50   143.29
## 44     ROM 11.30 22.35 49.88 1.92  3.90  8.36   142.50
## 45     RUS 10.77 21.87 49.11 1.91  3.87  8.38   141.31
## 46     SAM 12.38 25.45 56.32 2.29  5.42 13.12   191.58
## 47     SIN 12.13 24.54 55.08 2.12  4.52  9.94   154.41
## 48     ESP 11.06 22.38 49.67 1.96  4.01  8.48   146.51
## 49     SWE 11.16 22.82 51.69 1.99  4.09  8.81   150.39
## 50     SUI 11.34 22.88 51.32 1.98  3.97  8.60   145.51
## 51     TPE 11.22 22.56 52.74 2.08  4.38  9.63   159.53
## 52     THA 11.33 23.30 52.60 2.06  4.38 10.07   162.39
## 53     TUR 11.25 22.71 53.15 2.01  3.92  8.53   151.43
## 54     USA 10.49 21.34 48.83 1.94  3.95  8.43   141.16
```

## a) The Mahalanobis distance is approximately chi???square distributed, if the data comes from a multivariate normal distribution and the number of observations is large. Use this chi??? square approximation for testing each observation at the 0.1% significance level and conclude which countries can be regarded as outliers. Should you use a multiple???testing correction procedure? Compare the results with and without one. Why is (or maybe is not) 0.1% a sensible significance level for this task?

The Mahalanobis distance is represented as,
$$d^2_M(\vec{x} - \bar{x}) = (\vec{x} - \bar{x})^T \boldsymbol{C}^{-1}(\vec{x} - \bar{x}),$$
where **C** is the sample covariance matrix calculated from the data.


```r
track_mat_data <- as.matrix(track_data[,2:8])
rownames(track_mat_data) <- track_data[,1]
#track_mat_data <- scale(track_mat_data, scale = FALSE)
n <- nrow(track_mat_data)

sample_variance = function(X) {
  
  X = as.matrix(X)
  
  identity = diag(nrow(X))
  one_n = matrix(1, nrow=nrow(X), ncol=1)
  
  inter =  identity - 1/nrow(X) * (one_n %*% t(one_n)) 
  
  return(1/nrow(X) * (t(X) %*% inter %*% X))
}

mahalanobis_distance = function(X) {
  X = as.matrix(X)
  
  V = sample_variance(X)
  ident = matrix(1, nrow=nrow(X), ncol=nrow(X))
  mu = 1/nrow(X) * (t(ident) %*% X)
  
  X_centered = X - mu
  
  return(diag(X_centered %*% solve(V) %*% t(X_centered)))
}

m_dist <- mahalanobis_distance(track_data[,2:8])
rownames(track_mat_data[which(m_dist > qchisq(0.99, ncol(track_mat_data))),])
```

```
## [1] "COK"  "KORN" "PNG"  "SAM"
```

```r
alpha = 0.001
outlier_indices = which(1 - pchisq(m_dist, ncol(track_data) - 1) < alpha)
track_data$country[outlier_indices]
```

```
## [1] KORN PNG  SAM 
## 54 Levels: ARG AUS AUT BEL BER BRA CAN CHI CHN COK COL CRC CZE DEN ... USA
```

## b) One outlier is North Korea. This country is not an outlier with the Euclidean distance. Try to explain these seemingly contradictory results.


```r
euclidean_distance <- function(data, X){
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  matrix_1 <- matrix(rep(1,n*n), ncol = n, nrow = n)
  sample_mean <- t(X) %*% matrix_1 / n

  d <- X - t(sample_mean)
  dist <- d %*% t(d)
  rownames(dist) <- X[,1]

  diagonal <- sqrt(diag(dist))
  diag_df <- data.frame(Country = data[,1], Diag_value = diagonal)
  diag_df[order(diag_df$Diag_value, decreasing = TRUE),]
}

euc_dist <- euclidean_distance(track_data, track_data[,2:8])

cat("Countries sorted in decreasing order of their euclidean distance : \n")
```

```
## Countries sorted in decreasing order of their euclidean distance :
```

```r
euc_dist
```

```
##    Country Diag_value
## 40     PNG  67.627962
## 11     COK  59.615175
## 46     SAM  38.524758
## 5      BER  20.616060
## 19     GBR  18.591462
## 21     GUA  18.158217
## 33     MAS  15.677070
## 29     KEN  15.177242
## 9      CHN  14.486679
## 28     JPN  14.215076
## 34     MRI  13.750148
## 18     GER  13.039467
## 54     USA  13.023696
## 15     DOM  12.939488
## 45     RUS  12.740366
## 39     NOR  12.582623
## 41     PHI  12.204487
## 25     IRL  11.451565
## 44     ROM  11.370744
## 12     CRC  10.794572
## 2      AUS  10.703279
## 4      BEL  10.608378
## 43     POR  10.351343
## 37     NED  10.231711
## 27     ITA  10.201641
## 35     MEX  10.054448
## 42     POL   9.896828
## 31    KORN   9.548406
## 13     CZE   9.408675
## 52     THA   8.851508
## 50     SUI   8.157574
## 30    KORS   7.717411
## 48     ESP   7.546571
## 38     NZL   7.177852
## 17     FRA   6.670036
## 6      BRA   6.382646
## 32     LUX   6.069117
## 51     TPE   6.014398
## 16     FIN   5.978177
## 7      CAN   5.718796
## 22     HUN   5.173356
## 36     MYA   4.940436
## 24     IND   4.588885
## 14     DEN   4.402629
## 47     SIN   3.695017
## 1      ARG   3.352526
## 23     INA   3.287141
## 49     SWE   3.275672
## 10     COL   2.851967
## 26     ISR   2.755565
## 53     TUR   2.587504
## 8      CHI   2.329972
## 3      AUT   1.656308
## 20     GRE   1.610905
```

```r
cat("Position of North Korea in this list : ", which(euc_dist$Country == "KORN"))
```

```
## Position of North Korea in this list :  28
```

On computing the Euclidean distance of the countries, it can be seen that North Korea is not an outlier. This is because the Euclidean distance computation treats data equally. Whereas, the Mahalanobis distance computation takes the covariance of each variable and obtains the correlation among variables. 

# Question 2: Test, confidence region and confidence intervals for a mean vector

## Look at the bird data in file T5-12.dat and solve Exercise 5.20 of Johnson, Wichern. Do not use any extra R package or built???in test but code all required matrix calculations. You MAY NOT use loops!


```r
# Importing the required data
bird_data <- read.table("T5-12.DAT")
colnames(bird_data) <- c("Tail_length", "Wing_length")
head(bird_data)
```

```
##   Tail_length Wing_length
## 1         191         284
## 2         197         285
## 3         208         288
## 4         180         273
## 5         180         275
## 6         188         280
```

## a) Find and sketch the 95% confidence ellipse for the population means $\mu_1$ and $\mu_2$. Suppose it is known that $\mu_1 = 190$ mm and $\mu_2 = 275$ mm for male hook-billed kites. Are these plausible values for the mean tail length and mean wing length for the female birds? Explain.


```r
alpha <- 0.95

# Given mu_0 values (Center of ellipse)
mu_1 <- 190
mu_2 <- 275
mu_0 <- c(mu_1, mu_2)

n <- nrow(bird_data)
p <- ncol(bird_data)

x_bar <- colMeans(bird_data)
S <- sample_variance(bird_data)
S_inverse <- solve(S)

eigen_values <- eigen(S)$values
eigen_vectors <- eigen(S)$vectors

chi_sq <- qchisq(alpha, p)
c <- sqrt(chi_sq)   # Expression (4-8)

# Half lengths
major_len <- sqrt(eigen_values[1]) * c    # length of major axes
minor_len <- sqrt(eigen_values[2]) * c    # length of minor axes
```

## b) Construct the simultaneous 95% $T^2$ intervals for $\mu_1$ and $\mu_2$ and the 95% Bonferroni intervals for $\mu_1$ and $\mu_2$.Compare the two sets of intervals. What advantage, if any, do the $T^2$ intervals have over the Bonferroni intervals?


```r
# 95% T^2 intervals
t2_interval1 <- c(0,0)
t2_interval1[1] <- x_bar[1] - sqrt((p*(n-1)/(n-p)) * 
                                     qf(0.05, p, n-p)) * sqrt(S[1,1]/n)
t2_interval1[2] <- x_bar[1] + sqrt((p*(n-1)/(n-p)) * 
                                     qf(0.05, p, n-p)) * sqrt(S[1,1]/n)

t2_interval2 <- c(0,0)
t2_interval2[1] <- x_bar[2] - sqrt((p*(n-1)/(n-p)) * 
                                     qf(0.05, p, n-p)) * sqrt(S[2,2]/n)
t2_interval2[2] <- x_bar[2] + sqrt((p*(n-1)/(n-p)) * 
                                     qf(0.05, p, n-p)) * sqrt(S[2,2]/n)

cat(paste("95% T^2 intervals for mu_1 : ", "[", t2_interval1[1], ", ",
          t2_interval1[2], "]\n"))
```

```
## 95% T^2 intervals for mu_1 :  [ 193.097227596845 ,  194.147216847599 ]
```

```r
cat(paste("95% T^2 intervals for mu_2 : ", "[", t2_interval2[1], ", ",
          t2_interval2[2], "]"))
```

```
## 95% T^2 intervals for mu_2 :  [ 279.087688286496 ,  280.46786726906 ]
```

```r
# 95% Bonferroni intervals
bon_interval1 <- c(0,0)
bon_interval1[1] <- x_bar[1] - qt(0.05/(2*p), n-1)*sqrt(S[1,1]/n)
bon_interval1[2] <- x_bar[1] + qt(0.05/(2*p), n-1)*sqrt(S[1,1]/n)

bon_interval2 <- c(0,0)
bon_interval2[1] <- x_bar[2] - qt(0.05/(2*p), n-1)*sqrt(S[2,2]/n)
bon_interval2[2] <- x_bar[2] + qt(0.05/(2*p), n-1)*sqrt(S[2,2]/n)

cat(paste("95% Bonferroni intervals for mu_1 : ", "[", bon_interval1[1], ", ",
          bon_interval1[2], "]\n"))
```

```
## 95% Bonferroni intervals for mu_1 :  [ 197.380417947191 ,  189.864026497254 ]
```

```r
cat(paste("95% Bonferroni intervals for mu_2 : ", "[", bon_interval2[1], ", ",
          bon_interval2[2], "]"))
```

```
## 95% Bonferroni intervals for mu_2 :  [ 284.717811924069 ,  274.837743631486 ]
```

## c) Is the bivariate normal distribution a viable population model? Explain with reference to Q-Q plots and a scatter diagram.





