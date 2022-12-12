## Housekeeping
setwd('/Users/lena/git/pca')
library(tidyverse)
library(geomtextpath)
library(nnet)
library(yardstick)
library(devtools)
library(ggbiplot)
library(psych)
library(MVN)
library(reshape2)

## Load and prepare data
df_wine <- data.table::fread("wine.data")
colnames(df_wine) <- c("class", 
                       "alcohol",
                       "malic_acid", 
                       "ash",
                       "alca_ash",
                       "magnesium",
                       "total_phenols",
                       "flav",
                       "nonflav_phenols",
                       "proan",
                       "col_intens",
                       "hue",
                       "protein",
                       "proline")

## A First Glance 

df_wine %>% 
  mutate(class = as.factor(class)) %>%  
  ggplot(aes(malic_acid, ash, color = class)) +
  geom_point()

df_wine %>% 
  mutate(class = as.factor(class)) %>%  
  ggplot(aes(protein, flav, color = class)) +
  geom_point()

## Barlett's sphericity test

z_score <- function(x){
  (x-mean(x))/sd(x)
}
data_testing <- df_wine %>% 
  select(-class)
data_testing <- data.frame(apply(data_testing, 2, z_score))

cor_testing <- cor(data_testing)
cortest.bartlett(cor_testing, n = nrow(data_testing))
# We fail to reject H0

## But is there normality? Test for multivariate normal distribution

mvn_test <- mvn(data_testing, mvnTest = "royston")
mvn_test$multivariateNormality
# We fail to reject H0. The underlying data is not multivariate normally distributed.

## heatmap for correlation between variables

melt_cor_testing <- melt(cor_testing)
ggplot(data = melt_cor_testing, aes(Var2, Var1, fill = value))+
  geom_tile()

## Split into test and train and define x and y

set.seed(48)
df_wine <- df_wine %>% 
  mutate(id = sample(x = c(0, 1), size = nrow(.), replace = TRUE, prob = c(.7, .3)))

train <- df_wine %>% 
  filter(id == 0) %>% 
  select(-id)
test <- df_wine %>% 
  filter(id == 1) %>% 
  select(-id)

train_x <- train[, c(2:14)]
train_y <- train[, 1]

test_x <- test[, c(2:14)]
test_y <- test[, 1]

## standardise the x variables

train_x <- data.frame(apply(train_x, 2, z_score))
test_x <- data.frame(apply(test_x, 2, z_score))

## Standard PCA from Scratch

pca_cov <- cov(train_x)
pca_eigen <- eigen(pca_cov)
pca_eigenvec <- pca_eigen$vectors
pca_eigenval <- pca_eigen$values

pc1 <- t(pca_eigenvec[,1])%*%t(train_x) # compute first PC on training data
pc2 <- t(pca_eigenvec[,2])%*%t(train_x) # compute second PC on training data
pc3 <- t(pca_eigenvec[,3])%*%t(train_x) # compute third PC on training data
pc4 <- t(pca_eigenvec[,4])%*%t(train_x) # compute fourth PC on training data

pca_eigenval <- data.frame(pca_eigenval) %>%
  mutate(explained_var = pca_eigenval/sum(pca_eigenval)) %>% 
  mutate(cum_var = cumsum(explained_var))

# visualisation

pca_eigenval %>% 
  ggplot(aes(c(1:nrow(.)), explained_var)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1, nrow(pca_eigenval), 1)) +
  xlab("Principal Component") +
  ylab("Explained Variance")

pca_eigenval %>% 
  ggplot(aes(c(1:nrow(.)), cum_var)) +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = seq(1, nrow(pca_eigenval), 1)) +
  xlab("Principal Component") +
  ylab("Cumulative Variance") +
  ylim(0,1)

pca_eigenval %>% ggplot(aes(c(1:nrow(.)), pca_eigenval)) +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = seq(1, nrow(pca_eigenval), 1)) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  xlab("Principal Component") +
  ylab("Eigenvalue")

pca <- data.frame(t(rbind(pc1, pc2, pc3, pc4, t(train_y)))) %>% 
  mutate(PC1 = V1) %>% 
  mutate(PC2 = V2) %>% 
  mutate(PC3 = V3) %>% 
  mutate(PC4 = V4) %>% 
  select(-c(V1, V2, V3, V4)) %>% 
  mutate(class = as.factor(class))

pca %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point()

## Biplot from Scratch

A <- pca_eigenvec[,1:4]
eigenval_x <- pca_eigenval[1:4,1]
Lambda <- diag(eigenval_x, nrow = 4, ncol = 4)

#Lambda12 <- (Lambda)%^%(1/2)
loadings <- A%*%(Lambda)^(1/2)
loadings <- data.frame(loadings) %>% 
  mutate(xstart = sample(x = 0, size = nrow(.), replace = TRUE)) %>% 
  mutate(ystart = sample(x = 0, size = nrow(.), replace = TRUE)) %>% 
  mutate(labels = colnames(data_testing))

loadings %>% 
  ggplot(aes(xstart, ystart, xend = X1, yend = X2, label = labels)) +
  geom_textsegment(arrow = arrow())

pca %>% 
  ggplot(aes(PC1, PC2, color = class)) +
  geom_point()
pca %>% 
  ggplot(aes(PC3, PC4, color = class)) +
  geom_point()

plot <- ggplot() +
  geom_point(aes(PC1, PC2, color = class), data = pca) +
  geom_textsegment(aes(xstart, ystart, xend = X1, yend = X2, label = labels), data = loadings)


## Biplot

wine_pca <- prcomp(train_x)
summary(wine_pca)
wine_pca$x[,2] <- wine_pca$x[,2]*-1
wine_pca$x[,4] <- wine_pca$x[,4]*-1
wine_pca$rotation[,1] <- loadings[,1]
wine_pca$rotation[,2] <- loadings[,2]
wine_pca$rotation[,3] <- loadings[,3]
wine_pca$rotation[,4] <- loadings[,4]

ggbiplot(wine_pca, 
         scale = 1,
         choices = 1:2, 
         groups = pca$class)

ggbiplot(wine_pca,
         scale = 1,
         choices = 3:4,
         groups = pca$class)

pca_inbuilt <- prcomp(train_x)
ggbiplot(pca_inbuilt, 
         scale = 1,
         choices = 1:2,
         groups = pca$class)

## Classification with logit regression

train$PC1 <- t(pc1)
train$PC2 <- t(pc2)

logistic_classification <- multinom(class ~ PC1 + PC2,
                               data = train)

# Predict on test data

pc1_test <- t(pca_eigenvec[,1])%*%t(test_x)
pc2_test <- t(pca_eigenvec[,2])%*%t(test_x) 
test_x$PC1 <- as.numeric(t(pc1_test))
test_x$PC2 <- as.numeric(t(pc2_test))

logit_predict <- predict(logistic_classification, test_x)

# Evaluation

validation <- cbind(logit_predict, test_y)
validation <- data.frame(validation) %>% 
  mutate(class = as.factor(class)) %>% 
  mutate(compare = if_else(logit_predict == class, TRUE, FALSE))

accuracy <- (sum(validation$compare))/nrow(validation)

conf_matrix <- conf_mat(validation, class, logit_predict)

conf_matrix %>% 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  theme(legend.position = "right") +
  labs(fill = "Abs. freq")
