## Housekeeping
setwd('/Users/lena/documents/R/PCA')
library(tidyverse)
library(keras)

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

train_x <- as.matrix(train[, c(2:14)])
train_y <- train[, 1]

test_x <- as.matrix(test[, c(2:14)])
test_y <- test[, 1]

## scale values

# minmax_scale <- function(x) {
#   (x-min(x))/(max(x)-min(x))
# }
# train_x <- apply(train_x, 2, minmax_scale)
# test_x <- apply(test_x, 2, minmax_scale)

z_score <- function(x){
  (x-mean(x))/sd(x)
}
train_x <- apply(train_x, 2, z_score)
test_x <- apply(test_x, 2, z_score)

## define autoencoder

code_size <- 2
encoder_input <- layer_input(shape = ncol(train_x))

encoder_output <- encoder_input %>%
  layer_dense(units = 16, activation = "tanh", use_bias = TRUE) %>%
  layer_dense(units = code_size, activation = "tanh")

encoder <- keras_model(encoder_input, encoder_output)
summary(encoder)

decoder_input <- layer_input(shape = code_size)
decoder_output <- decoder_input %>%
  layer_dense(units = 16, activation = "tanh", use_bias = TRUE) %>%
  layer_dense(units = ncol(train_x))

decoder <- keras_model(decoder_input, decoder_output)
summary(decoder)

ae_input <- layer_input(shape = ncol(train_x))
ae_output <- ae_input %>% 
  encoder() %>% 
  decoder()
ae <- keras_model(ae_input, ae_output)
summary(ae)  

ae %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = "mean_absolute_error",
) 

ae %>% fit(
  x = train_x,
  y = train_x,
  epochs = 100,
  batch_size = 16,
  verbose = 1,
)

code_ae_train <- encoder %>% 
  predict(train_x)
code_ae_test <- encoder %>% 
  predict(test_x)

## Classification with logit regression

class_train <- cbind(train_y, code_ae_train)
class_train$class <- as.factor(class_train$class)

logit <- multinom(class ~ V1 + V2,
                  data = class_train)
logit_predict <- predict(logit, code_ae_test)

## Evaluation

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
