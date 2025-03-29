library(corrplot)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(car)
library(caret)

data_raw <- read.csv("Diamond Data.csv")
str(data_raw)

data <- data_raw
data <- data[data$Report!="AGS",]

diamond <- data

hist(diamond$Carat.Size, main = "Distribution of Carat Size", xlab = "Price")
hist(diamond$Price, main = "Distribution of Price", xlab = "Price")
barplot(table(diamond$Clarity), main = "Distribution of Clarity", xlab = "Clarity", ylab = "Count")
barplot(table(diamond$Cut), main = "Distribution of Cut", xlab = "Cut", ylab = "Count")

numeric_data <- diamond[sapply(diamond, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "full", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

ggplot(diamond, mapping=aes(x=Carat.Size, y=Price, color=Color)) +
  geom_point() +
  facet_grid(Cut ~ Clarity)

diamond$Cut <- as.factor(diamond$Cut)
diamond$Clarity <- as.factor(diamond$Clarity)
diamond$Color <- as.factor(diamond$Color)


PRESS <- function(model) {
  i <- residuals(model)/(1 - lm.influence(model)$hat)
  sum(i^2)
}

model4 <- lm(log(Price) ~ Carat.Size + I(Carat.Size^2) + I(Carat.Size^3) + 
               Color + Cut + Clarity + 
               Carat.Size:Color + Carat.Size:Cut+ Carat.Size:Clarity +  Color:Clarity,  data=diamond)
summary(model4)


autoplot(model4)
influencePlot(model4)
PRESS(model4)
vif(model4)
AIC(model4) # AIC(model3, model4) for comparison


cv_control <- trainControl(method = "cv", number = 10)

cv_model <- train(log(Price) ~ Carat.Size + I(Carat.Size^2) +I(Carat.Size^3) + Color + Cut + Clarity +
                    Carat.Size:Color + Carat.Size:Cut + Carat.Size:Clarity + Color:Clarity,
                  data = diamond,
                  method = "lm",
                  trControl = cv_control)

print(cv_model)

train_preds <- predict(model4, newdata = diamond)
train_rmse <- sqrt(mean((train_preds - log(diamond$Price))^2))
cat("Training RMSE:", train_rmse, "\n")

saveRDS(model4, file = "tool/Estimator/model_final.rds")

df <- data.frame(
  log_price = log(diamond$Price),
  fitted_values = model4$fitted.values
)

pred <- predict(model4, interval = "confidence")
df$lower <- pred[, "lwr"]
df$upper <- pred[, "upr"]

ggplot(df, aes(x = log_price, y = fitted_values)) +
  geom_point(color = "blue", alpha = 0.7) +  # Scatter plot of fitted values
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.02, color = "red", alpha = 0.2) +  # Confidence interval bars
  labs(x = "Log(Price)", y = "Fitted Values", title = "Fitted Values with Confidence Interval") +
  theme_minimal()

