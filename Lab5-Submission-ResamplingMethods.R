.libPaths()

lapply(.libPaths(), list.files)

if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

if (!is.element("mlbench", installed.packages()[, 1])) {
  install.packages("mlbench", dependencies = TRUE)
}
require("mlbench")

data("PimaIndiansDiabetes")

dim(PimaIndiansDiabetes)

sapply(PimaIndiansDiabetes, class)

str(PimaIndiansDiabetes)

train_index <- createDataPartition(PimaIndiansDiabetes$diabetes,
                                   p = 0.75,
                                   list = FALSE)
PimaIndiansDiabetes_train <- PimaIndiansDiabetes[train_index, ]
PimaIndiansDiabetes_test <- PimaIndiansDiabetes[-train_index, ]

PimaIndiansDiabetes_model_nb_e1071 <- # nolint
  e1071::naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass +
                      pedigree + age,
                    data = PimaIndiansDiabetes_train)

PimaIndiansDiabetes_model_nb_caret <- # nolint
  caret::train(diabetes ~ ., data =
                 PimaIndiansDiabetes_train[, c("pregnant", "glucose", "pressure",
                                             "triceps", "insulin", "mass",
                                             "pedigree",
                                             "age",
                                             "diabetes")],
               method = "naive_bayes")

predictions_nb_e1071 <-
  predict(PimaIndiansDiabetes_model_nb_e1071,
          PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                       "triceps", "insulin", "mass",
                                       "pedigree",
                                       "age")])

predictions_nb_caret <-
  predict(PimaIndiansDiabetes_model_nb_caret,
          PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                     "triceps", "insulin", "mass",
                                     "pedigree",
                                     "age")])

print(predictions_nb_e1071)
caret::confusionMatrix(predictions_nb_e1071,
                       PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                                  "triceps", "insulin", "mass",
                                                  "pedigree",
                                                  "age", "diabetes")]$diabetes)

plot(table(predictions_nb_e1071,
           PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                        "triceps", "insulin", "mass",
                                        "pedigree",
                                        "age", "diabetes")]$diabetes))

print(PimaIndiansDiabetes_model_nb_caret)
caret::confusionMatrix(predictions_nb_e1071,
                       PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                                    "triceps", "insulin", "mass",
                                                    "pedigree",
                                                    "age", "diabetes")]$diabetes)
plot(table(predictions_nb_caret,
           PimaIndiansDiabetes_test[, c("pregnant", "glucose", "pressure",
                                        "triceps", "insulin", "mass",
                                        "pedigree",
                                        "age", "diabetes")]$diabetes))


train_index <- createDataPartition(PimaIndiansDiabetes$`diabetes`, # nolint
                                   p = 0.80, list = FALSE)
PimaIndiansDiabetes_train <- PimaIndiansDiabetes[train_index, ]
PimaIndiansDiabetes_test <- PimaIndiansDiabetes[-train_index, ]

PimaIndiansDiabetes_model_nb_klaR <- # nolint
  klaR::NaiveBayes(`diabetes` ~ .,
                   data = PimaIndiansDiabetes_train)

PimaIndiansDiabetes_model_nb_e1071 <- # nolint
  e1071::naiveBayes(`diabetes` ~ .,
                    data = PimaIndiansDiabetes_train)

predictions_nb_e1071 <-
  predict(PimaIndiansDiabetes_model_nb_e1071,
          PimaIndiansDiabetes_test[, 1:9])

print(PimaIndiansDiabetes_model_nb_e1071)
caret::confusionMatrix(predictions_nb_e1071,
                       PimaIndiansDiabetes_test$`diabetes`)

plot(table(predictions_nb_e1071,
           PimaIndiansDiabetes_test$`diabetes`))


PimaIndiansDiabetes_cor <- cor(PimaIndiansDiabetes[, 1:8])
View(PimaIndiansDiabetes_cor)


train_index <-
  createDataPartition(PimaIndiansDiabetes$`diabetes`,
                      p = 0.75, list = FALSE)
PimaIndiansDiabetes_train <- PimaIndiansDiabetes[train_index, ] # nolint
PimaIndiansDiabetes_test <- PimaIndiansDiabetes[-train_index, ] # nolint



train_control <- trainControl(method = "boot", number = 500)

PimaIndiansDiabetes_model_lm <- caret::train(`insulin` ~
                                               `pregnant` + `glucose` +
                                               `pressure` + `triceps` +
                                               `mass` + `pedigree` +
                                               `age`,
                                             data = PimaIndiansDiabetes_train,
                                             trControl = train_control,
                                             na.action = na.omit, method = "lm", metric = "RMSE")

predictions_lm <- predict(PimaIndiansDiabetes_model_lm, newdata = PimaIndiansDiabetes_test)


print(PimaIndiansDiabetes_model_lm)
print(predictions_lm)

new_data <-
  data.frame(`diabetes` = c(1), `insulin` = c(0),
               `pregnant` = c(6), `glucose` = c(88),
               `pressure` = c(72), `triceps` = c(30),
               `mass` = c(35.3), `pedigree` = c(0.349),
               `age` = c(40))
new_data$`diabetes` <-
  as.factor(new_data$`diabetes`) # nolint

predictions_lm_new_data <-
  predict(PimaIndiansDiabetes_model_lm, new_data)

print(predictions_lm_new_data)


train_index <- createDataPartition(PimaIndiansDiabetes$`insulin`,
                                   p = 0.75, list = FALSE)
PimaIndiansDiabetes_train <- PimaIndiansDiabetes[train_index, ]
PimaIndiansDiabetes_test <- PimaIndiansDiabetes[-train_index, ]

train_control <- trainControl(method = "cv", number = 10)

PimaIndiansDiabetes_model_lm <- caret::train(`insulin` ~ `glucose` + `pressure` + `triceps` + `mass` + `pedigree` + `age`,
                                             data = PimaIndiansDiabetes_train,
                                             trControl = train_control, na.action = na.omit,
                                             method = "lm", metric = "RMSE")

predictions_lm <- predict(PimaIndiansDiabetes_model_lm, PimaIndiansDiabetes_test[, 2:9])

print(PimaIndiansDiabetes_model_lm)
print(predictions_lm)

train_control <- trainControl(method = "cv", number = 5)

PimaIndiansDiabetes_model_lda <-
  caret::train(`diabetes` ~ ., data = PimaIndiansDiabetes_train,
               trControl = train_control, na.action = na.omit, method = "lda2",
               metric = "Accuracy")

predictions_lda <- predict(PimaIndiansDiabetes_model_lda,
                           PimaIndiansDiabetes_test[, 1:9])

print(PimaIndiansDiabetes_model_lda)
caret::confusionMatrix(predictions_lda, PimaIndiansDiabetes_test$diabetes)

PimaIndiansDiabetes_model_nb <-
  e1071::naiveBayes(`diabetes` ~ ., data = PimaIndiansDiabetes_train)

predictions_nb_e1071 <-
  predict(PimaIndiansDiabetes_model_nb, PimaIndiansDiabetes_test[, 1:9])

print(PimaIndiansDiabetes_model_nb)
caret::confusionMatrix(predictions_nb_e1071, PimaIndiansDiabetes_test$diabetes)


train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

PimaIndiansDiabetes_model_svm <-
  caret::train(`diabetes` ~ ., data = PimaIndiansDiabetes_train,
               trControl = train_control, na.action = na.omit,
               method = "svmLinearWeights2", metric = "Accuracy")

predictions_svm <- predict(PimaIndiansDiabetes_model_svm, PimaIndiansDiabetes_test[, 1:9])

print(PimaIndiansDiabetes_model_svm)
caret::confusionMatrix(predictions_svm, PimaIndiansDiabetes_test$diabetes)



train_control <- trainControl(method = "LOOCV")

PimaIndiansDiabetes_model_nb_loocv <-
  caret::train(`diabetes` ~ ., data = PimaIndiansDiabetes_train,
               trControl = train_control, na.action = na.omit,
               method = "naive_bayes", metric = "Accuracy")

predictions_nb_loocv <-
  predict(PimaIndiansDiabetes_model_nb_loocv, PimaIndiansDiabetes_test[, 1:9])

print(PimaIndiansDiabetes_model_nb_loocv)
caret::confusionMatrix(predictions_nb_loocv, PimaIndiansDiabetes_test$diabetes)
