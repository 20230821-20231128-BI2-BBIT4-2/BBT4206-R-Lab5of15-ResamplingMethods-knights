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

## Load sample datasets that are provided as part of a package ----
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

