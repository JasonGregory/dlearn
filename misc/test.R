i_data <- readr::read_rds("inst/other_data/propertyData.rds")
titanic <- readr::read_csv("inst/other_data/train.csv")

library(dplyr)
library(dlearn)
options(max.print=999999)
options(scipen=999)

# test different datasets -------------------------------------------------
explore_importance(mtcars, "mpg")
cor(mtcars$mpg, mtcars$disp, method = "pearson")
explore_importance(select(titanic, -PassengerId), "Survived", "reg:logistic")
temp <- explore_importance(select(i_data, -parcelid, -taxvaluedollarcnt), "taxamount")

# updated code ------------------------------------------------------------

#data <- mtcars
#var_outcome <- "mpg"
#objective <- "reg:linear"

data <- select(i_data, -parcelid, -taxvaluedollarcnt)
var_outcome <- "taxamount"
objective <- "reg:linear"

explore_importance <- function(data, var_outcome, objective = "reg:linear") {
  # Preparing importance data
  treatment <- vtreat::designTreatmentsZ(data, colnames(data), verbose = FALSE)
  treated <- as_tibble(vtreat::prepare(treatment, data)) %>%
    select(-contains("catP"))
  importance <- tibble(variable = names(treated)) %>%
    left_join(treatment$scoreFrame, c("variable" = "varName")) %>%
    select(variable, origName, code) %>%
    group_by(origName) %>%
    mutate(var_count = n()) %>%
    filter((var_count == 2 & code != "isBAD") | var_count == 1) %>%
    select(-var_count) %>%
    filter(row_number() == 1) %>%
    ungroup
  var_outcome <- importance %>% filter(origName == var_outcome) %>% lselect(variable)
  treated <- select(treated, one_of(importance$variable)) %>%
    select(var_outcome, everything())

  # correlation matrix ---------------------------

  corr_importance <- Hmisc::rcorr(as.matrix(treated))$r %>% as.data.frame %>%
    tibble::rownames_to_column(var = "vars") %>%
    tidyr::gather(-vars, key = "variable", value = "correlation") %>%
    left_join(select(importance, origName, variable), "variable") %>%
    arrange(vars, desc(abs(correlation)))

  corr_outcome <- corr_importance %>%
    filter(vars == var_outcome) %>%
    select(-vars, -origName)

  corr_importance <- corr_importance %>%
    filter(variable != vars) %>%
    select(vars, origName, correlation) %>%
    rename(variable = origName) %>%
    group_by(vars) %>%
    tidyr::nest(.key = corr_tbl)

  # variable importance --------------------------

  trainIndex <- caret::createDataPartition(lselect(treated, var_outcome),
                                           p = .75,
                                           list = FALSE,
                                           times = 1)

  dataTrain <- treated[trainIndex,]
  dataTest <- treated[-trainIndex,]

  dTrain <- xgboost::xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = lselect(dataTrain, var_outcome))
  dTest <- xgboost::xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = lselect(dataTest, var_outcome))

  # Boosted Model (will need a paramater to adjust for the objective function)
  watchlist <- list(train=dTrain, test=dTest)
  bst <- xgboost::xgb.train(data=dTrain, nround=200, watchlist=watchlist, objective = objective,
                            early_stopping_rounds = 5, verbose = FALSE, eta = .3)

  bst_importance <- as_tibble(xgboost::xgb.importance(colnames(dataTrain[,-1]), model = bst))
  bst_importance <- bst_importance %>%
    mutate(bst_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, bst_gain = Gain) %>%
    select(variable, bst_gain, bst_rank)

  importance <- importance %>%
    left_join(bst_importance, "variable") %>%
    mutate(bst_gain = if_else(is.na(bst_gain), 0, bst_gain),
           bst_rank = if_else(is.na(bst_rank), max(bst_importance$bst_rank), bst_rank),
           bst_rank = if_else(variable == var_outcome, nrow(importance), bst_rank)
           ) %>%
    left_join(corr_outcome, "variable") %>%
    left_join(corr_importance, c("variable" = "vars")) %>%
    select(-variable, -code) %>%
    rename(variable = origName) %>%
    arrange(bst_rank)

  names(importance$corr_tbl) <- importance$variable

  top <- lselect(importance, corr_tbl) %>%
    group_by(name) %>%
    mutate(rank = min_rank(desc(abs(correlation)))) %>%
    filter(rank == 1 | rank == 2) %>%
    group_by(name, rank) %>%
    mutate(top_count = n()) %>%
    arrange(name, rank) %>%
    ungroup() %>%
    mutate(variable = paste(variable, "(", round(correlation,2), ")", sep = "")) %>%
    mutate(top_value = if_else(top_count == 1, variable, "multiple")) %>%
    select(name, rank, top_value) %>%
    distinct() %>%
    tidyr::spread(rank, top_value) %>%
    rename(corr_1 = `1`, corr_2 = `2`) %>%
    mutate(corr_2 = if_else((corr_1 %in% c("multiple","distinct") & is.na(corr_2)), corr_1, corr_2))

  importance <- importance %>%
    left_join(top, c("variable" = "name")) %>%
    mutate(r2_outcome = correlation^2) %>%
    select(variable, bst_gain, bst_rank, correlation, r2_outcome, corr_1, corr_2, corr_tbl) %>%
    rename(corr_outcome = correlation)

  return(importance)
}
