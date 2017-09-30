#' Summarize data to quickly learn about it.
#'
#' @param data Data frame that you want to explore.
#' @param type Default is basic. For more advanced summary do type = "advanced".
#' @return A data frame
#' @export

summarize_data <- function(data, type = "basic") {
  if (type == "basic") {

    describe <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )
    describe <- mutate(describe,
                       null_perc = round(null_count/total_count, 6),
                       distinct_perc = round(distinct_count/total_count, 6)
    ) %>%
      select(variable, class, null_perc, distinct_perc, null_count, distinct_count, total_count)

  } else {

    describe1 <- tibble::tibble(
      variable = names(data),
      class = purrr::map_chr(data, ~class(.x)),
      null_count = purrr::map_dbl(data, ~sum(is.na(.x))),
      distinct_count = purrr::map_dbl(data, ~data.table::uniqueN(.x)),
      total_count = purrr::map_dbl(data, ~length(.x))
    )

    distinct <- describe1 %>% filter(total_count == distinct_count) %>% lselect(variable)

    if (length(distinct) != 0) {

      tbl_distinct <- filter(describe1, variable == distinct) %>%
        mutate(table = list(tibble(value = "distinct",
                                   count = total_count,
                                   rank = 1) %>% distinct)
        ) %>%
        select(variable, table)

      tbl_describe <- tibble::tibble(
        variable = names(select(data, -one_of(distinct))),
        table = select(data, -one_of(distinct)) %>%
          purrr::map(~ data.table::data.table(.x)[, .N, keyby = .x] %>%
                       as_tibble %>%
                       rename(count = N, value = .x) %>%
                       mutate(value = as.character(value)) %>%
                       mutate(rank = min_rank(desc(count))) %>%
                       arrange(desc(count))
          )
      )

      describe1 <- describe1 %>%
        left_join(bind_rows(tbl_distinct, tbl_describe), by = "variable")

      names(describe1$table) <- describe1$variable

    } else {

      describe1 <- describe1 %>%
        mutate(
          table = purrr::map(data, ~ data.table::data.table(.x)[, .N, keyby = .x] %>%
                               as_tibble %>%
                               rename(count = N, value = .x) %>%
                               mutate(value = as.character(value)) %>%
                               mutate(rank = min_rank(desc(count))) %>%
                               arrange(desc(count)))
        )
    }

    numeric <- select_if(data, is.numeric)

    describe2 <- tibble::tibble(
      variable = names(numeric),
      max = purrr::map_dbl(numeric, ~max(.x, na.rm = TRUE)),
      min = purrr::map_dbl(numeric, ~min(.x, na.rm = TRUE)),
      standard_deviation = purrr::map_dbl(numeric, ~sd(.x, na.rm = TRUE)),
      mean = purrr::map_dbl(numeric, ~mean(.x, na.rm = TRUE)),
      median = purrr::map_dbl(numeric, ~median(.x, na.rm = TRUE))
    )

    top <- describe1 %>%
      lselect(table) %>%
      filter(rank == 1 | rank == 2) %>%
      group_by(name, rank) %>%
      mutate(top_count = n()) %>%
      arrange(name, rank) %>%
      ungroup() %>%
      mutate(top_value = if_else(top_count == 1, value, "multiple")) %>%
      select(name, rank, top_value) %>%
      distinct() %>%
      tidyr::spread(rank, top_value) %>%
      rename(top_1 = `1`, top_2 = `2`) %>%
      mutate(top_2 = if_else((top_1 %in% c("multiple","distinct") & is.na(top_2)), top_1, top_2))

    describe <- describe1 %>%
      left_join(describe2, by = "variable") %>%
      left_join(top, by = c("variable" = "name")) %>%
      mutate(null_perc = round(null_count/total_count, 6),
             distinct_perc = round(distinct_count/total_count, 6)
      ) %>%
      select(variable, class, null_perc, distinct_perc, top_1, top_2, max, min, mean, median, standard_deviation, null_count, distinct_count, total_count, table)
    names(describe$table) <- describe$variable
  }
  return(describe)
}

#' Summary of variable imporance.
#'
#' @param data Data frame that you want to explore. Processes about 378,000 data cells per second.
#' @param var_outcome Outcome variable.
#' @param objective Either "reg:linear" or "reg:logistic".
#' @return A data frame
#' @export

summarize_importance <- function(data, var_outcome = "none", objective = "reg:linear") {
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
  var_outcome <- if (var_outcome != "none") {
    importance %>% filter(origName == var_outcome) %>% lselect(variable)
    } else {var_outcome}

  if (var_outcome != "none") {
    treated <- select(treated, one_of(importance$variable)) %>%
      select(var_outcome, everything())
  } else {
    treated <- select(treated, one_of(importance$variable))
  }

  # correlation matrix ---------------------------

  corr_importance <- Hmisc::rcorr(as.matrix(treated))$r %>% as.data.frame %>%
    tibble::rownames_to_column(var = "vars") %>%
    tidyr::gather(-vars, key = "variable", value = "correlation") %>%
    left_join(select(importance, origName, variable), "variable") %>%
    arrange(vars, desc(abs(correlation)))

  if (var_outcome != "none") {
    corr_outcome <- corr_importance %>%
      filter(vars == var_outcome) %>%
      select(-vars, -origName)
  }

  corr_importance <- corr_importance %>%
    filter(variable != vars) %>%
    select(vars, origName, correlation) %>%
    rename(variable = origName) %>%
    group_by(vars) %>%
    tidyr::nest(.key = corr_tbl)

  # variable importance --------------------------

  if (var_outcome != "none") {
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
  } else {
    importance <- importance %>%
      left_join(corr_importance, c("variable" = "vars")) %>%
      select(-variable, -code) %>%
      rename(variable = origName)
  }

  names(importance$corr_tbl) <- importance$variable

  top <- lselect(importance, corr_tbl) %>%
    group_by(name) %>%
    mutate(rank = min_rank(desc(abs(correlation))),
           corr50_count = length(correlation[abs(correlation) >= .5])) %>%
    filter(rank == 1 | rank == 2) %>%
    group_by(name, rank) %>%
    mutate(top_count = n()) %>%
    arrange(name, rank) %>%
    ungroup() %>%
    mutate(variable = paste(variable, "(", round(correlation,2), ")", sep = "")) %>%
    mutate(top_value = if_else(top_count == 1, variable, "multiple")) %>%
    select(name, rank, top_value, corr50_count) %>%
    distinct() %>%
    tidyr::spread(rank, top_value) %>%
    rename(corr_1 = `1`, corr_2 = `2`) %>%
    mutate(corr_2 = if_else((corr_1 %in% c("multiple","distinct") & is.na(corr_2)), corr_1, corr_2))

  if (var_outcome != "none") {
    importance <- importance %>%
      left_join(top, c("variable" = "name")) %>%
      mutate(r2_outcome = correlation^2) %>%
      select(variable, bst_gain, bst_rank, r2_outcome, corr50_count, corr_1, corr_2, corr_tbl)
  } else {
    importance <- importance %>%
      left_join(top, c("variable" = "name")) %>%
      select(variable, corr50_count, corr_1, corr_2, corr_tbl) %>%
      arrange(desc(corr50_count))
  }

  names(importance$corr_tbl) <- importance$variable

  return(importance)
}

#'
#' summarize_plots <- function() {
#'
#' }
#'
#' summarize_models <- function() {
#'
#' }
