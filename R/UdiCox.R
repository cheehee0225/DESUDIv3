runUdiCox <- function(outputFolder, exportFolder, databaseId, minCellCount = 5,
                      targetIds = 980:983, analysisIds = 1:8) {
  ref <- readRDS(file.path(outputFolder, "cmOutput", "outcomeModelReference.rds"))
  todo <- ref[ref$analysisId %in% analysisIds & ref$targetId %in% targetIds, ]
  todo <- todo[order(todo$cohortMethodDataFile), ]
  cache <- list()
  res <- list()
  for (i in seq_len(nrow(todo))) {
    r <- todo[i, ]
    u <- ref[ref$analysisId == r$analysisId + 100 & ref$targetId == r$targetId &
               ref$comparatorId == r$comparatorId & ref$outcomeId == r$outcomeId, ]
    if (nrow(u) == 0) next
    pop <- getStudyPop(outputFolder, r)
    if (is.null(pop) || length(unique(pop$treatment)) < 2) next
    f1 <- r$cohortMethodDataFile
    f2 <- u$cohortMethodDataFile[1]
    if (is.null(cache[[f1]])) cache[[f1]] <- getCmdCohorts(outputFolder, f1, FALSE)
    if (is.null(cache[[f2]])) cache[[f2]] <- getCmdCohorts(outputFolder, f2, TRUE)
    d <- merge(pop, cache[[f1]], by = "rowId")
    d <- merge(d, cache[[f2]], by =c("personId", "treatment", "cohortStartDate"), all.x = TRUE)
    res[[length(res) + 1]] <- fitUdiCox(d, r, minCellCount)
  }
  out <- do.call(rbind, res)
  out <- data.frame(database_id = databaseId, out)
  write.csv(out, file.path(exportFolder, "udi_cox_result.csv"), row.names = FALSE)
  invisible(out)
}

getCmdCohorts <- function(outputFolder, fileName, withUdi) {
  cmd <- CohortMethod::loadCohortMethodData(file.path(outputFolder, "cmOutput", fileName))
  co <- as.data.frame(cmd$cohorts)
  if (!withUdi) return(co[, c("rowId", "personId")])
  ids <- c(9981, 9982, 9983, 9991, 9992, 9993)
  cv <- as.data.frame(dplyr::filter(cmd$covariates, covariateId %in% ids))
  dia <- cv[cv$covariateId < 9990, c("rowId", "covariateId")]
  len <- cv[cv$covariateId > 9990, c("rowId", "covariateId")]
  colnames(dia)[2] <- "udiDia"
  colnames(len)[2] <- "udiLen"
  co <- merge(co[, c("rowId", "personId", "treatment", "cohortStartDate")], dia, all.x = TRUE)
  co <- merge(co, len, all.x = TRUE)
  co$rowId <- NULL
  co
}

fitUdiCox <- function(d, r, minCellCount) {
  d <- d[!is.na(d$udiDia) & !is.na(d$udiLen), ]
  d$status <- as.integer(d$outcomeCount > 0)
  d$udiDia <- factor(d$udiDia)
  d$udiLen <- factor(d$udiLen)
  adj <- c("udiDia", "udiLen")[c(nlevels(d$udiDia), nlevels(d$udiLen)) > 1]
  fs <- list(without_udi = "treatment + strata(stratumId)",
             with_udi = paste(c("treatment", adj, "strata(stratumId)"), collapse = " +"))
  cnt <- function(x) ifelse(x > 0 & x < minCellCount, -minCellCount, x)
  rows <- lapply(names(fs), function(m) {
    est <- c(NA, NA, NA, NA)
    fit <- tryCatch(survival::coxph(as.formula(paste("Surv(survivalTime, status) ~", fs[[m]]), env = asNamespace("survival")), data = d),
                    error = function(e) NULL)
    if (!is.null(fit) && "treatment" %in% names(coef(fit)) && sum(d$status[d$treatment == 1]) > 0 && sum(d$status[d$treatment == 0]) > 0) {
      s <- summary(fit)
      est <- c(s$conf.int["treatment", c(1,3,4)], s$coefficients["treatment", 5])
    }
    data.frame(analysis_id = r$analysisId, target_id = r$targetId, comparator_id = r$comparatorId,
               outcome_id = r$outcomeId, model = m, covariates = fs[[m]],
               target_subjects = cnt(sum(d$treatment == 1)), comparator_subjects = cnt(sum(d$treatment == 0)),
               target_outcomes = cnt(sum(d$status[d$treatment == 1])),
               comparator_outcomes = cnt(sum(d$status[d$treatment == 0])),
               hr = est[1], ci_95_lb = est[2], ci_95_ub = est[3], p = est[4], row.names = NULL)
  })
  do.call(rbind, rows)
}

udiCache <- new.env()

getStudyPop <- function(outputFolder, r) {
  if(r$strataFile != "") return(readRDS(file.path(outputFolder, "cmOutput", r$strataFile)))
  if(r$sharedPsFile == "") return(NULL)
  if(!identical(udiCache$file, r$cohortMethodDataFile)) {
    udiCache$cmd <- CohortMethod::loadCohortMethodData(file.path(outputFolder, "cmOutput", r$cohortMethodDataFile))
    udiCache$file <- r$cohortMethodDataFile
    udiCache$al <- getStudyCmAnalysisList()
  }
  a <- udiCache$al[[which(sapply(udiCache$al, function(x) x$analysisId) == r$analysisId)]]
  args <- a$createStudyPopArgs
  args$cohortMethodData <- udiCache$cmd
  args$outcomeId <- r$outcomeId
  pop <- do.call(CohortMethod::createStudyPopulation, args)
  ps <- readRDS(file.path(outputFolder, "cmOutput", r$sharedPsFile))
  pop$propensityScore <- ps$propensityScore[match(pop$rowId, ps$rowId)]
  if (a$matchOnPs) pop <- do.call(CohortMethod::matchOnPs, append(list(population = pop), a$matchOnPsArgs))
  if (a$stratifyByPs) pop <- do.call(CohortMethod::stratifyByPs, append(list(population = pop), a$stratifyByPsArgs))
  pop
}