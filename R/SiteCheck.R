# Site check: aggregate counts that explain empty cohorts or zero outcome events.
# Output: <outputFolder>/site_check.csv (copied into export by exportResults).
# Counts between 1 and minCellCount - 1 are written as -minCellCount.

runSiteCheck <- function(connection,
                         cdmDatabaseSchema,
                         vocabularyDatabaseSchema = cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         cohortTable,
                         outputFolder,
                         minCellCount = 5) {
  dbms <- attr(connection, "dbms")
  run <- function(checkType, item, sql) {
    sql <- SqlRender::render(sql,
                             cdm = cdmDatabaseSchema,
                             vocab = vocabularyDatabaseSchema,
                             cohort_database_schema = cohortDatabaseSchema,
                             cohort_table = cohortTable)
    sql <- SqlRender::translate(sql, targetDialect = dbms)
    res <- tryCatch(DatabaseConnector::querySql(connection, sql),
                    error = function(e) NULL)
    if (is.null(res)) {
      return(data.frame(check_type = checkType, item = item, n_records = NA, n_persons = NA,
                        note = "query failed"))
    }
    names(res) <- tolower(names(res))
    data.frame(check_type = checkType, item = item,
               n_records = res$n_records[1], n_persons = res$n_persons[1], note = "")
  }

  rows <- list()

  # 1. Cohort sizes (all cohorts, including empty ones)
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "DESUDIv3")
  cohorts <- read.csv(pathToCsv)
  for (id in sort(cohorts$cohortId)) {
    rows[[length(rows) + 1]] <- run("cohort", as.character(id), paste0(
      "SELECT COUNT(*) AS n_records, COUNT(DISTINCT subject_id) AS n_persons ",
      "FROM @cohort_database_schema.@cohort_table WHERE cohort_definition_id = ", id, ";"))
  }

  # 2. How each product EDI code is found in DEVICE_EXPOSURE
  edi <- c(Orsiro = "J5083094", Resolute_Onyx = "J8083006", BioMime = "J8083033", Synergy = "J8083129")
  for (p in names(edi)) {
    code <- edi[[p]]
    item <- paste0(p, " ", code)
    bySv <- paste0("de.device_source_value LIKE '%", code, "%'")
    byConcept <- paste0("de.device_source_concept_id IN (SELECT concept_id FROM @vocab.CONCEPT WHERE concept_code = '", code, "')")
    rows[[length(rows) + 1]] <- run("edi_concept_in_vocabulary", item, paste0(
      "SELECT COUNT(*) AS n_records, 0 AS n_persons FROM @vocab.CONCEPT WHERE concept_code = '", code, "';"))
    rows[[length(rows) + 1]] <- run("edi_by_source_value", item, paste0(
      "SELECT COUNT(*) AS n_records, COUNT(DISTINCT de.person_id) AS n_persons ",
      "FROM @cdm.DEVICE_EXPOSURE de WHERE ", bySv, ";"))
    rows[[length(rows) + 1]] <- run("edi_by_source_concept_id", item, paste0(
      "SELECT COUNT(*) AS n_records, COUNT(DISTINCT de.person_id) AS n_persons ",
      "FROM @cdm.DEVICE_EXPOSURE de WHERE ", byConcept, ";"))
    rows[[length(rows) + 1]] <- run("edi_either_with_des_concept", item, paste0(
      "SELECT COUNT(*) AS n_records, COUNT(DISTINCT de.person_id) AS n_persons ",
      "FROM @cdm.DEVICE_EXPOSURE de WHERE de.device_concept_id = 45772824 AND (", bySv, " OR ", byConcept, ");"))
    rows[[length(rows) + 1]] <- run("edi_either_with_udi", item, paste0(
      "SELECT COUNT(*) AS n_records, COUNT(DISTINCT de.person_id) AS n_persons ",
      "FROM @cdm.DEVICE_EXPOSURE de WHERE de.device_concept_id = 45772824 AND (", bySv, " OR ", byConcept, ") ",
      "AND de.unique_device_id IS NOT NULL AND de.unique_device_id <> '';"))
  }

  # 3. Building blocks of the CST-related revascularization outcome (963)
  rows[[length(rows) + 1]] <- run("cst_component", "CST/AMI condition in IP/ER visit", paste0(
    "SELECT COUNT(*) AS n_records, COUNT(DISTINCT co.person_id) AS n_persons ",
    "FROM @cdm.CONDITION_OCCURRENCE co ",
    "JOIN @cdm.VISIT_OCCURRENCE v ON co.visit_occurrence_id = v.visit_occurrence_id AND co.person_id = v.person_id ",
    "WHERE v.visit_concept_id IN (9201, 9203, 262) AND co.condition_concept_id IN ",
    "(SELECT descendant_concept_id FROM @vocab.CONCEPT_ANCESTOR WHERE ancestor_concept_id IN (4225958, 4108215, 4048534, 4046022));"))
  rows[[length(rows) + 1]] <- run("cst_component", "PCI procedure (4283892 and descendants)", paste0(
    "SELECT COUNT(*) AS n_records, COUNT(DISTINCT po.person_id) AS n_persons ",
    "FROM @cdm.PROCEDURE_OCCURRENCE po WHERE po.procedure_concept_id IN ",
    "(SELECT descendant_concept_id FROM @vocab.CONCEPT_ANCESTOR WHERE ancestor_concept_id = 4283892);"))
  rows[[length(rows) + 1]] <- run("cst_component", "Drug-eluting stent device (45772824)", paste0(
    "SELECT COUNT(*) AS n_records, COUNT(DISTINCT de.person_id) AS n_persons ",
    "FROM @cdm.DEVICE_EXPOSURE de WHERE de.device_concept_id = 45772824;"))

  out <- do.call(rbind, rows)
  mask <- function(x) ifelse(!is.na(x) & x > 0 & x < minCellCount, -minCellCount, x)
  isVocab <- out$check_type == "edi_concept_in_vocabulary"
  out$n_records[!isVocab] <- mask(out$n_records[!isVocab])
  out$n_persons[!isVocab] <- mask(out$n_persons[!isVocab])
  write.csv(out, file.path(outputFolder, "site_check.csv"), row.names = FALSE)
  invisible(out)
}
