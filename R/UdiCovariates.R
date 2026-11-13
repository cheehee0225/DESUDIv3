
createUDIDCovariateSettings <- function(useUDIDiameters = TRUE) {
  covariateSettings <- list(useUDIDiameters = useUDIDiameters) 
  attr(covariateSettings, "fun") <- "DESUDIv3::getDbUDIDCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


getDbUDIDCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cohortTable = "#cohort_person",
                                   cohortId = -1,
                                   cdmVersion = "5",
                                   rowIdField = "subject_id",
                                   covariateSettings,
                                   aggregated = FALSE) {
  writeLines("Constructing UDI diameter covariates")
  if (covariateSettings$useUDIDiameters == FALSE) {
    return(NULL)
  }
  if (aggregated)
    stop("Aggregation not supported")
  # Some SQL to construct the covariate:
  source_udi <- read.csv(system.file("csv", "DesProductMapping.csv", package = "DESUDIv3"))
  source_udi <- source_udi %>% mutate(
    diameter = as.numeric(sapply(strsplit(spec, "\\*"), `[`, 1)),
    length = as.numeric(sapply(strsplit(spec, "\\*"), `[`, 2)),
    uniqueDeviceId = as.character(udi_di))
  
  sql <- paste("SELECT @row_id_field AS row_id, 998 AS covariate_id,",
               "DEVICE_SOURCE_VALUE, UNIQUE_DEVICE_ID",
               "FROM @cohort_table c",
               "INNER JOIN @cdm_database_schema.DEVICE_EXPOSURE de",
               "ON de.person_id = c.subject_id",
               "WHERE cohort_start_date = DEVICE_EXPOSURE_START_DATE",
               "AND (de.DEVICE_SOURCE_VALUE like '%J5083094%' or de.DEVICE_SOURCE_VALUE like '%J8083033%' or de.DEVICE_SOURCE_VALUE like '%J8083006%' or de.device_source_value like '%J8083129%')",
               "{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}")
  sql <- SqlRender::render(sql,
                           cohort_table = cohortTable,
                           cohort_id = cohortId,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates_tmp <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  covariates_tmp <- covariates_tmp %>% mutate(uniqueDeviceId = as.character(as.numeric(uniqueDeviceId))) %>% left_join(source_udi, by = "uniqueDeviceId") %>% select(rowId, covariateId, diameter)
  covariates <- covariates_tmp %>% filter(!is.na(diameter)) %>% group_by(rowId, covariateId) %>% slice_min(diameter) %>% unique()
  # covariates <- covariates %>% filter(!is.na(diameter)) %>% select("rowId", "covariateId") %>% mutate(covariateValue = 1)
  colnames(covariates) <- c("rowId", "covariateId", "covariateValue")
  covariates <- covariates %>% ungroup() %>% mutate(covariateId = ifelse(covariateValue <= 2.5, 9981, ifelse(covariateValue <= 3, 9982, 9983)), covariateValue = 1)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(9981, 9982, 9983),
                             covariateName = c("UDI diameter small (<=2.50)", "UDI diameter medium (>2.50 to <=3.00)", "UDI diameter large (>3.00)"),
                             analysisId = 261110,
                             conceptId = 0)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 261110,
                            analysisName = "UDI Diameter",
                            domainId = "UDI",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}


createUDILCovariateSettings <- function(useUDILengths = TRUE) {
  covariateSettings <- list(useUDILengths = useUDILengths)
  attr(covariateSettings, "fun") <- "DESUDIv3::getDbUDILCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


getDbUDILCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cohortTable = "#cohort_person",
                                   cohortId = -1,
                                   cdmVersion = "5",
                                   rowIdField = "subject_id",
                                   covariateSettings,
                                   aggregated = FALSE) {
  writeLines("Constructing UDI length covariates")
  if (covariateSettings$useUDILengths == FALSE) {
    return(NULL)
  }
  if (aggregated)
    stop("Aggregation not supported")
  # Some SQL to construct the covariate:
  source_udi <- read.csv(system.file("csv", "DesProductMapping.csv", package = "DESUDIv3"))
  source_udi <- source_udi %>% mutate(
    diameter = as.numeric(sapply(strsplit(spec, "\\*"), `[`, 1)),
    length = as.numeric(sapply(strsplit(spec, "\\*"), `[`, 2)),
    uniqueDeviceId = as.character(udi_di))
  
  sql <- paste("SELECT @row_id_field AS row_id, 999 AS covariate_id,",
               "DEVICE_SOURCE_VALUE, UNIQUE_DEVICE_ID",
               "FROM @cohort_table c",
               "INNER JOIN @cdm_database_schema.DEVICE_EXPOSURE de",
               "ON de.person_id = c.subject_id",
               "WHERE cohort_start_date = DEVICE_EXPOSURE_START_DATE",
               "AND (de.DEVICE_SOURCE_VALUE like '%J5083094%' or de.DEVICE_SOURCE_VALUE like '%J8083033%' or de.DEVICE_SOURCE_VALUE like '%J8083006%' or de.device_source_value like '%J8083129%')",
               "{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}")
  sql <- SqlRender::render(sql,
                           cohort_table = cohortTable,
                           cohort_id = cohortId,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
  # Retrieve the covariate:
  covariates_tmp <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  covariates_tmp <- covariates_tmp %>% mutate(uniqueDeviceId = as.character(as.numeric(uniqueDeviceId))) %>% left_join(source_udi, by = "uniqueDeviceId") %>% select(rowId, covariateId, length)
  covariates <- covariates_tmp %>% filter(!is.na(length)) %>% group_by(rowId, covariateId) %>% slice_max(length) %>% unique()
  # covariates <- covariates %>% filter(!is.na(length)) %>% select("rowId", "covariateId") %>% mutate(covariateValue = 1)
  colnames(covariates) <- c("rowId", "covariateId", "covariateValue")
  covariates <- covariates %>% ungroup() %>% mutate(covariateId = ifelse(covariateValue <= 20, 9991, ifelse(covariateValue < 30, 9992, 9993)), covariateValue = 1)
  # Construct covariate reference:
  covariateRef <- data.frame(covariateId = c(9991, 9992, 9993),
                             covariateName = c("UDI length short (<=20)", "UDI length medium (>20 to <30)", "UDI length long (>=30)"),
                             analysisId = 261111,
                             conceptId = 0)
  # Construct analysis reference:
  analysisRef <- data.frame(analysisId = 261111,
                            analysisName = "UDI Lengths",
                            domainId = "UDI",
                            startDay = 0,
                            endDay = 0,
                            isBinary = "Y",
                            missingMeansZero = "Y")
  # Construct analysis reference:
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  return(result)
}

