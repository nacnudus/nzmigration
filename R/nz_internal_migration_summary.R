#' New Zealand Internal Migration Summary Statistics 2013
#'
#' A dataset containing tables of summary statistics describing internal
#' migration in New Zealand, from the 2013 census.
#'
#' @format A data frame with 351972 rows and 11 variables.  This single table
#' includes data from all the separate tables in the original spreadsheet, so
#' some of the column names are generic (`var1` ... `var6`).
#'
#' * `table` Name of the table from the original spreadsheet
#' * `title` Title of the table from the original spreadsheet
#' * `subtitle` Subtitle of the table from the original spreadsheet
#' * `var1` First variable of the table from the original spreadsheet
#' * `...`
#' * `var6` Sixth variable of the table from the original spreadsheet
#' * `count` Number of people
#' * `flag` Only one flag is used, `..C`, to mean 'confidential'.
#'
#' @source
#' \url{http://m.stats.govt.nz/~/media/Statistics/browse-categories/population/migration/internal-migration-tables/int-mig-2013-summary-tables.xlsx}
#' \url{http://m.stats.govt.nz/browse_for_stats/population/Migration/internal-migration/tables.aspx}
#'
"nz_internal_migration_summary"

