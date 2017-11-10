library(tidyverse)
library(tidyxl)
library(unpivotr)
library(naturalsort)
library(here)

summary_path <- here("inst", "extdata", "int-mig-2013-summary-tables.xlsx")

import_sheet <- function(cells) {
  anchor_cells <- # The top-left cell of each group of column headers
    cells %>%
    filter(col == 1, row >= 6,
           !is.na(formats$local$border$top$style[local_format_id])) %>%
    pull(address)
  cells %>%       # In chunks, below each set of column headers
    filter(row >= 6) %>%
    arrange(row, col) %>%
    mutate(header_group = cumsum(address %in% anchor_cells)) %>%
    nest(-header_group) %>%
    mutate(data = map(data, import_header_group)) %>%
    unnest() %>%
    select(-header_group) %>%
    mutate(title = filter(cells, address == "A3")$character,
           subtitle = filter(cells, address == "A4")$character) %>%
    select(title, subtitle, starts_with("var"), count, flag)
}

import_header_group <- function(cells) {
  col_headers <-
    cells %>%
    filter(col >= 2, row <= min(row) + 2, !is.na(character)) %>%
    select(row, col, character) %>%
    split(.$row) %>%
    map2(c("var2", "var3", "var4"), ~ rename(.x, UQ(.y) := character))
  table_header <- filter(cells, row == min(row), col == 1)$character
  row_headers <- # top level are bold, next level are not bold
    cells %>%
    filter(col == 1, row >= 9, !is.na(character),
           is.na(formats$local$border$top$style[local_format_id])) %>%
    select(row, col, character, local_format_id) %>%
    split(formats$local$font$bold[.$local_format_id]) %>%
    map(~ select(.x, - local_format_id)) %>%
    map2(c("var6", "var5"), ~ rename(.x, UQ(.y) := character))
  data_cells <- # at the intersection of the row and column headers
    cells %>%
    semi_join(col_headers[[3]], by = "col") %>%
    semi_join(row_headers[[1]], by = "row") %>%
    select(row, col, count = numeric, flag = character) # flag = e.g. "..C" for 'Confidential'
  data_cells %>%
    mutate(var1 = table_header) %>%
    NNW(col_headers[[1]]) %>%
    NNW(col_headers[[2]]) %>%
    NNW(col_headers[[3]]) %>%
    WNW(row_headers[[2]]) %>%
    WNW(row_headers[[1]]) %>%
    select(-row, -col)
}

# Load the data
allcells <- xlsx_cells(summary_path)
formats <- xlsx_formats(summary_path)

# Check the number formatting.  It's fine, nothing suspicious.
alltables <- filter(allcells, sheet != "Contents")
alltables$numFmt <- formats$local$numFmt[alltables$local_format_id]
sort(table(alltables$numFmt))
filter(alltables, numFmt == "#;##0") %>%
  select(address, numeric, character) %>%
  print(n = Inf)

# Filter alldata for cells above the "Confidentiality" statement (if present)
alldata <-
  alltables %>%
  select(sheet, address, row, col, character, numeric, local_format_id) %>%
  nest(-sheet) %>%
  mutate(confidentiality_row =
           map_dbl(data,
                   ~ min(Inf,
                         filter(.x,
                                str_detect(character,
                                           "^Confidentiality"))$row))) %>%
  unnest() %>%
  group_by(sheet) %>%
  filter(row < confidentiality_row) %>%
  ungroup()

# Import each data sheet
out <-
  alldata %>%
  rename(table = sheet) %>%
  nest(-table) %>%
  arrange(naturalsort(table)) %>%
  mutate(data = map(data, import_sheet)) %>%
  unnest()

nz_internal_migration_summary <- out

write.table(nz_internal_migration_summary,
            row.names = FALSE,
            quote = FALSE,
            sep = "\t",
            file=gzfile(here("inst",
                             "extdata",
                             "nz-internal-migration-summary.tsv.gz")))

use_data(nz_internal_migration_summary, overwrite = TRUE)
