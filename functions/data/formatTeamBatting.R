formatTeamBatting <- function(year, team) {
  pg <- xml2::read_html(paste0("data/", year, "/raw/", team, "_br.html"))
  raw <- rvest::html_node(pg, '#team_batting') %>% rvest::html_table() %>% as.data.table
  raw <- raw[PA != "PA" & AB > 0 & !grepl(' Totals', Name) & !grepl('Rank in ', Name)]
  raw[, Rk := NULL]
  raw[, Name := gsub("*", "", Name, fixed=TRUE)]
  raw[, Name := gsub("#", "", Name, fixed=TRUE)]
  raw[, Name := gsub(" \\(.*\\)", "", Name)]
  raw[, Team := team]
  setcolorder(raw, c('Name', 'Pos', 'Team'))
  for (i in 4:ncol(raw)) {
    raw[, names(raw)[i] := as.numeric(raw[[i]])]
  }
  raw[, `1B` := H - `2B` - `3B` - HR]
  setnames(raw, 'BA', 'AVG')
  raw
}
