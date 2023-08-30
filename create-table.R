# 1. Load required packages -----------------------------------------------

list.of.packages <- c("reactable", "data.table", "here", "htmltools", "htmlwidgets", "webshot", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 2. Retrieve data --------------------------------------------------------

data <- data.table::fread(here::here("comparison-dbms.csv"),quote="", header=T)

column_transformer <- function(value) {
  string <- value
  if (stringr::str_detect(value, ":n:")) {
    string <- stringr::str_replace(value, ":n:", "<span><img alt='' src='./assets/icons/icon.png' style='width: 16.38px; height: 12.8px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>")
  } else if (str_detect(value, ":y:")) {
    string <- stringr::str_replace(value, ":y:", "<span><img alt='' src='./assets/icons/62fc3b327bf0d9337241e112_check.png' style='width: 16.38px; height: 12.8px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>")
  } else if (str_detect(value, ":u:")) {
    string <- stringr::str_replace(value, ":u:", "<span><img alt='' src='./assets/icons/62fc3b3276df8460a3e8d91b_output-onlinepngtools.png' style='width: 20.46px; height: 16.00px; margin-left: 0.00px; margin-top: 0.00px; transform: rotate(0.00rad) translateZ(0px); -webkit-transform: rotate(0.00rad) translateZ(0px);' title=''></i></span>")
  # } else if (!is.na(as.numeric(value))) {
  #   span(class = "number", value)
  }
  markdown::mark(string)
}

# data <- class(apply(data, c(1:2), column_transformer))
# data.frame(lapply(data,column_transformer))
# map_df(.x, .f) returns a data frame

tbl <- reactable(data, 
                 pagination = FALSE,
                 borderless=T,
                 columnGroups = list(
                   colGroup(name = "OLTP", columns = c("Oracle", "MySQL", "PostgreSQL", "CockroachDB","Yugabyte"), html = T),
                   colGroup(name = "OLAP", columns = c("Clickhouse", "Doris", "Databricks", "Snowflake","DuckDB"), html = T),
                   colGroup(name = "<a href='https://dl.acm.org/doi/pdf/10.1145/3514221.3522565'>HTAP</a>", columns = c("TiDB", "SingleStore"), html = T)
                 ),
                 defaultColDef = colDef(
                   html = TRUE,
                   vAlign = "center",
                   headerVAlign = "bottom",
                   minWidth = 90,
                   headerClass = "header",
                   align = "center",
                   class = "border-left"
                 ),
          columns = list(
            item = colDef(
              name = "",
              align = "left",
              #resizable = TRUE,
              cell = function(value, index) {
                # this is ugly but will fix later
                title <- column_transformer(paste0(value, column_transformer(data[index, "description"][[1]])))
                title <- as.character(div(class = "item-content-left", HTML(title)))
                if (data[index, "is_advanced"][[1]]) {
                  advanced_tag <- div(class = "item-content-right", span(class = "tag", "Advanced"))
                  title <- paste0(title, tagList(advanced_tag))
                }
                title <- paste0("<div class = 'item-wrapper'>", title, "</div>")
                title
              },
              minWidth = 280,
              class = "item"
            ),
            `Oracle` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Oracle</div>"
            ),
            `MySQL` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>MySQL</div>"
            ),
            `PostgreSQL` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>PostgreSQL</div>"
            ),
            `Snowflake` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Snowflake</div>"
            ),
            `Databricks` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Databricks</div>"
            ),
            `Clickhouse` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Clickhouse</div>"
            ),
            `Doris` = colDef(
              align = "center",
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Doris</div>"
            ),            
            `CockroachDB` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>CockroachDB</div>"
            ),            
            `Yugabyte` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>Yugabyte</div>"
            ),        
            `TiDB` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>TiDB</div>"
            ),
            `SingleStore` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>SingleStore</div>"
            ),            
            `DuckDB` = colDef(
              cell = function(value) column_transformer(value),
              name = "<div class='header-wrapper'>DuckDB</div>"
            ),
            description = colDef(show = FALSE),
            is_advanced = colDef(show = FALSE)
          ),
          highlight = F,
          theme = reactableTheme(
            #highlightColor = "#f3fafb",
            headerStyle = list(borderColor = "hsl(0, 0%, 90%)"),
            # Vertically center cells
            cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center")
          ),
          defaultExpanded = T,
          rowStyle = function(index) {
            if (index %in% c(2:11, 19:23, 31:38, 42:46, 51:65)) list(backgroundColor = "rgb(247, 246, 235)")
            else if (index %in% c(13:17, 25:29, 40, 48:49)) list(backgroundColor = "rgb(237, 241, 246)")
          },
          sortable = F,
          class = "comparison-tbl")

#Make sure the locale is set to English
Sys.setlocale("LC_TIME", "C")

tbl <- div(class = "comparison",
    div(class = "comparison-header", id = 'header01',  tags$br(),
        h2(class = "comparison-title", "Feature comparison disk-oriented Database Management Systems (DBMS) - WIP"),
        paste0("Date of comparison: ", format(Sys.Date(), "%B")," 2023")
    ),
    tbl
)

tbl <- htmltools::browsable(
  tagList(list(
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
      tags$link(rel = "stylesheet", type = "text/css", href = "./assets/styles/styles.css")
    ),
    tbl
  ))
)

# Saving ------------------------------------------------------------------
html_file <- "www/table.html"
htmltools::save_html(tbl, file = html_file, libdir = "lib")

Sys.setenv(OPENSSL_CONF="/dev/null")
webshot::webshot(url = html_file, file = "www/assets/images/img.png", delay = 0.1, vwidth = 2000)
