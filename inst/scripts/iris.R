library(shiny)
library(dtedit2)

data_setup <- function(dat) {
  tmp <- tempfile()
  write.csv(dat, tmp, row.names = FALSE)
  tmp
}

data_writer <- function(path) {
  force(path)
  function(data) write.csv(data, path, row.names = FALSE)
}

data_reader <- function(path) {
  force(path)
  function() read.csv(path)
}

data_cleaner <- function(path) {
  force(path)
  function() unlink(path)
}

server <- function(input, output) {

  dat <- datasets::iris

  sl <- unique(dat$Sepal.Length)

  args <- list(
    Sepal.Length = list(choices = split(as.character(sl), floor(sl)))
  )

  dat$Sepal.Length <- as.factor(dat$Sepal.Length)

  rownames(dat) <- NULL

  input_col <- "Species"
  edit_cols <- setdiff(colnames(dat), input_col)

  path <- data_setup(dat)
  onStop(data_cleaner(path))

  read_data <- data_reader(path)
  write_data <- data_writer(path)

  sub_iris <- reactiveVal(read_data()[0L, ])

  observeEvent(input$species, {
    all_dat <- read_data()
    sub_iris(all_dat[all_dat$Species == input$species, ])
  })

  dtedit(
    input, output, "sub_iris", sub_iris,
    fields = build_modal_fields(dat, edit_cols, args = args),
    values = stats::setNames("species", input_col),
    insert = function(x) {
      write_data(rbind(read_data(), x))
    }
  )
}

ui <- fluidPage(
  h3("DTedit"),
  selectInput("species", "Species", c("", levels(iris$Species))),
  uiOutput("sub_iris")
)

shinyApp(ui = ui, server = server)
