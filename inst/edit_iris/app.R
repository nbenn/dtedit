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
  dat <- cbind(row_id = seq_len(nrow(dat)), dat)

  sl <- unique(dat$Sepal.Length)
  sw <- unique(dat$Sepal.Width)

  sw <- data.frame(
    major = as.character(floor(sw)),
    minor = as.character(sw)
  )

  args <- list(
    Sepal.Length = list(
      choices = split(as.character(sl), floor(sl)),
      selected = sl[1L]
    ),
    Sepal.Width = list(
      choices = shinytreeview::make_tree(sw, colnames(sw)),
      selected = sw$minor[1L]
    )
  )

  typs <- c(Sepal.Width = "shinytreeview::treecheckInput")

  dat$Sepal.Length <- as.factor(dat$Sepal.Length)
  dat$Sepal.Width <- as.factor(dat$Sepal.Width)

  rownames(dat) <- NULL

  input_col <- "Species"
  edit_cols <- setdiff(colnames(dat), c(input_col, "row_id"))

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
    cols = setdiff(colnames(dat), "row_id"),
    fields = build_modal_fields(dat, edit_cols, types = typs, args = args),
    values = stats::setNames("species", input_col),
    insert = function(new, dat) {
      old <- read_data()
      new <- cbind(row_id = max(old$row_id) + 1L, new)
      write_data(rbind(old, new))
      rbind(dat, new)
    },
    delete = function(row, dat) {
      old <- read_data()
      hit <- which(old$row_id == dat$row_id[row])
      stopifnot(length(hit) == 1L)
      write_data(old[-hit, ])
      dat[-row, ]
    },
    update = function(row, new, dat) {
      old <- read_data()
      hit <- which(old$row_id == dat$row_id[row])
      stopifnot(length(hit) == 1L)
      new <- cbind(row_id = old$row_id[hit], new)
      old[hit, ] <- new
      write_data(old)
      dat[row, ] <- new
      dat
    }
  )

  exportTestValues(
    sub_iris = sub_iris(),
    read_data = read_data
  )
}

ui <- fluidPage(
  h3("DTedit"),
  selectInput("species", "Species", c("", levels(iris$Species))),
  uiOutput("sub_iris")
)

shinyApp(ui = ui, server = server)
