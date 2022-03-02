#' Run the shinyshval application
#' @export
run_app <- function() { # nocov start
  shiny::runApp(system.file("app", package = "shinyshval"))
}

#' UI function for shinyshval
#' @description Top-level UI function
#' @param request request to app (e..g for bookmarking)
#' @import shinydashboard
#' @import shiny
#' @export
app_ui <- function(request) {
  header <- dashboardHeader(title = "shinyshval")
  sidebar <- dashboardSidebar(disable = TRUE, collapsed = TRUE)
  body <- dashboardBody(
    # less space between boxes
    tags$head(
      tags$style(
        HTML(
          "[class*='col-lg-'],[class*='col-md-'],[class*='col-sm-'],[class*='col-xs-']{
              padding-right:5px !important;
              padding-left:5px !important;}"
        )
      )
    ),
    fluidRow(
      column(6,
        tabBox(
          title = "Inputs",
          width = 12,
          side = "left",
          id = "tabset_inputs",
          tabPanel("Estimated",
            rhandsontable::rHandsontableOutput("hot_est"),
            "Estimated values will be overwritten by measured values ('Measured' tab)."
          ),
          tabPanel("Measured",
            rhandsontable::rHandsontableOutput("hot_raw"),
            "You can also copy and paste here."
          )
        )
      ),
      column(3,
        box(title = "Settings",
          width = 12,
          status = "primary",
          shinyWidgets::radioGroupButtons(
            inputId = "sel_application",
            label = "Default scenario",
            choices = c("downwards", "up/sidewards"),
            justified = TRUE,
            selected = "downwards"),
          hr(),
          shinyWidgets::radioGroupButtons(
            inputId = "sel_species",
            label = "Species",
            choices = c("Honey Bee", "Bumble Bee", "Solitary Bee"),
            justified = TRUE, selected = "Honey Bee"),
          shinyWidgets::radioGroupButtons(
            inputId = "sel_stadium",
            label = "Stadium",
            choices = c("adult", "larvae"),
            justified = TRUE,
            selected = "adult"),
          conditionalPanel(
           condition = "input.sel_species == 'Honey Bee' && input.sel_stadium == 'adult'",
            shinyWidgets::radioGroupButtons(inputId = "sel_categ",
              label = "Category",
              choices = c("forager", "nurse"),
              justified = TRUE,
              selected = "forager")
          ),
          conditionalPanel(
           condition = "(input.sel_species == 'Honey Bee' && input.sel_stadium == 'adult' && input.sel_categ == 'forager') | (input.sel_species == 'Bumble Bee' && input.sel_stadium == 'adult')",
           shinyWidgets::radioGroupButtons(inputId = "sel_type",
            label = "Type",
            choices = c("acute", "chronic"),
            justified = TRUE,
            selected = "chronic")
          ),
          hr(),
          shinyWidgets::materialSwitch(inputId = "twa",
            label = "Consider decay (TWA)?",
            value = FALSE,
            status = "success"),
          conditionalPanel(
            condition = "input.twa == true",
            numericInput("dt50_p", "DT50 in pollen [days]", value = 2, min = 0),
            numericInput("dt50_n", "DT50 in nectar [days]", value = 2, min = 0),
            numericInput("t", "Time windows [days]", value = 10)
          )
        )
      ),
      column(3,
        box(title = "90th percentile shortcut value (SHVAL)",
          width = 12,
          status = "primary",
          shinyWidgets::progressBar(id = "pb_input",
                     title = "No. correct inputs",
                     value = 0,
                     status = "warning"),
          valueBoxOutput("shval", width = 12)
        )
      )
    ),
    fluidRow(
      column(6, actionButton("info", label = "Help", icon = icon("info")))
    )
  )

  dashboardPage(
    skin = "blue",
    header,
    sidebar,
    body
  )
}


#' Server function for shinyshval
#' @description Contains all server logic
#' @param input internal
#' @param output internal
#' @param session internal
#' @import rhandsontable
#' @export
app_server <- function(input, output, session) {
  session$allowReconnect(TRUE)

  # reactive values ---------------------------------------------------------
  ncol_raw <- 8

  values <- reactiveValues(
    hot_raw = data.frame(
      `Residues in pollen [ln RUD mg/kg]` = rep(NA_real_, ncol_raw),
      `Residues in nectar [ln RUD mg/kg]` = rep(NA_real_, ncol_raw),
      `Pollen consumption [mg]` = rep(NA_real_, ncol_raw),
      `Nectar consumption [mg]` = rep(NA_real_, ncol_raw),
      `Nectar sugar concentration in % [0 - 1]` = rep(NA_real_, ncol_raw)
    ),
    hot_est = data.frame(
      param = c("Residues in pollen [ln RUD mg/kg]",
                "Residues in nectar [ln RUD mg/kg]",
                "Pollen consumption [mg]",
                "Nectar consumption [mg]",
                "Nectar sugar concentration in % [0 - 1]"),
      mean = rep(NA_real_, 5),
      sd = rep(NA_real_, 5),
      min = rep(NA_real_, 5),
      max = rep(NA_real_, 5),
      bg = rep(NA_real_, 5),
      stringsAsFactors = FALSE
    )
  )


  # Reactives ---------------------------------------------------------------
  param <- reactive({
    r <- values[["hot_raw"]]
    e <- values[["hot_est"]]

    # map names for backend
    names(r) <- c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s")
    e$param <- c("RUD_p", "RUD_n", "CONS_p", "CONS_s", "CNT_s")

    p <- try(shinyshval::get_param(r, e))
    if (inherits(p, "try-error"))
      return(NULL)
    return(p)
  }) %>% debounce(333)

  # compute shval value
  shvals <- reactive({
    shiny::validate(
      shiny::need(nrow(param()) == 5, "Error")
    )
    tryCatch(
      shinyshval::sim(param(),
        twa = input$twa,
        dt50_p = input$dt50_p,
        dt50_n = input$dt50_n,
        t = input$t),
      error = function(err) NULL)
  })

  # default parameters
  defaults <- reactive({
    shinyshval::default_estimates(species = input$sel_species,
      stadium = input$sel_stadium,
      category = input$sel_categ,
      type = input$sel_type,
      application = input$sel_application)
  }) %>% debounce(250)


  # Observers ---------------------------------------------------------------

  # update progressbar
  observe({
    req(param())
    if (nrow(param()) < 5) {
      status <- "danger"
    } else {
      status <- "success"
    }
    shinyWidgets::updateProgressBar(session = session, id = "pb_input",
      value = nrow(param()) / 5 * 100,
      status = status)
  })

  # save changes to table from user input
  observe({
    if (!is.null(input$hot_raw)) {
      r <- rhandsontable::hot_to_r(input$hot_raw)
      names(r) <- c("Residues in pollen [ln RUD mg/kg]",
                    "Residues in nectar [ln RUD mg/kg]",
                    "Pollen consumption [mg]",
                    "Nectar consumption [mg]",
                    "Nectar sugar concentration in % [0 - 1]")
      values[["hot_raw"]] <- r
    }

    if (!is.null(input$hot_est)) {
      values[["hot_est"]]  <- rhandsontable::hot_to_r(input$hot_est)
    }
  })

  # Change estimates table according to defaults
  observeEvent(defaults(), {
    req(defaults())
    req(values[["hot_est"]])
    def <- defaults()
    # transform names for frontend
    def$param <- c("Residues in pollen [ln RUD mg/kg]",
                   "Residues in nectar [ln RUD mg/kg]",
                   "Pollen consumption [mg]",
                   "Nectar consumption [mg]",
                   "Nectar sugar concentration in % [0 - 1]")

    values[["hot_est"]] <- def
  })

  # Show help
  observeEvent(input$info, {
    showModal(
      modalDialog(
        title = NULL,
        includeHTML("info.html"),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
  })

  # outputs -----------------------------------------------------------------
  # Table for raw data
  output$hot_raw <- renderRHandsontable({
    x <- values[["hot_raw"]]
    rhandsontable(x, height = 260, digits = 16) %>%
      hot_cols(colWidths = 120,
               renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })

  # table for estimates
  output$hot_est <- renderRHandsontable({
    rhandsontable(values[["hot_est"]], rowHeaders = NULL,
                  height = 250, digits = 16) %>%
      hot_cols(renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
      hot_col("param", readOnly = TRUE, colWidths = 280) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  # compute shval value
  output$shval <- renderValueBox({
    req(isTruthy(shvals()))

    valueBox(
      round(mean(shvals()), 2),
      paste0("95% CI: (",
        round(stats::quantile(shvals(), 0.025), 2),
        " - ",
        round(stats::quantile(shvals(), 0.975), 2), ")"),
      icon = icon("info"),
      color = "orange"
    )
  })
} # nocov end
