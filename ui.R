library(shiny)
library(DT)
library(grinn)

shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", type="image/x-icon", href="favicon.ico")
  ),
  titlePanel(title=img(src="logo.png", width = 250), windowTitle="grinn"),
  fluidRow(
    column(3, wellPanel(
      selectInput("fnCall", "Select function",
        c("fetchGrinnNetwork", "fetchCorrNetwork", "fetchDiffCorrNetwork",
          "fetchCorrGrinnNetwork", "fetchDiffCorrGrinnNetwork", 
          "fetchGrinnCorrNetwork", "fetchGrinnDiffCorrNetwork",
          "convertToGrinnID")
      ),
      p('Click to visit',a(href='http://kwanjeeraw.github.io/grinn/',target='_blank','homepage')),
      br()
    )),
    column(9,
      # This outputs the dynamic UI component
      uiOutput("ui")
    )
  )#end fluidRow
))#end shinyUI, fluidPage