#library(shinysurveys)
questions = c("Belasting", "Vrijheid", "Gedachten", "Eenzaamheid")
responses <- c("Helemaal niet", "Niet", "Gemiddeld", "Wel", "Heel erg")
df <- expand.grid(responses, questions, stringsAsFactors = FALSE)[c(2,1)]
names(df) <- c("question", "option")
df$input_type = "mc"
df$input_id = df$question
df$dependence = NA
df$dependence_value = NA
df$required = T

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

library(shiny)

shinyUI(
  fluidPage(
    #withMathJax(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
    shinyjs::inlineCSS(css),
    
    # Application title
    #titlePanel(title = "Single-paper CLPM meta-analysis"),
    
    # Sidebar with a slider input for number of bins
    mainPanel(
      
      tabsetPanel(
        id = "navbar",
        tabPanel(
          "Input",
          HTML("<h1>Instructions:</h1>Adjust the values of the predictors below to customize predictions for an individual adolescent. Predictors are ranked by importance, so adjusting the top predictors will have the most impact. All predictors are set to the median value in the validation sample, so any unchanged slider represents the most common value. Move to the 'Assessment' panel when done.<br><br><br>"),
          uiOutput("sliders")
        ),
        tabPanel(
          "Assessment",
          #uiOutput("dv_select"), # This goes hand in hand with renderUI
          
          #selectInput(
          #  inputId = "weighting",
          #  label = "Which weights to use:",
          #  choices = c("Random-effects", "Fixed-effects", "Unweighted"),
          #  selected = "Random-effects"
          #),
          HTML("<h1>Risk of emotion dysregulation</h1><br>The Figure below shows the developmental trajectories of emotion dysregulation for two groups of adolescents: those at relatively low (green) or high (red) risk for emotion dysregulation. Press 'Update plot' to plot a blue line for the assessed adolescent's predicted trajectory. The transparent blue ribbon indicates the 95% prediction interval for this adolescent. The adolescent may be at elevated risk of emotion dysregulation if the blue line approaches the red line.<br>"),
          htmlOutput("report"),
          HTML("<br>"),
          #htmlOutput("imp_header"),
          htmlOutput("plot_header"),
          actionButton("update", "Update plot", width = "150px"),
          plotOutput("classplot"),
          HTML("<br><br><br>")
          
        )#,
        # tabPanel(
        #   "Upload",
        #   h3("Choose data source"),
        #   h4("Upload data (CSV file):"),
        #   p(
        #     "Upload a .csv spreadsheet with the columns 'Label', 'Estimate', 'SE', and 'Sample'. You can export a .csv file (comma separated values) from Excel, SPSS, etc."
        #   ),
        #   uiOutput("file_input"), 
        #   actionButton("load", "Load and analyze", width = "150px"), 
        #   downloadButton("SEMSSMA_template.csv", "CSV template", width = "150px")
        #   # 
        #   # # In ui.R:
        #   # downloadLink('downloadData', 'Download')
        #   ## ---------------------------------------------
        #   
        #   
        #   
        #   
        # )
      )
    )
  )
)
