library(shiny)
library(shinyjs)
if (FALSE)
  require("V8")
library(ggplot2)
library(DT)

traverse_light <- function(row, tree){
  if(!is.null(tree[["parameters"]])){
    return(tree[["parameters"]])
  } else {
    traverse_light(row = row,
                   tree = tree[c("left_child", "right_child")][[(do.call(tree$rule$relation, list(row[tree$rule$name], tree$rule$value))+1)]])
  }
}
predict_traj <- function(forest, r, parameters){
  pars <- sapply(forest, function(t){
    traverse_light(r, t)
  })
  times = matrix(c(rep(1, 5), 0:4, (0:4)^2), nrow = 5)
  preds <- apply(pars, 2, function(p){
    rowSums(matrix(p, nrow(times), ncol(times), byrow = TRUE) * times)
  })
  apply(preds, 1, quantile, probs = c(.025, .5, .975))
}
desc_cat <- read.csv("desc_cat.csv", stringsAsFactors = FALSE)
desc_num <- read.csv("desc_num.csv", stringsAsFactors = FALSE)
renm <- read.csv("scale_rename.csv", stringsAsFactors = FALSE, header = FALSE)
num_mat <- read.csv("shiny_num_mat.csv", stringsAsFactors = FALSE)
rownames(num_mat) <- num_mat$name
num_mat <- as.matrix(num_mat[,-1])
p <- readRDS("classplot_color.RData")
forest <- readRDS("forest_light.RData")

# plot_biv <- readRDS("shiny_plot.RData")
# mixmod <- readRDS("shiny_mixmod.RData")
# newdat <- structure(list(belasting = c(3.2, 3, 3.5, 4.5),
#                          vrijheid = c(2, 1.5, 1.3, 3),
#                          gedachten = c(2.5, 2.3, 2.5, 4.2),
#                          eenzaamheid = c(2.7, 3.2, 2.8, 3)),
#                     row.names = 1:4, class = "data.frame")
#mixmod <- mxModel(mixmod, mxData(newdat, type = "raw"))

shinyServer(function(input, output, session) {
  reactives <- reactiveValues()
  reactives[["df"]] <- "temp"
  
  # Dynamically create sliders
  output$sliders <- renderUI({
    # https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
    slidelist <- lapply(desc_num$name, function(i) {
      sliderInput(inputId = paste0("slide_", renm$V1[renm$V2 == i]),
                  label = i,
                  min = -2, max = 2, value = 0, step = 1)
    })
    radiolist <- lapply(unique(desc_cat$name), function(n){
      radioButtons(inputId = paste0("cat_", renm$V1[renm$V2 == n]),
                   label = n,
                   choices = desc_cat$Category[desc_cat$name == n][order(desc_cat$X.[desc_cat$name == n], decreasing = TRUE)])
    })
    # Layout in columns
    fluidRow(
      column(width = 6,
             slidelist
      ),
      column(width = 6,
             radiolist
      )
    )
  })
  
  output$file_input = renderUI({
    fileInput(
      "file",
      NULL,
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )#,
      #width = "300px"
    )
  })
  
  output$mplus_file_input = renderUI({
    fileInput(
      "mplus_file",
      NULL,
      accept = c(
        'text/plain')#,
      #width = "300px"
    )
  })
  observeEvent(input$submit, {
    survdat <- getSurveyData()
    survdat[[1]] <- NULL
    survdat$response <- c("Helemaal niet" = 1,
                          "Niet" = 2,
                          "Gemiddeld" = 3,
                          "Wel" = 4,
                          "Heel erg" = 5)[survdat$response]
    reactives[["response_data"]] <- survdat
  })
  observeEvent(input$update, {
    newdata <- c(
      lapply(renm$V1[renm$V2 %in% desc_num$name], function(i){
        colm <- input[[paste0("slide_", i)]]
        num_mat[i, (colm+3)]
      }),
      lapply(renm$V1[renm$V2 %in% desc_cat$name], function(i){
        input[[paste0("cat_", i)]]
      }))
    names(newdata) <- c(renm$V1[renm$V2 %in% desc_num$name],
                        renm$V1[renm$V2 %in% desc_cat$name]
                        )
    reactives[["df"]] <- newdata
  })
  observeEvent(input$load, {
    if (is.data.frame(input$file)) {
      data <- read.csv(input$file$datapath)
      reactives[["response_data"]] <- data
    }
  })
  
  
  # output$welzijnsmeter.csv <- downloadHandler(
  #   filename = "welzijnsmeter.csv",
  #   content = function(file) {
  #     write.csv(structure(list(Label = structure(c(1L, 3L, 2L, 4L, 1L, 3L, 2L, 
  #                                                  4L, 1L, 3L, 2L, 4L), .Label = c("a2.ON.a1", "a2.ON.b1", "b2.ON.a1", 
  #                                                                                  "b2.ON.b1"), class = "factor"), Estimate = c(0.3, 0.2, 0.4, 0.642, 
  #                                                                                                                               0.5, 0.3, 0.3, 0.6, 0.67, 0.43, 0.32, 0.53), SE = c(0.004, 0.003, 
  #                                                                                                                                                                                   0.01, 0.04, 0.042, 0.04115, 0.074, 0.003, 0.012, 0.03, 0.002, 
  #                                                                                                                                                                                   0.005), Sample = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
  #                                                                                                                                                                                                      3L)), class = "data.frame", row.names = c(NA, -12L)), 
  #               file, row.names = FALSE)
  #   },
  #   contentType = "text/csv"
  # )
  # 
  
  output$report <- renderUI({
    
    
    #cp <- class_prob(mixmod, type = "individual")$individual
    #indiv_classprob <- cp[5, , drop = TRUE]
    #reactives <- list(indiv_classprob = indiv_classprob)
    
    # classtext <- paste0("<ul", 
    #                     paste0("<li>", names(cpind), ": ", round(100*cpind), "%</li>"),
    #                     "</ul>", collapse = "")
    # HTML(paste0(
    #   "Volgens uw antwoorden op de Welzijnsmeter past u het beste bij de volgende typen naasten:",
    #   classtext,
    #   collapse = ""
    # ))
  })
  
  output$classplot <- renderImage({
    outfile <- tempfile(fileext = '.png')
    if(isFALSE(reactives[["df"]][1] == "temp")){
      preds <- predict_traj(forest, reactives$df)
      df_indiv <- data.frame(Time = 0:4,
                             Value = preds[2,],
                             Class = "ind"
      )
      df_rib <- data.frame(Time = 0:4,
                           y1 = preds[1,],
                           y2 = preds[3,])
      p <- p +
        geom_ribbon(data=df_rib, aes(x = Time, ymin=y1,ymax=y2), fill="blue", alpha=0.2) +
        geom_line(data = df_indiv, aes(x = Time, y = Value), colour = "blue", size = 2)
    }
    ggsave(outfile, p, units = "px", width = 1000, height = 800, dpi = 200)
    p
    list(
      src = outfile,
      width = 600,
      height = 450
    )
  }, deleteFile = TRUE)
  
  
})
