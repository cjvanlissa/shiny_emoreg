library(shiny)
library(shinyjs)
if (FALSE)
  require("V8")
library(ggplot2)
library(DT)
df2 <- eval(parse(text = 'structure(list(de2 = -0.190141930194051, de3 = -0.0245242835106653, 
    de4 = -0.155272703723064, de5 = -0.197226525084319, de6 = -0.0749305742875438, 
    geslacht = 2L, leeftijd_target_11 = 13.2457221081451, leeftijd_moeder_11 = 41.4948665297741, 
    leeftijd_sibling_11 = 11.4140999315537, leeftijd_vader_11 = 42.0780287474333, 
    d_sib_age = 1.8316221765914, d_par_age = 0.583162217659201, 
    brpmoe_lmh = 2L, brpvad_lmh = 3L, sesgez_low = 0L, bg11aa04 = 2L, 
    reli = 1L, awareness = -0.947417933017618, clarity = -1.30088021357663, 
    impulsivity = 1.90518170511257, goals = -0.388862177622228, 
    accept = -0.17363270618939, strategies = -2.1628178848281, 
    balancedrelated = -0.939579480238324, extraversion = -1.32912805199444, 
    agreeableness = -0.0289783684448405, conscientious = -3.32325780135187, 
    neuroticism = -2.32960596523136, openness = -0.701872931710095, 
    BIS = 0.320359116916617, BAS = 0.186433303000392, externalizing = -1.74267171901295, 
    conflict_parents = 0.847624487745558, CRSI_en_ab = 0.257173077379458, 
    CRSI_ps_ab = -2.31397596385848, CRSI_wi_ab = -1.04501474370614, 
    CRSI_co_ab = -0.615224645444001, CRSI_en_av = -0.119229567003732, 
    CRSI_ps_av = 1.68770086823993, CRSI_wi_av = -0.21211869598088, 
    CRSI_co_av = 0.465508237582381, CRSI_en_am = -0.320318055330195, 
    CRSI_ps_am = -0.521682335912593, CRSI_wi_am = -0.326445255945067, 
    CRSI_co_am = -1.13904996034278, dailyhassles = 0.266766265307863, 
    DMD_happy = -1.31836172129696, DMD_angry = -1.63563374071052, 
    DMD_anxious = -1.48722518678485, DMD_sad = -0.558452101017284, 
    DMD_guilt = -1.02960550471628, DMD_tired = -1.94692750746265, 
    DMD_tense = -2.27567595783264, delinquency = -0.537057631784433, 
    peer_delinq = -0.509634568927588, peer_contact = 0.134660280835413, 
    conflictfrequency = -2.8466769588417, conflictemo = -1.88899472717432, 
    IRI_emp_aa = -0.710142666222056, IRI_pet_aa = 1.64042088168142, 
    IRI_fan_aa = -1.3671239816833, IRI_ped_aa = -0.791745844592677, 
    tolerance = -1.16302254756569, intrusiveness = -0.294500841375879, 
    emotionalresponse = -2.22778566006627, annoyed = -0.979878606370968, 
    supportivecriticism = -2.15019303948588, support_friend = -1.20652489790452, 
    negativeaffect_friend = -0.875371477787925, power_friend = -0.0997928026038957, 
    support_mother = -1.05669890756434, negativeaffect_mother = -0.82029116966581, 
    power_mother = 0.772315243448044, support_father = 0.569095005087547, 
    negativeaffect_father = -0.810793424659891, power_father = 0.562540358175786, 
    parknow = 1.18023168453312, disclosure = -2.59565876872372, 
    sollicitation = -1.99415499668797, conbehpar = 2.10089693103168, 
    conpsy = 1.28274435445012, par_consist = -0.252840691530262, 
    peermanag = -0.371671179574697, perpphys = -1.46299703956462, 
    perprelat = -1.88903522237978, vicphys = -1.17422115085023, 
    vicrelat = -1.8731563753519, prosoc = 1.83890692031801, pubert = -3.84121094224969, 
    depression = 0.345564755958946, anxiety = 1.79162868976385, 
    selfconc = 0.352510114073287, school = 0.62237465566387, 
    victim = -0.30461436370204, alcohol = -0.568896437925413, 
    cigarettes = -0.511111111111111, drugs = -0.102922755969181, 
    externalizing_psych = -0.728105245115356), type = "original", row.names = 1L, class = c("worcs_data", 
"data.frame"))
'))
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
p <- readRDS("classplot_color.RData")
forest <- readRDS("forest_light.RData")

vars_num <- desc_num$name
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
    lapply(seq(vars_num), function(i) {
      sliderInput(inputId = paste0("range", vars_num[i]),
                  label = vars_num[i],
                  min = -3, max = 3, value = 0)
    })
    
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
    reactives[["df"]] <- df2
  })
  observeEvent(input$load, {
    if (is.data.frame(input$file)) {
      data <- read.csv(input$file$datapath)
      reactives[["response_data"]] <- data
    }
  })
  
  
  output$welzijnsmeter.csv <- downloadHandler(
    filename = "welzijnsmeter.csv",
    content = function(file) {
      write.csv(structure(list(Label = structure(c(1L, 3L, 2L, 4L, 1L, 3L, 2L, 
                                                   4L, 1L, 3L, 2L, 4L), .Label = c("a2.ON.a1", "a2.ON.b1", "b2.ON.a1", 
                                                                                   "b2.ON.b1"), class = "factor"), Estimate = c(0.3, 0.2, 0.4, 0.642, 
                                                                                                                                0.5, 0.3, 0.3, 0.6, 0.67, 0.43, 0.32, 0.53), SE = c(0.004, 0.003, 
                                                                                                                                                                                    0.01, 0.04, 0.042, 0.04115, 0.074, 0.003, 0.012, 0.03, 0.002, 
                                                                                                                                                                                    0.005), Sample = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
                                                                                                                                                                                                       3L)), class = "data.frame", row.names = c(NA, -12L)), 
                file, row.names = FALSE)
    },
    contentType = "text/csv"
  )


  output$diagram_header <- renderUI({
    #h5(strong(ifelse(is.null(reactives[["table_estimates"]]), "", "Table of meta-analytic estimates: ")))
    h5(strong("Class plot"))
  })
  output$report <- renderUI({
    
    
    #cp <- class_prob(mixmod, type = "individual")$individual
    #indiv_classprob <- cp[5, , drop = TRUE]
    #reactives <- list(indiv_classprob = indiv_classprob)
    cpind <- reactives$indiv_classprob
    cpind <- sort(cpind, decreasing = TRUE)
    classtext <- paste0("<ul", 
                        paste0("<li>", names(cpind), ": ", round(100*cpind), "%</li>"),
                        "</ul>", collapse = "")
    HTML(paste0(
      "Volgens uw antwoorden op de Welzijnsmeter past u het beste bij de volgende typen naasten:",
      classtext,
      collapse = ""
      ))
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
