shinyServer(
    function(input, output, session) {
        shinyjs::removeCssClass(selector = "nav", class = "navbar-default")
        shinyjs::addCssClass(selector = "nav", class = "navbar-inverse")
        shinyjs::addCssClass(selector = "#phrase", class = "input-sm")
        shinyjs::addCssClass(selector = "#btnSuggestion", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btn3DPlot", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btnWordCloud", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btn_gts", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btn_gts_stem", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btn_ktz", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#btn_ktz_stem", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#process_doc", class = "btn-primary btn-small")
        shinyjs::addCssClass(selector = "#qual_high", class = "btn-danger btn-xs")
        shinyjs::addCssClass(selector = "#qual_medium", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#qual_small", class = "btn-danger btn-xs")
        shinyjs::addCssClass(selector = "#qual_high_acc", class = "btn-danger btn-xs")
        shinyjs::addCssClass(selector = "#qual_medium_acc", class = "btn-success btn-xs")
        shinyjs::addCssClass(selector = "#qual_small_acc", class = "btn-danger btn-xs")
        shinyjs::hide("showSuggestion")
        shinyjs::hide("show3DPlot")
        shinyjs::hide("showWordCloud")
        shinyjs::hide("show_gts")
        shinyjs::hide("show_gts_stem")
        shinyjs::hide("show_ktz")
        shinyjs::hide("show_ktz_stem")
        shinyjs::hide("workinprogress")
        shinyjs::hide("qual_radio")
        shinyjs::hide("qual_radio_acc")
        observeEvent(input$btnSuggestion, {
          st <- input$showSuggestion
          shiny::updateCheckboxInput(session, "showSuggestion", NULL, !st)
          shinyjs::toggleClass("btnSuggestion", "btn-danger")
        })
        observeEvent(input$btn3DPlot, {
          st <- input$show3DPlot
          shiny::updateCheckboxInput(session, "show3DPlot", NULL, !st)
          shinyjs::toggleClass("btn3DPlot", "btn-danger")
        })
        observeEvent(input$btnWordCloud, {
          st <- input$showWordCloud
          shiny::updateCheckboxInput(session, "showWordCloud", NULL, !st)
          shinyjs::toggleClass("btnWordCloud", "btn-danger")
        })
        observeEvent(input$btn_gts, {
          st <- input$show_gts
          shiny::updateCheckboxInput(session, "show_gts", NULL, !st)
          shinyjs::toggleClass("btn_gts", "btn-danger")
        })
        observeEvent(input$btn_gts_stem, {
          st <- input$show_gts_stem
          shiny::updateCheckboxInput(session, "show_gts_stem", NULL, !st)
          shinyjs::toggleClass("btn_gts_stem", "btn-danger")
        })
        observeEvent(input$btn_ktz, {
          st <- input$show_ktz
          shiny::updateCheckboxInput(session, "show_ktz", NULL, !st)
          shinyjs::toggleClass("btn_ktz", "btn-danger")
        })
        observeEvent(input$btn_ktz_stem, {
          st <- input$show_ktz_stem
          shiny::updateCheckboxInput(session, "show_ktz_stem", NULL, !st)
          shinyjs::toggleClass("btn_ktz_stem", "btn-danger")
        })
        observeEvent(input$qual_high, {
          if (input$qual_radio != "high") {
            shinyjs::removeClass("qual_high", "btn-danger")
            shinyjs::addClass("qual_high", "btn-success")
            shinyjs::removeClass("qual_medium", "btn-success")
            shinyjs::addClass("qual_medium", "btn-danger")
            shinyjs::removeClass("qual_small", "btn-success")
            shinyjs::addClass("qual_small", "btn-danger")
            shiny::updateRadioButtons(session, "qual_radio", selected = "high")
          }
        })
        observeEvent(input$qual_medium, {
          if (input$qual_radio != "medium") {
            shinyjs::removeClass("qual_high", "btn-success")
            shinyjs::addClass("qual_high", "btn-danger")
            shinyjs::removeClass("qual_medium", "btn-danger")
            shinyjs::addClass("qual_medium", "btn-success")
            shinyjs::removeClass("qual_small", "btn-success")
            shinyjs::addClass("qual_small", "btn-danger")
            shiny::updateRadioButtons(session, "qual_radio", selected = "medium")
          }
        })
        observeEvent(input$qual_small, {
          if (input$qual_radio != "small") {
            shinyjs::removeClass("qual_high", "btn-success")
            shinyjs::addClass("qual_high", "btn-danger")
            shinyjs::removeClass("qual_medium", "btn-success")
            shinyjs::addClass("qual_medium", "btn-danger")
            shinyjs::removeClass("qual_small", "btn-danger")
            shinyjs::addClass("qual_small", "btn-success")
            shiny::updateRadioButtons(session, "qual_radio", selected = "small")
          }
        })  
        observeEvent(input$qual_high_acc, {
          if (input$qual_radio_acc != "high") {
            shinyjs::removeClass("qual_high_acc", "btn-danger")
            shinyjs::addClass("qual_high_acc", "btn-success")
            shinyjs::removeClass("qual_medium_acc", "btn-success")
            shinyjs::addClass("qual_medium_acc", "btn-danger")
            shinyjs::removeClass("qual_small_acc", "btn-success")
            shinyjs::addClass("qual_small_acc", "btn-danger")
            shiny::updateRadioButtons(session, "qual_radio_acc", selected = "high")
          }
        })
        observeEvent(input$qual_medium_acc, {
          if (input$qual_radio_acc != "medium") {
            shinyjs::removeClass("qual_high_acc", "btn-success")
            shinyjs::addClass("qual_high_acc", "btn-danger")
            shinyjs::removeClass("qual_medium_acc", "btn-danger")
            shinyjs::addClass("qual_medium_acc", "btn-success")
            shinyjs::removeClass("qual_small_acc", "btn-success")
            shinyjs::addClass("qual_small_acc", "btn-danger")
            shiny::updateRadioButtons(session, "qual_radio_acc", selected = "medium")
          }
        })
        observeEvent(input$qual_small_acc, {
          if (input$qual_radio_acc != "small") {
            shinyjs::removeClass("qual_high_acc", "btn-success")
            shinyjs::addClass("qual_high_acc", "btn-danger")
            shinyjs::removeClass("qual_medium_acc", "btn-success")
            shinyjs::addClass("qual_medium_acc", "btn-danger")
            shinyjs::removeClass("qual_small_acc", "btn-danger")
            shinyjs::addClass("qual_small_acc", "btn-success")
            shiny::updateRadioButtons(session, "qual_radio_acc", selected = "small")
          }
        })
        spaceTyped <- reactive({
          tail(stringr::str_split(input$phrase, " ")[[1]], 1) == "" | input$phrase == ""
        })
        ngpred <- reactive({
          ngram.predict(input$phrase, input$qual_radio)
        })
        nb_col_models <- reactive({
          nbmod <- 0
          if (input$show_gts == TRUE) nbmod <- nbmod + 1
          if (input$show_gts_stem == TRUE) nbmod <- nbmod + 1
          if (input$show_ktz == TRUE) nbmod <- nbmod + 1
          if (input$show_ktz_stem == TRUE) nbmod <- nbmod + 1
          if (nbmod != 0) nb_c_m <- as.integer(12 / nbmod)
          else nb_c_m = 12
          nb_c_m
        })

        rvs.plot3D.gts <- reactiveValues(plot = NULL)
        rvs.plot3D.gts.stem <- reactiveValues(plot = NULL)
        rvs.plot3D.ktz <- reactiveValues(plot = NULL)
        rvs.plot3D.ktz.stem <- reactiveValues(plot = NULL)
        rvs.wordcloud.gts <- reactiveValues(wcplot = NULL)
        rvs.wordcloud.gts.stem <- reactiveValues(wcplot = NULL)
        rvs.wordcloud.ktz <- reactiveValues(wcplot = NULL)
        rvs.wordcloud.ktz.stem <- reactiveValues(wcplot = NULL)
        
        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            predictions <- ngpred()[["res.gts"]][1:10]
            print("res.gts")
            print(predictions)
            unique_pred <- unique(predictions$grams)
            Prob <- matrix(rep(0, 40), nrow = 10, ncol = 4)
            for (i in 1:length(unique_pred)) {
              for (j in 1:4) {
                pred <- predictions[n==j & grams==unique_pred[i]]
                if (nrow(pred)!=0) Prob[i, j] <- pred[1, p.gts]
              }
            }
            axx <- list(nticks = 4, title = "")
            axy <- list(nticks = 10, title = "")
            axz <- list(title = "", showticklabels = TRUE)
            g <- plot_ly(x = 1:4, y = as.factor(unique_pred), z = ~Prob, type = "surface", showscale = F, width = 400, height = 400) %>%
              layout(autosize = T,
                     scene = list(yaxis=axy, xaxis=axx, zaxis = axz, margin = list(l=0,r=0,t=0,b=0,pad=0), width = 400, height = 400,
                                  camera = list(eye = list(x = 2.0, y = 1.8, z = 1.8))))
            rvs.plot3D.gts$plot <- g
          }
        })
          
        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            predictions <- ngpred()[["res.gts.stem"]][1:10]
            print("res.gts.stem")
            print(predictions)
            unique_pred <- unique(predictions$grams)
            Prob <- matrix(rep(0, 40), nrow = 10, ncol = 4)
            for (i in 1:length(unique_pred)) {
              for (j in 1:4) {
                pred <- predictions[n==j & grams==unique_pred[i]]
                if (nrow(pred)!=0) Prob[i, j] <- pred[1, p.gts]
              }
            }
            axx <- list(nticks = 4, title = "")
            axy <- list(nticks = 10, title = "")
            axz <- list(title = "", showticklabels = TRUE)
            g <- plot_ly(x = 1:4, y = as.factor(unique_pred), z = ~Prob, type = "surface", showscale = F, width = 400, height = 400) %>%
              layout(autosize = T,
                     scene = list(yaxis=axy, xaxis=axx, zaxis = axz, margin = list(l=0,r=0,t=0,b=0,pad=0), width = 400, height = 400,
                                  camera = list(eye = list(x = 2.0, y = 1.8, z = 1.8))))
            rvs.plot3D.gts.stem$plot <- g
          }
        })
        
        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            predictions <- ngpred()[["res.ktz"]][1:10]
            print("res.ktz")
            print(predictions)
            unique_pred <- unique(predictions$grams)
            Prob <- matrix(rep(0, 40), nrow = 10, ncol = 4)
            for (i in 1:length(unique_pred)) {
              for (j in 1:4) {
                pred <- predictions[n==j & grams==unique_pred[i]]
                if (nrow(pred)!=0) Prob[i, j] <- pred[1, p.ktz]
              }
            }
            axx <- list(nticks = 4, title = "")
            axy <- list(nticks = 10, title = "")
            axz <- list(title = "", showticklabels = TRUE)
            g <- plot_ly(x = 1:4, y = as.factor(unique_pred), z = ~Prob, type = "surface", showscale = F, width = 400, height = 400) %>%
              layout(autosize = T,
                     scene = list(yaxis=axy, xaxis=axx, zaxis = axz, margin = list(l=0,r=0,t=0,b=0,pad=0), width = 400, height = 400,
                                  camera = list(eye = list(x = 2.0, y = 1.8, z = 1.8))))
            rvs.plot3D.ktz$plot <- g
          }
        })
        
        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            predictions <- ngpred()[["res.ktz.stem"]][1:10]
            print("res.ktz.stem")
            print(predictions)
            unique_pred <- unique(predictions$grams)
            Prob <- matrix(rep(0, 40), nrow = 10, ncol = 4)
            for (i in 1:length(unique_pred)) {
              for (j in 1:4) {
                pred <- predictions[n==j & grams==unique_pred[i]]
                if (nrow(pred)!=0) Prob[i, j] <- pred[1, p.ktz]
              }
            }
            axx <- list(nticks = 4, title = "")
            axy <- list(nticks = 10, title = "")
            axz <- list(title = "", showticklabels = TRUE)
            g <- plot_ly(x = 1:4, y = as.factor(unique_pred), z = ~Prob, type = "surface", showscale = F, width = 400, height = 400) %>%
              layout(autosize = T,
                     scene = list(yaxis=axy, xaxis=axx, zaxis = axz, margin = list(l=0,r=0,t=0,b=0,pad=0), width = 400, height = 400,
                                  camera = list(eye = list(x = 2.0, y = 1.8, z = 1.8))))
            rvs.plot3D.ktz.stem$plot <- g
          }
        })

        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            rvs.gts$buttons <- list()
            len <- length(unique(ngpred()[["res.gts"]][1:10]$grams))
            for (i in 1:len) {
              rvs.gts$buttons[[i]] <- actionButton(inputId = paste0("button.gts.",i),
                                                   label = unique(ngpred()[["res.gts"]][1:10]$grams)[i], class = "btn-primary btn-xs")
            }
            rvs.gts.stem$buttons <- list()
            len <- length(unique(ngpred()[["res.gts.stem"]][1:10]$grams))
            for (i in 1:len) rvs.gts.stem$buttons[[i]] <- actionButton(inputId = paste0("button.gts.stem.",i),
                                                                       label = unique(ngpred()[["res.gts.stem"]][1:10]$grams)[i], class = "btn-primary btn-xs")
            rvs.ktz$buttons <- list()
            len <- length(unique(ngpred()[["res.ktz"]][1:10]$grams))
            for (i in 1:len) rvs.ktz$buttons[[i]] <- actionButton(inputId = paste0("button.ktz.",i),
                                                                  label = unique(ngpred()[["res.ktz"]][1:10]$grams)[i], class = "btn-primary btn-xs")
            rvs.ktz.stem$buttons <- list()
            len <- length(unique(ngpred()[["res.ktz.stem"]][1:10]$grams))
            for (i in 1:len) rvs.ktz.stem$buttons[[i]] <- actionButton(inputId = paste0("button.ktz.stem.",i),
                                                                       label = unique(ngpred()[["res.ktz.stem"]][1:10]$grams)[i], class = "btn-primary btn-xs")
          }
        })

        output$plot3D.gts <- renderPlotly(rvs.plot3D.gts$plot)
        output$plot3D.gts.stem <- renderPlotly(rvs.plot3D.gts.stem$plot)
        output$plot3D.ktz <- renderPlotly(rvs.plot3D.ktz$plot)
        output$plot3D.ktz.stem <- renderPlotly(rvs.plot3D.ktz.stem$plot)


        observeEvent(eventExpr = input$phrase, handlerExpr = {
          if (spaceTyped()) {
            rvs.wordcloud.gts$wcplot <- wordcloud(ngpred()[["res.gts"]]$grams, ngpred()[["res.gts"]]$p.gts, max.words = 50,
                                             colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
            rvs.wordcloud.gts.stem$wcplot <- wordcloud(ngpred()[["res.gts.stem"]]$grams, ngpred()[["res.gts.stem"]]$p.gts, max.words = 50,
                                                colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
            rvs.wordcloud.ktz$wcplot <- wordcloud(ngpred()[["res.ktz"]]$grams, ngpred()[["res.ktz"]]$p.ktz, max.words = 50,
                      colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
            rvs.wordcloud.ktz.stem$wcplot <- wordcloud(ngpred()[["res.ktz.stem"]]$grams, ngpred()[["res.ktz.stem"]]$p.ktz, max.words = 50,
                      colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
          }
        })

        output$wordcloud_gts <- renderPlot({
          wordcloud(ngpred()[["res.gts"]]$grams, ngpred()[["res.gts"]]$p.gts, max.words = 50,
                    colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
        })
        output$wordcloud_gts_stem <- renderPlot({
          wordcloud(ngpred()[["res.gts.stem"]]$grams, ngpred()[["res.gts.stem"]]$p.gts, max.words = 50,
                    colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
        })
        output$wordcloud_ktz <- renderPlot({
          wordcloud(ngpred()[["res.ktz"]]$grams, ngpred()[["res.ktz"]]$p.ktz, max.words = 50,
                    colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
        })
        output$wordcloud_ktz_stem <- renderPlot({
          wordcloud(ngpred()[["res.ktz.stem"]]$grams, ngpred()[["res.ktz.stem"]]$p.ktz, max.words = 50,
                    colors = brewer.pal(6, "Dark2"), scale = c(4, 1))
        })

        rvs.gts <- reactiveValues(buttons = list(actionButton(inputId = "button.gts.1", label = 1)))
        rvs.gts.stem <- reactiveValues(buttons = list(actionButton(inputId = "button.gts.stem.1", label = 1)))
        rvs.ktz <- reactiveValues(buttons = list(actionButton(inputId = "button.ktz.1", label = 1)))
        rvs.ktz.stem <- reactiveValues(buttons = list(actionButton(inputId = "button.ktz.stem.1", label = 1)))
        
        output$suggestions.gts <- renderUI(rvs.gts$buttons)
        for(ii in 1:10){
          local({
            i <- ii
            observeEvent(eventExpr = input[[paste0("button.gts.",i)]],
                         handlerExpr = {
                           if (unique(ngpred()[["res.gts"]][1:10]$grams)[i] != "END") updateTextInput(session, 'phrase',
                                   value = paste(trimws(input$phrase), unique(ngpred()[["res.gts"]][1:10]$grams)[i], ""))
                           })
          })
        }
        output$suggestions.gts.stem <- renderUI(rvs.gts.stem$buttons)
        for(ii in 1:10){
          local({
            i <- ii
            observeEvent(eventExpr = input[[paste0("button.gts.stem.",i)]],
                         handlerExpr = {
                           if (unique(ngpred()[["res.gts"]][1:10]$grams)[i] != "END") updateTextInput(session, 'phrase',
                                    value = paste(trimws(input$phrase), unique(ngpred()[["res.gts.stem"]][1:10]$grams)[i], ""))
                         })
          })
        }
        output$suggestions.ktz <- renderUI(rvs.ktz$buttons)
        for(ii in 1:10){
          local({
            i <- ii
            observeEvent(eventExpr = input[[paste0("button.ktz.",i)]],
                         handlerExpr = {
                           if (unique(ngpred()[["res.gts"]][1:10]$grams)[i] != "END") updateTextInput(session, 'phrase',
                                    value = paste(trimws(input$phrase), unique(ngpred()[["res.ktz"]][1:10]$grams)[i], ""))
                         })
          })
        }
        output$suggestions.ktz.stem <- renderUI(rvs.ktz.stem$buttons)
        for(ii in 1:10){
          local({
            i <- ii
            observeEvent(eventExpr = input[[paste0("button.ktz.stem.",i)]],
                         handlerExpr = {
                           if (unique(ngpred()[["res.gts"]][1:10]$grams)[i] != "END") updateTextInput(session, 'phrase',
                                    value = paste(trimws(input$phrase), unique(ngpred()[["res.ktz.stem"]][1:10]$grams)[i], ""))
                         })
          })
        }
        output$benchmarkUI <- renderUI({
          list(
            conditionalPanel(condition="input.show_gts == true", column(nb_col_models(),
                   fluidRow(
                     tags$div(h5("Stupid backoff", style = "border-bottom: 2px solid darkblue"),
                              conditionalPanel(condition="input.showSuggestion == true", uiOutput('suggestions.gts')),
                              conditionalPanel(condition="input.show3DPlot == true", plotlyOutput('plot3D.gts', width = "400px", height = "350px")),
                              conditionalPanel(condition="input.showWordCloud == true", plotOutput('wordcloud_gts', width = "400px", height = "400px"))
                      )
                     ), inline = TRUE)
            ),
            conditionalPanel(condition="input.show_gts_stem == true", column(nb_col_models(),
                   fluidRow(
                     tags$div(h5("Stupid backoff with stemming", style = "border-bottom: 2px solid darkblue"),
                              conditionalPanel(condition="input.showSuggestion == true", uiOutput('suggestions.gts.stem')),
                              conditionalPanel(condition="input.show3DPlot == true", plotlyOutput('plot3D.gts.stem', width = "400px", height = "350px")),
                              conditionalPanel(condition="input.showWordCloud == true", plotOutput('wordcloud_gts_stem', width = "400px", height = "400px"))
                     )
                     ), inline = TRUE)
            ),
            conditionalPanel(condition="input.show_ktz == true", column(nb_col_models(),
                  fluidRow(
                    tags$div(h5("Katz backoff", style = "border-bottom: 2px solid darkblue"),
                             conditionalPanel(condition="input.showSuggestion == true", uiOutput('suggestions.ktz')),
                             conditionalPanel(condition="input.show3DPlot == true", plotlyOutput('plot3D.ktz', width = "400px", height = "350px")),
                             conditionalPanel(condition="input.showWordCloud == true", plotOutput('wordcloud_ktz', width = "400px", height = "400px"))
                    )
                    ), inline = TRUE)
            ),
            conditionalPanel(condition="input.show_ktz_stem == true", column(nb_col_models(),
                   fluidRow(
                     tags$div(h5("Katz backoff with stemming", style = "border-bottom: 2px solid darkblue"),
                              conditionalPanel(condition="input.showSuggestion == true", uiOutput('suggestions.ktz.stem')),
                              conditionalPanel(condition="input.show3DPlot == true", plotlyOutput('plot3D.ktz.stem', width = "400px", height = "350px")),
                              conditionalPanel(condition="input.showWordCloud == true", plotOutput('wordcloud_ktz_stem', width = "400px", height = "400px"))
                     )
                     ), inline = TRUE)
            )
          )
        })
        traceVals <- reactiveValues(trace_content = "")
        accuracy_table <- eventReactive(input$process_doc, {
          shinyjs::show("workinprogress")
          accuracyProcess(input$document, input$qual_radio_acc, session, traceVals)
        })
        output$process_doc_trace <- renderUI({
          tags$textarea(id = "process_doc_trace", class = "form-control", style = "width: 100%; height: 400px; font-size: 12px",
                        readonly = "", traceVals$content)
        })
        output$accuracyTable <- renderTable({
          accuracy_table()
        }, digits = 6, striped = TRUE, hover = TRUE)

})

