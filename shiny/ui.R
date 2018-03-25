shinyUI(
  tagList(
    useShinyjs(),
    navbarPage("4-grams Next Word Prediction", collapsible = TRUE,
     tabPanel("Interactive prediction",
        fluidPage(theme = "bootstrap.min.css", useShinyjs(),
            tags$div(class = "navbar-default", style= "min-height: 0px",
             tags$div(class = "navbar-header", style = "min-height: 0px; height: 0px",
                HTML(
                  '<button type="button" class="navbar-toggle" style="width:15px; height: 11px; padding: 1px; line-height: 2px" data-toggle="collapse" data-target="#show_options">
                   <span class="sr-only">Toggle navigation</span>
                   <span class="icon-bar" style="width: 11px; heigth: 1px"></span>
                   </button>'
                )),
             tags$div(class = "navbar-collapse collapse", id = "show_options", style = "text-align:center",
                tags$div(style = "display:table; align: center; vertical-align:top",
                 tags$div(style = "display:table-cell; text-align:right",
                  actionButton("btnSuggestion", "Suggestions"),
                  actionButton("btn3DPlot", "3D plots"),
                  actionButton("btnWordCloud", "Word clouds"),
                  checkboxInput("showSuggestion", "Show suggestions", value = TRUE),
                  checkboxInput("show3DPlot", "Show 3D plots", value = TRUE),
                  checkboxInput("showWordCloud", "Show word clouds", value = TRUE)
                 ),
                 tags$div(style = "display:table-cell; vertical-align: middle","|"),
                 tags$div(style = "display:table-cell; text-align:center",
                  actionButton("btn_gts", "Stupid Backoff"),
                  actionButton("btn_gts_stem", "Stupid Backoff-stem"),
                  actionButton("btn_ktz", "Katz Backoff"),
                  actionButton("btn_ktz_stem", "Katz Backoff-stem"),
                  checkboxInput("show_gts", "Show Stupid Backoff", value = TRUE),
                  checkboxInput("show_gts_stem", "Show Stupid Backoff-stem", value = TRUE),
                  checkboxInput("show_ktz", "Show Katz Backoff", value = TRUE),
                  checkboxInput("show_ktz_stem", "Show Katz Backoff-stem", value = TRUE)
                 ),
                 tags$div(style = "display:table-cell; vertical-align: middle","|"),
                 tags$div(style = "display:table-cell; text-align:left",
                    tags$div(class = "btn-group",
                      actionButton("qual_high", "High"),
                      actionButton("qual_medium", "Medium"),
                      actionButton("qual_small", "Low")
                    ),
                    radioButtons("qual_radio", label = NULL, choices = list("high", "medium", "small"), selected = "medium")
                 )
                )
             )
          ),
         fluidRow(
           textInput('phrase', label='Type a text', value='Show must go ', width="100%")
         ),
         fluidRow(
          uiOutput('benchmarkUI')
         )
        )
     ),
     tabPanel("Acurracy",
        fluidPage(theme = "bootstrap.min.css", useShinyjs(),
          tags$div(class = "navbar-default", style= "min-height: 0px",
             tags$div(class = "navbar-header", style = "min-height: 0px; height: 0px",
                HTML(
                  '<button type="button" class="navbar-toggle" style="width:15px; height: 11px; padding: 1px; line-height: 2px" data-toggle="collapse" data-target="#show_options_acc">
                   <span class="sr-only">Toggle navigation</span>
                   <span class="icon-bar" style="width: 11px; heigth: 1px"></span>
                   </button>'
                )),
             tags$div(class = "navbar-collapse collapse", id = "show_options_acc", style = "text-align:center",
                tags$div(style = "display:table; align: center; vertical-align:top",
                   tags$div(style = "display:table-cell; text-align:left",
                    tags$div(class = "btn-group",
                       actionButton("qual_high_acc", "High"),
                       actionButton("qual_medium_acc", "Medium"),
                       actionButton("qual_small_acc", "Low")
                    ),
                    radioButtons("qual_radio_acc", label = NULL, choices = list("high", "medium", "small"), selected = "medium")
                   )
                )
             )
          ),
          fluidRow(
             tags$label("Paste your text here and click Process", `for` = "document"),
             tags$textarea(id = "document", class = "form-control", style = "width: 100%; height: 200px;", "\"Gerard Depardieu voted in the Russian presidential election at a polling station at the Russian embassy in France. Come!,\" it tweeted, urging other Russians in France to follow suit.
 
A video of the moment, tweeted by a reporter from Russian broadcaster RT, was widely circulated on French news sites, where it triggered a tide of mostly contemptuous responses.
 
\"I don't like insults but for me this man is a sellout,\" one person tweeted in French. Another posted a GIF of a man vomiting.
 
Depardieu caused an uproar in 2012 when he declared he would hand back his French passport and move to Belgium to avoid a tax hike by the then Socialist government. His popularity slumped further after he accepted a Russian passport -- and a bear hug -- from Putin.
 
The larger-than-life star of \"Cyrano de Bergerac\" and the Asterix & Obelix franchise, who is still a French citizen, has since become a staunch defender of the Kremlin's policies, including its annexation of Crimea.
                           
                           \"I'm a citizen of the world,\" he told Italian newspaper Corriere della Sera in 2016. \"France is likely to become a Disneyland for foreigners, populated by imbeciles making wine and stinky cheese for tourists.\"")
          ),
          fluidRow(
            
            tags$div(style = "display: table; vertical-align: middle; width: 100%",
              tags$div(id = "workinprogress", style = "display: table-cell; vertical-align: middle; width: 50%;text-align: center; color: #406791; font-weight: bold",
                       "Work in progress. Please wait..."
              ),
              tags$div(style = "display: table-cell; vertical-align: middle; width: 50%; text-align: right; padding-top: 5px; padding-right: 5px",
                                          actionButton("process_doc", label = "Process")
              )
            )
          ),
          fluidRow(
            column(7,
              uiOutput("process_doc_trace")
            ),
            column(5,
              tableOutput('accuracyTable')
            )
          )
        )
     ),
     tabPanel("Instructions",
        includeHTML("html/HowToUse.html")
     ),
     tabPanel("Development lines",
        includeHTML("html/DevelopmentLines.html")
     ),
     tabPanel("Improvements",
        includeHTML("html/Improvements.html")
     ),
     tabPanel("Code",
              h3("Github repository"),
              p("Code of shiny application, and of R presentation are available on the link below"),
              a("https://github.com/vinceforce/coursera-capstone", href = "https://github.com/vinceforce/coursera-capstone", target = "_blank")
     )
  )
  )
)