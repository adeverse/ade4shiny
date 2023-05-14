introtab <- tabItem(tabName = "introtab",

                    fluidRow( column(width = 3, tags$img(src = 'https://sdray.github.io/ade4/logo.svg')),
                              #column(width = 1),
                              column(width = 9, h2("Welcome to ade4's shiny dashboard !"), br(),
                 h4( em("Ade4"), " (Analysis of Ecological Data : Exploratory and Euclidean Methods in Environmental Sciences) is a", br(), "R package dedicated to statistical analysis of environmental data.", br(),
                     "It is developped at the", a("Laboratoire de Biométrie et Biologie Évolutive (LBBE) ", href = "https://lbbe-web.univ-lyon1.fr/en"), "in Claude Bernard Lyon 1 University.", br(),
                      "Learn more about ade4 ", a("here!", href = "https://sdray.github.io/ade4/index.html", target="_blank"))
                              )
                    ),
      
               fluidRow(
                 box(title = "What to use this dashboard for",
                     "We provide a clickable interface to help you get started with data analysis using R and ade4.", br(), br(),
                     tags$div(
                       tags$ul(
                         tags$li("Explore the dashboard using ade4 built-in datasets, or start analysing your own data."),
                         tags$li("Download the analysis results and the R script allowing you to reproduce it on your own."),
                         tags$li("Load the results back in and carry on with your analysis."),
                         tags$li("Create graphical outputs to display your results thanks to our 'Visualisation' tab.")
                       )),
                     ),
                 
                 box(title = "Currently implemented analysis", strong("One-table analyses"), br(),
                     tags$div(
                       tags$ul(
                         tags$li("Principal Component Analysis (PCA)"),
                         tags$li("Correspondence Analysis (COA)"),
                         tags$li("Multiple Correspondence Analysis (MCA)")
                       )),
                 strong("One-table with groups"), br(),
                     tags$div( tags$ul( tags$li("Between-Group Analysis (BGA)") ) ),
                 strong("Two-table analyses"), br(),
                     tags$div(
                       tags$ul(
                         tags$li("Coinertia Analysis (CA)"),
                         tags$li("Principal Component Analysis with respect to Instrumental Variables (PCAIV)")
                       ))
                 )
               ),
               h3("Ressources"),
               fluidRow(
                 column(width = 2, h4(a("Website", href = 'https://sdray.github.io/ade4/index.html', icon("globe", size = 'large')))),
                 column(width = 2, h4(a("Papers", href= 'https://sdray.github.io/ade4/articles/papers.html', icon("newspaper", size = 'large')))),
                 column(width = 2, h4(a("Book", href = 'https://sdray.github.io/ade4/articles/book.html', icon("book", size = 'large')))),
                 column(width = 2, h4(a("FAQ", href = 'https://sdray.github.io/ade4/articles/faq.html', icon("question", size = 'large')))),
                 column(width = 3, h4(a("Mailing list", href = 'https://listes.univ-lyon1.fr/sympa/info/adelist', icon("envelope", size = 'large')))),
                 ),
               h3("Vignettes"),
               fluidRow(
                     tags$div(tags$ul(
                       tags$li(a("Description of environmental variables structures", href = 'https://sdray.github.io/ade4/articles/ChapEnvVarStruct.html'),
                               tags$li(a("Description of species structures", href = 'https://sdray.github.io/ade4/articles/ChapSpeciesStruct.html'))
                       )))),
              )
              
# )
