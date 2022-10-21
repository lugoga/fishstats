require(tidyverse)
require(sf)
# require(terra)
# require(tidyterra)
require(tmap)
require(magrittr)
require(ggpmisc)
require(highcharter)
require(plotly)
require(leaflet)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinyalert)

require(sizeMat)
require(LBSPR)
require(TropFishR)
require(conflicted)


theme_set(theme_bw(base_size = 12))
tmap_mode(mode = "view")

conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "annotate", winner = "ggplot2")


# Data ----
## lates niloticus ----

np = read_csv("datasets/lates_niloticus.csv")

np = np %>% 
  mutate(lon = lon_deg + lon_min/60, lat = (lat_deg+lat_min/60)*-1) %>% 
  dplyr::select(date, year, location, lon, lat,strata, mean_depth_m, tl_cm:swimbladder_length_cm)

np.sf = np %>% 
  distinct(location, .keep_all = TRUE) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



summarstats = np %>% 
  select(location, tl_cm, wt_g, strata) %>% 
  group_by(location) %>% 
  ggpubr::get_summary_stats(type = "common") %>% 
  filter(variable == "tl_cm") %>% 
  ungroup()


sites = np %>% distinct(location) %>% pull()

ln.stock = np %>% 
  mutate(month = lubridate::month(date, label = FALSE),
         new_date = lubridate::make_date(year = 2021, month = month)) %>% 
  filter(tl_cm < 100) %>% 
  relocate(new_date, .after = date)


ui = navbarPage(
  title = "R4Fs", theme = bslib::bs_theme(version = 5, bootswatch = "journal"), collapsible = TRUE,
  # user interface ----
  ## home panel----
  tabPanel(title = "Home",
           fluidRow(
             column(width = 1),
             column(width = 9, 
                    tags$h3("Fisheries & Environment at our Fingertips"),
                    tags$hr()
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$p("A fishstats is a shiny app that provides an intuitive fisheries and environment data visualiations. The app has rich algorithms that perform analyses and allows users to interact with data and gain insight of key fisheries metrics in the marine and freshwater ecosystems. The tool is useful during this decade where the ocean and blue economy is recognized as the The Decade of the Ocean. "),
                    tags$p("The Decade provides a common framework to ensure that ocean science can fully support countries’ actions to sustainably manage the ocean and more particularly to achieve the 2030 Agenda for Sustainable Development – through the creation of a new foundation, across the science-policy interface, to strengthen the management of the ocean and coasts for the benefit of humanity.")
                    ),
             column(width = 3,
                    tags$p("A vast majority of the ocean remains unmapped, unobserved and unexplored. Our understanding of the ocean and its contribution to sustainability largely depends on our capacity to conduct effective ocean science - through research and sustained observations, supported by adequate infrastructures and investments."),
                    tags$img(src='un_ocean_decade.jpg',height='263',width='350')
                    ),
             column(width = 3,
                    tags$p("This decade of the ocean comes down with a slogan the science we want for the ocean we need! But we flip it a little bit and should read the information we want for the ocean we need, why. Because we are at the time of precedented generation of data than ever before. Data generated from sensors to satellite is enormous. "),
                    tags$img(src='SDG-logo.png',height='263',width='268')
                    ),
             column(width = 2,
                    helpText("Click or scan this QRCODE to access a live link of the fishStats's GitHub Repository. A repository contain the source code, files and images used to develop this app"), tags$br(),
                    tags$a(href='https://github.com/lugoga/fishstats', tags$img(src='fishstats_github.png',height='147',width='150')),
                    tags$hr()
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8,
                    tags$h4("What does the app do?"),
                    tags$hr(),
                    tags$p("This app demostrates how to generate, describe, visualize , analyse, model data and estimate fisheries stocks.")
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$li("The first set of graphs focuses on gender balance, showing the relative proportion of explicitly Male and Female words within each sentence, and the words used within sentences which contained predominantly Male or predominantly Female words.")
                    ),
             column(width = 3,
                    tags$li("The second set presents a Sentiment analyis, showing the different emotions detected in the text and the evolution of positive and negative emotions throughout the text. Once again, the graphs allow users to see how the Male and Female words in their text are tied to the emotions.")
             ),
             column(width = 3,
                    tags$li("The third set presents a Topic analysis, showing the user how their words clustered together, and highlighting again where those topics were tied to words in predominantly Male or Female sentences.")
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8, 
                    tags$h4("But, Why?"),
                    tags$hr()
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8,
                    tags$p("People fish to generate a catch, whether they are on the deck of a megatrawler in the frigid waters of the North Pacific, in a canoe along an African coast, or even walking with their children on a reef flat collecting invertebrates for their next meal on an island in the Indian Ocean. Thus, reliable information on current and past catches are the foundation for understanding fisheries and are crucial baseline data for any attempt to project or predict future catches")
                    )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8, 
                    tags$h4("Sources of Data used to develop the R4FS app"),
                    tags$hr()
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8,
                    tags$b("OpenFisheries: "), tags$a(href="http://www.openfisheries.org/", "The home page,"),tags$br(),
                    tags$b("Deep Sea Fishing Authority: "), tags$a(href="https://www.dsfa.go.tz/home", "The statistics page,"),tags$br(),
                    tags$b("Indian Ocean Tuna Commission: "), tags$a(href="https://iotc.org/", "News page"),tags$br(),
                    tags$br()
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8, 
                    tags$h4("Developers"),
                    tags$hr()
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 6,
                    tags$b("Masumbuko Semba: "), tags$a(href="https://semba-blog.netlify.app/", "A hinge of fisheries and oceanographical data", "and", tags$a(href = "https://semba.netlify.app/", " A gateway a tools for data analysis in R")),
                    # "Masumbuko Semba, The Nelson Mandela African Institution of Science and Technology",tags$br(),
                    # "Nyamisi Peter, University of Dar es Salaam",
                    tags$br(),
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 8, 
                    tags$h4("Acknowlegements"),
                    tags$hr(),
                    tags$p("R for fisheries science (R4FS) is a collaborative effort of international organization, private and public institutions and many people who have contributed .....")
             )
           ),
           fluidRow(
             column(width = 1),
             column(width = 11,
                    tags$br(),
                    tags$img(src = "FAO_logo.svg.png", width = "113px", height = "115px"),
                    tags$img(src = "coat.png", width = "100px", height = "115px"), 
                    tags$img(src = "un_ocean_decade_logo.jpg", width = "107px", height = "115px"), 
                    tags$img(src = "wiomsa.png", width = "177px", height = "115px"), 
                    tags$img(src = "Tafiri-logo-recreated.png", width = "112px", height = "115px"), 
                    tags$img(src = "dsfa.png", width = "102px", height = "115px"), 
                    tags$img(src = "udsm.png", width = "109px", height = "115px"), 
                    tags$img(src = "nmaist.png", width = "130px", height = "115px"), 
                    tags$img(src = "gmes.png", width = "307px", height = "115px"),
                    tags$br()
             )
           ),
           fluidRow()
           ),
  tags$br(),
  tags$br(),
  ## simulation panel----
    tabPanel(title = "Simulate",
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Linear"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Linear sequences can be transformed to nonlinear sequences of numbers using a variety of functions. Some examples are given below")),
             column(width = 1,
                    numericInput(inputId = "first", label = "", value = 1, min = 0, max = 9, step = 1),
                    numericInput(inputId = "second", label = "", value = 10, min = 5, max = 20, step = 1)
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "seq_id")
                    ),
             column(width = 3,
                    plotOutput(outputId = "linearPlotId", height = "200px", width = "200px"))
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Non Linear"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Linear sequences can be transformed to nonlinear sequences of numbers using a variety of functions. Some examples are given below")),
             column(width = 1,
                    numericInput(inputId = "moja", label = "", value = 1, min = 0, max = 9, step = 1),
                    numericInput(inputId = "mbili", label = "", value = 10, min = 10, max = 20, step = 2)
                    ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "nonseq_id")
                    ),
             column(width = 3,
                    plotOutput(outputId = "nonlinearplot_id", height = "200px", width = "200px")),
             
             
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Logarithmic"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Linear sequences can be transformed to nonlinear sequences of numbers using a variety of functions. Some examples are given below")),
             column(width = 1,
                    numericInput(inputId = "mojaLog", label = "", value = 1, min = 0, max = 9, step = 1),
                    numericInput(inputId = "mbiliLog", label = "", value = 11, min = 10, max = 20, step = 2)
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "log_id")
             ),
             column(width = 3,
                    plotOutput(outputId = "logplot_id", height = "200px", width = "200px")),
             
             
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Cyclic"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Many phenomena such as temperature or market activity occur in regularly repeating cycles. The sin() and cos() functions can be used to create cyclical sequences.")),
             column(width = 1,
                    numericInput(inputId = "mojasin", label = "", value = 1, min = 0, max = 9, step = 1),
                    numericInput(inputId = "mbilisin", label = "", value = 16, min = 10, max = 20, step = 2)
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "sin_id")
             ),
             column(width = 3,
                    plotOutput(outputId = "sinplot_id", height = "200px", width = "200px")),
             
             
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Random Distribution"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("A normally distributed sequence of numbers can be created using the qnorm() quantile function.")),
             column(width = 1,
                    numericInput(inputId = "mojaqnorm", label = "Mean", value = 25),
                    numericInput(inputId = "mbiliqnorm", label = "SD", value = 2)
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "qnorm_id")
             ),
             column(width = 3,
                    plotOutput(outputId = "qnormplot_id", height = "200px", width = "200px")),
             
             
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Normal Distribution"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("A normally distributed sequence of numbers can be created using the qnorm() quantile function.")),
             column(width = 1,
                    numericInput(inputId = "sample_kubwa", label = "Samples", value = 20),
                    numericInput(inputId = "mojarnorm", label = "Mean", value = 25),
                    numericInput(inputId = "mbilirnorm", label = "SD", value = 2)
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "rnorm_id")
             ),
             column(width = 3,
                    plotOutput(outputId = "rnormplot_id", height = "200px", width = "200px")),
             
             
           ),
           
           fluidRow(
             column(width = 1),
             column(width = 5,
                    tags$h4("Logistic Distribution"))
           ),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("A normally distributed sequence of numbers can be created using the qnorm() quantile function.")),
             column(width = 1,
                    numericInput(inputId = "mojalogit", label = "Minimum", value = 1),
                    numericInput(inputId = "mbililogit", label = "Maximum", value = 20),
                    numericInput(inputId = 'logis_id', label = "Simulate", value = 100, min = 20, max = 100,step = 20 )
             ),
             column(width = 3,
                    helpText("You obtain a sequence of numbers in vector form ranged from the lowest to the maximum numbers specified"),
                    verbatimTextOutput(outputId = "logit_id")
             ),
             column(width = 3,
                    plotOutput(outputId = "logitplot_id", height = "200px", width = "200px")),
             
             
           )

           
           ),
  ## Descriptive Panel----
  tabPanel(title = "Descriptive",
           fluidRow(),
           tags$br()
           ,
           fluidRow(
             column(width = 1),
             column(width = 1,
                    helpText("Just slide buttons to plot"),
                    sliderInput(inputId = "tl_id", label = "", value = c(10, 60), min = 0, max = 80, step = 10),
                    sliderInput(inputId = "sample_id", label = "", min = 5, max = 30, value = 15, step = 5)
                    ),
             column(width = 2,
                    plotOutput(outputId = "histogram_id", height = "250px")
                    ),
             column(width = 2,
                    # verbatimTextOutput(outputId = "summaryParameters"),
                    plotOutput(outputId = "box_id", height = "250px", width = "200px")
                    ),
             column(width = 2,
                    # verbatimTextOutput(outputId = "summaryParameters"),
                    plotOutput(outputId = "box_idw", height = "250px", width = "200px")
             ),
             column(width = 3,
                    highchartOutput(outputId = "bar_id", width = "400px", height = "250px")
                    
                    )
           ),
           tags$br()
           ,
           fluidRow(
             column(width = 1), column(width = 10, tags$hr())
           ),
           tags$br()
           ,
           fluidRow(
             column(width = 1),
             column(width = 1,
                    helpText("The station has been pre-filtered and only those meet criteria are in display"),
                    uiOutput(outputId = "sites_id")
             ),
             column(width = 1,
                    helpText("Limit length and polynomial's order"),
                    sliderInput(inputId = "tl_id2", label = "", min = 0, value = c(0, 90), max = 100, step = 10),
                    sliderInput(inputId = "poly_id", label = "", value = 3, min = 1, max = 3, step = 1)
                    ),
             column(width = 2,
                    plotOutput(outputId = "scatter_id", height = "250px")
                    ),
             column(width = 2,
                    plotOutput(outputId = "scatterLog_id", height = "250px")
                    ),
             column(width = 2,
                    tmapOutput(outputId = "location_id", height = "250px")
                    )
           ),
           tags$br()
           
           
           ),
  ## statistical panell ----
  tabPanel(title = "Statistics",
           fluidRow(
             column(width = 1),
             column(width = 6,
                    tags$h3("What is Statistics Used for?"),
                    tags$hr())
           ),
           fluidRow(
             column(width = 1),
             column(width = 3,
                    tags$p("Statistics is used in all kinds of science and business applications. Statistics gives us more accurate knowledge which helps us make better decisions. Statistics can focus on making predictions about what will happen in the future. It can also focus on explaining how different things are connected.")
                    ),
             column(width = 3,
                    tags$li("Develop questions"),
                    tags$li("search Data: collect or use existing"),
                    tags$li("Explore and Describe Data"),
                    tags$li("Analyse and Model"),
                    tags$li("Draw conclusion and Share results"),
                    tags$p("It is important to keep all three steps in mind for any questions we want more knowledge about.")
                    ),
             column(width = 3,
                    tags$p("Knowing which types of data are available can tell you what kinds of questions you can answer with statistical methods. Knowing which questions you want to answer can help guide what sort of data you need. A lot of data might be available, and knowing what to focus on is important.")
                    )
           ),
           fluidRow(),
           fluidRow(),
           fluidRow(),
           fluidRow(),
           fluidRow(),
           fluidRow()
           ),
  ## fishstock panel ----
  tabPanel(title = "Stocks",
           fluidRow(),
           fluidRow(
             column(width = 1),
             column(width = 2,
                    tags$p("Culluptat abore quiam audis sunt odigeni tiorepr eptatur as reption conseque re laccull esciisq uatendipit aliquam ni omnis porehenetur? Bus nes suscit, quidunt fugit prere quam sus cuptae estem. Ut audit qui offici corectur, ut por maio quiam nimus. ? Bus nes suscit, quidunt fugit prere quam sus cuptae estem. Ut audit qui offici corectur, ut por maio quiam nimus"),
                    sliderInput(inputId = "length_id", label = "", min = 0, max = 110, value = c(0,100), step = 10)
                    ),
             column(width = 1,
                    helpText("toggle to select or unselect one or more sampled years"),
                    checkboxGroupButtons(
                      inputId = "year_id",
                      label = "",
                      choices = c("2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                      selected = "2021",
                      direction = "vertical",
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle", 
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o", 
                                    style = "color: steelblue"))
                      )
                    ),
             column(width = 2,
                    helpText("With maturity level, the L50 is estimated using a robust gonadal information"), 
                    plotOutput(outputId = "gonadal_id", width = "250px", height = "350px")),
             column(width = 2,
                    helpText("When maturity level is missing, the adult and juvenile are estimated using classification techniques"), 
                    # plotOutput(outputId = "juveAdult_id", width = "250px", height = "350px"),
                    plotOutput(outputId = "freqPlot_id", width = "250px", height = "250px")
                    ),
             column(width = 2,
                    helpText("When maturity level is missing, the L50 is estimated using a morphometric information"), 
                    # plotOutput(outputId = "morpho_id", width = "250px", height = "350px"),
                    plotlyOutput(outputId = "freqPie_id", width = "250px", height = "250px")
                    )
             
           ),
           fluidRow(
             column(width = 1),
             column(width = 10, tags$hr())
           ),
           # fluidRow(
           #   column(width = 1),
           #   column(width = 1,
           #          ),
           #   column(width = 3,
           #          plotOutput(outputId = "freqPlot_id", width = "250px", height = "250px")),
           #   column(width = 3,
           #          plotlyOutput(outputId = "freqPie_id", width = "250px", height = "250px"))
           # ),
           fluidRow(),
           fluidRow()
           
           ),
  tabPanel(title = "Environment"),
  tabPanel(title = "Projections")
  
)


server = function(input, output, session){
  
  np.reactive = reactive({
    np.filtered = np %>% 
      filter(tl_cm>= input$tl_id[1] &tl_cm <= input$tl_id[2])
  })
  
  output$histogram_id = renderPlot({
    
    np.reactive() %>% 
    ggplot(aes(x = tl_cm))+
      geom_histogram(fill = "cyan4", color = "beige") +
      geom_vline(xintercept = np.reactive()$tl_cm %>% quantile(), color = "red") +
      labs(x = "Total length (cm)", y = "Frequency")
  })
  
  
  output$box_id = renderPlot({
    np.reactive() %>% 
      ggplot(aes(x = tl_cm))+
      geom_boxplot(fill = "cyan4", outlier.colour = "#FF0000")+
      coord_flip()+
      labs(x = "Total length (cm)", y = "")+
      theme(axis.text.x = element_blank())
  })
  
  output$box_idw = renderPlot({
    np.reactive() %>% 
      ggplot(aes(x = wt_g))+
      geom_boxplot(fill = "cyan4", outlier.colour = "#FF0000")+
      coord_flip()+
      labs(x = "Weight (gm)", y = "")+
      theme(axis.text.x = element_blank())
  })
  
  output$summaryParameters = renderPrint({
    np.reactive() %$% EnvStats::summaryStats(tl_cm) 
  })
  
  
  sampled.tb = reactive({
    summarstats %>% 
      # filter(n == ) %>% 
      mutate(colorGroup = if_else(n < 30, "Poor", "Good"))%>% 
      sample_n(size = input$sample_id) %>%
      arrange(-n) 
  })
  
  output$bar_id = renderHighchart({
    
    sampled.tb() %>%
      hchart(type = "bar", 
             hcaes(x = location, y = n, color = c(Good = "#1D681F", Poor = "#FF0000")[colorGroup])) %>% 
      hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>%
      hc_yAxis(title = list(text = "Sample size"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(pointFormat = "Sample: <b>{point.y}</b>") %>%
      hc_plotOptions(series = list(cursor = "default")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_chart(backgroundColor = "transparent")
  })
  

  output$sites_id = renderUI({
    
    pickerInput(inputId = "Individual_sites", label = "", 
                choices = sampled.tb() %>% distinct(location) %>% pull()
                )

  })
  
  
  
  output$scatter_id = renderPlot({
    
    np %>% 
      filter(location %in% input$Individual_sites & between(tl_cm, input$tl_id2[1], input$tl_id2[2])) %>% 
      ggplot(aes(x = tl_cm, y = wt_g)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~poly(x, input$poly_id), color = "blue", fill = "blue", alpha = .1)+
      # scale_x_continuous(trans = scales::log10_trans())+
      # scale_y_continuous(trans = scales::log10_trans())+
      labs(x = "Length (cm)", y = "Weight (g)")+
      stat_correlation(mapping = aes(label = paste(after_stat(cor.label),
                                                   # after_stat(t.value.label),
                                                   after_stat(p.value.label),
                                                   after_stat(n.label),
                                                   sep = '*"; "*')))+
      stat_fit_deviations(formula = y~poly(x, input$poly_id), colour = "red") 
      
  })
  
  output$scatterLog_id = renderPlot({
    
    np %>% 
      filter(location %in% input$Individual_sites & between(tl_cm, input$tl_id2[1], input$tl_id2[2])) %>% 
      ggplot(aes(x = tl_cm, y = wt_g)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~poly(x, input$poly_id), color = "blue", fill = "blue", alpha = .1)+
      scale_x_continuous(trans = scales::log10_trans())+
      scale_y_continuous(trans = scales::log10_trans())+
      labs(x = "Log-length (cm)", y = "Log-weight (g)")+
      stat_correlation(mapping = aes(label = paste(after_stat(cor.label),
                                                   # after_stat(t.value.label),
                                                   after_stat(p.value.label),
                                                   after_stat(n.label),
                                                   sep = '*"; "*')))
    
  }) 
  
  output$location_id = renderTmap({
    
    tm_shape(shp = np.sf, name = "Sampling Sites") +
      tm_markers(id = "location", popup.vars = c("Depth [m]" = "mean_depth_m", "Length [cm]" = "tl_cm"), clustering = TRUE)+
    tm_shape(shp = np.sf %>% filter(location %in% input$Individual_sites), name = "Selected Site")+
      tm_markers(id = "location", popup.vars = c("Depth [m]" = "mean_depth_m", "Length [cm]" = "tl_cm", "Weight [g]" = "wt_g"))
    
  })
  
  
  ## Simulate ----
  output$seq_id = renderPrint({
    a= seq(input$first, input$second)
    a
    
  })
  
  
  output$linearPlotId  = renderPlot({
    a= seq(input$first, input$second)
    b= a
    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = a, y = b))+
      geom_point()+
      geom_smooth(se = FALSE, color = "red", formula = y~poly(x, 1))
  })
  
  output$nonseq_id = renderPrint({
    a = seq(input$moja, input$mbili)
    exp(a)
    
  })
  
  output$nonlinearplot_id = renderPlot({
    a = seq(input$moja, input$mbili)
    b = exp(a)

    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = a, y = b))+
      geom_point()+
      geom_smooth(se = FALSE, color = "red", formula = y~poly(x, 3))
    
  })
  
  output$log_id = renderPrint({
    a = seq(input$mojaLog, input$mbiliLog)
    log(a)
    
  })
  
  output$logplot_id = renderPlot({
    a = seq(input$mojaLog, input$mbiliLog)
    b = log(a)
    
    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = a, y = b))+
      geom_point()+
      geom_smooth(se = FALSE, color = "red", formula = y~poly(x, 1))
    
  })
  
  
  output$sin_id = renderPrint({
    a = seq(input$mojasin, input$mbilisin)
    sin(a)
    
  })
  
  output$sinplot_id = renderPlot({
    a = seq(input$mojasin, input$mbilisin)
    b = sin(a)
    
    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = a, y = b))+
      geom_point()+
      geom_line(color = "red")
    
  })
  
  
  
  
  output$qnorm_id = renderPrint({
    a = seq(0, 1, 0.02)
    b = qnorm(p = a, mean = input$mojaqnorm, sd = input$mbiliqnorm)
    tb = tibble(a,b)
    
    tb %>% dplyr::filter(is.finite(b)) %>% sample_n(size = 25) %>% pull()
    
  })
  
  output$qnormplot_id = renderPlot({
    a = seq(0, 1, 0.02)
    b = qnorm(p = a, mean = input$mojaqnorm, sd = input$mbiliqnorm)
    
    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = b))+
      geom_histogram(color = "ivory", bins = 8)
    
  })
  
  
  
  output$rnorm_id = renderPrint({
    a = input$sample_kubwa
    b = rnorm(n = a, mean = input$mojarnorm, sd = input$mbilirnorm)
    b    

  })
  
  output$rnormplot_id = renderPlot({
    a = input$sample_kubwa
    b = rnorm(n = a, mean = input$mojarnorm, sd = input$mbilirnorm)
    
    tb = tibble(a,b)
    tb %>% 
      ggplot(aes(x = b))+
      geom_histogram(color = "ivory", bins = 18)
    
  })
  
  
  
  output$logit_id = renderPrint({
    
    a = input$mojalogit:input$mbililogit
    b = SSlogis(input = a, Asym = input$logis_id, xmid = 10, scal = 1)
    b
       
    
  })
  
  output$logitplot_id = renderPlot({
    a = input$mojalogit:input$mbililogit
    b = SSlogis(input = a, Asym = input$logis_id, xmid = 10, scal = 1)
    
    tb = tibble(a,b)
    
    tb %>% 
      ggplot(aes(x = a, y = b))+
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks())+
      scale_y_continuous(name = "values",breaks = scales::pretty_breaks())+
      theme(axis.title.x = element_blank())+
      annotate(geom = "text",x = 3.5, y = 10, label = "Initial\n Colonization", color = "darkorchid" )+
      annotate(geom = "text",x = 13.5, y = input$logis_id/2, label = "High Growth", color = "cyan4" )+
      annotate(geom = "text",x = 16.5, y = input$logis_id - 5, label = "Limit of \nCarrying Capacity", color = "darkorange" )
      

  })
  
  output$gonadal_id = renderPlot({
    
    ln_l50 = ln.stock %>% 
      filter(year == input$year_id) %>%
      gonad_mature(varNames = c("tl_cm", "maturity"), 
                   inmName = c("I","II"),
                   matName = c("III","IV","V","VI"), 
                   method = "fq", niter = 50)
    
    
    # ln_l50 %>% print()
    

    ln_l50 %>% 
      plot(xlab = "Total length (cm.)", 
           ylab = "Proportion mature", 
           col = c("blue", "red"), 
           onlyOgive = TRUE, 
           las= 1)
  })
  
  
  
  freq = reactive({ 
    np %>% 
      mutate(year = as.character(year)) %>% 
    filter(year == input$year_id & tl_cm > input$length_id[1] & tl_cm < input$length_id[2]) 
  })
  
  output$freqPlot_id = renderPlot({
    freq() %>% 
      ggplot() +
      geom_freqpoly(aes(x = tl_cm), bins = 50)+
      # scale_x_log10()+
      scale_x_continuous(trans = scales::log2_trans()) +
      scale_y_continuous(limits = c(0,NA), expand = c(0,NA))+
      geom_vline(xintercept = 45.2, linetype = 2, color = "red")+
      geom_vline(xintercept = 51, linetype = 1, color = "blue")+
      # facet_wrap(~year)+
      labs(x = "Total length (cm)", y = "Frequency")+
      theme(strip.background = element_blank())+
      theme(panel.grid.minor = element_blank())
  })
  
  output$freqPie_id = renderPlotly({
    freq() %>% 
      mutate(group = if_else(tl_cm >= 50, "Adult", "Juvenile")) %>% 
      group_by(group) %>% 
      tally() %>% 
      # hchart(type = "pie", hcaes(x = group, y = n)) %>% 
      plot_ly(type = "pie", labels = ~group, values = ~n, hole = .4) %>% 
      plotly::layout(title = "", 
                     showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  
  
}


shinyApp(ui, server)




