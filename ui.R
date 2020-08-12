ui <- bootstrapPage(
  
# # # # # # # # # # # # # # # # # # # # # # # # # 
#                                               #                   
#                Home Tab                       #
#                                               #
# # # # # # # # # # # # # # # # # # # # # # # # #

  
 navbarPage("Southend-on-Sea Crime Explorer", id = "nav",
    
     tabPanel("Home",
        div(class = "outer",
            ## Set up the styling, logo and favicon
              tags$head(
                ## Custom CSS
                includeCSS("style.css"),
                #includeScript("code.js"),
                tags$script(type="text/javascript", src = "code_header.js"),
                #HTML("<link rel='icon' href='favicon.ico'>")
                tags$link(rel = "icon", href = "favicon.png")
                  ),
            ## Main panel with guidance
          mainPanel(          
          h2("Introduction"),
          p("Welcome to the Southend-on-Sea Crime Explorer. This tool has been developed by Southend-on-Sea Borough Council as part of the SmartSouthend open data platform."),
          p("This tool is split into three tabs whose functions are detailed below."),
          p("Crime data has been obtained from the police open data portal: data.police.uk"),
          p("Last updated: April 2020"),
          hr(),
          h3("Guidance"),
          br(),
            tabsetPanel(
              tabPanel("Crime Map",
                br(),
                actionLink("link_to_tabpanel_a", "Go to tab"),
                br(),
                br(),
                p("This tab is an interactive crime map detailing the location of individual crimes in Southend-on-Sea."),
                p("Use the date filter to view the location of crimes committed since the 1 January 2017."),
                p("The default view will show the most recent month's data."),
                p("Use the ward filter to view the crimes committed in specific Southend wards."),
                p("Use the crime filter to view the location of specific types of crimes."),
                br()
              ),
              tabPanel("Contextual Mapping",
                br(),
                actionLink("link_to_tabpanel_b", "Go to tab"),
                br(),
                br(),
                p("This tab allows the user to overlay the location of specific crimes with additional contextual information."),
                p("Use the date filter to view the location of crimes committed since the 1 January 2019."),
                p("The default view will show the most recent month's data."),
                p("Use the crime filter to view the location of specific types of crimes."),
                p("Use the layers button to filter between different contextual layers."),
                p("Available contextual information:"),
                  tags$ul(
                    tags$li(tags$a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/835115/IoD2019_Statistical_Release.pdf", "Indices of Multiple Deprivation (2019)")),
                    tags$li(tags$a(href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareapopulationdensity", "Population density (population per square kilometer)"))
                       ),
                       br()),
              tabPanel("Cluster Analysis",
                br(),
                actionLink("link_to_tabpanel_c", "Go to tab"),
                br(),
                br(),
                p("This tab shows the results of a cluster analysis of crime type by location."),
                p("This is an experimental development tab and likely to change."),
                p("Cluster analysis divides data into groups or clusters that are similar."),
                p("The goal of cluster analysis is to divide the data so that the observations within a group or cluster are more similar to each other (related) than to observations in different cluster (unrelated). "),
                p("In this case, we are looking for similarities between the total number of three user defined crime types committed within different lower super output areas (LSOAs) located in Southend."),
                p("Use the crime filter to select three crime types to compare. At present, the functionality of this analysis is limited to only three crime types."),
                p("The cluster analysis uses all data from 1 January 2019 to 1 April 2020."),
                       br())
              )
          )
        )
      ),
     
     # # # # # # # # # # # # # # # # # # # # # # # # # 
     #                                               #                   
     #            Interactive Crime Map              #
     #                                               #
     # # # # # # # # # # # # # # # # # # # # # # # # #

     tabPanel("Crime Map",
        div(class = "outer",
            
            # If not using custom CSS, set height of leafletOutput to a number instead of percent
            leafletOutput("mymap", width="100%", height="100%"),
          
            ## This is the hovering panel for the inputs and chart output
            absolutePanel(id = "controls", style = "overflow-y:scroll; max-height: 750px",
                          class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                          width = 350, height = "auto",
                          
                          h2(" "),
                          
                          ## SelectDateRange - date range
                          dateRangeInput(inputId = "date_range"
                                      ,label = "Select Date Range"
                                      ,start = max(crime$Month)
                                      ,end = max(crime$Month)
                                      ,min = min(crime$Month)
                                      ,max = max(crime$Month)
                                      ,format = "yyyy-mm"
                                      ,width = '100%'),
                          ## PickerInput - ward
                          pickerInput(inputId = "geography",
                                      label = "Select Wards",
                                      choices = c('Belfairs' =  'E05002212',
                                                  'Blenheim Park' = 'E05002213',
                                                  'Chalkwell' = 'E05002214',
                                                  'Eastwood Park' = 'E05002215',
                                                  'Kursaal' = 'E05002216',
                                                  'Leigh' = 'E05002217',
                                                  'Milton' = 'E05002218',
                                                  'Prittlewell' = 'E05002219',
                                                  'Shoeburyness' = 'E05002222',
                                                  'Southchurch' = 'E05002223',
                                                  'St. Laurence' = 'E05002220',
                                                  "St. Luke's" = 'E05002221',
                                                  'Thorpe' = 'E05002224',
                                                  'Victoria' = 'E05002225',
                                                  'West Leigh' = 'E05002227',
                                                  'West Shoebury' = 'E05002228',
                                                  'Westborough' = 'E05002226'),
                                      selected = c('Belfairs' =  'E05002212',
                                                   'Blenheim Park' = 'E05002213',
                                                   'Chalkwell' = 'E05002214',
                                                   'Eastwood Park' = 'E05002215',
                                                   'Kursaal' = 'E05002216',
                                                   'Leigh' = 'E05002217',
                                                   'Milton' = 'E05002218',
                                                   'Prittlewell' = 'E05002219',
                                                   'Shoeburyness' = 'E05002222',
                                                   'Southchurch' = 'E05002223',
                                                   'St. Laurence' = 'E05002220',
                                                   "St. Luke's" = 'E05002221',
                                                   'Thorpe' = 'E05002224',
                                                   'Victoria' = 'E05002225',
                                                   'West Leigh' = 'E05002227',
                                                   'West Shoebury' = 'E05002228',
                                                   'Westborough' = 'E05002226'),
                                      options = list('actions-box' = TRUE),
                                      multiple = TRUE),
                          ## PickerInput - crime
                          pickerInput(inputId = "crime",
                                      label = "Select Crime",
                                      choices = c(levels(crime$Crime.type)),
                                      selected = c(levels(crime$Crime.type)),
                                      options = list('actions-box' = TRUE),
                                      multiple = TRUE),
                          
                          ## Output for the bar chart
                          highchartOutput("hchart"),
                          
                          ## Output for trend
                          highchartOutput("hchart_trend")
                              )
                    ),
        ## Link to police data
        tags$a(id="cite", href="https://data.police.uk/",
                 'Data Source: data.police.uk'
        )
),

# # # # # # # # # # # # # # # # # # # # # # # # # 
#                                               #                   
#            Contextual Map                     #
#                                               #
# # # # # # # # # # # # # # # # # # # # # # # # #

tabPanel("Contextual Mapping",
            div(class = "outer",
         
             # If not using custom CSS, set height of leafletOutput to a number instead of percent
             leafletOutput("crime_map_additional", width = "100%", height="100%"),
             
             ## This is the hovering panel for the inputs and chart output
             absolutePanel(id = "controls", style = "overflow-y:scroll; max-height: 750px",
                           class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                           width = 350, height = "auto",
                           
                           ## Bit of space
                           h2(" "),
                           
                           ## SelectDateRange - date range
                           dateRangeInput(inputId = "date_range_additional"
                                          ,label = "Select Date Range"
                                          ,start = max(crime$Month)
                                          ,end = max(crime$Month)
                                          ,min = min(crime$Month)
                                          ,max = max(crime$Month)
                                          ,format = "yyyy-mm"
                                          ,width = '100%'),
                           
                           ## PickerInput - crime
                           pickerInput(inputId = "crime_additional",
                                       label = "Select Crime",
                                       choices = c(levels(crime$Crime.type)),
                                       selected = "Anti-social behaviour",
                                       options = list('actions-box' = TRUE),
                                       multiple = FALSE),
                           
                           ## Output for trend
                           highchartOutput("hchart_trend2")
             )
          ),
         ## Link to police data source
         tags$a(id="cite", href="https://data.police.uk/",
                'Data Source: data.police.uk'
         )
         ),

# # # # # # # # # # # # # # # # # # # # # # # # # 
#                                               #                   
#            Cluster Analysis                   #
#                                               #
# # # # # # # # # # # # # # # # # # # # # # # # #
tabPanel("Cluster Analysis - Experimental",
         
        ## Side bar with crime input
        sidebarLayout(
          sidebarPanel(
          p("Choose three crime types from the drop down list below."),
          em("Please note that selecting less than, or more than three crimes is not currently possible."),
          br(),
          br(),
        ## SelectInput - drop down list to years to choose from
        pickerInput(inputId = "crimecluster",
           label = "Select three crime types:",
           choices = c(levels(crime$Crime.type)),
           selected = c("Drugs", "Possession of weapons", "Violence and sexual offences"),
           multiple = TRUE,
           options = list("max-options" = 3)
           )                            
            ),
          mainPanel(
            
            ## Stop showing error messages
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            
            ## split the main panel into three sections
            tabsetPanel(
              ## Section 1 - Output
              tabPanel("Output",
                    br(),
                    ## Descriptive text
                    p("The map below is a visual representation of the cluster analysis performed on the crime types selected."),
                    p("The colours represent different clusters with each lower super output area (LSOA) assigned to a specific cluster based on their similarity to other LSOAs within that cluster."),
                    p("Hover over each LSOA to see which electoral ward that LSOA belongs to and to which cluster that LSOA also belongs."),
                    br(),
                    ## Cluster map
                    leafletOutput("cluster_map"),
                    br(),
                    textOutput("text1"),
                    br(),
                    ## Descriptive text and table
                    h4("Summary Counts:"),
                    p("The table below provides more detail around the total number of crimes recorded within each clustering group, broken down by crime type."),
                    p("Each cluster contains LSOAs that are 'similar' based on the number and type of crimes reported within each LSOA."),
                    p("One cluster will typically contain LSOAs which together have the highest recorded number of all three crime types within Southend. You can then compare these counts to the map above to see whether it is a small number of LSOAs that might have the highest number of crime types selected."),
                    p("A second cluster will then typically contain LSOAs which together have the next highest numbers of all three crime types within Southend. By comparing these counts to the map above, we might see that this cluster contains more LSOAs but has a smaller total number of recorded crimes than the first cluster."),
                    p("Further clusters will then contain LSOAs with a decreasing number of crimes. It is also possible that further clusters will contain LSOAs where only two out of the three selected crime types have been reported."),
                    p("By comparing the total counts of crime committed in each cluster in the table below to the location of each cluster on the map we will begin to understand where specific 'pockets' of related crimes are being committed."),
                    tableOutput("cluster_table")
            ),
              ## Section 2 - Methodology
            tabPanel("How it works",
                     br(),
                     h4("Overview"),
                     p("Put simply, a cluster analysis divides data into groups or clusters that are similar."),
                     p("The goal is to divide the data in such a way so that the observations within a group or cluster are more similar to each other (related) than to observations in different cluster (unrelated). "),
                     p("In this case, we are grouping lower super output areas (LSOAs) in Southend based on how similar each LSOA is to another in terms of the total number of speficic crimes committed in those LSOAs."),
                     p("By plotting the LSOAs on a map and colour coding them based on the cluster each LSOA belongs to, we can begin to see areas of Southend that are similar to each other based on the number and type of crimes committed."),
                     p("It might then be possible to compare these clusters to other contextual or demographic information in an attempt to understand potential drivers of crime."),
                     br(),
                     br(),
                     h4("Technical Methodology"),
                     p("This analysis is using a random forest with partitioning around medoids (PAM) clustering approach."),
                     p("A random forest in an unsupervised method was chosen because there are a number of advantages which suit the data available, these are:"),
                     tags$ul(
                       tags$li("Robust against outliers and highly skewed variables"),
                       tags$li("Handles mixed data"),
                       tags$li("Can be used to eliminate features by examining variable importance"),
                       tags$li("The dissimilarity matrix produced can be used as an input to other techniques such as PAM")
                     ),
                     br(),
                     p("The dissimilarity matrix from the random forest is entered into a partition around medoids (PAM) clustering algorithm."),
                     p("A medoid is an observation of a cluster that minimises the dissimilarity (in this case using a radnom forest approach) between the other observations in that cluster. Put simply, if running a cluster analysis with five clusters, you will have five partitions of data with each observation within the cluster being more similar to other observations within that cluster than to observations within other clusters."),
                     p("The PAM algorithm iterates over the following steps:"),
                     tags$ol(
                       tags$li("Randomly select n observations as the initial medoid"),
                       tags$li("Assign each observation to the closest medoid"),
                       tags$li("Swap each medoid and non-medoid observation, computing the dissimilarity between them"),
                       tags$li("Select the composition that minimises the total dissimilarity"),
                       tags$li("Repeat steps 2 through 4 until there is no change in the medoids")
                     )
                     ),
              ## Section 3 - Contextual comparison
            tabPanel("Contextual Comparison",
                     br(),
                     h5("Cluster Output"),
                     p("Compare cluster groups to other contextual information."),
                     leafletOutput("cluster_map_sync"),
                     br(),
                     h5("Contextual Comparison"),
                     leafletOutput("deprivation_map_sync"))
          )
        )
      )
    )
 )
)
