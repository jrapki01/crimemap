server <- function(input, output, session){
  
## This will control the links in the guidance section of the homepage
## Basically setting up action buttons and then an observe function to make the switch
  observeEvent(input$link_to_tabpanel_a, {
    newvalue <- "Crime Map"
    updateTabItems(session, "nav", newvalue)
  })
  observeEvent(input$link_to_tabpanel_b, {
    newvalue <- "Contextual Mapping"
    updateTabItems(session, "nav", newvalue)
  })  
  observeEvent(input$link_to_tabpanel_c, {
    newvalue <- "Cluster Analysis - Experimental"
    updateTabItems(session, "nav", newvalue)
  })
  
  
## This is the reactive filter on the crime data
  data_crime <- reactive({
    
      crime %>% 
      filter(Month >= input$date_range[1] & Month <= input$date_range[2]) %>% 
      filter(Crime.type %in% input$crime) %>% 
      filter(WD19CD %in% input$geography) %>% 
      group_by(lookup2) %>% 
      mutate(Longitude = Longitude,
             Latitude = Latitude,
             Location = Location,
             WD19NM = WD19NM,
             Crime.type = Crime.type,
             Count = Count) %>% 
      ungroup()
      
    
  })
  
## This is the reactive filter on the crime data for timeseries
  data_crime_trend <- reactive({
    
    crime %>% 
      filter(Month >= input$date_range[1] & Month <= input$date_range[2]) %>% 
      filter(Crime.type %in% input$crime) %>% 
      filter(WD19CD %in% input$geography) %>% 
      group_by(Month, Crime.type) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      mutate(Crime.type = as.factor(Crime.type))
    
  })

## This is the reactive filter on the ward shapefile
  data_wards <- reactive({
    
      wards %>% 
      filter(WD19CD %in% input$geography)
    
  })
  
## This is the reactive filter on the lsoa shapefile so that the deprivation map will match to wards selected
  data_lsoa <- reactive({
    
    lsoa %>% 
      filter(WD18CD %in% input$geography)
    
  })
  
## This monitors the ward input and returns a TRUE if a user deselects all - this will then
## force leaflet to just show a shape of Southend
  blank_geography <- reactive({is.null(input$geography)
    
  })
  
## This monitors the crime input and returns a TRUE if a user deselects all - this will then
## force leaflet to just show the ward outlines
  blank_crime <- reactive({is.null(input$crime)
    
  })
  
## This will control the dataset for cluster analysis
  
  cluster_data <- reactive({
    
    groups <- as.data.frame(input$crimecluster)

      crime %>% 
      filter(Crime.type %in% input$crimecluster) %>% 
      mutate(Crime.type = as.character(Crime.type)) %>% 
      group_by(LSOA11CD) %>% 
      summarise(Group1 = sum(Crime.type == groups[1,]),
                Group2 = sum(Crime.type == groups[2,]),
                Group3 = sum(Crime.type == groups[3,])) %>% 
      ungroup() %>% 
      tibble::column_to_rownames('LSOA11CD') %>% 
      as.data.frame() %>% 
      scale()
  })
  
## This will calculate the most effective number of clusters to use
  
  cluster_number <- reactive({
    
    numMeans <- NbClust(cluster_data(), min.nc = 3, max.nc = 6, method = "kmeans")
    
    getmode(numMeans$Best.partition)

  })
  
## This will calculate the clusters and save the output as a list of LSOA and Cluster
  cluster_rf_analysis <- reactive({
    
    set.seed(1)
    
    rf <- randomForest::randomForest(x = cluster_data(), ntree = 4000, proximity = T)
    
    dissMatrix <- sqrt(1 - rf$proximity)
    
    set.seed(123)
    
    pamRF <- pam(dissMatrix, k = cluster_number())
    
    rf_values <- unname(pamRF$clustering)
    
    rf_clusters <- names(pamRF$clustering) %>% 
      as_tibble %>% 
      rename(LSOA11CD = 1) %>% 
      mutate(LSOA11CD = as.factor(LSOA11CD)) %>% 
      mutate(Clusters = rf_values)
  })
  
## This will merge the cluster data to the lsoa shapefile for plotting
  cluster_map <- reactive({
    
    x <- merge(lsoa, cluster_rf_analysis(), by.x = "LSOA11CD", by.y = "LSOA11CD")
    
  })
  
## This will merge the cluster data to the crime data to create a summary statistics
  cluster_summary <- reactive({
    
    z <- merge(crime, cluster_rf_analysis(), by.x = "LSOA11CD", by.y = "LSOA11CD")
    
    z <- z %>% 
      select(Crime.type, Clusters) %>%
      mutate(Crime.type = as.factor(Crime.type),
             Clusters = as.factor(Clusters)) %>% 
      filter(Crime.type %in% input$crimecluster) %>% 
      group_by(Crime.type, Clusters) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      rename('Crime Type' = Crime.type) %>% 
      pivot_wider(names_from = Clusters, values_from = Count) %>% 
      select(1, 2, 3, everything())
  
  })
  
## This is the data filter for the details map
  data_crime_additional <- reactive({
    
    crime %>% 
      filter(Month >= input$date_range_additional[1] & Month <= input$date_range_additional[2]) %>% 
      filter(Crime.type %in% input$crime_additional) %>% 
      group_by(lookup2) %>% 
      mutate(Longitude = Longitude,
             Latitude = Latitude,
             Location = Location,
             WD19NM = WD19NM,
             Crime.type = Crime.type,
             Count = Count) %>% 
      ungroup()
    
  })
  
## This is the data filter for the additional details trend
  data_crime_trend_summary <- reactive({
    
    crime %>% 
      filter(Month >= input$date_range_additional[1] & Month <= input$date_range_additional[2]) %>% 
      filter(Crime.type %in% input$crime_additional) %>% 
      group_by(Month, Crime.type) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      mutate(Crime.type = as.factor(Crime.type))
    
  })

## This is the leaflet output
  output$mymap <- renderLeaflet({
    
    ## If statement to monitor if a user has deselected all
    if(blank_geography() == TRUE){   
      
      ## If ward input is blank then return just the shape of Southend
      m <- leaflet() %>%
      addPolygons(data = district,
                  weight=2,
                  opacity=1,
                  color="grey",
                  dashArray= "3",
                  fillOpacity = 0.5) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
        addEasyButton(easyButton(
          icon="fa-home", title="Zoom to Level",
          onClick=JS("function(btn, map){ map.setZoom(13); }")))}
    
    ## If crime input is blank then return just a ward map
    else if(blank_crime() == TRUE){
      
      m <- leaflet() %>%
        addPolygons(data = data_wards(),
                    weight=2,
                    opacity=1,
                    color="grey",
                    dashArray= "3",
                    fillOpacity = 0.5) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addEasyButton(easyButton(
          icon="fa-home", title="Zoom to Level",
          onClick=JS("function(btn, map){ map.setZoom(13); }")))}
    
    ## This is the important bit and returns a ward level map of crime based on user input
    else{
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s<br/>%s<br/>Count: %g",
      data_crime()$Crime.type, data_crime()$WD19NM, data_crime()$Location, data_crime()$Count
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet() %>%
      addMapPane("base_ward", zIndex = 410) %>%
      addMapPane("crime_circles", zIndex = 420) %>% 
      addPolygons(data = data_wards(),
                  group = "Ward Level",
                  weight=2,
                  opacity=1,
                  color="grey",
                  dashArray= "3",
                  fillOpacity = 0.5,
                  options = pathOptions(pane = "base_ward")) %>%
      addCircleMarkers(data = data_crime()
                       ,lng = ~Longitude
                       ,lat = ~Latitude
                       ,group = ~Crime.type
                       ,color = ~pal(Crime.type)
                       #,radius = ~ifelse(Count <=10, 5, ifelse(Count > 10 & Count <= 50, 10, ifelse(Count >50 & Count <=100, 15, 20)))
                       ,radius = ~Count*2
                       ,stroke = FALSE
                       ,fillOpacity = 0.95
                       ,label= labels
                       ,labelOptions = labelOptions(
                         style=list("font-weight" = "normal", padding ="3px 8px")
                         ,textsize="15px"
                         ,direction="auto")
                       ,options = pathOptions(pane = "crime_circles")) %>% 
      addProviderTiles(providers$CartoDB.Positron)%>%
      addEasyButton(easyButton(
        icon="fa-home", title="Zoom to Level",
        onClick=JS("function(btn, map){ map.setZoom(13); }"))) %>%
      addLegend(pal = pal, values = crime$Crime.type, title = "Crime Types", position = "bottomleft")
    }
  }) 
  
  output$hchart <- renderHighchart({
    
    hc <- hchart(data_crime()$Crime.type, type = "bar", name = "Count", showInLegend = FALSE)
    
  })
  
  output$hchart_trend <- renderHighchart({
    
    hc_t <- hchart(data_crime_trend(), type = "line", hcaes(x = Month, y = Count , group = Crime.type), showInLegend = FALSE) %>% hc_colors(mypalette)
  })
  
  
  output$cluster_map <- renderLeaflet({

    df <- cluster_map()
    
    bins_cluster <- c(1:6)
    pal_cluster <- colorBin("Set1", domain = df$Clusters, bins = bins_cluster, reverse=TRUE)
    
    labels_cluster <- sprintf(
      "<strong>%s</strong><br/>%s<br/>Cluster: %g",
      df$WD18NM, df$LSOA11NM_x, df$Clusters
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addPolygons(data = df,
                  weight=2,
                  opacity=1,
                  color="grey",
                  dashArray= "3",
                  fillColor = ~pal_cluster(Clusters),
                  fillOpacity = 0.5,
                  label = labels_cluster) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addEasyButton(easyButton(
        icon="fa-home", title="Zoom to Level",
        onClick=JS("function(btn, map){ map.setZoom(13); }"))) %>% 
      addLegend(pal = pal_cluster, values = df$Clusters, title = "Clusters", position = "bottomleft")
    
  }) 
  
  output$cluster_table <- renderTable(cluster_summary())
  
  output$text1 <- renderText({
    paste("Based on the chosen combination of crime types there are ", cluster_number(), " LSOA clusters.")
  })
  
  
  output$cluster_map_sync <- renderLeaflet({
    
    df <- cluster_map()
    
    bins_cluster <- c(1:6)
    pal_cluster <- colorBin("Set1", domain = df$Clusters, bins = bins_cluster, reverse=TRUE)
    
    labels_cluster <- sprintf(
      "<strong>%s</strong><br/>%s<br/>Cluster: %g",
      df$WD18NM, df$LSOA11NM_x, df$Clusters
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addPolygons(data = df,
                  weight=2,
                  opacity=1,
                  color="grey",
                  dashArray= "3",
                  fillColor = ~pal_cluster(Clusters),
                  fillOpacity = 0.5,
                  label = labels_cluster) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addEasyButton(easyButton(
        icon="fa-home", title="Zoom to Level",
        onClick=JS("function(btn, map){ map.setZoom(13); }"))) %>%
      addLegend(pal = pal_cluster, values = df$Clusters, title = "Clusters", position = "bottomright") %>% 
      syncWith("maps")
    
  }) 
  
  output$deprivation_map_sync <- renderLeaflet({
    
    labels_additional <- sprintf(
      "<strong>%s</strong><br/>%s",
      lsoa$WD18NM, lsoa$LSOA11NM_x
    ) %>% lapply(htmltools::HTML)
    
   m <- leaflet() %>%
      addMapPane("base_lsoa", zIndex = 440) %>%
      addMapPane("dep_lsoa", zIndex = 410) %>% 
      addMapPane("den_lsoa", zIndex = 420) %>% 
      addPolygons(data = lsoa,
                  weight=2,
                  opacity=1,
                  color="grey",
                  dashArray= "3",
                  fillOpacity = 0,
                  label = labels_additional,
                  options = pathOptions(pane = "base_lsoa")) %>%
      addPolygons(data = lsoa
                  ,group = "Deprivation Level"
                  ,fillColor = ~pal_dep(IMD)
                  ,fillOpacity = 0.5
                  ,weight = 2
                  ,opacity = 0.5
                  ,dashArray= "3"
                  ,color = ~pal_dep(IMD)
                  ,options = pathOptions(pane = "dep_lsoa")) %>%
      addPolygons(data = lsoa
                  ,group = "Population per Sq Km"
                  ,fillColor = ~pal_den(DENSITY)
                  ,fillOpacity = 0.5
                  ,weight = 2
                  ,opacity = 0.5
                  ,dashArray= "3"
                  ,color = ~pal_den(DENSITY)
                  ,options = pathOptions(pane = "den_lsoa")) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%  
      addEasyButton(easyButton(
       icon="fa-home", title="Zoom to Level",
       onClick=JS("function(btn, map){ map.setZoom(13); }"))) %>% 
      addLayersControl(
        overlayGroups = c("Deprivation Level", "Population per Sq Km"),
        options = layersControlOptions(collapsed = TRUE),
        position = c("topleft")
      ) %>%
      addLegend(pal = pal_dep, values = lsoa$IMD, title = "IMD Decile", group = "Deprivation Level",
                position = "bottomright") %>%
      addLegend(pal = pal_den, values = lsoa$DENSITY, title = "Population per Sq Km", group = "Population per Sq Km",
                position = "bottomright") %>%
      hideGroup(c("Ward Level", "Population per Sq Km")) %>%
     syncWith("maps") 

    
  }) 
  
  ## This is the leaflet output
  output$crime_map_additional <- renderLeaflet({

      labels_additional <- sprintf(
        "<strong>%s</strong><br/>%s<br/>%s<br/>Count: %g",
        data_crime_additional()$Crime.type, data_crime_additional()$WD19NM, data_crime_additional()$Location, data_crime_additional()$Count
      ) %>% lapply(htmltools::HTML)
      
      m <- leaflet() %>%
        # addMapPane("base_lsoa", zIndex = 430) %>%
        addMapPane("dep_lsoa", zIndex = 410) %>% 
        addMapPane("den_lsoa", zIndex = 420) %>% 
        addMapPane("crime_circles", zIndex = 440) %>% 
        # addPolygons(data = lsoa,
        #             weight=2,
        #             opacity=1,
        #             color="grey",
        #             dashArray= "3",
        #             fillOpacity = 0,
        #             options = pathOptions(pane = "base_lsoa")) %>%
        addPolygons(data = lsoa
                    ,group = "Deprivation Level"
                    ,fillColor = ~pal_dep(IMD)
                    ,fillOpacity = 0.7
                    ,weight = 2
                    ,opacity = 0.5
                    ,dashArray= "3"
                    ,color = ~pal_dep(IMD)
                    ,options = pathOptions(pane = "dep_lsoa")) %>%
        addPolygons(data = lsoa
                    ,group = "Population per Sq Km"
                    ,fillColor = ~pal_den(DENSITY)
                    ,fillOpacity = 0.7
                    ,weight = 2
                    ,opacity = 0.5
                    ,dashArray= "3"
                    ,color = ~pal_den(DENSITY)
                    ,options = pathOptions(pane = "den_lsoa")) %>%
        addCircleMarkers(data = data_crime_additional()
                         ,lng = ~Longitude
                         ,lat = ~Latitude
                         ,color = "grey"
                         #,radius = ~ifelse(Count <=10, 5, ifelse(Count > 10 & Count <= 50, 10, ifelse(Count >50 & Count <=100, 15, 20)))
                         ,radius = ~Count*2
                         ,stroke = FALSE
                         ,fillOpacity = 0.95
                         ,label = labels_additional
                         ,labelOptions = labelOptions(
                           style=list("font-weight" = "normal", padding ="3px 8px")
                           ,textsize="15px"
                           ,direction="auto")
                         ,options = pathOptions(pane = "crime_circles")) %>% 
        addProviderTiles(providers$CartoDB.Positron)%>%
        addEasyButton(easyButton(
          icon="fa-home", title="Zoom to Level",
          onClick=JS("function(btn, map){ map.setZoom(13); }"))) %>%
        addLayersControl(
          overlayGroups = c("Deprivation Level", "Population per Sq Km"),
          options = layersControlOptions(collapsed = TRUE),
          position = c("topleft")
        ) %>%
      addLegend(pal = pal_dep, values = lsoa$IMD, title = "IMD Decile", group = "Deprivation Level",
                position = "bottomleft") %>%
      addLegend(pal = pal_den, values = lsoa$DENSITY, title = "Population per Sq Km", group = "Population per Sq Km",
                  position = "bottomleft") %>%
      hideGroup(c("Ward Level", "Population per Sq Km"))
  })
  
  output$hchart_trend2 <- renderHighchart({
    
    hc_t <- hchart(data_crime_trend_summary(), type = "line", hcaes(x = Month, y = Count , group = Crime.type), showInLegend = FALSE)
  })
  
                          
}
