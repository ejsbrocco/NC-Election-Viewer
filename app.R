library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(reshape2)

#### Read in data and make filepaths for geojsons ####
load("data/NCEV2016_data.RData")
fn = data.frame(countyNames,filepath = paste0("data/geojson/NC2016_",countyNames$county,".geojson"))

####  User Interface  ####
ui = dashboardPage(
  dashboardHeader(
    title = "NC Election Viewer (beta)",
    titleWidth = 275
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 9,
             box(width = NULL,solidHeader = TRUE,
                 leafletOutput("map",height = 500)
             ),
             box(width = NULL,solidHeader = TRUE,
                 dataTableOutput("turnoutTable"))
      ),
      column(width = 3,
             box(width = NULL, solidHeader = TRUE,
                 selectInput("countySelect","Select County",
                             choices = countyNames$county,
                             selected = countyNames$county[32]),
                 selectInput("selectDate","Select election date",
                             choices = c("11/08/2016"),
                             selected = "11/08/2016"),
                 selectInput("turnoutVar","Turnout statistics to get",
                             choices = c(
                               Overall = "total",
                               Party = "party_cd",
                               Race = "race_code",
                               Ethnicity = "ethnic_code",
                               Sex = "sex_code",
                               Age = "age"),
                             selected = "total"),
                 selectInput("rawData","Display data as",
                             choices = c(
                               "Raw Counts" = 1,
                               "% of Registered Voters" = 0,
                               "% of Precinct Total" = -1),
                             selected = 1),
                 actionButton("goButton","Click to Get Data")
                 ),
             box(width = NULL, solidHeader = TRUE,
                 p(
                   class = "text-muted",
                   paste("NC Election Viewer was developed to improve situational",
                         "awareness of past voter performance in North Carolina",
                         "elections and is intended for use by political organizers",
                         "interested in improving NC voter participation at the precinct",
                         "level. It is provided free of charge for non-commercial use,",
                         "and while data have been checked for errors, the author makes",
                         "no warantees and cannot guarantee the accuracy of source data.")
                 ),
                 p(class = "text-muted",
                   paste("Note: Chrome users may need to decrease zoom level on",
                         "browser in order for map legend to display properly.")),
                 p(
                   class = "text-muted",
                   HTML(
                     paste("Data Source: NC State Board of Elections",
                         '<br/>',"Data last updated 7/21/2017")
                     )),
                 p(
                   class = "text-muted",
                   HTML(
                     paste("NC Election Viewer, version 1.0 (beta)",
                           '<br/>',"Shiny Application by EJ Sbrocco, Ph.D.",
                           '<br/>',"Biologist/Data Scientist",
                           '<br/>',"Contact: ejsbrocco@gmail.com")
                   ))
                 )
             )
      )
    # fluidRow(
    #   box(width = NULL,solidHeader = TRUE,
    #       dataTableOutput("turnoutTable"))
    # )
  )
)

####  Server ####


## Function to read in correct spatial data file 
server = function(input,output,session){
  
  ## Reactive: subset and select turnout data
  formatTurnout = eventReactive(input$goButton,{
    if(input$rawData == 1){
      if(input$turnoutVar == "total"){
        tmp = subset(turnout.data,county == input$countySelect)
        out = aggregate(data.frame(Total = tmp$total),by = list(Precinct = tmp$precinct),sum)
        return(out)
      } else {
        tmp = subset(turnout.data,county == input$countySelect)
        tmp2 = aggregate(data.frame(Total = tmp$total),by = list(Precinct = tmp$precinct,demo = tmp[,input$turnoutVar]),sum)
        out = dcast(tmp2,Precinct~demo,value.var = "Total")
        return(out)
      }
    }
    if(input$rawData == 0){
        if(input$turnoutVar == "total"){
          # Get turnout
          to.tmp = subset(turnout.data,county == input$countySelect)
          to.agg = aggregate(data.frame(Total = to.tmp$total),by = list(Precinct = to.tmp$precinct),sum)
         
          # Get voter registration
          vr.tmp = subset(vreg.data,county == input$countySelect)
          vr.agg = aggregate(data.frame(Total = vr.tmp$total),by = list(Precinct = vr.tmp$precinct),sum)
 
          # Only keep rows and columns present in both
          keepPre = intersect(to.agg$Precinct,vr.agg$Precinct)
          keepVars = intersect(colnames(to.agg),colnames(vr.agg))
          
          # Now subset to make to and vr same
          to.sub = subset(to.agg,Precinct %in% keepPre,select = keepVars)
          to.sub = to.sub[order(to.sub$Precinct),]
          
          vr.sub = subset(vr.agg,Precinct %in% keepPre,select = keepVars)
          vr.sub = vr.sub[order(vr.sub$Precinct),]

          # Get % of registered voters
          pct = round(100*to.sub[,-1]/vr.sub[,-1],2)
          pct[pct > 100] = 100
          out = data.frame(Precinct = to.sub$Precinct,pct)
          colnames(out) = c(keepVars[1],paste0(keepVars[-1]," (% reg)"))
          return(out)
        } else {
          # Get turnout
          to.tmp = subset(turnout.data,county == input$countySelect)
          to.agg = aggregate(data.frame(Total = to.tmp$total),by = list(Precinct = to.tmp$precinct,demo = to.tmp[,input$turnoutVar]),sum)
          to.cast = dcast(to.agg,Precinct~demo,value.var = "Total")
          
          # Get voter registration
          vr.tmp = subset(vreg.data,county == input$countySelect)
          vr.agg = aggregate(data.frame(Total = vr.tmp$total),by = list(Precinct = vr.tmp$precinct,demo = vr.tmp[,input$turnoutVar]),sum)
          vr.cast = dcast(vr.agg,Precinct~demo,value.var = "Total")
          
          # Only keep rows and columns present in both
          keepPre = intersect(to.cast$Precinct,vr.cast$Precinct)
          keepVars = intersect(colnames(to.cast),colnames(vr.cast))
          
          # Now subset to make to and vr same
          to.sub = subset(to.cast,Precinct %in% keepPre,select = keepVars)
          to.sub = to.sub[order(to.sub$Precinct),]
          
          vr.sub = subset(vr.cast,Precinct %in% keepPre,select = keepVars)
          vr.sub = vr.sub[order(vr.sub$Precinct),]
          
          # Merge so that data lines up
          pct = round(100*to.sub[,-1]/vr.sub[,-1],2)
          pct[pct > 100] = 100
          out = data.frame(Precinct = to.sub$Precinct,pct)
          colnames(out) = c(keepVars[1],paste0(keepVars[-1]," (% reg)"))
          return(out)
          }
    }
    if(input$rawData == -1){
      if(input$turnoutVar == "total"){
        tmp = subset(turnout.data,county == input$countySelect)
        out = aggregate(data.frame(Total = tmp$total),by = list(Precinct = tmp$precinct),sum)
        out[,-1] = round(100*out[,-1]/out[,-1],2)
        colnames(out)[-1] = paste0(colnames(out)[-1]," (% tot)")
        return(out)
      } else {
        tmp = subset(turnout.data,county == input$countySelect)
        tmp2 = aggregate(data.frame(Total = tmp$total),by = list(Precinct = tmp$precinct,demo = tmp[,input$turnoutVar]),sum)
        out = dcast(tmp2,Precinct~demo,value.var = "Total")
        out[,-1] = round(100*out[,-1]/rowSums(out[,-1],na.rm = TRUE),2)
        colnames(out)[-1] = paste0(colnames(out)[-1]," (% tot)")
        return(out)
      }
    }
    
  })
  
  
  output$turnoutTable = renderDataTable({
    formatTurnout()
    })

  
  ## Reactive: load county map, based on input$countySelect
  shp = eventReactive(formatTurnout(),{
    fnSelect = fn[which(fn$county == input$countySelect),"filepath"]
    tmp = readOGR(as.character(fnSelect),layer="OGRGeoJSON")
    return(merge(tmp,formatTurnout(),by.x = "precinct",by.y = "Precinct",all.x = TRUE))
    })
  
  varnames = eventReactive(formatTurnout(),{
    colnames(formatTurnout())[-1]
    })
  
  legvals = eventReactive(formatTurnout(),{
    z = range(formatTurnout()[,-1],na.rm = TRUE)
    return(seq(z[1],z[2],length.out = nrow(formatTurnout())))
  })
  colpal = eventReactive(formatTurnout(),{
    z = range(formatTurnout()[,-1],na.rm = TRUE)
    return(colorNumeric(palette = "viridis",domain = z))
  })

  output$map = renderLeaflet({
    shp2 = shp()
    var = varnames()

    m = leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    for(i in 1:length(var)){
      vname = var[i]
      z.i = range(formatTurnout()[,vname],na.rm = TRUE)
      pal.i = colorNumeric(palette = "viridis",domain = z.i)
      
      m = m %>%
        addPolygons(data = shp2,
                    group = vname,
                    stroke = TRUE,
                    weight = 0.5,
                    opacity = 1,
                    color = "black",
                    smoothFactor = 0.2,
                    fillOpacity = 0.5,
                    fillColor = pal.i(shp2@data[,vname]),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup= paste0("<strong>Precinct ",shp2$precinct,"</strong>",
                                 "<br>",vname,": ",shp2@data[,vname]))
     }

    values = seq(0,100)
    pal = colorNumeric(palette = "viridis",domain = c(0,100))
    
    m = m %>%
      addLegend("topleft",colors = pal(seq(0,100,10)),
      # addLegend("topleft",pal = pal,values = values,
                title = "Legend",
                labels = c("Low",rep("",4),"Mid",rep("",4),"High"),
                opacity = 0.5) %>% 
      addLayersControl(
        baseGroups = var,
        options = layersControlOptions(collapsed = FALSE))
    m
  })
}

shinyApp(ui, server)
