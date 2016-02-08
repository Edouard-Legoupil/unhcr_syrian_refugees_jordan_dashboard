library(jsonlite)
library(dplyr)
library(dygraphs)
library(ggvis)
library(googleVis)
library(choroplethrAdmin1)
library(choroplethr)
library(curl)

#######################################Time Series Plot######################################
source("helper.R")
JOR <- fromJSON("http://data.unhcr.org/api/population/search.json?name=JOR&level=country&instance_id=syria")
demo_total <- demo_JOR(JOR)
who_total <- who_JOR(JOR)
doc <- doc_JOR(JOR)
governorate <- JOR_gov(JOR)
gov_demos_all<-gov_demos(governorate)
gov_who_all <- gov_who(governorate)
jor<-plot_map_jor(gov_demos_all)

####################################################################################


shinyServer(function(input, output) {
  
#######################Jordan map####################
  jor%>%
    ggvis(~long, ~lat) %>%
    layer_paths(data = jor %>% group_by(name), 
                strokeWidth := 0, fill = ~total) %>%
    hide_axis("x") %>% hide_axis("y") %>%
    add_tooltip(function(data){paste("Gov: ", data$name,
                                     "<br>", "Total: ",
    as.character(formatC(data$total,format="d", big.mark=',')))}, "hover")%>%
    bind_shiny("all_of_jordan_map")
  
#####################time series#########################

output$time_series <- renderDygraph({
  plot_dygraph(ref_time)
})

#####################demographics#######################

output$demo_jordan <- renderGvis({
  plot_bar(demo_total)         
})


#####################docs###############################

output$docs_jor <- table(doc)

#########################demo gov###########################


output$demo_per_gov <- renderGvis({
  
  gov_d<-input$gov
  selected_demo <-select_demo_gov(gov_demos_all,gov_d)
  plot_bar(selected_demo)         
})


#########################who_what###########################


output$who_what <-  renderDataTable({
  gov<-input$gov
  selected_table<-select_who_gov(gov_who_all,gov)
  
}, escape = FALSE, options = list(pageLength = 10))
  

output$gov_name_selected <- renderText({
  gov_name(input$gov)})


output$gov_name_selected_2 <- renderText({
  gov_name(input$gov)})

output$date_time_srs <- renderText(date_pop_JO)

output$date_demo_total <- renderText(date_demo_JO)

output$update_gov <- renderText({demo_gov_time(updates,input$gov)})

})


