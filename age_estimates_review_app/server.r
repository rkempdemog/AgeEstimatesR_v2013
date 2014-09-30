library(shiny)
library(dplyr)
library(ggvis)
library(reshape2)
library(tidyr)
library(car)
age5v=read.csv("age5.csv")
#delete if it doesn't work
age5v=age5v%>%
  select(age5:Cnetmig2011)%>%
  melt(id.vars=c("age5","county","name"))%>%
  separate(variable, into=c("stub", "yr"), sep=-3)%>%
  mutate(ageaxis=ordered(age5,
                         levels=1:18,
                         labels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79","80 to 84", "85 and Over" )),
         Source=recode(stub, "'tp'='Estimates';'Cindy20'='Projections';'deaths20'='Estimates';'Cdeaths20'='Projections';'netmig'='Estimates';'Cnetmig20'='Projections'"),
         Variable=recode(stub, "'tp'='Population';'Cindy20'='Population';'deaths20'='Deaths';'Cdeaths20'='Deaths';'netmig'='Net Migration';'Cnetmig20'='Net Migration'"),
         Year=paste("20", yr, sep=""))

shinyServer(function(input,output, session){

  age5=reactive({age5v%>%
    filter(county==input$county, Year==input$year)
  })

  age5%>%
    ggvis(x=~ageaxis, y=~value, stroke=~Source)%>%
    filter(Variable=="Population")%>%
    layer_lines()%>%
    add_axis("x", title="Age Group", title_offset=70, properties=axis_props(labels=list(angle=45, dx=20)))%>%
    add_axis("y", title="Population", title_offset=60)%>%
     bind_shiny("ggvis", "ggvis_ui")
  
  age5%>%
    ggvis(x=~ageaxis, y=~value, stroke=~Source)%>%
    filter(Variable=="Net Migration")%>%
    layer_lines()%>%
    add_axis("x", title="Age Group", title_offset=70, properties=axis_props(labels=list(angle=45, dx=20)))%>%
    add_axis("y", title="Migrants", title_offset=60)%>%
    bind_shiny("netmig", "ggvis_ui") 
  
  age5%>%
    ggvis(x=~ageaxis, y=~value, stroke=~Source)%>%
    filter(Variable=="Deaths")%>%
    layer_lines()%>%
    add_axis("x", title="Age Group", title_offset=70, properties=axis_props(labels=list(angle=45, dx=20)))%>%
    add_axis("y", title="Deaths", title_offset=60)%>%
    bind_shiny("deaths", "ggvis_ui")
  
})