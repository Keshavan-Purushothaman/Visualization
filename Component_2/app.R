library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(maps)
library(readxl)
library(ggpubr)
library(shinyWidgets)


## Reading the excel file
historical_emission=read_excel("historical_emissions.xlsx")
historical_emission=data.frame(historical_emission)

cc_data= read.csv("country_continent.csv")
cc_data= data.frame(cc_data)


## The sector Energy is a culmination of various other sectors, hence we remove those rows
historical_emission=historical_emission %>% filter(historical_emission$Sector!="Energy")

historical_emission$Country[historical_emission$Country=="United States"]="USA"
historical_emission$Country[historical_emission$Country=="United Kingdom"]="UK"


cc_data$region[cc_data$region=="United States"]="USA"
cc_data$region[cc_data$region=="United Kingdom"]="UK"

### the data for year 1990 and 1991 is in character type.. we need to convert into numeric
historical_emission$X1991= as.double(historical_emission$X1991)
historical_emission$X1990= as.double(historical_emission$X1990)

### To drop the NA values 
historical_emission=historical_emission %>% drop_na()

chloropeth= function(data,year,gas_name,color_code){
  data=data %>% filter(data$Country!="World")
  data=data[c("Country","Sector","Gas",paste0("X",year))]
  names(data)=c("region","Sector","Gas","emission")
  data=data %>% filter(data$Sector=="Total including LUCF")
  data=data[-c(2)]
  data=data %>% filter(data$Gas==gas_name)
  mapdata=map_data("world")
  mapdata=left_join(mapdata,data,by="region")
  ggplot(mapdata,aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=emission), color="black")+
    scale_fill_distiller(palette = color_code)+
    guides(colour = guide_coloursteps(show.limits = TRUE))+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks =element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size=15),
          legend.text = element_text(size=16),
          legend.key.height = unit(2.5,'cm'),
          legend.key.width = unit(1,'cm'),
          legend.title = element_text(size = 20))+
    xlab("")+ylab("")+ggtitle(paste0(gas_name," Emissions (MtCO2) ",year))
  
}

stacked_bar_c=function(data,year,sector_name,country_name){
  
  df=data.frame()
  for(i in 1:length(country_name)){
    df=rbind.data.frame(df,
                        data %>% filter(Country==country_name[i]) %>% filter(Sector==sector_name))
  }
  
  if(sector_name=="Total including LUCF"){
    title_sect=paste0("GHG Emissions ",year)
  }
  else{
    title_sect=paste0(sector_name," sector " ,"GHG Emissions ",year)
  }
  
  data=df
  data=data[c("Country","Sector","Gas",paste0("X",year))]
  names(data)=c("Country","Sector","Gas","emission")
  data=data[-c(2)]
  
  #data$Gas =factor(data$Gas, levels = data$Gas[order(desc(data$emission))])
  
  
  ggplot(data = data, aes(x=Country ,y = emission, fill=Gas)) +
    geom_bar(position="dodge", stat = "identity", color="black",width = 0.8) +
    coord_flip() +
    theme(legend.position="right",
          legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=15),
          title = element_text(size = 15),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 15))+
    scale_fill_manual(values = c("#e63946","#f8961e","#43aa8b","#9e0059","#bc6c25"))+
    labs(y = "Emission (MtCO2) \n", title = title_sect )
}



stacked_bar=function(data,year,sector_name,country_name){
  
  data=data %>% filter(data$Country==country_name)
  
  if(sector_name=="Total including LUCF"){
    title_sect=paste0("GHG Emissions ",year)
  }
  else{
    title_sect=paste0(sector_name," sector " ,"GHG Emissions ",year)
  }
  
  data=data[c("Sector","Gas",paste0("X",year))]
  names(data)=c("Sector","Gas","emission")
  data = data %>% filter(Sector==sector_name) %>% filter(Gas!="All GHG")
  data=data[-c(1)]
  
  label = paste0(' [',round(data$emission/ sum(data$emission) * 100, 1), "% ]   ")
  data$Gas= paste0(data$Gas,label)
  
  data$Gas =factor(data$Gas, levels = data$Gas[order(desc(data$emission))])
  
  ggplot(data = data, aes(x="",y = emission, fill=Gas)) +
  geom_bar(stat = "identity", width = 0.8, color="black") +
  coord_flip() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1,'cm'),
        legend.text = element_text(size=15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size=10),
        title = element_text(size = 15),)+
  scale_fill_manual(values = sample(c("#f8961e","#43aa8b","#9e0059","#e63946"),4 ))+
  labs(y = "Emission (MtCO2) \n", title = title_sect ) 
}


sideways_bar= function(data,year,gas_name){
  
  if(gas_name=="Total including LUCF"){
    title_sect=paste0("Top 5 Emittors of ", "overall GHG emissions-" ,year)
  }
  else{
    title_sect=paste0("Top 5 Emittors of ", gas_name," emissions-" ,year)
  }
  
  data=data %>% filter(data$Country!="World")
  data=data[c("Country","Sector","Gas",paste0("X",year))]
  names(data)=c("region","Sector","Gas","emission")
  data=data %>% filter(data$Sector=="Total including LUCF")
  data=data[-c(2)]
  data= data %>% filter(Gas==gas_name) %>% filter(rank(desc(emission))<=5)
  data$region =factor(data$region, levels = data$region[order(data$emission)])
  ggplot(data = data, aes(x = region, y = emission, fill=region)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    theme(legend.position="none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size=13),
          title = element_text(size = 15))+
    scale_fill_manual(values = sample(c("#ffbe0b","#fb5607","#ff006e","#1d3557","#3a86ff","#e63946","#a8dadc","#457b9d","#8338ec"),5))+
    labs(x = "\n Country ", y = "Emission (MtCO2) \n", title = title_sect ) 
}


top=function(data,year,gas_name){
  
  if(gas_name=="Total including LUCF"){
    title_sect=paste0("overall GHG emissions (",year,")")
  }
  else{
    title_sect=paste0(gas_name," emissions(",year,")")
  }
  
  data=data %>% filter(data$Country!="World")
  data=data[c("Country","Sector","Gas",paste0("X",year))]
  names(data)=c("region","Sector","Gas","emission")
  data=data %>% filter(data$Sector=="Total including LUCF")
  data=data[-c(2)]
  data_grouped=data %>% filter(Gas==gas_name) 
  
  top_5= (data_grouped %>% filter(rank(desc(emission))<=5))$region
  top_5=paste0(" The top 5 emittors of ",title_sect ," that is " ,
               paste(top_5,collapse = ", "), 
              " is removed from consideration ( as they are outliers )" )
  
  return( top_5 )
}

box_plot=function(data,year,gas_name){
  
  data=data %>% filter(data$Country!="World")
  data=data[c("Country","Sector","Gas",paste0("X",year))]
  names(data)=c("region","Sector","Gas","emission")
  data=data %>% filter(data$Sector=="Total including LUCF")
  data=data[-c(2)]
  data_grouped=data %>% filter(Gas==gas_name) 
  
  top_5= (data_grouped %>% filter(rank(desc(emission))<=5))$region
  data_grouped=data_grouped %>% filter(region!=top_5)
  
  data_grouped=left_join(data_grouped,cc_data,by="region")

  col=sample(c("#ff0054","#ff5400","#ffbd00","#ecbcfd","#9e0059","#219ebc"),6)
  
  
  b1=ggplot(data_grouped %>% filter(Continent=="Asia"),aes(x="",y=emission))+geom_boxplot(fill=col[1])+xlab("Asia")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  b2=ggplot(data_grouped %>% filter(Continent=="Europe"),aes(x="",y=emission))+geom_boxplot(fill=col[2])+xlab("Europe")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  b3=ggplot(data_grouped %>% filter(Continent=="Africa"),aes(x="",y=emission))+geom_boxplot(fill=col[3])+xlab("Africa")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  b4=ggplot(data_grouped %>% filter(Continent=="North America"),aes(x="",y=emission))+geom_boxplot(fill=col[4])+xlab("North America")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  b5=ggplot(data_grouped %>% filter(Continent=="South America"),aes(x="",y=emission))+geom_boxplot(fill=col[5])+xlab("South America")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  b6=ggplot(data_grouped %>% filter(Continent=="Oceania"),aes(x="",y=emission))+geom_boxplot(fill=col[6])+xlab("Oceania")+
    stat_summary(fun=mean, geom="point", size=2, color="black",shape=15)+ylab("Emissions (MtCO2e units)")+stat_boxplot(geom = "errorbar")+
    theme(axis.title.x = element_text(size = 18))
  ggarrange(b1,b2,b3,b4,b5,b6,nrow = 2,ncol=3)

}

piechart= function(data,year,gas_name){
  
  data=data %>% filter(data$Country=="World")
  data=data[c("Sector","Gas",paste0("X",year))]
  names(data)=c("Sector","Gas","emission")
  data = data %>% filter(Sector!="Total including LUCF") %>% filter(Sector!="Total excluding LUCF") %>% filter(Gas==gas_name) %>% filter(Sector!="Land-Use Change and Forestry")
  
  label = paste0(' [',round(data$emission/ sum(data$emission) * 100, 1), "% ]")
  data$Sector= paste0(data$Sector,label)
  
  ggplot(data, aes(x=2, y=emission, fill=Sector)) +
    geom_bar(stat="identity", width=1, color="black")+
    xlim(0.5,2.5)+
    coord_polar("y",0)+
    theme(axis.ticks = element_blank(),
          axis.line = element_blank(),
          rect = element_blank(),
          axis.text = element_blank(),
          axis.title= element_blank(),
          title = element_text(size = 20),
          legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20))+
    scale_fill_manual(values = c("#ffbe0b","#005f73","#fb5607","#ff006e","#0a9396","#8338ec","#3a86ff","#3a0ca3","#723d46","#55a630"))+
    ggtitle(paste0("Sector wise emissions- ", year))
}

his_gas_trend=function(data,gas_name){
  world_emission= data %>% filter(Country=="World")
  world_trend= world_emission %>% filter(Gas==gas_name) %>% filter(Sector=="Total including LUCF")
  world_trend=world_trend[-c(2,3,4,5)]
  world_trend=pivot_longer(world_trend,!Country,names_to ="Year",values_to ="emission")
  world_trend$Year=seq(2019,1990)
  
  col=sample(c("#ff0054","#ff5400","#ffbd00","#ecbcfd","#9e0059","#219ebc"),1)
  
  ggplot(world_trend,aes(x=Year,y=emission))+ 
    geom_line(color=col[1],lwd=1)+
    geom_point(size=1.5,shape=15)+
    theme_gray()+
    scale_x_continuous( breaks=c(seq(1990,2019,5),2019) )+
    ylab("Emission (MtCO2e units)")+
    ggtitle(  paste0("Trend in ",gas_name," emissions over the past 3 decades") )+
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 15),
      title = element_text(size = 17 )
    )

}


bar_chart= function(data,country_name,year,gas_name){
  
  data=data %>% filter(data$Country!="World")
  data = data %>% filter(Sector!="Total including LUCF") %>% filter(Sector!="Total excluding LUCF") %>% filter(Gas==gas_name)%>% filter(Country==country_name)
  year_label=paste0("X",year)
  data=data[c("Sector",year_label)]
  names(data)=c("Sector","emission")
  
  data$Sector =factor(data$Sector, levels = data$Sector[order(desc(data$emission))])
  
  ggplot(data, aes(x=Sector, y=emission, fill=Sector)) +
    geom_bar(stat="identity", width=1, color="black")+
    theme(axis.text.x = element_blank(),
          legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size = 20),
          title = element_text(size = 17)
          )+
    scale_fill_manual(values = sample(c("#ffbe0b","#005f73","#fb5607","#ff006e","#0a9396","#deaaff","#8338ec","#3a86ff","#3a0ca3","#723d46","#55a630"),size = 11)  )+
    ylab("Emission (MtCO2e units)")+
    ggtitle(paste0("Sector wise ", gas_name,  " emissions-",year))
}

sector_trend_f=function(data,country_name,gas_name,sectors){
  
  data=data %>% filter(data$Country!="World")
  
  df=data.frame()
  for(i in 1:length(sectors)){
    df=rbind.data.frame(df,data %>% filter(Country==country_name) %>% filter(Sector==sectors[i]) %>% filter(Gas==gas_name))
  }
  data=df
  #data=data %>% filter(Country==country_name) %>% filter(Sector==sectors) %>% filter(Gas==gas_name)
  data=data[-c(1,2,4,5)]
  names(data)=c("Sector",seq(2019,1990))
  data=pivot_longer(data,!Sector,names_to = "Year",values_to = "emission")
  ggplot(data,aes(x=Year,y=emission,group=Sector,col=Sector))+ 
    geom_line(size=1.25)+
    geom_point(size=1.25,shape=15,col="black")+
    theme_gray()+
    theme(legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          axis.title = element_text(size=15),
          title = element_text(size = 15))+
    scale_x_discrete(breaks=c(seq(1990,2019,5),2019))+
    ylab("Emission (MtCO2e units)")+
    ggtitle("Sector Wise Trend")
  
}


country_trend_g=function(data,country_name,gas_name,sector){
  
  data=data %>% filter(data$Country!="World")
  
  df=data.frame()
  for(i in 1:length(gas_name)){
    df=rbind.data.frame(df,data %>% filter(Country==country_name) %>% filter(Gas==gas_name[i]) %>% filter(Sector==sector))
  }
  data=df
  #data=data %>% filter(Country==country_name) %>% filter(Sector==sectors) %>% filter(Gas==gas_name)
  data=data[-c(1,2,3,5)]
  names(data)=c("Gas",seq(2019,1990))
  data=pivot_longer(data,!Gas,names_to = "Year",values_to = "emission")
  ggplot(data,aes(x=Year,y=emission,group=Gas,col=Gas))+ 
    geom_line(size=1.25)+
    geom_point(size=1.25,shape=15,col="black")+
    theme_gray()+
    theme(legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          axis.title = element_text(size=15),
          title = element_text(size = 15))+
    scale_x_discrete(breaks=c(seq(1990,2019,5),2019))+
    ylab("Emission (MtCO2e units)")+
    ggtitle("Various GHG Trend")
  
}

sector_trend_c=function(data,country_name,gas_name,sectors){
  
  data=data %>% filter(data$Country!="World")
  
  df=data.frame()
  for(i in 1:length(country_name)){
    df=rbind.data.frame(df,data %>% filter(Country==country_name[i]) %>% filter(Sector==sectors) %>% filter(Gas==gas_name))
  }
  data=df
  #data=data %>% filter(Country==country_name) %>% filter(Sector==sectors) %>% filter(Gas==gas_name)
  data=data[-c(2,3,4,5)]
  names(data)=c("country",seq(2019,1990))
  data=pivot_longer(data,!country,names_to = "Year",values_to = "emission")
  ggplot(data,aes(x=Year,y=emission,group=country,col=country))+ 
    geom_line(size=1.25)+
    geom_point(size=1.25,shape=15,col="black")+
    theme_gray()+
    theme(legend.key.size = unit(1,'cm'),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          axis.title = element_text(size=15),
          title = element_text(size = 15))+
    scale_x_discrete(breaks=c(seq(1990,2019,5),2019))+
    ylab("Emission (MtCO2e units)")+
    ggtitle("Historic Trend")
  
}




country_names= setdiff(  sort(unique(as.character(historical_emission$Country) )), c("World") )



para1 <- h4(p(" Climate change is one of the most pressing issues 
             that is concerning humanity today and its seriousness 
             is growing by the day. Climate change refers to the change
             in earth's climatic conditions and patterns. This process 
             is steady and natural, but anthropogenic activities have 
             made it uneven and, in many cases, accelerated. 
             Global warming is one such process.Global warming is 
             often associated with a runaway greenhouse effect. 
             The greenhouse effect describes the process of certain 
             gases (including carbon dioxide (CO2), methane, nitrous 
             oxide (N2O), fluorinated gases, and ozone) trapping solar 
             radiation in a planet's lower atmosphere. Greenhouse gases
             let the sun's light shine onto Earth's surface, but they
             trap the heat that reflects back up into the atmosphere.
             In this way, they act like the glass walls of a greenhouse. 
             The greenhouse effect is a natural phenomenon and
             keeps Earth warm enough to sustain life. However, 
             human activities that include burning fossil fuels and 
             cutting down forests release greenhouse gases into the  
             atmosphere at an unprecedented rate.Increasing 
             temperatures can change the climate impacts and even the
             classification of a region.",align="justify",style="line-height: 27px;"))
para2 <- h4(p("Recognizing the urgency of the situation, world leaders convened 
           in Rio de Janeiro in 1992 (Earth Summit) and agreed to work together
           to combat climate change. An important achievement of the summit was 
           an agreement on the Climate Change Convention which in turn led to 
           the Kyoto Protocol and the famous Paris Agreement(2015). 
           A tremendous amount of research is being done on the causes and
           the effects of climate change. Enormous amount of data is collected 
           from various sources such as government agencies, NGO etc. 
           to understand these situations better. These problems are now 
           being studied and simulated using advanced scientific methods 
           such as machine learning and artificial intelligence 
           (Earth Simulator supercomputer)",align="justify",style="line-height: 27px;"))

para3 <- h4(p("Climate Watch is an online platform designed to empower policymakers, researchers,
              media and other stakeholders with the open climate data, visualizations and 
              resources they need to gather insights on national and global progress on climate change.
              Climate Watch brings together dozens of datasets for the first time to let users analyze 
              and compare the Nationally Determined Contributions (NDCs) under the Paris Agreement, 
              access historical emissions data, discover how countries can leverage their climate goals
              to achieve their sustainable development objectives and use models to map new pathways to
              a lower carbon, prosperous future.(Climate Watch Historical GHG Emissions. 2022. Washington,
              DC: World Resources Institute."),align="justify",style="line-height: 27px;")
para4 <- h4(p(" The data being used is the \"Climate Watch Historical Emissions data\" which contains
              sector-level greenhouse gas (GHG) emissions data for 194 countries and the European
              Union for the period 1990-2019, including emissions of the six major GHGs
              from most major sources and sinks. Non-CO2 emissions are expressed in 
              CO2 equivalents using 100-year global warming potential values from the IPCC Fourth 
              Assessment Report. I will be using Historical emission data from Climate Analysis
              Indicators Tool (CAIT) data source. The following are the columns of the data ")
            ,align="justify",style="line-height: 27px;")
data_desciption <- 
  h4(HTML("
    <ol>
      <li>Country : 194 country names (This includes the country=\"world\")</li>
      <li>Data Source: Climate Analysis Indicators Tool (CAIT) (Same for all the rows)</li>
      <li>Sector: The following sectors are included:-
        <ul type=\"disc\">
          <li>Energy: This sector includes
            <ul>
              <li>Electricity/Heat</li>
              <li>Manufacturing/Construction</li>
              <li>Transportation</li>
              <li>Fugitive Emissions ( leaks and other irregular releases of gases )</li>
              <li>Other Fuel Combustion</li>
            </ul></li>
          <li>Waste(emission due to decomposition of waste)</li>
          <li>Agriculture</li>
          <li>Bunker Fuels (Fuel used in ships)</li>
          <li>Industrial Processes</li>
          <li>Land-use Change and Forestry (LUCF)</li>
          <li>Building</li></ul>
      LUCF can act as source and a sink for GHG gases. Hence unlike other sectors LUCF can take both positive and negative values.Based on this, there are two totals that can be calculated i.e Total including LUCF and Total excluding LUCF (Both of these are included in the Sector Column).Column Sector is considered to be Categorical variable.</li> 
      <li> Gas:The following Green House Gases(GHG) are considered:-
        <ul type=\"disc\">
          <li>All Green House Gases (Overall emission - Value denotes the cummulative of the below 4 gases) </li>
          <li>Carbon-dioxide (CO<sub>2</sub>)</li>
          <li>Nitrous-oxide (N<sub>2</sub>O)</li>
          <li>Methane (CH<sub>4</sub>)</li>
          <li>(fluorinated) F-Gas</li>
        </ul>
      This is a categorical variable </li>
      <li>Units: MtCO<sub>2</sub>e (Million tons CO<sub>2</sub> equivalent)(same for all the rows)</li>
      <li>Emission data (1990-2019): (Next 30 Columns)(Positive and Negative numerical variable)</li>
    </ol>
  "),style="line-height: 27px;")


## Defining UI

ui <- fluidPage(
  
  title = "Green House Gas Emissions",
  titlePanel(
    fluidRow(
      div("Green House Gas Emission",style=" color:#f94144; font-size:40px", 
      img(height = 50, width = 100, src = "factory.jpg")),
      align="center"
    )
  ),
  
  navbarPage("",theme = shinytheme("united"),
             
             tabPanel(h4("Introduction"),
                      fluidPage(
                        mainPanel( div("Introduction", style=" color:#f94144; font-size:35px"),
                                   tags$hr( style=" border: 1px solid red; width:40%;  " ),
                                   para1,
                                   img(src="global_warming.png"),
                                   para2,
                                   width=12,
                                   align="center"
                        )
                      )
             ),
             
             
             tabPanel(h4("Dataset Description"),
                      
                      mainPanel(div(" Dataset Description ", align="center",style=" color:#f94144; font-size:35px; "),
                                tags$hr( style=" border: 1px solid red; width:40%; align:center; " ),
                                width = 12,
                                para3,
                                tags$a(HTML( '<center><img src="Climate_watch.jpg ",height= 200, width=400> </center> ') ,
                                       href="https://www.climatewatchdata.org/data-explorer/historical-emissions?historical-emissions-data-sources=cait&historical-emissions-gases=all-ghg&historical-emissions-regions=All%20Selected&historical-emissions-sectors=total-including-lucf%2Ctotal-including-lucf&page=1" ),
                                para4,
                                data_desciption,
                                br(),br(),br(),br()
                                
                      )
                      ),
             
             
             tabPanel(h4("World Emissions"),
                      
                      fluidPage(
                        
                        mainPanel(div(" World Emissions", align="center",style=" color:#f94144; font-size:35px; "),
                                  tags$hr( style=" border: 1px solid red; width:40%; align:center; " ),
                                  width = 12,
                                  
                                  div("The following plots can be used to analyse the world GHG emissions over the years(1990-2019)."
                                      , align="center",style=" font-size:17px; "),
                                  
                                  br(),
                                  selectInput("world_year", "Choose an year:",
                                                         choices= seq(2019,1990)),
                                  br(),
                                  
                                  radioButtons("Gas","Choose a gas",
                                               inline = TRUE,
                                               c("All Green House Gases " = "All GHG",
                                                 "Carbon-dioxide" = "CO2",
                                                 "Nitrous Oxide" = "N2O",
                                                 "Methane"="CH4",
                                                 "Fluorine based Gases"="F-Gas")
                                  ),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  
                                  align="center",
                                  
                                  fluidPage(
                                    
                                    navlistPanel(
                                      "Plots",
                                      
                                      tabPanel( "Total GHG Emissions ",
                                                mainPanel(plotOutput("gas_profile"),
                                                          width = 12)
                                      ),
                                      
                                      tabPanel("Chloropleth ",
                                               mainPanel(plotOutput("chloro"),
                                                         width = 12)
                                      ),
                                      tabPanel("Top 5 Emittors ",
                                               mainPanel(plotOutput("sidebar"),
                                                         width = 12)
                                      ),
                                      tabPanel("Emissions Continent Wise ",
                                               mainPanel(textOutput("top_5"),
                                                         br(),
                                                         br(),
                                                         plotOutput("bxplt"),
                                                         width = 12)
                                      ),
                                      tabPanel("Sector Wise emissions ",
                                               mainPanel(plotOutput("dntchart"),
                                                         width = 12)
                                      ),
                                      tabPanel("Historic Trend Line",
                                               mainPanel(plotOutput("gas_trend"),
                                                         width = 12)
                                      ),
                          
                                      widths = c(3,9)
                                    )
                                    
                                  )
                                  
                                  )
                      )
                      ),
             
             tabPanel(h4("Country Emissions"),
                      
                      fluidPage(
                        
                        mainPanel(div(" Country Wise Emissions", align="center",style=" color:#f94144; font-size:35px; "),
                                  tags$hr( style=" border: 1px solid red; width:40%; align:center; " ),
                                  width = 12,
                                  align="center",
                                  
                                  selectInput("country", "Choose a Country:",
                                              selected = "Brazil",
                                              choices= sort(unique(as.character(historical_emission$Country)) )
                                  ),
                                  br(),
                                  
                                  div("GHG profiling across sectors", align="center", style=" color:#f94144; font-size:20px; "),
                                  tags$hr(align="center", style=" border: 1px solid red; width:20%; " ),
                                  
                                  selectInput("country_year", "Choose an year:",
                                              choices= seq(2019,1990)),
                                  
                                  tabsetPanel(
                                    tabPanel("Overall GHG Emissions",
                                             br(),
                                             br(),
                                             br(),
                                             plotOutput("country_gas_dist"),
                                             width =12
                                             ),
                                    tabPanel("GHG Emissions Across Sectors",
                                             width=12,
                                             br(),
                                             br(),
                                             
                                             radioButtons("sector_name","Choose a sector",
                                                          inline = TRUE,
                                                          choices = setdiff( unique(as.character(historical_emission$Sector)),
                                                                             c("Total including LUCF","Total excluding LUCF","Land-Use Change and Forestry"))
                                                          
                                             ),
                                             br(),
                                             br(),
                                             
                                             plotOutput("country_gas_sect_dist")
                                             
                                             ),
                                    tabPanel("Historic Trend",
                                             width=12,
                                             br(),
                                             br(),
                                             
                                             checkboxGroupInput("country_gas_dist","Choose gas",
                                                          inline = TRUE,
                                                          selected = "All GHG",
                                                          c("All Green House Gases " = "All GHG",
                                                            "Carbon-dioxide" = "CO2",
                                                            "Nitrous Oxide" = "N2O",
                                                            "Methane"="CH4",
                                                            "Fluorine based Gases"="F-Gas")
                                             ),
                                            
                                             br(),
                                             
                                             plotOutput("hist_country_gas_trnd")
                                            
                                             
                                             
                                             )
                                  ),
                                  
                                  br(),
                                  br(),
                                  
                                  
                                  
                                  div(" Sector Wise Profile", align="center", style=" color:#f94144; font-size:20px; "),
                                  tags$hr(align="center", style=" border: 1px solid red; width:20%; " ),
                                  
                                  
                                  radioButtons("country_gas","Choose a gas",
                                               inline = TRUE,
                                               c("All Green House Gases " = "All GHG",
                                                 "Carbon-dioxide" = "CO2",
                                                 "Nitrous Oxide" = "N2O",
                                                 "Methane"="CH4",
                                                 "Fluorine based Gases"="F-Gas")
                                               ),
                                  br(),
                                  br(),
                                  
                                  tabsetPanel(
                                    tabPanel(" Year wise emission of gases across sectors ",
                                            
                                             br(),
                                             br(),
                                             
                                             selectInput("year", "Choose an year:",
                                                         choices= seq(2019,1990)),
                                                         
                                             plotOutput("year_chart")
                                             ),
                                    tabPanel(" Historic trend line ",
                                             
                                             br(),
                                             br(),
                                             checkboxGroupInput("sector","Choose a Sector",
                                                                inline = TRUE,
                                                                selected = "Electricity/Heat",
                                                                choices = setdiff( unique(as.character(historical_emission$Sector)),
                                                                                   c("Total including LUCF","Total excluding LUCF"))
                                                                ),
                                             
                                             plotOutput("sector_trend")
                                             )
                                                      
                                  )
                                  )
                      )
                      ),
             tabPanel(h4("Comparisons"),
                      
                      fluidPage(

                        div(" Comparisons Between Countries", align="center",style=" color:#f94144; font-size:35px; "),
                        tags$hr( style=" border: 1px solid red; width:40%; align:center; " ),
                        width = 12,
                        align="center",
                        
                        fluidRow(
                        
                        column(2,offset=3, selectInput("country1", "Country 1 :",
                                                       selected = "India",
                                                       choices= c("Select a country",country_names )          
                              )),
                        
                        column(2,selectInput("country2", "Country 2 :",
                                             selected = "China",
                                             choices= c("Select a country",country_names )
                        )),
                        
                        column(2,selectInput("country3", "Country 3 :",
                                             selected = "USA",
                                             choices= c("Select a country",country_names )
                        ))
                        
                        ),
                        
                        div("GHG Profiling across Sectors", align="center", style=" color:#f94144; font-size:20px; "),
                        tags$hr(align="center", style=" border: 1px solid red; width:20%; " ),
                        
                        selectInput("country_comp_year", "Choose an year:",
                                    choices= seq(2019,1990)),
                        
                        br(),
                        div("Overall GHG  Emissions", align="center", style=" color:#f94144; font-size:15px; "),
                        br(),
                        
                        textOutput("error_comp"),
                        plotOutput("country_c_gas_dist"),
                        
                        br(),br(),br(),br(),br(),br(),

                        # div("GHG Emission across Sectors", align="center", style=" color:#f94144; font-size:15px; "),
                        br(),     
                        radioButtons("sector_comp_name","Choose a sector",
                                      inline = TRUE,
                                      selected = "Agriculture",
                                      choices = setdiff( unique(as.character(historical_emission$Sector)),
                                                         c("Total including LUCF","Total excluding LUCF"))
                                                
                                    ),
                        br(),
                        br(),
                        plotOutput("country_c_sect_gas_dist"),
                        br(),br(),br(),br(),br(),br(),br(),
                        br(),
                      
                        div("Historic Trend", align="center", style=" color:#f94144; font-size:20px; "),
                        tags$hr(align="center", style=" border: 1px solid red; width:20%; " ),
                        br(),
                        
                        fluidRow(
                          
                          column(2,offset=4, selectInput("gas_comp", "Choose a gas :",
                                                         selected = "All GHG",
                                                         choices= c("All Green House Gases " = "All GHG",
                                                                    "Carbon-dioxide" = "CO2",
                                                                    "Nitrous Oxide" = "N2O",
                                                                    "Methane"="CH4",
                                                                    "Fluorine based Gases"="F-Gas")         
                          )),
                          
                          column(2,selectInput("sector_comp", "Choose a sector:",
                                               selected = "Total including LUCF",
                                               choices= c("Total including LUCF",
                                                             (setdiff( unique(as.character(historical_emission$Sector)),
                                                                                    c("Total including LUCF","Total excluding LUCF"))))
                          ))
                        ),
                        
                        br(),br(),
                        
                        
                        plotOutput("hist_c_trnd")
                        
                                   
                      )
             )
                      
                      
                      
                      
             
  )
)

server <- function(input,output,session){
  
  
  observe({
    
    possible_sect_1=setdiff( unique(as.character( (historical_emission %>% filter(Gas==input$gas_comp))$Sector)),
                           c("Total including LUCF","Total excluding LUCF"))
    updateSelectInput(session,"sector_comp",
                      selected = "Total including LUCF",
                      choices= c("Total including LUCF",possible_sect_1)
                                    
                      )
    
    possible_sect_2=setdiff( unique(as.character( (historical_emission %>% filter(Gas==input$country_gas) %>% filter(Country==input$country) )$Sector)),
                             c("Total including LUCF","Total excluding LUCF"))
    updateCheckboxGroupInput(session,"sector",
                             inline = TRUE,
                             selected = possible_sect_2[1],
                             choices= c(possible_sect_2)
                      
    )
    
    possible_sect_3=setdiff( unique(as.character( (historical_emission %>% filter(Country==input$country))$Sector)),
                             c("Total including LUCF","Total excluding LUCF"))
    updateRadioButtons(session,"sector_name",
                       inline = TRUE,
                       selected = possible_sect_3[1],
                       choices = c(possible_sect_3)
                       )
    
    x=input$country1
    y=input$country2
    z=input$country3
    
    
    if(x!="Select a country" ){
    updateSelectInput(session,"country2",
                      selected = y,
                      choices = c("Select a country",setdiff( country_names ,c(x,z))))
    
    updateSelectInput(session,"country3",
                      selected = z,
                      choices = c("Select a country",setdiff( country_names ,c(x,y))))
      
    }
    
    if(y!="Select a country"){
      updateSelectInput(session,"country1",
                        selected = x,
                        choices = c("Select a country",setdiff( country_names ,c(y,z))))
      
      updateSelectInput(session,"country3",
                        selected = z,
                        choices = c("Select a country",setdiff( country_names ,c(x,y))))
      
    }
    
    if(z!="Select a country"){
      updateSelectInput(session,"country1",
                        selected = x,
                        choices = c("Select a country",setdiff( country_names ,c(y,z))))
      
      updateSelectInput(session,"country2",
                        selected = y,
                        choices = c("Select a country",setdiff( country_names ,c(x,z))))
      
    }
  })
  
  output$chloro = renderPlot( chloropeth(historical_emission,input$world_year,input$Gas,"Spectral"), width = 900 , height = 500 )
  
  output$top_5 = renderText(top(historical_emission,input$world_year,input$Gas))
  
  output$sidebar= renderPlot(sideways_bar(historical_emission,input$world_year,input$Gas),height = 400,width = 1000)
  
  
  output$gas_profile= renderPlot( stacked_bar(historical_emission,input$world_year,"Total including LUCF","World"),height = 200,width = 1000) 
  
  output$bxplt=renderPlot(box_plot(historical_emission,input$world_year,input$Gas),height = 1000 , width = 900)
  
  output$dntchart=renderPlot(piechart(historical_emission,input$world_year,input$Gas), height = 500, width = 800)
  
  output$gas_trend=renderPlot(his_gas_trend(historical_emission, input$Gas),height = 500, width = 1000)
  
  output$year_chart=renderPlot(bar_chart(historical_emission,input$country,input$year,input$country_gas), width = 1300 , height = 500)
  
  
  output$sector_trend= renderPlot({
    validate(
      need( (input$sector!=""),"Select a sector" )
    )
    sector_trend_f(historical_emission,input$country,input$country_gas,input$sector)
    },width = 1300 , height = 500)

  output$country_gas_dist= renderPlot( stacked_bar(historical_emission,input$country_year,"Total including LUCF",input$country),height = 200 )
  output$country_gas_sect_dist= renderPlot( stacked_bar(historical_emission,input$country_year,input$sector_name,input$country),height = 200)
  
  
  output$hist_country_gas_trnd = renderPlot({
    validate(
      need( (input$country_gas_dist!=""),"Select a Gas" )
    )
    country_trend_g(historical_emission,input$country, input$country_gas_dist, "Total including LUCF" )
    },width = 1100 , height = 400 )
  
  output$country_c_gas_dist = renderPlot({ 
    validate(
      need( (input$country1!="Select a country" || input$country2!="Select a country" || input$country3!="Select a country"),"Select a country" )
    )
    stacked_bar_c(historical_emission,input$country_comp_year,"Total including LUCF",c( input$country1,input$country2,input$country3 ) )
    },width = 1000 , height = 500 )
  
  
  
  output$country_c_sect_gas_dist= renderPlot( {
    validate(
      need( (input$country1!="Select a country" || input$country2!="Select a country" || input$country3!="Select a country"),"Select a country" )
    )
    stacked_bar_c(historical_emission,input$country_comp_year, input$sector_comp_name ,c( input$country1,input$country2,input$country3 ) )
    },width = 1000 , height = 500 )
  
  
  
  output$hist_c_trnd= renderPlot({
    validate(
      need( (input$country1!="Select a country" || input$country2!="Select a country" || input$country3!="Select a country"),"Select a country" )
    )
    sector_trend_c(historical_emission,c( input$country1,input$country2,input$country3 ),input$gas_comp,input$sector_comp)
    },width = 1300 , height = 500 )
  
}
shinyApp(ui=ui,server=server)