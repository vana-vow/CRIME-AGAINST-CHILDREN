#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dash)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(readr)
library(tidyverse)
library(treemap)
library(sunburstR)
library(tibble)
library(treemapify)

data <- read.csv("fcrime.csv")




con <- select(data,STATE.UT,YEAR,TOTAL.CRIMES.AGAINST.CHILDREN,ABETMENT.OF.SUICIDE,BUYING.OF.GIRLS.FOR.PROSTITUTION,EXPOSURE.AND.ABANDONMENT,FOETICIDE,INFANTICIDE,KIDNAPPING.and.ABDUCTION.OF.CHILDREN,MURDER.OF.CHILDREN,OTHER.CRIMES.AGAINST.CHILDREN,PROCURATION.OF.MINOR.GILRS,PROHIBITION.OF.CHILD.MARRIAGE.ACT,RAPE.OF.CHILDREN,SELLING.OF.GIRLS.FOR.PROSTITUTION)
years_total_df <- con[,2:15] %>% group_by(YEAR) %>% summarize_all(funs(sum))

colnames(years_total_df) <- c("Year","Suicide","Buying", "Abandonment", "Foeticide","Infanticide", "KidnapAbd","Murder","Procuration","Childmrg","Rape","Selling")

years_total_df$Crime <- rowSums(years_total_df[ , c(2:14)],na.rm=TRUE)


yearly_crime_df <- data.frame(Year=integer(), Crime=integer(), Number=integer())
for (row in 1:nrow(years_total_df)){
    year <- years_total_df[row, "Year"]
    Suicide <- years_total_df[row, "Suicide"]
    Buying <- years_total_df[row, "Buying"]
    Abandonment <- years_total_df[row, "Abandonment"]
    Foeticide <- years_total_df[row, "Foeticide"]
    Infanticide <- years_total_df[row, "Infanticide"]
    KidnapAbd <- years_total_df[row, "KidnapAbd"]
    Murder <- years_total_df[row, "Murder"]
    Procuration <- years_total_df[row, "Procuration"]
    Childmrg <- years_total_df[row, "Childmrg"]
    Rape <- years_total_df[row, "Rape"]
    Selling <- years_total_df[row, "Selling"]
    
    
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Suicide", Suicide)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Buying", Buying)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Abandonment", Abandonment)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Foeticide", Foeticide)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Infanticide", Infanticide)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "KidnapAbd", KidnapAbd)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Murder", Murder)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Procuration", Procuration)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Childmrg", Childmrg)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Rape", Rape)
    yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Selling", Selling)
    
}

f1 <- data %>%
    plot_ly(
        x = ~ABETMENT.OF.SUICIDE, 
        y = ~STATE.UT, 
        size = ~ABETMENT.OF.SUICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>ABETMENT OF SUICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f2 <- data %>%
    plot_ly(
        x = ~BUYING.OF.GIRLS.FOR.PROSTITUTION, 
        y = ~STATE.UT, 
        size = ~BUYING.OF.GIRLS.FOR.PROSTITUTION, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>BUYING OF GIRLS FOR PROSTITUTION: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )



f3 <- data %>%
    plot_ly(
        x = ~EXPOSURE.AND.ABANDONMENT, 
        y = ~STATE.UT, 
        size = ~EXPOSURE.AND.ABANDONMENT, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>EXPOSURE AND ABANDONMENT: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )



f4 <- data %>%
    plot_ly(
        x = ~FOETICIDE, 
        y = ~STATE.UT, 
        size = ~FOETICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>FOETICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f5 <- data %>%
    plot_ly(
        x = ~INFANTICIDE, 
        y = ~STATE.UT, 
        size = ~INFANTICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>INFANTICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f6 <- data %>%
    plot_ly(
        x = ~KIDNAPPING.and.ABDUCTION.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~KIDNAPPING.and.ABDUCTION.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>KIDNAPPING and ABDUCTION OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f7 <- data %>%
    plot_ly(
        x = ~MURDER.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~MURDER.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>MURDER OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f8 <- data %>%
    plot_ly(
        x = ~OTHER.CRIMES.AGAINST.CHILDREN, 
        y = ~STATE.UT, 
        size = ~OTHER.CRIMES.AGAINST.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>OTHER CRIMES AGAINST CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f9 <- data %>%
    plot_ly(
        x = ~PROCURATION.OF.MINOR.GILRS, 
        y = ~STATE.UT, 
        size = ~PROCURATION.OF.MINOR.GILRS, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>PROCURATION OF MINOR GILRS: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f10 <- data %>%
    plot_ly(
        x = ~PROHIBITION.OF.CHILD.MARRIAGE.ACT, 
        y = ~STATE.UT, 
        size = ~PROHIBITION.OF.CHILD.MARRIAGE.ACT, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>PROHIBITION OF CHILD MARRIAGE ACT: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )
f11 <- data %>%
    plot_ly(
        x = ~RAPE.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~RAPE.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>RAPE OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

f12 <- data %>%
    plot_ly(
        x = ~SELLING.OF.GIRLS.FOR.PROSTITUTION, 
        y = ~STATE.UT, 
        size = ~SELLING.OF.GIRLS.FOR.PROSTITUTION, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>SELLING OF GIRLS FOR PROSTITUTION: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )




tn1 <- data %>%
    plot_ly(
        x = ~ABETMENT.OF.SUICIDE, 
        y = ~STATE.UT, 
        size = ~ABETMENT.OF.SUICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>ABETMENT.OF.SUICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )



tn2 <- data %>%
    plot_ly(
        x = ~BUYING.OF.GIRLS.FOR.PROSTITUTION, 
        y = ~STATE.UT, 
        size = ~BUYING.OF.GIRLS.FOR.PROSTITUTION, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>BUYING OF GIRLS FOR PROSTITUTION: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )



tn3 <- data %>%
    plot_ly(
        x = ~EXPOSURE.AND.ABANDONMENT, 
        y = ~STATE.UT, 
        size = ~EXPOSURE.AND.ABANDONMENT, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>EXPOSURE AND ABANDONMENT: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )



tn4 <- data %>%
    plot_ly(
        x = ~FOETICIDE, 
        y = ~STATE.UT, 
        size = ~FOETICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>FOETICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn5 <- data %>%
    plot_ly(
        x = ~INFANTICIDE, 
        y = ~STATE.UT, 
        size = ~INFANTICIDE, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>INFANTICIDE: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn6 <- data %>%
    plot_ly(
        x = ~KIDNAPPING.and.ABDUCTION.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~KIDNAPPING.and.ABDUCTION.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>KIDNAPPING and ABDUCTION OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn7 <- data %>%
    plot_ly(
        x = ~MURDER.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~MURDER.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>MURDER OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn8 <- data %>%
    plot_ly(
        x = ~OTHER.CRIMES.AGAINST.CHILDREN, 
        y = ~STATE.UT, 
        size = ~OTHER.CRIMES.AGAINST.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>OTHER CRIMES AGAINST CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn9 <- data %>%
    plot_ly(
        x = ~PROCURATION.OF.MINOR.GILRS, 
        y = ~STATE.UT, 
        size = ~PROCURATION.OF.MINOR.GILRS, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>PROCURATION OF MINOR GILRS: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn10 <- data %>%
    plot_ly(
        x = ~PROHIBITION.OF.CHILD.MARRIAGE.ACT, 
        y = ~STATE.UT, 
        size = ~PROHIBITION.OF.CHILD.MARRIAGE.ACT, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>PROHIBITION OF CHILD MARRIAGE ACT: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )
tn11 <- data %>%
    plot_ly(
        x = ~RAPE.OF.CHILDREN, 
        y = ~STATE.UT, 
        size = ~RAPE.OF.CHILDREN, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>RAPE OF CHILDREN: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

tn12 <- data %>%
    plot_ly(
        x = ~SELLING.OF.GIRLS.FOR.PROSTITUTION, 
        y = ~STATE.UT, 
        size = ~SELLING.OF.GIRLS.FOR.PROSTITUTION, 
        frame = ~YEAR, 
        hoverinfo = "text",
        type = 'bar',
        mode = 'markers',
        hovertemplate = paste(
            "<b>State: %{y}<br></b>",
            "<b>SELLING OF GIRLS FOR PROSTITUTION: %{x}<br></b>",
            "<extra></extra>"
        ),
        color = ~STATE.UT
    ) %>%
    layout(
        xaxis = list(
            type = "log"
        )%>%animation_opts(1)
    )

ww <- plot_ly(
    labels=c("CRIME AGAINST CHILDREN",
             "DEATH RELATED CRIMES",
             "MURDER OF CHILDREN",
             "RAPE OF CHILDREN",
             "ABETMENT OF SUICIDE",
             "HEREDITARY",
             "EXPOSURE AND ABANDONMENT",
             "FOETICIDE",
             "INFANTICIDE",
             "KIDNAPPING and ABDUCTION OF CHILDREN",
             "OTHER CRIMES AGAINST CHILDREN",
             "GIRL CHILD RELATED CRIMES",
             "PROCURATION OF MINOR GILRS",
             "BUYING OF GIRLS FOR PROSTITUTION",
             "SELLING OF GIRLS FOR PROSTITUTION",
             "PROHIBITION OF CHILD MARRIAGE ACT"
             ),
    
    parents=c("",
              "CRIME AGAINST CHILDREN",
              "DEATH RELATED CRIMES",
              "DEATH RELATED CRIMES",
              "DEATH RELATED CRIMES",
              "CRIME AGAINST CHILDREN",
              "HEREDITARY",
              "HEREDITARY",
              "HEREDITARY",
              "CRIME AGAINST CHILDREN",
              "CRIME AGAINST CHILDREN",
              "CRIME AGAINST CHILDREN",
              "GIRL CHILD RELATED CRIMES",
              "PROCURATION OF MINOR GILRS",
              "PROCURATION OF MINOR GILRS",
              "GIRL CHILD RELATED CRIMES"
              ),
    
    
    values=c("",88867,21277,66777,813,3594,1859,658,1077,84969,86592,9573,4473,555,1074,3471),
    type='sunburst'
)



bar_chart_total_crimes <- ggplot(data=years_total_df, aes(x=Year, y=Crime, fill=factor(Year))) + 
    geom_bar(stat="identity")  
labs(fill = "Year")# Define UI for application that draws a histogram


ui <- fluidPage(

    ui <- dashboardPage(
        dashboardHeader(title = "Crime Data Analysis"),
        dashboardSidebar( sidebarMenu(
            menuItem("Description", tabName = "home", icon = icon("home")),
            menuItem("Data Analysis", tabName = "data", icon = icon("play")),
            menuItem("Conclusion",tabName = "inference",icon=icon("table"))
        )),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName ="home",tags$h1("CRIME DATA ANALYSIS"),
                    tags$h4("Children of our country are valued assets. 
Protection of children from crime and sexual abuse is considered one of the prime agendas for achieving millennium developmental goals. 
Crime is a very old concept and it is transmitted to the society from generation to generation.
Crime produces law and order situation. It is a social evil. 
It is generated by the society and the society also suffers a lot because of crime committed by its members. 
The rising wave of crime to-day has caused alarm in the public. 
In this project, we analyze the patterns of various crime performed against children on a dataset with demographic information of crime in India from the year 2001-2012.
"),
                    tags$br(),
                    
                    tags$h2("DATASET"),
                    tags$h4("First we obtained the dataset from the site",tags$strong("data.gov.in"),". We performed data cleaning and data wrangling."),
                    tags$h4("Then we performed the exploratory data analysis on the datasets that we cleaned."),
                    tags$br(),
                    tags$h2("EXPLORATORY DATA ANALYSIS"),
                    tags$h4("Visualization of data is the appearance of data in a pictographic or graphical form. This form facilitates top management to understand the data visually and get the messages of difficult concepts or identify new patterns. The approach of the personal understanding to handle data; applying diagrams or graphs to reflect vast volumes of complex data is more comfortable than presenting over tables or statements. In this study, we conduct data processing and data visualization for crime report data against children
                    that occurred in the India in the range of 2001 to 2012 using R language. As the result shows, by using those methods, 
                we can gain insights, understandings, new patterns, and do visual analytics from the existing data. "),
                    tags$br(),
                    tags$h2("INSPIRATION"),
                    tags$h4("The motivation behind taking up this topic for the research is that every aware citizen in today's modern world wants to live in a safe environment and neighborhood. 
However it is a known fact that crime in some form, exists in our society.
Although we cannot control what goes on around us, we can definitely try to take a few steps to aid the government and police authorities in trying to control them.
Hence, taking inspiration from the facts stated above, we decided to process this data provided and analyze it to identify the trends in crime against children over the years
")
                    
                ),
                tabItem(tabName = "data",tags$h1("Exploratory Data Analysis : Plots"),     
 
                                                   fluidRow(
                            
                            box(
                                width = 6, height = "428px",
                                
                                valueBox(29, "States", icon = icon("landmark"), color="blue"),
                                
                                valueBox(8, "Union Territories", icon = icon("bell"), color = "olive"),  
                                
                                valueBox(11, "Crime Types", icon = icon("gopuram"), color = "yellow"),  
                                
                                
                                tags$br(),
                                
                                tags$h4("We have analyzed over 11 crime types against children across all the States and Union Territories of India. We have also analyzed the crime types over these years individually."),tags$br(),
                                tags$h4("The line chart of Individual crime type over the years 2001-2012 shows the number of cases in each type of crime
                          and the aggregated total crimes over the 12 years."),tags$br(),
                                tags$h4("Cases due to suicide, child marriage increased especially from 2011 to 2012. Cases due to selling,buying of children, kidnapping and abduction, rape, abandonement decreased in the end of the year 2012.")
                                
                            ),
                            
                            box(
                                title = paste("Individual Crime Type over the Years 2001-2012") , status ="success", background="black", width = 6,
                                plotlyOutput(("linegraph"), height="365px")
                            )
                            
                        ),
                        fluidRow(
                            box(
                                title = "Crime over the Years", status ="success", width= 6,
                                plotOutput(outputId = "barchart", height = "350px", hover="plot_hover"), tags$br(),
                                tags$h4("This bar plot shows the total number of cases over the 12 years. We can see that the cases were increased steadily from 2002 to 2008 and 2011 to 2012. We have observed that 2009 crimes were controlled due to elections.")
                            ),
                            box(
                                title = paste("Crime Breakdown") , status ="success", width =6,
                                plotOutput(outputId = "treemap", height = "350px"), tags$br(),
                                tags$h4("The treemap plot gives us an idea about the majorly occuring crime, we can see that suicide cases is the highest, second highest is Child Marriage, third highest is Murder and so on.")
                            )
                        ),
                        tags$br(),
                        
                        fluidRow(column(9,selectInput("cr","Choose the Crime:",choices=c("Suicide","Buying", "Abandonment", "Foeticide","Infanticide", "KidnapAbd","Murder","Procuration","Childmrg","Rape","Selling")))),
                        fluidRow(plotlyOutput("Plot1")),tags$br(),
                        
                        fluidRow(column(10,selectInput("cr1","Choose the Crime:",choices=c("Suicide","Buying", "Abandonment", "Foeticide","Infanticide", "KidnapAbd","Murder","Procuration","Childmrg","Rape","Selling")))),
                        fluidRow(plotlyOutput("Plot3")),tags$br(),
                        
                        fluidRow(plotlyOutput("Plot2")), tags$br(),
                        
                        tags$h4("The sunburst graph shows the cases of crime against children in the year 2001-2012. The highest is Kidnapping and abduction, second highest is Rape of children.
                The third highest is cases with Murder of children. 
                The fourth highest is Procuration of girls.
                The fifth highest is Exposure and abandonment.
                The least number of cases is Foeticide."), tags$br(),
                        
                        
                        
                ),

                tabItem(
                    tabName = "inference",tags$h1("CONCLUSION"),
                    tags$h4("We conclude by saying that with all the visualizations it can be noted that the parliment elections year i.e 2009 has the the number of crimes occuring controlled due to elections. 
But it should be noted that some of the crimes like suicide, murder, Child Marriage and procuration has increased steadily in the country.
We can also see that the highest number of crimes against children has been recorded in Uttar Pradesh.
By this analysis, we can help the government take actions wherever required in the states of India and reduce crimes against children."),
                    
                )
                
            ))
        ,skin=c("purple"))
   
)

# Define server logic required to draw a histogram
server <- function(input, output,session) { 
    
    output$linegraph <- renderPlotly({
        years_total_df%>%plot_ly(x=~Year)%>%
            add_lines(y=years_total_df$Suicide,name="Suicide",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Buying,name="Buying",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Abandonment,name="Exposure",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Foeticide,name="Foeticide",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Infanticide,name="Infanticide",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$KidnapAbd,name="Kidnapping and Abduction",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Procuration,name="Procuration",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Childmrg,name="Child Marriage",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Rape,name="Rape",line= list(shape = "line"))%>%
            add_lines(y=years_total_df$Selling,name="Selling",line= list(shape = "line"))
        
    })
    
    
    
    
    output$barchart <- renderPlot(bar_chart_total_crimes)
    
    output$treemap <- renderPlot({
        total_year <- yearly_crime_df %>% group_by(`Crime`) %>% summarize_all(funs(sum), na.rm=TRUE)
        #print(crime_total_df)
        print(total_year)
        ggplot(total_year, aes(fill= `Crime`, area=`Number`)) + 
            geom_treemap() +
            geom_treemap_text(colour = "white", place="centre", label=paste(total_year$Crime,": ",total_year$Number)) +
            labs(title=paste("Aggregated Crime Distribution for 2001-2012")) +
            theme(legend.position="right")  +
            scale_fill_brewer(palette="Paired")
    })
    
    
    
    output$Plot1<- renderPlotly({
        
        if(input$cr=="Suicide"){
            subplot(f1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Buying"){
            subplot(f2,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Abandonment"){
            subplot(f3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Foeticide"){
            subplot(f4,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Infanticide"){
            subplot(f5,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="KidnapAbd"){
            subplot(f6,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Murder"){
            subplot(f7,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Other"){
            subplot(f8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Procuration"){
            subplot(f9,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Childmrg"){
            subplot(f10,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Rape"){
            subplot(f11,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr=="Selling"){
            subplot(f12,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
    }
    )
    
    output$Plot3<- renderPlotly({
        if(input$cr1=="Suicide"){
            subplot(tn1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Buying"){
            subplot(tn2,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Abandonment"){
            subplot(tn3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Foeticide"){
            subplot(tn4,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Infanticide"){
            subplot(tn5,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="KidnapAbd"){
            subplot(tn6,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Murder"){
            subplot(tn7,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Other"){
            subplot(tn8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Procuration"){
            subplot(tn9,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Childmrg"){
            subplot(tn10,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Rape"){
            subplot(tn11,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
        else if(input$cr1=="Selling"){
            subplot(tn12,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        }
    }
    )
    
    output$Plot2<- renderPlotly({
        subplot(ww,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
        
    }
    )
    
    
    
}
shinyApp(ui, server)
