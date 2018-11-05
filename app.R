library(shiny)
library(dplyr)
#library(ggplot2)
library(shinydashboard)
library(tidyr)
#library(rsconnect)
library(readr)
library(lubridate)
library(directlabels)
library(timevis)
#library(DT)
#library(ggthemes)
#library(plotly)
library(tidyselect)
library(janitor)
library(htmltools)
#library(webshot)
#library(vistime)


ui <-  dashboardPage(skin="black",
                    
                dashboardHeader(title="10th Mountain Calendar App",titleWidth = 450),

                dashboardSidebar(
                  sidebarMenu(
                    checkboxGroupInput(inputId = "cal_type",label="Calendar Selection:",choices = c("Executive"="Executive Calendar","LRTC","Staff"),selected = "LRTC"),
                    uiOutput("calSelect"),
                    hr(),
                    img(src="mtnpatch.png",height="300px",width="200px")
                  )
                  
                ),
                dashboardBody(
  
                  div(style= "overflow: scroll; height: 700px",uiOutput("test"))
                )
  )

#save(sgs1,file="caldata.rda")
server <- function(input, output, session) {

  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }

  sgs1 <- read_csv("div_calendar.csv",
                   col_types = cols(`End Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                    `Start Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                    Unit = col_factor(levels = c("10MD HQ","HHBN", "86 IBCT", "1 IBCT",
                                                                 "2 IBCT", "3 IBCT", "DIVARTY","10 CAB",
                                                                 "10 SBDE", "Tennant", "NET/NEF T&E","LFS/Leader Schools", "Visiting Units"))))

  sgs2 <- sgs1 %>%
    clean_names() %>%
    tidyr::separate(col=calendar,into=paste("cal",seq(1:3),sep=""),sep=";#") %>%
    tidyr::separate(col=sr_ldr_coverage,into=paste("ldr",seq(1:12),sep=""),sep=";#") %>%
    tidyr::separate(col=executive_options,into=paste("type",seq(1:3),sep=""),sep=";#") %>%
    gather(Category1,calendar,c(cal1:cal3)) %>%
    gather(Category2,exec,c(type1:type3,ldr1:ldr12)) %>%
    filter(!is.na(calendar) | summary_event == TRUE) %>%
    select(-Category1,-Category2) %>%
    mutate(group_name=as.factor(case_when(
      calendar %in% "LRTC" ~ as.character(unit),
      calendar %in% "SRTC" ~ calendar,
      calendar %in% "Staff" ~ event,
      calendar %in% "Executive Calendar"  & is.na(exec) == TRUE ~ "Executive Calendar",
      calendar %in% "Executive Calendar" & !is.na(exec) == TRUE ~ exec,
      TRUE ~ "N/A"
    ))) %>%
    mutate(group=as.numeric(group_name)) %>%
    mutate(style = case_when(
      group == 4 ~ "background-color: Red;
      width: calc(0px + 100%);
      border-style: dashed;
      border-color: green;
      font-size: 70%;",
      group == 1 ~ "background-color: Orange;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 2 ~ "background-color: Gray;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 3 ~ "background-color: LightBlue;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 5 ~ "background-color: Yellow;
      width: calc(0px + 100%);
      font-size: 70%;",
      
      TRUE ~ "background-color: Gray;
      width: calc(0px + 100%);
      font-size: 70%;"
    )) %>%
    distinct()
  
  cal_data <- reactive({

    sgs3 <- sgs2 %>%
      dplyr::filter(calendar %in% input$cal_type) %>%
      dplyr::select(event,sub_event,start_date,calendar,end_date,group_name,group,style) %>%
      distinct()
  })
  
  cal_data_filter <- reactive({
    
    if(is.null(input$unitSelectCall)) {
      sgs4 <- cal_data()
      sgs4
    } else {
       sgs4 <- cal_data() %>%
        filter(group_name %in% input$unitSelectCall)
      sgs4
    }
    
  })
  
  cal_filter <- reactive({
    cal_iso <- cal_data() %>%
      select(group_name) %>%
      distinct()
    cal_iso
  })
  
  output$calSelect <- renderUI({
    selectInput("unitSelectCall","Filter:",c(cal_filter()),multiple = TRUE)
  })

  make_timeline <- reactive({
    
    
    timedata <- data.frame(
      content=case_when(
        cal_data_filter()$calendar %in% "Staff" ~ cal_data_filter()$sub_event,
        TRUE ~ paste3(cal_data_filter()$sub_event,cal_data_filter()$event,sep= " ")
      ),
      start=cal_data_filter()$start_date,
      end=cal_data_filter()$end_date,
      group=cal_data_filter()$group,
      #subgroup=cal_data_filter()$subgroup,
      #title=cal_data_filter()$title,
      #className=cal_data_filter()$className,
      #type=cal_data_filter()$type,
      style=cal_data_filter()$style,stringsAsFactors = FALSE)
    
    timedata <- timedata %>%
      arrange(group)
    
    group_cal=data.frame(id=cal_data_filter()$group, content=cal_data_filter()$group_name,order=cal_data_filter()$group)
    
    group_cal <- group_cal %>%
      distinct(id,.keep_all=TRUE)
    
    #print(group_cal)
    t2 <- timevis(timedata, groups=group_cal, showZoom=TRUE,options=list(
      start=as.POSIXct(cut(Sys.Date(), "month")),
      end=as.POSIXct(cut(Sys.Date(), "month")) + months(4),
      orientation = "both",
      verticalScroll = "true",
      horizontalScroll = "true",
      zoomKey = "ctrlKey",
      stack = "false",
      verticalScroll = "true",
      width="150%"
    ))
    
    styles <- "
  .vis-item .vis-item-overflow { overflow: visible; }
  .vis-time-axis .vis-text.vis-saturday,
  .vis-time-axis .vis-text.vis-sunday { background: Grey; }
  .vis-time-axis .vis-text.vis-sunday { background: Grey; }
  .vis-item .vis-item-content { padding: 0px; }
  "
    
    t2 <- tagList(list(tags$head(tags$style(styles, type="text/css")), t2))
    
    t2

  })
  
 
  output$test <- renderUI ({
    make_timeline()
  })

}

shinyApp(ui = ui, server = server)

