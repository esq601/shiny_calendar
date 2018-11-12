library(shiny)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(readr)
library(lubridate)
library(directlabels)
library(timevis)
library(stringr)
library(tidyselect)
library(janitor)
library(htmltools)
library(htmlwidgets)
library(DT)

pw <- "calendarUpdate"

styles <- "
      .vis-item .vis-item-overflow { overflow: visible; }
.vis-time-axis .vis-grid.vis-vertical.vis-minor.vis-saturday { background: Grey; }
.vis-time-axis .vis-grid.vis-vertical.vis-minor.vis-sunday { background: Grey; }
.vis-time-axis .vis-text.vis-sunday { background: Grey; }
.vis-item .vis-item-content { padding: 0px;}
"

ui <-  dashboardPage(skin="black",
                    
                dashboardHeader(title="10th Mountain Calendar App",titleWidth = 450),

                dashboardSidebar(
                  sidebarMenu(
                    checkboxGroupInput(inputId = "cal_type",label="Calendar Selection:",choices = c("Executive"="Executive Calendar","LRTC","Staff"),selected = "LRTC"),
                    uiOutput("calSelect"),
                    hr(),
                    img(src="mtnpatch.png",height="300px",width="200px"),
                    #actionButton("preview","Preview"),
                    actionButton("time","Add Time"),
                    dateInput("check_date","What date?"),
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv"))
                  ),
                  passwordInput("uploadpw","Upload Password:")
                  
                ),
                dashboardBody(
                 tabsetPanel(
                   tabPanel("Calendar",
                            div(style= "overflow: scroll; max-height: 800px",tagList(list(tags$head(tags$style(styles, type="text/css")), timevisOutput("make_timeline")))),
                            DTOutput("table1")
                            ),
                   tabPanel("List",
                            fluidRow(column(checkboxInput("monthcheck","Date View"),width = 3),
                                     column(htmlOutput("dateselector"),width=6),
                                     column(actionButton("movedate","Add Month"),width=1),
                                     column(actionButton("backdate","Prev. Month"),width = 1),
                                     column(actionButton("resetdate","Reset Dates"),width=1)
                                     ),
                            fluidRow(DTOutput("table2"))
                            )
                 )
                  
                  #div(style= styles,timevisOutput("make_timeline"))
                )
  )
?fileInput
#save(sgs1,file="caldata.rda")
server <- function(input, output, session) {

  # sliderStart <- Sys.Date() + months
  # sliderEnd <- Sys.Date() + months(1)
  
  output$dateselector <- renderUI({
    dateRangeInput("dateslide","Select Date Range:",min=min(sgs1$`Start Date`),max=max(sgs1$`End Date`),start =floor_date(Sys.Date(),unit = "months"),
                   end= floor_date(Sys.Date() +months(1),unit = "months"),format = "mm/yy",startview = "year")
  })

  observeEvent(input$movedate,{
    updateDateRangeInput(session,"dateslide",
                          start=floor_date(input$dateslide[1] + months(1),unit="months"),
                         end=floor_date(input$dateslide[2] + months(1),unit="months")
    )
  })
  
  observeEvent(input$backdate,{
    updateDateRangeInput(session,"dateslide",
                         start=floor_date(input$dateslide[1] - months(1),unit="months"),
                         end=floor_date(input$dateslide[2] - months(1),unit="months")
    )
  })
  
  observeEvent(input$resetdate,{
    updateDateRangeInput(session,"dateslide",
                         start=floor_date(Sys.Date(),unit = "months"),
                         end=floor_date(Sys.Date() +months(1),unit = "months")
    )
  })

#  ?observeEvent
  paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
  }

  # sgs1 <- read_csv("div_calendar.csv",
  #                  col_types = cols(`End Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
  #                                   `Start Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
  #                                   Unit = col_factor(levels = c("10MD HQ","HHBN", "86 IBCT", "1 IBCT",
  #                                                                "2 IBCT", "3 IBCT", "DIVARTY","10 CAB",
  #                                                                "10 SBDE", "Tennant", "NET/NEF T&E","LFS/Leader Schools", "Visiting Units")),
  #                                   ID = col_character()),
  #                  locale = locale(tz = "US/Eastern"))
  # 
  # save(sgs1,file="calendar.rda")
  load(file="calendar.rda")
  
  observeEvent(input$file1,{
    tryCatch(
      {
        sgs2 <- sgs1
        sgs1 <- read_csv(input$file1$datapath,
                         col_types = cols(`End Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                          `Start Date` = col_datetime(format = "%m/%d/%Y %H:%M"),
                                          Unit = col_factor(levels = c("10MD HQ","HHBN", "86 IBCT", "1 IBCT",
                                                                       "2 IBCT", "3 IBCT", "DIVARTY","10 CAB",
                                                                       "10 SBDE", "Tennant", "NET/NEF T&E","LFS/Leader Schools", "Visiting Units")),
                                          ID = col_character()),
                         locale = locale(tz = "US/Eastern"))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    if(input$uploadpw == pw){
      
      if(sum(str_count(colnames(sgs1),c("Event","Sub-Event","Unit","Start Date","End Date","Calendar",
                                        "Executive Options","Exec Legend","SR LDR Coverage","Comments","Location","Summary Event","ID")))==13){
        showModal(modalDialog(
          title="Complete!",
          "File Upload Successful!  Refresh your browser to see updates."
        ))
        save(sgs1,file="calendar.rda")
        save(sgs2,file="backup.rda")
        load(file="calendar.rda")
      } else {
        showModal(modalDialog(
          title="Error",
          "The file uploaded did not contain the correct fields.  Please ensure your .csv formatted file contains the following items:
        Event, Sub-Event, Unit, Start Date, End Date, Calendar, Executive Options, Exec Legend, SR LDR Coverage, Comments, Location, Summary Event, ID.  
          Columns must be in the order listed above."
        ))
      }
    } else {
      showModal(modalDialog(
        title="Password",
        "Incorrect password!"
      ))
    }
    #print(head(sgs1))  #save(sgs1,file="calendar.rda")
    #load(file="calendar.rda")
    
  })
?str_count
  sgs2 <- sgs1 %>%
    clean_names() %>%
    mutate(dual_cal=(case_when(
      (stringr::str_detect(sgs1$Calendar, "LRTC") & stringr::str_detect(sgs1$Calendar,"Executive Calendar")) == TRUE  ~ "Training",
      TRUE ~ "filter"
    ))) %>%
    tidyr::separate(col=calendar,into=paste("cal",seq(1:3),sep=""),sep=";#") %>%
    tidyr::separate(col=sr_ldr_coverage,into=paste("ldr",seq(1:12),sep=""),sep=";#") %>%
    tidyr::separate(col=executive_options,into=paste("type",seq(1:3),sep=""),sep=";#") %>%
    gather(Category1,calendar,c(cal1:cal3)) %>%
    gather(Category2,exec,c(type1:type3,ldr1:ldr12,dual_cal)) %>%
    filter(Category2 != "dual_cal" | (Category2 == "dual_cal" & exec != "filter")) %>%
    filter(!is.na(calendar) | summary_event == TRUE) %>%
    select(-Category1,-Category2) %>%
    mutate(group_name=as.factor(case_when(
      calendar %in% "Training" ~ "Training",
      calendar %in% "LRTC" ~ as.character(unit),
      calendar %in% "SRTC" ~ calendar,
      calendar %in% "Staff" ~ event,
      calendar %in% "Executive Calendar" & !is.na(exec) == TRUE ~ exec,
      TRUE ~ "N/A"
    ))) %>%
    mutate(title=case_when(
      difftime(end_date,start_date)  <= 86400 ~ paste(strftime(start_date,"%H%M"),strftime(end_date,"%H%M"),sep=" - "),
      difftime(end_date,start_date)  > 86400 ~ paste(strftime(start_date,"%d %b %y"),strftime(end_date,"%d %b %y"),sep=" - "),
      TRUE ~ "Invalid Format")
      ) %>%
    mutate(group=as.numeric(group_name)) %>%
    mutate(style = case_when(
      group == 4 ~ "background-color: CornflowerBlue;
      color: Black;
      width: calc(0px + 100%);
      border-style: dashed;
      border-color: Black;
      font-size: 70%;",
      group == 1 ~ "background-color: Orange;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 2 ~ "background-color: LightSlateGray;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 3 ~ "background-color: LightBlue;
      width: calc(0px + 100%);
      font-size: 70%;",
      group == 5 ~ "background-color: DarkOrange;
      width: calc(0px + 100%);
      font-size: 70%;",
      exec_legend %in% "FORSCOM" ~ "background-color: Red;
      border-color:red;
      width: calc(0px + 100%);
      font-size: 70%;",
      exec_legend %in% "XVIII ABN Corps" ~ "background-color: rgb(244, 177, 131);
      border-color:rgb(244, 177, 131);
      width: calc(0px + 100%);
      font-size: 70%;",
      exec_legend %in% "Division/Other" ~ "background-color: rgb(220,220,220);
      border-color:Black;
      width: calc(0px + 100%);
      font-size: 70%;",
      exec_legend %in% "Engagements" ~ "background-color: rgb(20, 220, 230);
      border-color:rgb(20, 220, 230);
      width: calc(0px + 100%);
      font-size: 70%;",
      exec_legend %in% "Informational/Opportunity" ~ "background-color: Yellow;
      border-color:GoldenRod;
      width: calc(0px + 100%);
      font-size: 70%;",
      TRUE ~ "background-color: Silver;
      border-color: Black;
      width: calc(0px + 100%);
      font-size: 70%;"
    )) %>%
    mutate(style=case_when(
      summary_event == TRUE ~ paste(style,"font-weight: bold;",sep=" "),
      TRUE ~ style
    )) %>%
    distinct()
#str(sgs2)
  sgs_table <- sgs1 %>%
    mutate(Events = paste3(Event,`Sub-Event`,sep=": ")) %>%
    mutate(col1 = paste(as.character(strftime(`Start Date`,"%d %b %y")))) %>%
    mutate(col2 = case_when(
      difftime(`End Date` , `Start Date`)  <= 86400 ~ paste(strftime(`Start Date`,"%H%M"),strftime(`End Date`,"%H%M"),sep=" - "),
      difftime(`End Date` , `Start Date`)  > 86400 ~ paste(as.character(strftime(`End Date`,"%d %b %y"))),
      TRUE ~ "Invalid Format" #
      )) %>%
    mutate(`SR LDRs`= str_replace_all(`SR LDR Coverage`,";#",", ")) %>%
    select(col1,col2,Events,`SR LDRs`,Location,Comments,Calendar,`Summary Event`,`Start Date`,`End Date`,Unit)

  

    cal_data <- reactive({

    sgs3 <- sgs2 %>%
      dplyr::filter(calendar %in% input$cal_type) %>%
      dplyr::select(event,sub_event,start_date,calendar,end_date,group_name,group,style,comments,id,title) %>%
      distinct()

    sgs3
  })
?grepl
    table_data <- reactive({
      
      table1 <- sgs_table %>%
        filter(grepl(c(input$cal_type),Calendar)) %>%
        distinct()
      
      table1$`Summary Event` <- as.character(table1$`Summary Event`)
      
      if(is.null(input$unitSelectCall)) {
        table2 <- table1
        table2
      } else {
        table2 <- table1 %>%
          filter(Unit %in% input$unitSelectCall | grepl(paste(input$unitSelectCall,collapse="|"),`SR LDRs`) | grepl(paste(input$unitSelectCall,collapse="|"),Events))
        table2
      }
      
      if(input$monthcheck == TRUE){
        table3 <- table2 %>%
          filter(`End Date` >= input$dateslide[1] & `Start Date` <= input$dateslide[2])
        table3
      } else {
        table2
      }
      
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

  timeline <- reactive({


    timedata <- data.frame(
      content=case_when(
        cal_data_filter()$calendar %in% "Staff" ~ cal_data_filter()$sub_event,
        TRUE ~ paste3(cal_data_filter()$sub_event,cal_data_filter()$event,sep= " ")
      ),
      start=cal_data_filter()$start_date,
      end=cal_data_filter()$end_date,
      group=cal_data_filter()$group,
      #title=iconv(cal_data_filter()$comments,sub = ""),
      #location=iconv(cal_data_filter()$location,sub = ""),
      subgroup=cal_data_filter()$id,
      title=cal_data_filter()$title,
      #className=cal_data_filter()$className,
      #type=cal_data_filter()$type,
      style=cal_data_filter()$style,stringsAsFactors = FALSE)

    timedata <- timedata %>%
      arrange(group)

    group_cal=data.frame(id=cal_data_filter()$group, content=cal_data_filter()$group_name,order=cal_data_filter()$group)

    group_cal <- group_cal %>%
      distinct(id,.keep_all=TRUE)

    t2 <- timevis(timedata, groups=group_cal, showZoom=TRUE,options=list(
      start=as.POSIXct(cut(Sys.Date(), "month")),
      end=as.POSIXct(cut(Sys.Date(), "month")) + months(4),
      orientation = "both",
      verticalScroll = "true",
      horizontalScroll = "true",
      zoomKey = "ctrlKey",
      stack = "false",
      verticalScroll = "true",
      multiselect = "true",
      width="160%"
    ))
    
    t2
  })
  
  output$make_timeline <- renderTimevis({
    timeline()
  })

   dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("password", "Password: ",
                placeholder = 'Input here...'
      ),
      span('(Try the name of a valid data object like "mtcars", ',
           'then a name of a non-existent object like "abc")'),
      if (failed)
        div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  
  observeEvent(input$preview, {
    test1 <- tagList(list(tags$head(tags$style(styles, type="text/css")), timeline()))

    save_html(test1,file="test.html")
    #webshot::webshot(url="test.html",file="test.png",vwidth=1000,vheight=600)
  })
  
  observeEvent(input$time,{
    removeCustomTime("make_timeline","custom_date")
    addCustomTime("make_timeline",input$check_date,"custom_date")
    
  })
  
  selected_item <- reactive({
    req(input$make_timeline_selected)
    req(input$make_timeline_data)
    
    df1 <- input$make_timeline_data %>%
      filter(id %in% c(input$make_timeline_selected)) %>%
      left_join(sgs1,by = c("subgroup"="ID")) %>%
      select("Event","Sub-Event","Unit","Start Date","End Date","Comments","Location")
    
    df1$`Start Date` <- as.POSIXct(df1$`Start Date`,format="%d/%m/%Y %H:%M")

    df1
  })

  output$table1 <- renderDT(
    
    datatable(selected_item(),filter = "top") %>%
      formatDate(c(4,5),method = "toLocaleString")
  )

  output$table2 <- renderDT(
    datatable(table_data(),extensions = 'Buttons',colnames = c("Date"=2,"End/Times"=3), options = list(
                columnDefs = list(list(visible=FALSE, targets=c(7:10))
                                  ),
                dom = 'Blrtip',
                ordering = F,
                pageLength = 25,
                lengthMenu = c(10,25,100,500),
                buttons = c('copy', 'excel', 'pdf'))
    ) %>%
      formatStyle(
        columns = "Events",
        valueColumns = "Summary Event",
        fontWeight = styleEqual(levels=c("TRUE"),values=c("bold"),default = "normal"),
        color = styleEqual(levels=c("TRUE"),values=c("Blue"),default = "Black")
      ) %>%
      formatStyle(
        "Date","white-space"="nowrap"
      ) %>%
      formatStyle(
        "End/Times","white-space"="nowrap"
      )
  )


}

shinyApp(ui = ui, server = server)


