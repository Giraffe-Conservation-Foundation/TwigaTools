#### R SHINY; ER & WB ##########################################################
### GCF CM v1.0 20240924
### GCF CM v1.1 20250211 updated giraffe_nw_monitoring with giraffe_nw_monitoring0 for Audi 2024 data processing
### GCF CM v1.2 20250227 updated for both giraffe_nw_monitoring AND giraffe_nw_monitoring0 
### GCF CM v1.3 20250410 updated to add leading zero for images when <4 numbers are retained
### GCF CM v1.4 20250527 modified for new shiny tab formats

tab1UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    tags$div(titlePanel("ER2WB: EarthRanger survey to GiraffeSpotter format (v1.4)")),  # main title
    
    fluidRow(
      column(12,                                                                  # blurb at the top of the app
             p("This tool helps you format your giraffe encounter data (submitted using the EarthRanger app) for upload to GiraffeSpotter."),
             p("Follow the steps below to download your GiraffeSpotter bulk import package."),
             p("Look out for progress notifications in the bottom right corner.")) ),
    
    fluidRow(column(12, h3("STEP 1: Enter your information -----------------------------------------------------------------------"),
                    p("Enter the information below unique to you and your survey"))  ),
    fluidRow(column(4, textInput("instance", "My EarthRanger instance", value="twiga.pamdas.org")),
             column(4, textInput("bearer_token", "My EarthRanger token", value="")),
             column(4, textInput("observer", "My initials", value="")) ),
    fluidRow(column(4, dateInput("date_start", "Survey start date", value=Sys.Date()-10)),
             column(4, dateInput("date_end", "Survey end date", value=Sys.Date())) ),
    fluidRow(column(4, selectInput("country", "My country", choices=c("CMR", "KEN", "NAM", "NANW", "TZA", "UGA", "ZMB"))), 
             column(4, uiOutput("site_ui"))),
    fluidRow(column(4, textInput("organisation", "My GiraffeSpotter organisation", value="Giraffe Conservation Foundation")),
             column(4, textInput("username", "My GiraffeSpotter username", value=""))),
    fluidRow(column(4, selectInput("species", "My species", choices=c("Masai","Northern","Reticulated","Southern"))),
             column(4, uiOutput("subsp_ui"))),
    tags$br(),
    
    fluidRow(column(12, h3("STEP 2: Process my data ------------------------------------------------------------------------------"),
                    p("Click the 'Process Data' button to fetch and format your survey data"))  ),
    fluidRow(column(12, actionButton("process_data", "Process Data"))  ),
    tags$br(),
    tags$br(),
    
    fluidRow(column(12, h3("STEP 3: Process my images --------------------------------------------------------------------------")) ),
    fluidRow(column(12, p("Select the single zipped folder (no subfolders) that contains your survey images (1GB maximum)"))  ),
    fluidRow(column(12, p("If you do not have images, but would like to check your survey data, please go to 'Download my ER data' at the bottom of the page."))  ),
    tags$br(),
    fluidRow(column(12, fileInput("file1", "Upload my image folder (ZIP file)", multiple=F, accept=c(".zip")))  ),
    fluidRow(column(12, p("When your folder has uploaded, click the 'Rename my images' button below to process your images"))  ),
    fluidRow(column(12, actionButton("rename_images", "Rename my images"))),
    fluidRow(column(12, tableOutput("rename_log")) ),
    tags$br(),
    fluidRow( column(12, p("When your images are processed, click the 'Download ZIP' button to download your folder ready for GiraffeSpotter bulk import"))  ),
    fluidRow( column(12, downloadButton("download_zip", "Download ZIP"))),
    tags$br(),
    fluidRow(column(12, h3("Done!"),
                    p("Use the files in the downloaded folder to do your GiraffeSpotter bulk import."))  ),
    fluidRow(column(12, p("If you would to process another survey, please refresh the app and before you start again."))),
    
    fluidRow( column(12, p("If you need help, please see below 'Help' section."))),
    
    tags$br(),
    tags$br(),
    tags$br(),
    fluidRow( column(12, h3("Help ----------------------------------------------------------------------------------------------------------"))  ),
    
    fluidRow(column(12, p("Help, I found a mistake in my data!"))  ),
    fluidRow(column(12, p("If you have noticed a mistake in your data, download your ER data using the 'Download my ER data' button below. "))),
    fluidRow(column(12, p("Find the entry with the error, and use its serial number ('evt_serial') to find the entry on ER web and fix the mistake on the web."))),
    fluidRow(column(12, p("Come back and start again."))  ),
    fluidRow(column(12, downloadButton("download_er_data", "Download my ER data"))),
    tags$br(),
    tags$br(),
    fluidRow(column(12, p("Help, it's something else! Please email courtney@giraffeconservation.org"))),
    tags$br() )
  
}


tab1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    options(shiny.maxRequestSize=1024*1024^2)                                     # increase folder upload size allowed to 1GB
    
    temp_dir = tempdir()                                                          # create temp directory for unzipping images to
    if (dir.exists(temp_dir)) {                                                   # ensure the temporary directory is empty
      unlink(temp_dir, recursive=T)
      dir.create(temp_dir)
    }
    
    processed_data = reactiveVal(NULL)                                            # store the processed ER data
    gs_final_data = reactiveVal(NULL)                                             # store the formatted GS data
    
    ensure_columns = function(df, expected_columns) {                             # function to ensure all expected columns are present even if NA in ER
      for (col in expected_columns) {
        if (!col %in% names(df)) {
          df[[col]] = NA
        }
      }
      return(df)
    }
    
    # create site selection dropdown based on country
    output$site_ui = renderUI({
      req(input$country)
      sites = switch(input$country,
                     "CMR" = c("BNNP"),
                     "KEN" = c("MMNR", "RHNP", "RUNP", "TENP", "TWNP"),
                     "NAM" = c("BACO", "BLCO", "BWNP", "DZCO", "EHGR", "GMCO", "KWCO", "MNCO", "MNNP", "MSCO", "MUNP", "MYCO", "NJCO", "NLNP", "NNCO", "SACO", "SBCO", "SKCO", "UIFA", "WUCO"),  
                     "NANW" = c("NANW"),
                     "TZA" = c("SANP"),
                     "UGA" = c("LMNP", "MFNP", "PUWR"),
                     "ZMB" = c("LVNP"))
      selectInput("gcf_site", "My site", choices=sites)
    })
    
    
    # create subsp selection dropdown based on species
    output$subsp_ui = renderUI({
      req(input$species)
      subsp = switch(input$species,
                     "Masai"= c("Luangwa"="tippelskirchi thornicrofti", "Masai"="tippelskirchi tippelskirchi"),
                     "Northern"= c("Kordofan"="camelopardalis antiquorum", "Nubian"="camelopardalis camelopardalis", "West African"="camelopardalis peralta"),
                     "Reticulated"= c(" "="reticulata"),
                     "Southern"= c("Angolan"="giraffa angolensis", "South African"="giraffa giraffa"))
      selectInput("subspecies", "My subspecies", choices=subsp)
    })
    
    
    
    ###### fetch ER survey data ####################################################
    fetch_er_data = function() {                                                  # fetch the data and process for both ER and GS
      req(input$instance, input$bearer_token)
      
      output$step_log = renderText("Starting data fetch...")
      
      token_auth = paste("Bearer", trimws(input$bearer_token))
      base_url = paste0("https://", input$instance, "/api/v1.0/activity/events/?page_size=1000&sort_by=event_time&event_category=monitoring_", tolower(input$country))
      updated_since = format(as.POSIXct(input$date_start), "%Y-%m-%dT%H:%M:%SZ")  # date filter to minimise download size
      
      showNotification("ER data requested...Please wait", type="message", duration=40) # pop up to notify process started
      
      res = tryCatch({
        GET(base_url, query = list(updated_since=updated_since), config=add_headers(Authorization=token_auth)) # limit to  updated within survey dates for faster/efficient pull
      }, error = function(e) {
        output$logs = renderText(paste("Error during API request:", e$message))   # print error if one
        return(NULL)
      })
      if (status_code(res) != 200) {
        output$logs = renderText(paste("API request failed with status code:", status_code(res)))  # print error if one
        return(NULL)
      }
      
      base_url = paste0("https://", input$instance, "/api/v1.0/activity/events/?page_size=1000&sort_by=event_time&event_category=monitoring_", tolower(input$country), "&use_curser=true")
      x=GET(base_url, query = list(updated_since=updated_since), config=add_headers(Authorization=token_auth))
      raw_response = rawToChar(x$content)
      apiresponse = jsonlite::fromJSON(raw_response)
      next_url = apiresponse$data$`next`
      apidata = apiresponse$data$results
      
      data = list()                                                               # Initialize a list to store all the data
      data[[1]] = apidata
      i = 2                                                                       # Loop to get all pages
      while (!is.null(next_url)) {
        x = GET(next_url, add_headers(Authorization = token_auth))
        raw = rawToChar(x$content)
        apiresponse = jsonlite::fromJSON(raw)
        next_url = apiresponse$data$`next`
        temp = apiresponse$data$results
        rownames(temp) = NULL
        data[[i]] = temp
        i = i + 1
      }
      
      
      # format nested herd info
      safe_mutate = function(df) {                                                  # deal with mixed photo numbers/chrs
        if ("giraffe_left" %in% colnames(df)) {
          df = df %>% mutate(giraffe_left=as.character(giraffe_left),
                             giraffe_left = stringr::str_pad(giraffe_left, width=4, pad="0"))
        }
        if ("giraffe_right" %in% colnames(df)) {
          df = df %>% mutate(giraffe_right=as.character(giraffe_right),
                             giraffe_right = stringr::str_pad(giraffe_right, width=4, pad="0"))
        }
        return(df)
      }
      herd_df = bind_rows(data) %>% 
        filter(time >= input$date_start & time <= input$date_end,                 # filter to survey dates to be quicker
               event_type %in% c(paste0("giraffe_survey_encounter_", tolower(input$country)),
                                 "giraffe_nw_monitoring", "giraffe_nw_monitoring0")) %>% 
        unnest(event_details, names_sep="_", keep_empty=T) %>% 
        mutate(event_details_Herd_2 = map(event_details_Herd, ~ if (is.null(.x)) tibble() else safe_mutate(as_tibble(.x)))) %>%  
        select(id, event_type, event_details_Herd_2) %>% 
        unnest(event_details_Herd_2, keep_empty=T) %>% 
        as.data.frame()
      
      # Conditional processing based on event_type (For Audi's NW project specifically)
      if (any(herd_df$event_type %in% c("giraffe_nw_monitoring", "giraffe_nw_monitoring0"))) {  # Fetch subject data
        sub = GET("https://twiga.pamdas.org/api/v1.0/subjects/?include_inactive=FALSE",
                  config=add_headers(Authorization=token_auth)) %>%           
          content("text") %>% 
          fromJSON(flatten=T) %>% 
          as.data.frame() %>% 
          select(subjectID=data.id, subjectName=data.name)
        herd_df = herd_df %>%                                                    # join with sub and replace giraffe_id with subjectName
          left_join(sub, by=c("giraffe_id"="subjectID")) %>%
          mutate(giraffe_id = if_else(event_type %in% c("giraffe_nw_monitoring","giraffe_nw_monitoring0") & !is.na(subjectName), subjectName, giraffe_id)) %>%
          select(-subjectName)
      }
      
      
      # format event info
      evt_df = bind_rows(data) %>% 
        filter(time>=input$date_start & time<=input$date_end,                     # filter to survey dates to be quicker
               event_type %in% c(paste0("giraffe_survey_encounter_", tolower(input$country)),
                                 "giraffe_nw_monitoring", "giraffe_nw_monitoring0")) %>%
        unnest(location, names_sep="_", keep_empty=T) %>% 
        unnest(reported_by, names_sep="_", keep_empty=T) %>% 
        unnest(event_details, names_sep="_", keep_empty=T) %>% 
        as.data.frame()
      # ensure all expected columns present to allow joining
      expected_cols_herd = c("id", "giraffe_id", "giraffe_age", "giraffe_sex", "giraffe_left", "giraffe_right", "giraffe_notes")
      expected_cols_evt = c("location_latitude", "location_longitude", "time", "event_type", "reported_by_id", "reported_by_name", "serial_number",
                            "event_details_herd_dist", "event_details_herd_size", "event_details_image_prefix", "event_details_river_system", "event_details_herd_notes", "event_category")
      herd_df = ensure_columns(herd_df, expected_cols_herd)
      evt_df = ensure_columns(evt_df, expected_cols_evt)
      # format final ER df
      final_df = left_join(herd_df, evt_df, by="id") %>% 
        select(evt_id="id", evt_serial="serial_number", evt_lat="location_latitude", evt_lon="location_longitude",
               evt_dttm="time", evt_cat="event_category", evt_type="event_type.y", usr_id="reported_by_id",
               usr_name="reported_by_name", gir_giraffeId="giraffe_id", gir_giraffeAge="giraffe_age", gir_giraffeSex="giraffe_sex",
               gir_giraffeRight="giraffe_right", gir_giraffeLeft="giraffe_left", gir_giraffeNotes="giraffe_notes",
               gir_herdSize="event_details_herd_size", gir_herdNotes="event_details_herd_notes", gir_riverSystem="event_details_river_system", gir_imagePrefix="event_details_image_prefix")
      
      showNotification("ER data formatted", type="message", duration=20)          # pop up to notify process finished
      
      
      
      processed_data(final_df)                                                    # store the processed ER data
      
      
      mycountry = input$country
      mysite=input$gcf_site
      mysite_code = input$gcf_site 
      mysite_name = recode(input$gcf_site, "BNNP"="Bouba Ndjida National Park",
                           "BWNP"="Bwabwata National Park",
                           "EHGR"="Etosha Heights Private Reserve",
                           "UIFA"="Uitkoms Farm",
                           "LMNP"="Lake Mburo",
                           "MUNP"="Mudumu National Park",
                           "PUWR"="Pian Upe",
                           "RUNP"="Ruma National Park",
                           "TENP"="Tsavo East National Park",
                           "TWNP"="Tsavo West National Park",
                           "MMNR"="Masai Mara National Reserve",
                           "NANW"="North-western Namibia",
                           "SKCO"="Sikunga Conservancy",
                           "NLNP"="Nkasa Lupala National Park",
                           "MNNP"="Mangetti National Park",
                           "GMCO"="George Mukoya Conservancy",
                           "MNCO"="Muduva Nyangana Conservancy",
                           "NJCO"="Najagna Conservancy",
                           "NNCO"="Nyae Nyae Conservancy",
                           "SACO"="Salambala Conservancy",
                           "MSCO"="Mashi Conservancy",
                           "MYCO"="Mayuni Conservancy",
                           "SBCO"="Sobbe Conservancy",
                           "BACO"="Bamunu Conservancy",
                           "DZCO"="Dzoti Conservancy",
                           "KWCO"="Kwandu Conservancy",
                           "BLCO"="Balyerwa Conservancy",
                           "WUCO"="Wuparo Conservancy")
      
      
      # Store both the site code and the full name
      mysite_code = input$gcf_site
      
      
      
      output$download_er_data = downloadHandler(filename=function() {             # ER Report Download Handler
        paste0("ER_events_", mycountry, mysite_code, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
        
        
        
      },
      content = function(file) {fetch_er_data()                                   # Fetch and store the data in processed_data()
        df = processed_data()                                                     # Use the stored data for ER report
        if (!is.null(df) && nrow(df) > 0) {
          write_xlsx(df, file)
          output$logs = renderText("ER report successfully written.")
        } else {
          showNotification("No data to download.", type="error")
        }
      })    
      
      
      # format nested herd info for GS
      gs_df = final_df %>% 
        mutate(herd_id = paste(input$country, mysite, format(ymd_hms(evt_dttm), "%Y%m%d%H%m%S"), sep="_")) %>% 
        group_by(evt_id) %>% 
        mutate(gir_giraffeAge = recode(gir_giraffeAge, "ad"="adult", 
                                       "sa"="subadult",
                                       "ju"="calf",
                                       "ca"="calf",
                                       "u"="unknown"),
               gir_giraffeSex = recode(gir_giraffeSex, "f"="female",
                                       "m"="male",
                                       "u"="unknown"),
               ad = sum(gir_giraffeAge=="adult", na.rm=T),
               af = sum(gir_giraffeAge=="adult" & gir_giraffeSex=="female", na.rm=T),
               am = sum(gir_giraffeAge=="adult" & gir_giraffeSex=="male", na.rm=T),
               sa = sum(gir_giraffeAge=="subadult", na.rm=T),
               sf = sum(gir_giraffeAge=="subadult" & gir_giraffeSex=="female", na.rm=T),
               sm = sum(gir_giraffeAge=="subadult" & gir_giraffeSex=="male", na.rm=T),
               ca = sum(gir_giraffeAge=="calf", na.rm=T),
               occurrenceID = paste(herd_id, sep="_"))
      # format final GS df
      gs_final = data.frame(
        Survey.vessel = "vehicle_based_photographic",
        Survey.id = paste0(input$country, "_", mysite, "_", format(ymd_hms(gs_df$evt_dttm), "%Y%m")),
        Occurrence.occurrenceID = paste(input$country, mysite, format(ymd_hms(gs_df$evt_dttm), "%Y%m%d%H%m%S"), sep="_"),
        Encounter.otherCatalogNumbers = "",
        Encounter.decimalLongitude = gs_df$evt_lon,
        Encounter.decimalLatitude = gs_df$evt_lat,
        Encounter.locationID = mysite_name,
        Encounter.verbatimLocality = "",
        Encounter.depth = "",
        Encounter.year = year(format(ymd_hms(gs_df$evt_dttm))),
        Encounter.month = month(format(ymd_hms(gs_df$evt_dttm))),
        Encounter.day = day(format(ymd_hms(gs_df$evt_dttm))),
        Encounter.hour = hour(format(ymd_hms(gs_df$evt_dttm))),
        Encounter.minutes = minute(format(ymd_hms(gs_df$evt_dttm))),
        Encounter.submitterOrganization = input$organisation,
        Encounter.submitterID = input$username,
        Occurrence.groupSize = gs_df$gir_herdSize,
        Occurrence.numAdults = gs_df$ad,
        Occurrence.numAdultFemales = gs_df$af,
        Occurrence.numAdultMales = gs_df$am,
        Occurrence.numSubAdults = gs_df$sa,
        Occurrence.numSubFemales = gs_df$sf,
        Occurrence.numSubMales = gs_df$sm,
        Occurrence.numCalves = gs_df$ca,
        Occurrence.observer = "",
        Occurrence.distance = "",
        Occurrence.bearing = "",
        Encounter.behavior = "",
        Encounter.sex = gs_df$gir_giraffeSex,
        Encounter.lifeStage = gs_df$gir_giraffeAge,
        Encounter.genus = "Giraffa",	
        Encounter.specificEpithet = input$subspecies,
        Encounter.occurrenceRemarks = gs_df$gir_giraffeNotes,
        Encounter.mediaAsset0 = toupper(paste0(input$country, "_", mysite, "_", format(ymd_hms(gs_df$evt_dttm), "%Y%m%d"), "_", input$observer, "_", gs_df$gir_imagePrefix, gs_df$gir_giraffeRight, ".JPG")),
        Encounter.mediaAsset1 = toupper(paste0(input$country, "_", mysite, "_", format(ymd_hms(gs_df$evt_dttm), "%Y%m%d"), "_", input$observer, "_", gs_df$gir_imagePrefix, gs_df$gir_giraffeLeft, ".JPG")),
        Encounter.individualID = gs_df$gir_giraffeId,
        MarkedIndividual.nickname = "",
        SatelliteTag.serialNumber = "",
        TissueSample.sampleID	= "",
        MicrosatelliteMarkersAnalysis.analysisID = "",
        SexAnalysis.processingLabTaskID	= "",
        SexAnalysis.sex	= "",
        `version GS_20200813` = "")
      
      
      # Remove "NA.JPG" or "NA.JPEG" from media asset columns
      gs_final = gs_final %>%
        mutate(Encounter.mediaAsset0 = if_else(grepl("NA\\.JPG$|NA\\.JPEG$", Encounter.mediaAsset0, ignore.case=T), NA_character_, Encounter.mediaAsset0),
               Encounter.mediaAsset1 = if_else(grepl("NA\\.JPG$|NA\\.JPEG$", Encounter.mediaAsset1, ignore.case=T), NA_character_, Encounter.mediaAsset1))
      
      showNotification("GS data formatted", type="message", duration=20)
      showNotification("Ready to move on to 'STEP 3: Process my images'", type="message", duration=20)
      
      
      gs_final_data(gs_final)                                                     # store the processed GS data
    }
    
    # Observer for the "Process Data" button
    observeEvent(input$process_data, {
      fetch_er_data()
    })
    
    rename_dir = reactiveVal(NULL)   
    
    
    ##### image rename #############################################################
    observeEvent(input$rename_images, {
      req(input$file1)
      
      output$process_message = renderText("Image formatting started... please wait")
      
      # debugging checks
      print(input$file1)
      zip_file_path = gsub("\\\\", "/", input$file1$datapath)                     # replace backslashes with forward slashes
      cat("Manually adjusted file path:", zip_file_path, "\n")
      if (!file.exists(zip_file_path)) {                                          # check if the uploaded file exists
        showNotification(paste("Uploaded file does not exist at path:", zip_file_path), type = "error")
        return()
      }
      
      # unzip the uploaded folder
      tryCatch({
        unzip(zip_file_path, exdir=temp_dir)
        output$process_message = renderText("Folder unzipped...")
      }, error = function(e) {
        showNotification(paste("Error unzipping file:", e$message), type="error") # print error if one
        return()
      })
      
      # log the contents of the unzipped directory
      unzipped_files = list.files(temp_dir, recursive=T, full.names=T)
      showNotification(paste("Unzipped files:", paste(unzipped_files, collapse=", ")), type="message")  # print files unzipped
      
      # list images from the unzipped directory recursively
      jpg = list.files(temp_dir, pattern="(?i)\\.(jpg|jpeg)$", full.names=T, recursive=T)  # list files, allows jpg/jpeg/JPG/JPEG
      if (length(jpg) == 0) {
        showNotification("No JPG/JPEG files found in the uploaded ZIP.", type="error")  # print error if one
        return()
      }
      showNotification(paste(length(jpg), "JPG/JPEG files found."), type="message")  # print files unzipped
      
      # record df
      rec = data.frame(filename=character(), stringsAsFactors=F)
      
      # Rename the images
      withProgress(message = 'Renaming images', value = 0, {
        total_files = length(jpg)
        for (i in seq_along(jpg)) {
          .x = jpg[i]
          tryCatch({
            exif = exifr::read_exif(path=.x, tags="DateTimeOriginal")
            datetime = exif$DateTimeOriginal
            if (is.null(datetime) || datetime=="") {
              datetime = Sys.time()
            }
            dttm = as.POSIXct(datetime, format="%Y:%m:%d %H:%M:%S")
            dttm2 = format(dttm, "%Y%m%d")
            orig = basename(.x)
            orig = sub(pattern = "\\..*$", replacement = "", orig)
            orig = stringr::str_pad(orig, width=4, pad="0")  # <-- pad with leading zeros
            new = toupper(paste0(paste(input$country, input$gcf_site, dttm2, input$observer, orig, sep="_"), ".JPG"))
            path = file.path(temp_dir, new)
            if (file.rename(.x, path)) {
              rec = bind_rows(rec, data.frame(filename=new))
              cat("Renamed:", basename(.x), "to", new, "\n")
              cat("Current rec data frame:\n")
              print(rec)
            } else {
              showNotification(paste("Failed to rename:", .x), type="error")
            }
          }, error = function(e) {
            showNotification(paste("Error renaming file:", .x, ":", e$message), type="error")
            return()
          })
          # Update progress
          incProgress(1 / total_files)
        }
        showNotification("Renaming completed.", type="message", duration=20)
        showNotification("Ready to 'Download ZIP'", type="message", duration=20)
      })
      
      output$rename_log = renderTable(rec)
      
      # Filter matching images based on GiraffeSpotter data frame
      gs_data = gs_final_data()  # Use the processed GS data
      if (is.null(gs_data)) {
        showNotification("GiraffeSpotter data is not available.", type="error")
        return()
      }
      
      # Log filenames for debugging
      cat("Filenames in rec:\n", paste(rec$filename, collapse="\n"), "\n")
      cat("Filenames in GiraffeSpotter data (Encounter.mediaAsset0):\n", paste(gs_data$Encounter.mediaAsset0, collapse="\n"), "\n")
      cat("Filenames in GiraffeSpotter data (Encounter.mediaAsset1):\n", paste(gs_data$Encounter.mediaAsset1, collapse="\n"), "\n")
      
      # Trim whitespace and convert to lowercase for comparison
      rec$filename = trimws(tolower(rec$filename))
      gs_data$Encounter.mediaAsset0 = trimws(tolower(gs_data$Encounter.mediaAsset0))
      gs_data$Encounter.mediaAsset1 = trimws(tolower(gs_data$Encounter.mediaAsset1))
      
      matching_images = rec %>% filter(filename %in% gs_data$Encounter.mediaAsset0 | filename %in% gs_data$Encounter.mediaAsset1)
      if (nrow(matching_images)==0) {
        showNotification("No matching images found in GiraffeSpotter data.", type="error")
        return()
      }
      showNotification(paste(nrow(matching_images), "matching images found."), type="message", duration=20)
      
      
      # Convert filenames back to uppercase
      matching_images$filename = toupper(matching_images$filename)
      rec$filename = toupper(rec$filename)
      gs_data$Encounter.mediaAsset0 = toupper(gs_data$Encounter.mediaAsset0)
      gs_data$Encounter.mediaAsset1 = toupper(gs_data$Encounter.mediaAsset1)
      
      # Create a temporary directory to store the matching images
      temp_subdir = file.path(temp_dir, "matching_images")
      if (!dir.exists(temp_subdir)) {
        dir.create(temp_subdir)
      }
      
      # Copy matching images to the temporary directory
      withProgress(message = 'Copying files', value=0, {
        total_files = nrow(matching_images)
        tryCatch({
          for (file in matching_images$filename) {
            src = file.path(temp_dir, file)
            if (file.exists(src)) {
              file.copy(src, temp_subdir)
            } else {
              showNotification(paste("File does not exist:", src), type="error")
            }
          }
          incProgress(nrow(matching_images) / total_files)
        }, error = function(e) {
          showNotification(paste("Error copying files:", e$message), type="error")
          return()
        })
        showNotification("Copying files completed.", type="message", duration=20)
      })
      
      # Save the GiraffeSpotter data to an Excel file
      mycountry=input$country
      mysite=input$gcf_site
      gs_data_path = file.path(temp_subdir, paste0("GS_bulkimport_", mycountry, mysite, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
      write_xlsx(gs_data, gs_data_path)
      
      # Compress the temporary directory into a ZIP file using zip::zipr
      zip_file = file.path(temp_dir, paste0("GS_bulkimport_", mycountry, mysite, "_", format(Sys.Date(), "%Y%m%d"), ".zip"))
      tryCatch({
        zip::zipr(zipfile = zip_file, files = list.files(temp_subdir, full.names=T))
        showNotification("ZIP file created.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error creating ZIP file:", e$message), type="error")
        return()
      })
      
      # Download handler for ZIP file
      # Provide a download link for the ZIP file
      output$download_zip = downloadHandler(
        filename = function() {
          paste0("GS_bulkimport_", mycountry, mysite, "_", format(Sys.Date(), "%Y%m%d"), ".zip")
        },
        content = function(file) {
          file.copy(zip_file, file)
          

        }
      )
    })  # end of observeEvent(input$rename_images)
  })    # end of moduleServer
}       # end of tab1Server
