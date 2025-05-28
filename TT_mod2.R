tab2UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Download patrol(s) shapefile from EarthRanger"),
    
    sidebarLayout(
      sidebarPanel(
        textInput(ns("bearer_token"), "My EarthRanger token", value=""),
   #     textInput(ns("csrf_token"), "CSRF Token (if needed)"),
   textInput(
     ns("username"), 
     label = HTML("My EarthRanger name<br><small>Enter your full name as it appears in EarthRanger (e.g., Courtney Marneweck not CMarneweck).</small>") ),
        dateInput(ns("date_start"), "Patrol(s) start date", value = Sys.Date() - 30),
        dateInput(ns("date_end"), "Patrol(s) end date", value = Sys.Date()),
   actionButton(ns("download_patrols"), "Fetch patrol(s)")
   
      ),
      
      mainPanel(
        verbatimTextOutput(ns("log")),
        downloadButton(ns("shp_download"), "Download shapefile")
      )
    )
  )
}

tab2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    patrol_sf <- reactiveVal(NULL)
    shapefile_path <- reactiveVal(NULL)
    user_id <- reactiveVal(NULL)
    
    # Set user_id based on input username
    observe({
      req(input$username)
      user_lookup <- list(
        "Frederike Otten" = "cd1abde8-0d9c-449f-9acb-2f201b8a0006",
        "Michael Brown" = "d01d16a3-ee87-4b73-9e8c-856ecf9fd536",
        "Emma Wells" = "8eb20288-2e90-4b91-8d36-092454ae4045",
        "Katie Ahl" = "f1e981a4-af77-4d04-9898-05c97c5430b3",
        "Elisante Kimambo" = "5291c0e0-7489-4eca-b8bb-c9977895754a",
        "Justin Didolanvi" = "31031739-54e3-4aeb-9888-93e8ca824b93",
        "Kalima Goodluck" = "456a5a49-e074-4344-ab08-24b9098cf8d8",
        "Tom Riffel" = "1bb71df6-b6e1-4e05-9f09-1e1d35768c41",
        "Adams Kipchumba" = "2b5037e7-94cd-4bad-a104-baa18f141f22",
        "Courtney Marneweck" = "5af7fa0e-b088-4e25-b21a-c14679c15d28",
        "Martina Kusters" = "2be6fff2-6236-4195-8322-6cb2918cb0e9",
        "Audi Shaanika" = "7bf4de82-f0af-43f3-bd39-e63e10270e8f",
        "Herbert Kasozi" = "8b4bddc4-18c6-4466-a8a1-8537d2175f65",
        "Julius Muge" = "5930d37d-0de5-4605-b9c9-7131f37f028d"
      )
      
      if (input$username %in% names(user_lookup)) {
        user_id(user_lookup[[input$username]])
      } else {
        user_id(NULL)
    #    output$log <- renderPrint("Username not found in list.")
      }
    })
    
    # Main logic: fetch, process, write shapefile
    observeEvent(input$download_patrols, {
      req(input$bearer_token, user_id())
      
      # Build filter
      filter_encoded <- paste0(
        '{"date_range":{"lower":"', input$date_start, 'T00:00:00.000Z","upper":"',
        input$date_end, 'T23:59:59.999Z"}}'
      ) %>% URLencode(reserved = TRUE)
      
      # Fetch patrol metadata
      meta <- GET(
        paste0("https://twiga.pamdas.org/api/v1.0/activity/patrols/?page_size=100&filter=", filter_encoded),
        add_headers(Authorization = paste("Bearer", input$bearer_token))
      ) %>%
        content("text", encoding = "UTF-8") %>%
        fromJSON(flatten = TRUE) %>%
        pluck("data", "results") %>%
        unnest(patrol_segments, keep_empty = TRUE, names_repair = "unique") %>%
        mutate(
          end_time = ymd_hms(time_range.end_time),
          start_time = ymd_hms(time_range.start_time),
          created_at = ymd_hms(leader.created_at),
          updated_at = ymd_hms(leader.updated_at)
        ) %>%
        select(
          patrol_id = "id...1",
          patrol_name = "title",
          patrol_type,
          user_id = "leader.id",
          user_name = "leader.name",
          patrol_start_dt = "start_time",
          patrol_end_dt = "end_time",
          patrol_start_y = "start_location.latitude",
          patrol_start_x = "start_location.longitude",
          patrol_end_y = "end_location.latitude",
          patrol_end_x = "end_location.longitude"
        )
      
      # Fetch track
      track <- GET(
        paste0("https://twiga.pamdas.org/api/v1.0/subject/", user_id(), "/tracks/?since=", input$date_start, "T00:00:00.000Z"),
        add_headers(Authorization = paste("Bearer", input$bearer_token))
      ) %>%
        content("text", encoding = "UTF-8") %>%
        fromJSON(flatten = TRUE) %>%
        pluck("data", "features") %>%
        unnest(
          cols = c(geometry.coordinates, properties.coordinateProperties.times),
          keep_empty = TRUE
        ) %>%
        mutate(
          dttm = ymd_hms(properties.coordinateProperties.times),
          x = geometry.coordinates[, 1],
          y = geometry.coordinates[, 2]
        ) %>%
        left_join(meta, by = c("properties.id" = "user_id")) %>%
        filter(dttm >= patrol_start_dt, dttm <= patrol_end_dt) %>%
        st_as_sf(coords = c("x", "y"), crs = 4326) %>%
        group_by(patrol_id) %>%
        summarise(
          user_name = first(properties.title),
          dttm = first(dttm),
          patrol_name = first(patrol_name),
          patrol_type = first(patrol_type),
          geometry = st_combine(geometry)
        ) %>%
        st_cast("LINESTRING")
      
      track <- st_make_valid(track)
      patrol_sf(track)
      
      # Write and zip shapefile
      shp_dir <- file.path(tempdir(), "shp")
      dir.create(shp_dir, showWarnings = FALSE)
      # construct file name structure
      initials = str_replace_all(toupper(str_extract_all(input$username, "\\b\\w")[[1]] %>% paste0(collapse = "")), "[^A-Z]", "")
      layer_name <- paste0("GCF_ERtrk_", initials, "_", format(input$date_start, "%y%m%d"), "_", format(input$date_end, "%y%m%d"))
      
      st_write(track, dsn = shp_dir, layer = layer_name, driver = "ESRI Shapefile", delete_layer = TRUE)
      Sys.sleep(1) # Give the OS a moment to flush files
      
      # List only the required shapefile components
      required_ext <- c("shp", "shx", "dbf", "prj")
      shp_files <- file.path(shp_dir, paste0(layer_name, ".", required_ext))
      
      # Check that all files exist
      if(!all(file.exists(shp_files))) {
        stop("Not all shapefile components were written. Check for .shp, .shx, .dbf, .prj files.")
      }
      shp_files <- normalizePath(shp_files, winslash = "/") # normalise shp paths
      
      zip_path <- tempfile(fileext = ".zip")
      zip::zipr(zipfile = zip_path, files = shp_files)
      
      shapefile_path(zip_path)
      output$log <- renderPrint("Zipped patrol shapefile created")
    })
    
    # Download handler (must be outside observeEvent!)
    output$shp_download <- downloadHandler(
      filename = function() {
        paste0("GCF_ERtrk_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        path <- shapefile_path()
        if (is.null(path) || !file.exists(path)) {
          stop("Shapefile ZIP does not exist or path is NULL.")
        }
        file.copy(path, file)
      }
    )
    
  })  # end moduleServer
}  # end tab2Server
