# iQuaCalc (Lite)

# ui.R


source('sidebarHelper.R')
source('bodyHelper.R')


# bootstrapPage(
  dashboardPage(
    # dashboardHeader(title = tags$b(tags$i('iQuaCalc (Lite)'))),
    dashboardHeader(title = 'iQuaCalc (Lite)'),
    
    dashboardSidebar(sbm),
    
    dashboardBody(
      
      tags$script('
                  $(".sidebar-toggle").on("click", function(){
                  $(window).trigger("resize");
                  })
                  '),
      
      # [PROBLEM] selectInput in splitLayout() -- as in wq_map_module.R -- "mis-behaves". The fix...
      # see: https://stackoverflow.com/questions/40077388/r-shiny-splitlayout-and-selectinput-issue
      tags$head(tags$style(HTML("
                                .shiny-split-layout > div {
                                overflow: visible;
                                }

                                .shiny-output-error-validation {
                                color: green; background-color: white; align: center; font-size: 120%; font-variant: small-caps;}

                                "))),
      
      # see: http://stackoverflow.com/questions/16970989/sliderinput-in-shiny-how-to-control-the-length-width-of-the-slider-in-shiny
      # tags$head(
      #   tags$style(type="text/css", ".irs { max-width: 600px; }")
      # ),
      includeCSS('www/custom.css'), 
      
      
      
      # initStore(id, namespace, privateKey = NULL)
      
      #         id -- name of the store on the INPUT REACTIVE OBJECT in the server function call
      #  namespace -- not as in traditional R, but the prefix when storing in the browser's local storage 
      #              It's the only way to separate data stored for different applications that are
      #              hosted by the same (sub-)domain
      # privateKey -- used to decrypt data; must be provided if you're going to have any encrypted fields
      initStore(        id = "store_iQlite_dash", 
                namespace  = "shinyStore-iQlite_dash", # Namespace must be unique to this application!
                privateKey = NULL),
      
      
      
      useShinyjs(),
      
      body)
  # )
)