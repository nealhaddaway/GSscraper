library(shiny)
library(RCurl)
library(cfhttr)
library(purrr)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(textclean)
library(stringi)
library(mgsub)
library(magrittr)
library(zen4R)
library(shinyalert)
library(shinybusy)
library(strex)

source('buildGSlinks.R')
source('save_htmls.R')
source('scrapeGS.R')
source('decode_dois.R')
source('build_ris.R')

# Define UI for application that draws a histogram
ui <- navbarPage("GSscraper", id = "tabs",

                 tabPanel("Home",
                          fluidRow(
                              column(10,
                                     h2('GSscraper'),
                                     br(),
                                     'Welcome to GSscraper, a web-based tool for extracting and reporting searches of Google Scholar.',
                                     br(),
                                     br(),
                                     'You can use this tool to perform structured searches of Google Scholar. Google Scholar is not a transparent or repeatable source of bibliographic data and should not be used as a primary source of information in a systematic review or rapid review. However, it can be a useful supplementary source when used alongside a comprehensive set of bibliographic databases. GSscraper helps you to be as transparent as possible in how you conduct and report your searches of Google Scholar, and helps you to download and store bibliographic data by exporting all search results as CSV or RIS data.',
                                     br(),
                                     br(),
                                     'The URLs of Google Scholar search results contain advanced search terms in a predictable and patterned manner, and we can use this pattern to design our own despoke search links:',
                                     br(),
                                     br(),
                                     tags$img(src="buildGSlinks.png", width = 800),
                                     br(),
                                     br(),
                                     hr(),
                                     br(),
                                     'To get started, enter your search terms in the "Search" tab. Next, download the pages of search results in the "Save HTMLs" tab. Finally, scrape the downloaded files in the "Scrape data" tab. You can then download a CSV / RIS file containing your search results and publish your search history and results to Zenodo.',
                                     br(),
                                     br(),
                                     tags$img(src="process.png", width = 800),
                                     br(),
                                     br(),
                                     'Be aware that repetitive searching may result in a temporary block from Google Scholar (your table of search results may be blank or missing some data). Please use this tool responsibly.',
                                     br(),
                                     br(),
                                     'Thanks for your interest!',
                                     br(),
                                     br(),
                                     hr(),
                                     br(),
                                     'GSscraper was produced by Neal Haddaway.', tags$a(href="https://github.com/nealhaddaway/GSscraper/", 'The code is available on Github'), '.',
                                     br(),
                                     br(),
                                     'Please cite as: Haddaway, NR (2022) GSscraper: An R package and Shiny app for exporting search results from Google Scholar. Zenodo. ', tags$a(href="https://doi.org/10.5281/zenodo.6458134", "10.5281/zenodo.6458134")
                                     ),
                              column(2,
                                     br(),tags$img(height = 150, src = "https://raw.githubusercontent.com/nealhaddaway/GSscraper/main/inst/shiny-examples/GSscraper/www/hex.png")),
                              add_busy_spinner(spin = "fading-circle", color = "#4c90fe", margins = c(70, 20))
                              )
                          ),

                 tabPanel("Search",
                          fluidRow(
                              column(10,
                                     h2('Build and check your searches'),
                                     br(),
                                     'Use this tab to build and check your searches. Your draft links will be printed below the data entry form.',
                                     br(),
                                     hr()),
                              column(7,
                                     div(
                                     style = "background-color:#f5f5f5;",
                                     p(style="color: #666; font-family: Arial,sans-serif; text-overflow: ellipsis; flex: 1 1 auto; font-size: 18px; text-align: center; padding-top: 10px; padding-bottom: 10px;",
                                       'Advanced search'))
                                     ),
                              column(7,
                                     br(),
                                     p(style = 'text-align: left;',
                                       strong('Find articles'),br(),
                                       tags$table(width = "100%",
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'with ', strong('all'), ' of the words'),
                                                   tags$td(width = "60%", textInput('and_terms', NULL, width =  "100%"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'with the ', strong('exact phrase')),
                                                   tags$td(width = "60%", textInput('exact_phrase', NULL, width =  "100%"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'with ', strong('at least one'), 'of the words'),
                                                   tags$td(width = "60%", textInput('or_terms', NULL, width =  "100%"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", strong('without'), ' the words'),
                                                   tags$td(width = "60%", textInput('not_terms', NULL, width =  "100%"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'where my words will occur'),
                                                   tags$td(width = "60%", radioButtons('anywhere', NULL,
                                                                                       choices = c('anywhere in the article', 'in the title of the article'),
                                                                                       selected = 'anywhere in the article'))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'return articles ', strong('authored'), ' by'),
                                                   tags$td(width = "60%", textInput('authors', NULL, width =  "100%", placeholder = "e.g., \"PJ Hayes\" or McCarthy"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'return articles ', strong('published'), ' in'),
                                                   tags$td(width = "60%", textInput('source', NULL, width =  "100%", placeholder = "e.g., J Biol Chem or Nature"))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'return articles ', strong('dated'), ' between'),
                                                   tags$td(width = "60%",
                                                           splitLayout(textInput('year_from', NULL, width =  "100%", placeholder = "e.g., 1996"), p(style = 'text-align: center;',' - '),
                                                                       textInput('year_to', NULL, width =  "100%", placeholder = "e.g., 1996"),
                                                                       cellWidths = c('45%', '5%', '45%')))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'scrape pages', strong('from'), ' - ', strong('to')),
                                                   tags$td(width = "60%",
                                                           splitLayout(numericInput('start_page', NULL, width =  "100%", value = 1, max = 100, step = 1), p(style = 'text-align: center;',' - '),
                                                                       numericInput('end_page', NULL, width =  "100%", value = 1, max = 100, step = 1),
                                                                       cellWidths = c('45%', '5%', '45%')))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'search ', strong('language')),
                                                   tags$td(width = "60%", textInput('language', NULL, width =  "100%", placeholder = "e.g., en", value = 'en'))
                                           ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'include ', strong('citations')),
                                                   tags$td(width = "60%", checkboxInput('incl_cit', NULL, value = TRUE))
                                                   ),
                                           tags$tr(width = "100%",
                                                   tags$td(width = "40%", 'include ', strong('patents')),
                                                   tags$td(width = "60%", checkboxInput('incl_pat', NULL, value = TRUE))
                                                   )),
                                           br(),
                                           p(style = 'text-align: center;',
                                             actionButton("google", "Build search links", icon("search"),
                                                          style="color: #fff; background-color: #4c90fe")
                                             )
                                       )
                                     ),
                              column(12,
                                     br(),
                                     uiOutput('preview_text')
                                     ),
                              add_busy_spinner(spin = "fading-circle", color = "#4c90fe", margins = c(70, 20))
                          )
                 ),

                 tabPanel("Save HTMLs",
                          fluidRow(
                              column(10,
                                     h2('Download your search results as HTML files'),
                                     br(),
                                     'Click the button below to save each page of search results for scraping in the next step.',
                                     br(),
                                     br(),
                                     strong('Specify the delay in seconds between file saves'),' (this can reduce the chances of being blocked by Google Scholar)',br(),
                                     numericInput('pause', NULL, value = 3, width = 75, step = 0.5),
                                     hr(),
                                     conditionalPanel(
                                         condition='input.google!=null && input.google!=""',
                                         actionButton("download_HTMLs", "Save search results")
                                         ),
                                     conditionalPanel(
                                         condition='input.download_HTMLs!=null && input.download_HTMLs!=""',
                                         br(),
                                         br(),
                                         downloadButton('report_download', 'Download search history file', icon = icon("file-download"))
                                         ),
                                     br(),
                                     br(),
                                     shinyjs::useShinyjs(),
                                     textOutput("text"),
                                     br(),
                                     br(),
                                     textOutput('save_report')
                                     ),
                              add_busy_spinner(spin = "fading-circle", color = "#4c90fe", margins = c(70, 20))
                          )
                 ),

                 tabPanel("Scrape data",
                          fluidRow(
                              column(10,
                                     h2('Scrape data from the downloaded search results'),
                                     br(),
                                     'Now, we can scrape search results based on patterns in the HTML code.',
                                     hr(),
                                     br(),
                                     conditionalPanel(
                                         condition='input.download_HTMLs!=null && input.download_HTMLs!=""',
                                         actionButton("scrape_HTMLs", "Scrape the results HTMLs"),
                                         br(),
                                         br(),
                                         textOutput('scrape_report'),
                                         br(),
                                         br(),
                                         conditionalPanel(
                                             condition='input.scrape_HTMLs!=null && input.scrape_HTMLs!=""',
                                             uiOutput('download_buttons'),
                                             uiOutput('citation_outcome')
                                             )
                                         ),
                                     br(),
                                     dataTableOutput('data'),
                                     br()
                              ),
                              add_busy_spinner(spin = "fading-circle", color = "#4c90fe", margins = c(70, 20))
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()

    #on pressing google, generate preview
    observeEvent(input$google, {

        rv$and_terms <- if(is.null(input$and_terms)==TRUE){''}else{input$and_terms}
        rv$exact_phrase <- if(is.null(input$exact_phrase)==TRUE){''}else{input$exact_phrase}
        rv$or_terms <- if(is.null(input$or_terms)==TRUE){''}else{input$or_terms}
        rv$not_terms <- if(is.null(input$not_terms)==TRUE){''}else{input$not_terms}
        rv$year_from <- if(is.null(input$year_from)==TRUE){''}else{input$year_from}
        rv$year_to <- if(is.null(input$year_to)==TRUE){''}else{input$year_to}
        rv$start_page <- if(is.null(input$start_page)==TRUE){''}else{as.numeric(input$start_page)}
        rv$pages <- if(is.null(input$end_page)==TRUE){''}else{as.numeric(input$end_page) - as.numeric(input$start_page) + 1}
        rv$language  <- input$language
        rv$incl_cit <- input$incl_cit
        rv$incl_pat <- input$incl_pat
        rv$titlesearch <- if(input$anywhere == 'in the title of the article'){TRUE} else {FALSE}
        rv$authors <- if(is.null(input$authors)==TRUE){''}else{input$authors}
        rv$source <- if(is.null(input$source)==TRUE){''}else{input$source}

        rv$links <- buildGSlinks(and_terms = rv$and_terms,
                                 exact_phrase = rv$exact_phrase,
                                 or_terms = rv$or_terms,
                                 not_terms = rv$not_terms,
                                 year_from = rv$year_from,
                                 year_to = rv$year_to,
                                 start_page = rv$start_page,
                                 pages = rv$pages,
                                 authors = rv$authors,
                                 source = rv$source,
                                 language = rv$language,
                                 incl_cit = rv$incl_cit,
                                 incl_pat = rv$incl_pat,
                                 titlesearch = rv$titlesearch)

        rv$links_report <- rv$links$report
        rv$links <- rv$links$link

        rv$links_tab <- data.frame(link = rv$links)

        #show preview of links
        output$preview <- renderTable({
            table <- rv$links_tab
            table$link <- paste0('<a href="', rv$links_tab$link, ' target="_blank">', rv$links_tab$link, '</a>')
            table
        }, sanitize.text.function = function(x) x)

        #render preview UI
        output$preview_text <- renderUI({
            tagList(
                "If you're happy with these links, proceed to the 'Results' tab to see your search results",
                br(),
                br(),
                tableOutput('preview')
            )
        })

    })

    #prepare HTML files for scraping
    observeEvent(input$download_HTMLs, {

        withCallingHandlers({
            shinyjs::html("text", "")
            htmls <- list()
            message('Initialising...')
            t0 <- Sys.time()
            for(i in 1:length(rv$links)){
                message(paste0('Saving page ', i, '...'))
                html <- save_html((rv$links)[i], pause = input$pause, backoff = FALSE)

                #if last html contains a CAPTCHA, pause
                if(grepl('CAPTCHA', html)  == TRUE){
                    showModal(modalDialog(
                        title = "You've been blocked ☹",
                        'It looks like Google Scholar has temporarily blocked you. Please navigate to Google Scholar to solve the CAPTCHA.',
                        br(),
                        br(),
                        'Pausing for 2 minutes...',
                        easyClose = TRUE,
                        footer = NULL
                    ))
                    rv$captcha <- TRUE
                    Sys.sleep(120)
                }

                htmls <- c(htmls, html)
                message(paste0('Page ', i, ' saved.'))
            }
            t1 <- Sys.time()
            response_delay <- round(as.numeric(t1-t0), 3)
            message(paste('Completed in',
                          round(response_delay, 3),
                          'seconds.'))
            rv$htmls <- htmls
        },
        message = function(m) {
            shinyjs::html(id = "text", html = m$message, add = TRUE)})

        output$save_report <- renderText({
            paste0(length(rv$htmls),' pages of results successfully downloaded. Proceed to the "Scrape data" tab to extract search results and download the final dataset.')
        })

        # download search report
        output$report_download <- downloadHandler(
            filename = function(){
                paste("GSscraper_searchRecord_", Sys.Date(), ".txt", sep = "")
            },
            content = function(file) {
                writeLines(rv$links_report, file)
            }
        )

    })


    #scrape HTML files
    observeEvent(input$scrape_HTMLs, {

        df <- data.frame()
        report_full <- ''
        report_page <- data.frame()
        for(i in 1:length(rv$htmls)){
            info <- get_info(unlist(rv$htmls[i]))
            data <- info$df
            report_full <- info$report$full
            report_page <- rbind(report_page, data.frame(info$report$page))
            df <- dplyr::bind_rows(df, data)
        }
        rv$report_full <- report_full
        rv$report_pages <- report_page
        df <- df[!duplicated(df), ] #remove duplicate rows
        df <- df[rowSums(is.na(df)) != ncol(df), ] #remove NA rows

        rv$data <- df

        output$data <- renderDataTable({
            table <- rv$data
            table[is.na(table)] <- ''
            table$links <- paste0('<a href="', table$links, ' target="_blank">', table$links, '</a>')
            table$ft_link_url <- paste0('<a href="', table$ft_link_url, ' target="_blank">', table$ft_link_url, '</a>')
            names(table)[names(table) == 'ft_link_type'] <- 'type'
            names(table)[names(table) == 'ft_link_source'] <- 'source'
            names(table)[names(table) == 'ft_link_url'] <- 'full text link'
            table
        }, escape = FALSE)


        output$scrape_report <- renderText({
            n_results <- nrow(rv$data)
            min_expected <- (length(rv$links) - 1) * 10
            if((n_results > min_expected) == FALSE){
                rv$ready4Zenodo <- FALSE
                paste0('A total of ', nrow(rv$data),' search results have been exported and are shown in the table below. This means that some of your links did not bring back any recognisable search results. Please check the links to ensure they work and you have not been temporarily blocked by Google.')
            } else {
                rv$ready4Zenodo <- TRUE
                paste0('A total of ', nrow(rv$data),' search results have been exported and are shown in the table below.')
            }
        })

        #if number of results demonstrates all pages scraped successfully, allow user to post to Zenodo
        observeEvent(input$post2Zenodo, {
            showModal(modalDialog(
                title = "Publish on Zenodo",
                'If you have finalised your search, you can post your search record and results on Zenodo to get a citable record with a Digital Object Identifier. Just enter your name, affiliation, ORCID and keywords below:',
                br(),br(),
                uiOutput('warning'),
                textInput('name', 'Your name'),
                textInput('affiliation', 'Your affiliation'),
                textInput('orcid', 'Your ORCID (if you have one)'),
                textInput('keywords', 'Keywords for your search (comma separated)'),
                actionButton('publish', 'Publish to Zenodo'),
                uiOutput('saving_zenodo'),
                uiOutput('citation'),
                easyClose = TRUE,
                footer = NULL
                ))
            })

        #if publish button is pushed
        observeEvent(input$publish, {

            if(identical(input$name, '') == TRUE){
                #warning text that input data is wrong
                output$warning <- renderUI({
                    tagList(p('You must enter a name to post your record',
                              style = "color:red;")
                            )
                })
                rv$zenodo_success <- FALSE
            } else {
                zenodo <- zen4R::ZenodoManager$new(
                    token = '9NdaVzTEqKO86zyrvCbi7IFtRJ1lEFm02s5srtQyVF84HQ85sgoql25K6ku1',
                    logger = "INFO" # use "DEBUG" to see detailed API operation logs, use NULL if you don't want logs at all
                )

                # Create record
                # Generate local empty record not yet deposited on Zenodo
                rv$title <- paste0('Google Scholar search record using GSscraper app: ', Sys.Date())

                # Get detailed help here: ?ZenodoRecord
                myrec <- zenodo$createEmptyRecord()
                myrec$setPublicationDate(as.Date(Sys.Date()))
                myrec$setAccessRight("open")
                myrec$setTitle(rv$title)
                myrec$setDescription(rv$links_report)
                myrec$setUploadType("dataset")
                if(identical(input$affiliation, '') == FALSE && identical(input$orcid, '') == FALSE){
                    myrec$addCreator(name = input$name,
                                     affiliation = input$affiliation,
                                     orcid = input$orcid)
                }
                if(identical(input$affiliation, '') == TRUE && identical(input$orcid, '') == TRUE){
                    myrec$addCreator(name = input$name)
                }
                if(identical(input$affiliation, '') == FALSE && identical(input$orcid, '') == TRUE){
                    myrec$addCreator(name = input$name,
                                     affiliation = input$affiliation)
                }
                if(identical(input$affiliation, '') == TRUE && identical(input$orcid, '') == FALSE){
                    myrec$addCreator(name = input$name,
                                     orcid = input$orcid)
                }
                if(identical(input$keywords, '') == FALSE){
                    myrec$setKeywords(c(unlist(strsplit(input$keywords, ', '))))
                }
                # Deposit on zenodo
                myrec <- zenodo$depositRecord(myrec)

                # Upload file - get detailed help here: ??Zenodo
                write.csv(rv$data, 'results.csv', row.names = FALSE)
                zenodo$uploadFile('results.csv', myrec)
                writeLines(rv$links_report, 'GSscraper search report.txt')
                zenodo$uploadFile('GSscraper search report.txt', myrec)

                # Find out doi from Zenodo
                rv$doi <- myrec$metadata$prereserve_doi$doi

                #build citation
                rv$citation <- paste0(
                    input$name,
                    ' (', substr(Sys.Date(), 1, 4), ') ',
                    rv$title,
                    '. Zenodo. https://doi.org/', rv$doi
                )
                #build ris file for citation
                rv$zenodo_ris <- paste0('\n',
                                        'TY  - DATA', '\n',
                                        'AU  - ', input$name, '\n',
                                        'TI  - ', rv$title, '\n',
                                        'PY  - ', substr(Sys.Date(), 1, 4), '\n',
                                        'AB  - ', rv$links_report, '\n',
                                        'DO  - ', rv$doi, '\n',
                                        'PB  - GSscraper and Zenodo', '\n',
                                        'ER  - ')
                #download handler for citation
                output$zenodo_ris_download <- downloadHandler(
                    filename = function() {
                        paste("search_record.ris", sep = "")
                    },
                    content = function(file) {
                        write.table(rv$zenodo_ris, file, col.names=FALSE, row.names=FALSE, quote=FALSE)
                        }
                )
                #publish full record to Zenodo
                myrec <- zenodo$publishRecord(myrec$id)

                #check if published OK
                Sys.sleep(1)
                check <- zenodo$getRecordById(myrec$id) #currently not working - triggering FALSE below even when a record has posted successfully

                if(is.null(check) == FALSE){
                    rv$zenodo_success <- TRUE
                } else {
                    rv$zenodo_success <- FALSE
                }
            }

            #render citation report and ris download button
            if(rv$zenodo_success == TRUE){
                #download button
                output$citation_outcome <- renderUI({
                    tagList(
                        br(),
                        downloadButton('zenodo_ris_download', 'Save your Zenodo citation')
                    )
                })
                #citation report
                output$citation <- renderUI({
                    if(is.na(rv$doi) == FALSE){
                        tagList(
                            br(),
                            br(),
                            'Successfully published to Zenodo. Here is your citation:',
                            br(),
                            rv$citation
                        )
                    }
                })
            } else {
                #failure report
                output$citation_outcome <- renderUI({
                    tagList(
                        'Your search could not be posted to Zenodo. Please check everything is in order and try again'
                    )
                })
            }

        })

        #download handler for CSV file
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("results.csv", sep = "")
            },
            content = function(file) {
                write.csv(rv$data, file, row.names = FALSE)
                }
        )


        #if downloaded HTMLs contained a blocking error (403) do nothing
        if(grepl('Error 403', rv$htmls[[length(rv$htmls)]]) == TRUE){

        } else {
            rv$ris <- build_ris(rv$data)

            # download articles as RIS
            output$ris_download <- downloadHandler(
                filename = function(){
                    paste("GS_results_", Sys.Date(), ".ris", sep = "")
                },
                content = function(file) {
                    write.table(rv$ris, file, col.names=FALSE, row.names=FALSE, quote=FALSE)
                }
            )

            #UI for download buttons
            output$download_buttons <- renderUI({
                tagList(
                    downloadButton('downloadData', 'Download results as CSV', icon = icon("file-download")),
                    br(),
                    br(),
                    downloadButton('ris_download', 'Download results as RIS', icon = icon("file-download")),
                    br(),
                    br(),
                    actionButton('post2Zenodo', 'Post search record and results to Zenodo')
                )
            })

        }

    })


}

# Run the application
shinyApp(ui = ui, server = server)
