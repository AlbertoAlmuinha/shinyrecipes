#' @title Recipes Gadget
#' @name recipesGadget
#' @description Gadget to use the data preprocessing 'recipes' package interactively.
#' @author Alberto Almuiña
#' @param .df data frame, data table or tibble on which the preprocessing steps will be trained.
#' @param bake_list A named list where each element is a data.frame, data.table or tibble on which the selected preprocessing steps can be applied.
#' @return
#' Returns a list containing the trained recipe as well as all the data to which the recipe has been applied within the gadget.
#' @export
#' @examples
#' \dontrun{
#' recipesGadget(.df = mtcars, bake_list = list('mtcars1' = mtcars1))
#' }


recipesGadget<-function(.df, bake_list = NULL){

  if(is.null(names(bake_list)) == T & is_list(bake_list)==T){stop('"bake_list" must be a named list.')}

  if(is_list(bake_list)==F & is.null(bake_list)==F){'"bake_list" must be a list or NULL.'}

  name<-deparse(substitute(.df))

  list_names<-purrr::map_chr(bake_list, ~{deparse(substitute(.x))})

  ui<-miniPage(

    ################### TITLE ##########################################

    setBackgroundImage(src = 'https://i.picsum.photos/id/1015/6000/4000.jpg'),
    gadgetTitleBar(title = 'Recipes Gadget',
                   right = miniTitleBarButton("done", "Accept", primary = TRUE),
                   left = miniTitleBarButton("cancel", 'Cancel', primary = TRUE)),

    ####################################################################

    miniTabstripPanel(

    ########################## PANEL RECIPE ############################

      miniTabPanel('Recipe', icon = icon('table'),

                   miniContentPanel(

                     dropdownButton(
                       tags$h4("How to use it?"),
                       hr(),
                       tags$p('The objective of this panel is to create the formula
                              from which the recipe will be created. The formula is made up
                              of predictor and outcome variables. For example:\n
                              out1 + out2 ~ pre1 + pre2 + pre3...'),
                       circle = TRUE, status = "primary", icon = icon("info"), width = "200px",
                       tooltip = tooltipOptions(title = "Click to see information...")
                     ),

                     bucket_list(
                       header = HTML('<h2 style="color:white; font-family:georgia;"><center><b> Recipes Formula </b></center></h2>'),
                       group_name = "recipe_formula",
                       orientation = "horizontal",
                       class = c("default-sortable", "custom-sortable"),
                       add_rank_list(
                         text = tags$b("Drag the columns"),
                         labels = c('.', names(.df)),
                         input_id = "columns"
                       ),
                       add_rank_list(
                         text = tags$b("Predictor Variables"),
                         labels = NULL,
                         input_id = "predictors"
                       ),
                       add_rank_list(
                         text = tags$b('Outcome Variables'),
                         labels = NULL,
                         input_id = 'outcomes'
                       )
                     ),
                     tags$style(
                                HTML("
                                      .rank-list-container.custom-sortable {
                                        background-color: #9999ff;
                                        min-height: 300px;
                                        max-height: 300px;
                                        overflow: auto;
                                      }
                                      .custom-sortable .rank-list-item {
                                        background-color: white;
                                      }")
                     ),

                     hr(),

                       actionBttn('set_recipe', 'Set Recipe'),

                     tags$head(
                       tags$style(HTML('#set_recipe{margin-left: 300px;'))
                     )

                   )),

    ########################## PANEL ANALYSIS ############################

    miniTabPanel('Analysis', icon = icon('chart-bar'),

                 miniContentPanel(

                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                   dropdownButton(
                     tags$h4("How to use it?"),
                     hr(),
                     tags$p('The objective of this panel is to assign the desired preprocessing
                             steps to each variable.'),
                     circle = TRUE, status = "primary", icon = icon("info"), width = "200px",
                     tooltip = tooltipOptions(title = "Click to see information...")
                   )),

                   div(style="display: inline-block;vertical-align:top; width: 400px;",HTML("<br>")),

                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                   switchInput(inputId = 'esquisse',
                               label = tags$b('esquisse'),
                               offStatus = 'danger',
                               value = FALSE)),

                   tags$head(
                     tags$style(HTML('#esquisse{margin-left: 1000px;'))
                   ),

                   hr(),

                   DT::dataTableOutput('table')
        )),

    ########################## PANEL ROLE/STEPS ############################

    miniTabPanel('Roles/Steps', icon = icon('shoe-prints'),

                 miniContentPanel(

                      dropdownButton(
                         tags$h4("How to use it?"),
                         hr(),
                         tags$p('The objective of this panel is to assign the desired preprocessing
                             steps to each variable.'),
                         circle = TRUE, status = "primary", icon = icon("info"), width = "200px",
                         tooltip = tooltipOptions(title = "Click to see information...")
                       ),

                      hr(),

                      div(style="display: inline-block;vertical-align:top; width: 200px;",
                      pickerInput(inputId = 'role_cols',
                                  label = 'Column to update role:',
                                  choices = names(.df),
                                  choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(dim(.df)[2]/2))),
                                  options = list(`actions-box` = TRUE, size = 6),
                                  multiple = TRUE
                                  )),

                      div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),

                      div(style="display: inline-block;vertical-align:top; width: 150px;",
                          textInput(inputId = 'role_text',
                                    label = 'New role:',
                                    value = 'Write a rol...')),

                      div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),

                      div(style="display: inline-block;width: 150px;margin-top:20px",
                          actionBttn(inputId = 'role_update',
                                     label = 'Update Role')),

                      hr(),

                      div(style="display: inline-block;vertical-align:top; width: 200px;",
                          pickerInput(inputId = 'steps',
                                      label = HTML('<h4 style="color:white"><b> Select One Step: </b></h4>'),
                                      choices = ls('package:recipes', pattern = '^step_'),
                                      choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(ls('package:recipes', pattern = '^step_'))/2))),
                                      options = list(size = 6, `live-search` = TRUE)
                          )),

                      div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),

                      div(style="display: inline-block;vertical-align:top; width: 150px;",
                          pickerInput(inputId = 'steps_select',
                                      label = HTML('<h4 style="color:white"><b> Select: </b></h4>'),
                                      choices = c('all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                  'all_nominal()', names(.df)),
                                      options = list(size = 6, style='btn-success'),
                                      choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(c('all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                                                                                                                                    'all_nominal()', names(.df)))/2)))
                                      )),

                      div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),

                      div(style="display: inline-block;vertical-align:top; width: 150px;",
                          pickerInput(inputId = 'steps_deselect',
                                      label = HTML('<h4 style="color:white"><b> Deselect: </b></h4>'),
                                      choices = c('None','all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                  'all_nominal()', names(.df)),
                                      options = list(size = 6, style = 'btn-danger'),
                                      choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(c('None','all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                                                                                                                                    'all_nominal()', names(.df)))/2)))
                                      )),

                      div(style="display: inline-block;vertical-align:top;width:150px;margin-top:40px;margin-left:200px",
                          actionBttn(inputId = 'step_update',
                                     label = 'Add Step')),

                      div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),

                      div(style="display: inline-block;vertical-align:top;width:150px;margin-top:40px;",
                          actionBttn(inputId = 'step_wizard',
                                     label = 'Step Wizard')),

                      hr(),

                      div(style="display: inline-block;vertical-align:top;width:150px;margin-left:300px;margin-top:20px;",
                          actionBttn(inputId = 'formula',
                                     label = 'Confirm'))

                          )



    ),

    ########################## PANEL PREP/BAKE ############################

    miniTabPanel(title = 'Prep/Bake', icon = icon('cookie-bite'),

                 miniContentPanel(

                   dropdownButton(
                     tags$h4("How to use it?"),
                     hr(),
                     tags$p('The objective of this panel is to train your recipe on one dataset
                            and then apply this recipe to other datasets.'),
                     circle = TRUE, status = "primary", icon = icon("info"), width = "200px",
                     tooltip = tooltipOptions(title = "Click to see information...")
                   ),

                   hr(),

                   div(style="display: inline-block;vertical-align:top;width:400px;overflow:auto",
                   verbatimTextOutput('code')),

                   div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),

                   div(style="display: inline-block;vertical-align:top;width:150px;margin-top:30px",
                       actionBttn(inputId = 'prep',
                                  label = 'Prep')),

                   tags$head(
                     tags$style(HTML('#code{max-height:100px;min-height:100px;'))
                   ),

                   hr(),

                   div(style="display: inline-block;vertical-align:top; width: 200px;",
                       pickerInput(inputId = 'bake_list',
                                   label = HTML('<h4 style="color:#1aff1a"><b> Select data to bake: </b></h4>'),
                                   choices = c(name, names(bake_list)),
                                   choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(c(name, names(bake_list)))/2))),
                                   options = list(`actions-box` = TRUE, size = 6, style='btn-success'),
                                   multiple = TRUE
                       )),

                   div(style="display: inline-block;vertical-align:top; width: 320px;",HTML("<br>")),

                   div(style="display: inline-block;vertical-align:top;width:150px;margin-top:40px;",
                       actionBttn(inputId = 'bake',
                                  label = 'Bake'))


                 )

                 )

    )
  )

  server<-function(input, output, session, .x = .df, .y = bake_list, .name = name){

    .y[[.name]]<-.x

    ########################## PANEL RECIPE ############################

    observeEvent(input$set_recipe,{sendSweetAlert(session,
                                                  title = 'Success',
                                                  type = 'success',
                                                  text = 'Recipe Created!')})

    .recipe<-eventReactive(input$set_recipe, {

      .f<-reformulate(termlabels = paste(input$predictors, collapse = ' + '),
                  response = paste(input$outcomes, collapse = ' + '))

      recipe(.f, .x)

    })

    ########################## PANEL ANALYSIS ############################

    job_id<-reactiveVal('')

    observeEvent(input$esquisse,{
      if(isTRUE(input$esquisse)){
        a<-rstudioapi::terminalExecute(command = str_glue("Rscript {system.file('script', 'esquisse.R', package = 'recipesgadget')} {.name}"))

        job_id(a)
      }

      if(isFALSE(input$esquisse) & job_id()!=''){
        rstudioapi::terminalKill(id = job_id())
      }
    })

    options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

    .x_table<-purrr::map_df(.x, tidyr::replace_na, replace="<span style=\"color:red;\">NA</span>")

    colnames(.x_table)<-stringr::str_glue('<span style="color:white; font-size:20px">{colnames(.x_table)}</span>')

    output$table<-DT::renderDataTable(DT::datatable(.x_table, options = list(searching = F,
                                                               pageLength = 8,
                                                               info = FALSE,
                                                               scrollX = TRUE,
                                                               lengthChange = FALSE),
                                            style = 'bootstrap4',
                                            filter = 'top',
                                            rownames = FALSE,
                                            escape = FALSE) %>%
                                  DT::formatStyle(columns = names(.x_table), color = 'white',
                                              backgroundColor = '#9999ff',
                                              fontWeight = 'bold'))

    ########################## PANEL ROLES/STEPS ############################

    ############# ROLES ##################

    n_role<-reactiveVal(0)
    n_step<-reactiveVal(0)
    n_param<-reactiveVal(0)
    val<-reactiveValues()

    observeEvent(input$role_update,{

      if(is.null(input$role_cols) | is.null(input$role_text)){sendSweetAlert(session,
                                                                             title = 'Error',
                                                                             type = 'error',
                                                                             text = 'You have not selected a column to update the role or have not written a role.')}

      if(!is.null(input$role_cols) & !is.null(input$role_text)){

        n_role(n_role()+1)

        if(n_role()==1){
          val$role_cols<-input$role_cols
          val$role_text<-input$role_text
        } else {

          val$role_cols<-c(val$role_cols, input$role_cols)
          val$role_text<-c(val$role_text, input$role_text)}

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Role Updated!')

      }

    })

    ############# STEPS ##################

    observeEvent(input$role_update,{

      updatePickerInput(session = session,
                        inputId = 'steps_select',
                        choices = c('all_outcomes()', 'all_predictors()', 'all_numeric()',
                                    'all_nominal()', names(.df), purrr::map_chr(val$role_text, ~{str_glue('has_role("{.x}")')})),
                        choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(c('all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                                                                                                                      'all_nominal()', names(.df),purrr::map_chr(val$role_text, ~{str_glue('has_role("{.x}")')}) ))/2)))
                        )

      updatePickerInput(session = session,
                        inputId = 'steps_deselect',
                        choices = c('None','all_outcomes()', 'all_predictors()', 'all_numeric()',
                                    'all_nominal()', names(.df), purrr::map_chr(val$role_text, ~{str_glue('has_role("{.x}")')})),
                        choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(c('None','all_outcomes()', 'all_predictors()', 'all_numeric()',
                                                                                                                                                      'all_nominal()', names(.df),purrr::map_chr(val$role_text, ~{str_glue('has_role("{.x}")')}) ))/2)))
      )

    })

    observeEvent(input$step_update, {

      if(is.null(input$steps) | is.null(input$steps_select)){sendSweetAlert(session,
                                                                            title = 'Error',
                                                                            type = 'error',
                                                                            text = 'You must select at least the step and selector. These conditions are not met.')}

      if(!is.null(input$steps) & !is.null(input$steps_select)){

        n_step(n_step()+1)

        if(n_step()==1){

          val$steps<-input$steps
          val$steps_select<-input$steps_select
          val$steps_deselect<-input$steps_deselect
          val$id<-n_step()

        } else{

          val$steps<-c(val$steps, input$steps)
          val$steps_select<-c(val$steps_select, input$steps_select)
          val$steps_deselect<-c(val$steps_deselect, input$steps_deselect)
          val$id<-c(val$id, n_step())

        }

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Step Added!')

      }

    })

    modal_controls <- glideControls(
      list(
        prevButton(),
        firstButton(
          class = "btn btn-danger",
          `data-dismiss`="modal",
          "Cancel"
        )
      ),
      list(
        nextButton(),
        lastButton(
          class = "btn btn-success",
          `data-dismiss`="modal",
          "Done"
        )
      )
    )

    ################################## WIZARD ###############################################

    exclude_params<-c('recipe', '...', 'role', 'trained', 'ref_data', 'columns', 'skip')

    observeEvent(input$step_wizard, {

      glide_modal <- modalDialog(
        title = "Steps Wizard",
        easyClose = FALSE,
        footer = NULL,
        size = 'l',
        glide(
          custom_controls = modal_controls,
          height = '300px',
          screen(
            next_label = 'Start <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
            p("This is the step generation wizard that allows for more advanced options. Shall we start?")
          ),
          screen(
            p("First, you must select a step from the list of available steps. You can only adding steps one by one."),
            hr(),
            wellPanel(style = 'background:#9999ff;border-color: black;border-width: 2px;',
            pickerInput(inputId = 'wsteps',
                        label = HTML('<h4 style="color:white"><b> Select One Step: </b></h4>'),
                        choices = ls('package:recipes', pattern = '^step_'),
                        choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(ls('package:recipes', pattern = '^step_'))/2))),
                        options = list(size = 6, `live-search` = TRUE, style = "btn-primary",
                                       showContent=TRUE))
                )
          ),
          screen(
            p("Please write an expression that allows you to select the columns you want the preprocessing step to apply to.
              It can be the name of a specific variable, the selection functions both by type and by role (for example, all_outcomes () and all_nominal ()).
              The following functions can also be used: tidyselect::starts_with(), tidyselect::ends_with(), tidyselect::contains (), tidyselect::matches(), tidyselect::num_range (), and tidyselect::everything ()"),
            hr(),
            wellPanel(style = 'background:#9999ff;border-color: black;border-width: 2px;',
            textInput(inputId = 'wsteps_select',
                      label = HTML('<h4 style="color:white"><b> Select: </b></h4>'),
                      value = '...'))
          ),
          screen(
            p("Please write an expression that allows you to deselect the columns you want the preprocessing step to apply to.
              It can be the name of a specific variable, the selection functions both by type and by role (for example, all_outcomes () and all_nominal ()).
              The following functions can also be used: tidyselect::starts_with(), tidyselect::ends_with(), tidyselect::contains (), tidyselect::matches(), tidyselect::num_range (), and tidyselect::everything ()"),
            hr(),
            wellPanel(style = 'background:#9999ff;border-color: black;border-width: 2px;',
            textInput(inputId = 'wsteps_deselect',
                      label = HTML('<h4 style="color:white"><b> Deselect: </b></h4>'),
                      value = 'None'))
          ),
          screen(
            p("In this panel, you can change the parameters associated with the selected
              preprocessing step. You need to change the desired parameters one by one.
              Skip this step if you prefer deafult function values."),
            br(),
            renderUI({
            wellPanel(style = 'background:#9999ff;border-color: black;border-width: 2px; height: 80px;',
            awesomeRadio(inputId = "params_sel",
                         label = HTML('<p style="color:white"><b> Parameters: </b></p>'),
                         choices = dplyr::setdiff(names(formals(input$wsteps)), exclude_params),
                         inline = TRUE,
                         status = 'warning',
                         checkbox = TRUE))}),
            br(),
            wellPanel(style = 'background: #9999ff;border-color: black; border-width: 2px;',
                      div(style="display: inline-block;vertical-align:top;width:150px;",
                      textInput(inputId = 'params_val',
                      label = HTML('<h5 style="color:white"><b> New Value: </b></h5>'))),
                      div(style="display: inline-block;vertical-align:top; width: 300px;",HTML("<br>")),
                      div(style="display: inline-block;vertical-align:top;width:150px;",
                      actionBttn(inputId = 'add_param',
                                 label = 'Change Parameter',
                                 size = 'sm')))
          ),
          screen(
            p("Press the button to apply the changes."),
            hr(),
            actionBttn(inputId = 'add_wizard_step', 'Add Step'),

            tags$head(
              tags$style(HTML('#add_wizard_step{margin-left: 280px;'))
            )
          )
        )
      )

      showModal(glide_modal)

    })

    observeEvent(input$add_wizard_step, {

      if(is.null(input$wsteps) | is.null(input$wsteps_select)){sendSweetAlert(session,
                                                                            title = 'Error',
                                                                            type = 'error',
                                                                            text = 'You must select at least the step and selector. These conditions are not met.')}

      if(!is.null(input$wsteps) & !is.null(input$wsteps_select)){

        n_step(n_step()+1)

        if(n_step()==1){

          val$steps<-input$wsteps
          val$steps_select<-input$wsteps_select
          val$steps_deselect<-input$wsteps_deselect
          val$id<-n_step()

        } else{

          val$steps<-c(val$steps, input$wsteps)
          val$steps_select<-c(val$steps_select, input$wsteps_select)
          val$steps_deselect<-c(val$steps_deselect, input$wsteps_deselect)
          val$id<-c(val$id, n_step())

        }

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Step Added!')

      }

    })

    observeEvent(input$add_param, {

      if(input$params_val == ''){sendSweetAlert(session,
                                                title = 'Error',
                                                type = 'error',
                                                text = 'You must enter a new parameter value. If you dont want to change it, skip this step.')}

      if(input$params_val != ''){

        n_param(n_param()+1)

        if(n_param()==1){

          val$arg_step<-input$wsteps
          val$arg<-input$params_sel
          val$arg_val<-input$params_val
          val$arg_id<-n_step()+1

        } else{

          val$arg_step<-c(val$arg_step, input$wsteps)
          val$arg<-c(val$arg, input$params_sel)
          val$arg_val<-c(val$arg_val, input$params_val)
          val$arg_id<-c(val$arg_id, n_step()+1)

        }

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Parameter Changed!')

      }

    })

    observeEvent(input$formula, {

      val$stg1<-purrr::pmap_chr(list(val$steps, val$steps_select, val$steps_deselect), function(.x, .y, .z){

        if(.z == 'None'){

          str_glue('{.x}({.y}')

        } else{

          str_glue('{.x}({.y}, -{.z}')

        }

      })

      if(length(val$arg)>0){

        val$stg1_args<-map2_chr(val$arg, val$arg_val, ~{str_glue('{.x} = {.y}')})

        val$df<-data.frame('steps' = val$arg_step,
                           'id' = val$arg_id,
                           'stg1_args' = val$stg1_args)

        val$df2<-data.frame('steps' = val$steps,
                            'id' = val$id,
                            'stg1' = val$stg1)

        val$df3<-dplyr::left_join(val$df2, val$df, by = c('steps', 'id')) %>% unique()

        val$final_steps<-purrr::map_chr(1:dim(val$df3)[1], function(i){

          row<-val$df3[i,]

          return(ifelse(is.na(row$stg1_args)==T,
                        row$stg1 %>% as.character(),
                        apply(row[,3:length(row)], 1, paste, collapse = ', ')))

        })

      } else{val$final_steps<-val$stg1}



      val$finalisimo<-paste('%>% ', val$final_steps, ')')

      val$roles<-purrr::map2_chr(val$role_cols, val$role_text, ~{str_glue('%>% update_role({.x}, new_role={dQuote(.y, FALSE)})')})

      val$formula<-stringr::str_glue('.recipe {paste(val$roles, collapse= " ")} {paste(val$finalisimo, collapse= " ")}')


      glide_modal_steps <- modalDialog(
        title = "Steps Wizard",
        easyClose = FALSE,
        footer = NULL,
        size = 'l',
        glide(
          custom_controls = modal_controls,
          height = '300px',
          screen(
            next_label = 'Start <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
            p("This is the steps confirmation glide. Shall we start?")
          ),
          screen(
          p("In this panel you can see all the steps you have selected.
            Please confirm the steps you want to stay with."),
          br(),
          wellPanel(style = 'background: #9999ff;border-color: black; border-width: 2px;',
            div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = 'confirmed_steps',
                            label = HTML('<h4 style="color:white"><b> Confirm Steps: </b></h4>'),
                            choices = str_replace_all(val$finalisimo, pattern = '%>%', replacement = '') %>% str_squish(),
                            options = list(size = 6, style = 'btn-success', `actions-box` = TRUE),
                            multiple = TRUE,
                            choicesOpt = list(style = rep(c("background: #3333ff; color: white;", "background: #b3b3ff; color: white;"), ceiling(length(val$finalisimo)/2)))
                )),
            div(style="display: inline-block;vertical-align:top; width: 300px;",HTML("<br>")),
            div(style="display: inline-block;vertical-align:top; width: 150px;margin-top:35px;",
                actionBttn(inputId = 'confirm_steps',
                           label = 'Confirm'))

          )
          )
          ))

      showModal(glide_modal_steps)

    })


    observeEvent(input$confirm_steps, {

      if(length(input$confirmed_steps)>0){

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Steps Confirmed!')

        val$end<-paste('%>% ', input$confirmed_steps)

        val$roles<-purrr::map2_chr(val$role_cols, val$role_text, ~{str_glue('%>% update_role({.x}, new_role={dQuote(.y, FALSE)})')})

        val$formula<-stringr::str_glue('.recipe {paste(val$roles, collapse= " ")} {paste(val$end, collapse= " ")}') %>% str_squish()

      } else{

        sendSweetAlert(session,
                       title = 'Error',
                       type = 'error',
                       text = 'You must select at least one step.')

      }



    })

    ########################## PANEL PREP/BAKE ############################

    output$code<-renderText({

      if(length(val$formula)>0){

    stringr::str_replace_all(paste(val$formula, '%>% prep(training = ', name, ')', sep = ' '), pattern = '%>%', replacement = '%>% \n  ') %>%
    stringr::str_replace_all(pattern = ' \\)', replacement = '\\)')

      } else {paste('Please first create a recipe and steps \nbefore preparing.')}

      })

    observeEvent(input$prep,{

      if(length(val$formula>0)){

        val$prep<-list()

        val$prep<-eval(parse(text = stringr::str_replace_all(paste(val$formula, '%>% prep(training = .x', ')', sep = ' '), pattern = '%>%', replacement = '%>% \n  ') %>%
                    stringr::str_replace_all(pattern = ' \\)', replacement = '\\)') %>%
                    stringr::str_replace(pattern = '.recipe', '.recipe()')))

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Recipe Prepared!')

      } else{

        sendSweetAlert(session,
                       title = 'Error',
                       type = 'error',
                       text = 'Please first create a recipe and steps before preparing.')

      }

    })

    observeEvent(input$bake, {

      if(length(input$bake_list)>0){

        val$bake<-list('data' = purrr::map(.y[input$bake_list], ~{bake(val$prep, .x)}))

        sendSweetAlert(session,
                       title = 'Success',
                       type = 'success',
                       text = 'Data Baked!')

      } else{

        sendSweetAlert(session,
                       title = 'Error',
                       type = 'error',
                       text = 'Please select a data to bake.')

      }

    })



    ########################## Configuration  ############################

    observeEvent(input$done, {
      returnValue <- list('recipe' = val$prep, 'data' = val$bake)
      stopApp(returnValue)
    })

    observeEvent(input$cancel, {
      returnValue <- 'See you soon!'
      stopApp(returnValue)
    })

  }

  runGadget(ui, server, viewer = dialogViewer("recipesGadget", width = 750, height = 750))

}
