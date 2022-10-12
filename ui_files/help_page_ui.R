#
# UI function defining layout of help page
#

help_page_ui <- 
      tabItem(tabName = "help_page",
              h3("Help with Power Calculations"),
              headerPanel(
                includeMarkdown("ui_files/help_page.md")
              )
      ) 
