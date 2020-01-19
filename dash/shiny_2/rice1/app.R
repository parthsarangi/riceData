library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

shinyApp(
    ui = dashboardPagePlus(
        header = dashboardHeaderPlus(
            title = "Menu",
            fixed = TRUE,
            enable_rightsidebar = TRUE,
            rightSidebarIcon = "gears",
            dropdownMenu(
                type = "tasks",
                badgeStatus = "danger",
                taskItem(value = 20, color = "aqua", "Refactor code"),
                taskItem(value = 40, color = "green", "Design new layout"),
                taskItem(value = 60, color = "yellow", "Another task"),
                taskItem(value = 80, color = "red", "Write documentation")
            )
        ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem("Home",tabName = "home"),
                menuItem("About",tabName = "about")
            )
        ),
        body = dashboardBody(
            tabItem(tabName = "home", 
                    h2("Home")
                    ),
            tabItem(tabName = "about",
                    h2("About Us")
                    ),
            setShadow(class = "dropdown-menu")
        ),
        rightsidebar = rightSidebar(),
        title = "DashboardPage"
    ),
    server = function(input, output) { }
)