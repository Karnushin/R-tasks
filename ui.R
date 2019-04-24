ui <- shinyUI(fluidPage(
titlePanel("Цены опциона по модели Блэка-Шоулза"),
sidebarLayout(
sidebarPanel(
sliderInput(
"S",
"Цена базового актива",
min = 80, max = 120, step = 1, value = 80,
animate = FALSE),

sliderInput(
"sigma",
"Волатильность доходности базисного актива",
min = 0.1, max = 0.5, value = 0.1,
animate = FALSE),

sliderInput(
"t",
"Время до исполнения опциона",
min = 0.1, max = 1.5, value = 1,
animate = FALSE),

radioButtons(
"option", label = h3("Опцион"),
choices = list(
"Call" = 1,
"Put" = 2), selected = 1)
),
mainPanel(
plotOutput("plot")
)
)
))
