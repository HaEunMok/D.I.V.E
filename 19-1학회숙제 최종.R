################################# 주차 요일 변수 데이터 ####################################

# 학교 특성상 주차의 영향을 많이 받기 때문에 아래의 네가지 분류보다 더 세분화 할 경우 연관성이 높아질 수 있음

library(lubridate)

store<- read.csv("transaction_final.csv")

############################# 매점 매출데이터 회귀분석 ###################################
#매점 매출 데이터 전처리

library(plyr)

# 회귀분석을 위해 일일 매출액을 도출
# ddply(data, 기준 변수, 수행할 함수): 기준 변수가 같은 데이터들에 함수를 적용
sumsales <- ddply(store, c("판매일","날씨","최저온도", "최고온도", "주차", "요일","행사"),
                  function(df1)sum(df1$매출합))
colnames(sumsales) <- c('판매일', '날씨', '최저온도', '최고온도', '주차', '요일', '행사','장바구니매출')
str(sumsales)

# 회귀모형: 외부변수를 추가할 경우 '+외부변수'를 해주면 됨

fit <- lm(장바구니매출~+날씨+최저온도+최고온도+주차+요일+행사, data=sumsales)
summary(fit)


# 예측
pre <- predict(fit, newdata = sumsales, interval = "predict")
pre <- as.data.frame(pre)
View(pre)
# 앞 내용과 동일
# 회귀모델 시각화는 생략
#시각화
# plot(x=, y= ): 기본 형태
# xlab, ylab: x축, y축 제목
# type: 그래프의 출력 형태 -> "p":점으로 / "l":선으로 / "o":점과 선 동시에 / "h":히스토그램 / ...
# col: 색깔 -> 2: 빨강, 4:파랑
# format(data, scientific=F): 지수 표기를 숫자 표기로 바꿔준다.
# 여기서는 지수값 표기로 인해 실제 매출 그래프가 나타나지 않아서 쓰임 ex) 8e+06 -> 8000000
#####################################여기부터 안됌###########################
# 실제매출
plot(x =sumsales$판매일, y = format(sumsales$장바구니매출, scientific=F)
     , ylab = "매출액", xlab ="날짜", type = "o", cex = 1, col = 2)
# 예상매출
# points(x=, y= ): plot위에 그래프를 또 그려줌
points(x = sumsales$판매일, y = pre$fit, type = "o", cex = 1, col = 4)

#####################################샤이니앱#########################
library(arules)
library(arulesViz)
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# install.packages("shiny")
library(shiny)


# UI 정의
ui <- shinyUI(fluidPage( # ShinyUI(fluidPage()): shiny app의 ui를 설정해줌(사실상 아웃라인 그리는 것)
  
  # title 제목
  titlePanel("매점 매출데이터 Shiny App"), 
  
  # sidebar(네모상자) 레이아웃 지정
  sidebarLayout(
    sidebarPanel( # 사이드바 제목
      h2("Business Intelligence"), # h1,h2,h3,h4,h5,h6: 글의 대제목 1~6까지 지정 가능
      p("Shiny app: 변수들의 조건들을 조정하여 데이터를 시각화할 수 있다."), # p: 내용 입력
      
      fluidRow( # fluidRow(): 행 추가
        
        column(8,
               dateRangeInput("dates", h3("Date range")))
        
      ),
      
      fluidRow(
        
        column(8, 
               sliderInput("times", h3("Purchase Time(Hour)"),
                           min = 0, max = 24, value = c(0, 24))
               
        )
      ),
      
      fluidRow(
        
        column(6, 
               checkboxGroupInput("weekdays", 
                                  h3("Weekdays"), 
                                  choices = list('월요일', 
                                                 '화요일', 
                                                 '수요일',
                                                 '목요일', 
                                                 '금요일',
                                                 '토요일',
                                                 '일요일'),
                                  selected = c('월요일', '화요일', '수요일', '목요일', '금요일', '토요일', '일요일')))
        
      ),
      
      fluidRow(
        
        column(6, 
               checkboxGroupInput("weeks", 
                                  h3("Weeks"), 
                                  choices = list("1주차" = '1', # 샤이니앱에서 출력되는 목록과 코드에 입력되는 값이 다름
                                                 "2주차" = '2', 
                                                 "3주차" = '3',
                                                 "4주차" = '4', 
                                                 "5주차" = '5',
                                                 "6주차" = '6',
                                                 "7주차" = '7',
                                                 "8주차" = '8', 
                                                 "9주차" = '9', 
                                                 "10주차" = '10',
                                                 "11주차" = '11', 
                                                 "12주차" = '12',
                                                 "13주차" = '13',
                                                 "14주차" = '14',
                                                 "15주차" = '15', 
                                                 "16주차" = '16', 
                                                 '여름계절',
                                                 '여름방학', 
                                                 '겨울계절',
                                                 '겨울방학',
                                                 '입주'),
                                  selected = c('1','2','3','4','5','6','7','8','9','10',
                                               '11','12','13','14','15','16','여름계절','여름방학',
                                               '겨울계절','겨울방학','입주')))
        
      ),
      
      fluidRow(
        
        column(6, 
               checkboxGroupInput("weathers", 
                                  h3("Weathers"), 
                                  choices = list('맑음', 
                                                 '흐림', 
                                                 '구름많음',
                                                 '구름조금', 
                                                 '비',
                                                 '눈',
                                                 '황사'),
                                  selected = c('맑음', '흐림', '구름많음', '구름조금', '비', '눈', '황사')))
        
      )
    ),
    
    mainPanel( # 메인 제목
      h1("Shiny app 시각화 그래프"), # h1: 대제목
      # a("",href=): 특정 글자에 하이퍼링크 지정
      br(),
      br(),
      textOutput("dates"),
      textOutput("times"),
      br(),
      br(),
      p("매점 인기품목 TOP20 그래프"),
      plotOutput("top20"),
      br(),
      br(),
      p("매점 연관분석 그래프"),
      plotOutput("arules"),
      br(),
      br(),
      p("매점 회귀분석 매출 예측 그래프"),
      plotOutput("linear")
    ))
)
)

server <- function(input, output) {
  
  output$dates <- renderText({
    paste("When did they buy(date)?", input$dates[1], "~", input$dates[2])
  })
  
  output$times <- renderText({
    paste("When did they buy(hour)?", input$times[1], "h ~", input$times[2], "h")
  })
  
  output$top20 <- renderPlot({
    if(input$times[1]<10){
      t1 <- paste0('0', as.character(input$times[1]), ':00:00')
    } else{
      t1 <- paste0(as.character(input$times[1]), ':00:00')
    }
    
    if(input$times[2]<10){
      t2 <- paste0('0', as.character(input$times[2]), ':00:00')
    } else{
      t2 <- paste0(as.character(input$times[2]), ':00:00')
    }
    
    store_filter <- store %>%
      filter(input$dates[1]<판매일, 판매일<input$dates[2]) %>%
      filter(t1<판매시간, 판매시간<t2) %>%
      filter(weekday %in% input$weekdays) %>%
      filter(week %in% input$weeks) %>%
      filter(날씨 %in% input$weathers)
    
    transactionData <- ddply(store_filter, c("전표","판매일"),
                             function(df1)paste(df1$품명,
                                                collapse = ","))
    transactionData$전표 <- NULL
    transactionData$판매일 <- NULL
    
    colnames(transactionData) <- c("items")
    
    write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = TRUE)
    tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',', rm.duplicates=TRUE)
    
    itemFrequencyPlot(tr, topN=20, type="absolute", col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
  })
  
  output$arules <- renderPlot({
    if(input$times[1]<10){
      t1 <- paste0('0', as.character(input$times[1]), ':00:00')
    } else{
      t1 <- paste0(as.character(input$times[1]), ':00:00')
    }
    
    if(input$times[2]<10){
      t2 <- paste0('0', as.character(input$times[2]), ':00:00')
    } else{
      t2 <- paste0(as.character(input$times[2]), ':00:00')
    }
    
    store_filter <- store %>%
      filter(input$dates[1]<판매일, 판매일<input$dates[2]) %>%
      filter(t1<판매시간, 판매시간<t2) %>%
      filter(weekday %in% input$weekdays) %>%
      filter(week %in% input$weeks) %>%
      filter(날씨 %in% input$weathers)
    
    transactionData <- ddply(store_filter, c("전표","판매일"),
                             function(df1)paste(df1$품명,
                                                collapse = ","))
    
    transactionData$전표 <- NULL
    transactionData$판매일 <- NULL 
    colnames(transactionData) <- c("items")
    
    write.csv(transactionData,"store_basket_transactions.csv", quote = FALSE, row.names = TRUE)
    tr <- read.transactions('store_basket_transactions.csv', format = 'basket', sep=',', rm.duplicates=TRUE)
    
    store_rules <- apriori(tr, parameter = list(supp=0.0001, conf=0.3, maxlen=10))
    
    plot(sort(store_rules[1:20], by="support"), method="graph", control = list(type="itemsets"))
  })
  
  output$linear <- renderPlot({
    if(input$times[1]<10){
      t1 <- paste0('0', as.character(input$times[1]), ':00:00')
    } else{
      t1 <- paste0(as.character(input$times[1]), ':00:00')
    }
    
    if(input$times[2]<10){
      t2 <- paste0('0', as.character(input$times[2]), ':00:00')
    } else{
      t2 <- paste0(as.character(input$times[2]), ':00:00')
    }
    
    store_filter <- store %>%
      filter(input$dates[1]<판매일, 판매일<input$dates[2]) %>%
      filter(t1<판매시간, 판매시간<t2) %>%
      filter(weekday %in% input$weekdays) %>%
      filter(week %in% input$weeks) %>%
      filter(날씨 %in% input$weathers)
    
    sumsales <- ddply(store_filter, c("판매일","날씨","최저온도", "최고온도", "week", "weekday"),
                      function(df1)sum(df1$매출합))
    colnames(sumsales) <- c('판매일', '날씨', '최저온도', '최고온도', 'week', 'weekday', '장바구니매출')
    
    fit <- lm(장바구니매출~날씨+최저온도+최고온도+week+weekday, data=sumsales)
    
    fit.con <- lm(장바구니매출~1, data = sumsales)
    fit.forward <- step(fit.con, scope=list(lower=fit.con, upper=fit), direction = "forward")
    
    pre <- predict(fit.forward, newdata = sumsales, interval = "predict")
    pre <- as.data.frame(pre)
    pre <- cbind(pre, sumsales$장바구니매출)
    
    tf <- NA
    pre <- cbind(pre, tf)
    colnames(pre) <- c('fit', 'lwr', 'upr', 'price', 'tf')
    
    pre$tf[pre$price >= pre$lwr & pre$price <= pre$upr] <- T
    pre$tf[is.na(pre$tf)] <- F
    
    prerates <- round(sum(pre$tf=="TRUE")/dim(pre)[1]*100, digits=2)
    
    plot(x = sumsales$판매일, y = format(pre$price, scientific=F)
         , ylab = "매출액", xlab ="날짜", type = "o", cex = 1, col = 2)
    
    points(x = sumsales$판매일, y = pre$fit, type = "o", cex = 1, col = 4)
    
    legend("topright", legend=c("실제매출", "예상매출"), pch = c(10, 10), col = c("red", "blue"))
    
    title(main=paste0("매점 회귀분석 예측 모델(", as.character(prerates), "% 예측률)"))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
