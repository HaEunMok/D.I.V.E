###14주차 과제###
### 1. 데이터 합치기+전처리  2. 회귀분석  3. 샤이니앱 만들기
"""
### 1. 데이터 합치기+전처리

transaction<-NULL
transaction<-read.csv('transaction_final.csv')
str(transaction)
View(transaction)

transaction_1<-NULL
transaction_1<-c("1","2018-04-01","2018-04-0110001","스크류","500","14:02:00","5","일요일","흐림","12","19","행사없음","0","0")
View(transaction_1)
transaction<-rbind(transaction_1,transaction)

colnames(transaction)<-c("number","판매일","전표","품명","매출합","판매시간","주차","요일","날씨","최저온도","최고온도","행사유무","행사","외부/내부")
transaction<-transaction[,-1]

#796380
transaction<-transaction[-c(796380:1048576),]
transaction<-transaction[complete.cases(transaction), ]
transaction$행사<-gsub("0","",transaction$행사)
transaction$`외부/내부`<-gsub("0","",transaction$`외부/내부`)
write.csv(transaction,"transaction_final.csv")
"""
### 2. 회귀분석

lp<-read.csv("transaction_final.csv")
head(lp)

# dim(dataframe): 몇행 몇열인지 반환해주는 함수
dim(lp) # 796379행 14열
View(lp)

# 결측값(NA값 제거)
lp <- lp[complete.cases(lp), ]
dim(lp)

# lm(y~x, data): data에서 x(변수)를 통해 y(예측할 값) 회귀분석
# 여기서 .은 data의 (가격을 제외한) 모든 변수를 의미
fit<-NULL
fit <-lm(매출합~행사유무, data=lp)
summary(fit)

# 가격~회사명+년식+토크 ... 이런식으로 원래 해줘야해

# 잔차(Residuals) : median을 가운데로 두고 다른 분위수가 대칭적으로 나타나는지 정도만 봐도 무방하다. boxplot 의미
# 추정치(Estimate) : (Intercept)는 회귀직선의 y절편, 나머지는 변수마다의 기울기(상관계수)를 나타낸다.
# 표준오차(Std. Error): 추정치의 표준오차로써, 이를 사용해서 회귀계수의 신뢰구간을 구할 수 있다.
# t value : 추정치를 표준오차로 나눈 값, 이를 통해 회귀계수가 통계적으로 유의한지 가설검정을 할 수 있다.

# p-value pr(>|t|) : 이게 작다는 것은 회귀계수가 유의하다는 뜻, 상관관계가 있다는 것을 보이고 싶다면 값이 작을수록 좋다.
# 수정설명력(Adjusted R-squared): 변수의 개수에 비해 데이터를 얼마나 잘 설명하는가를 나타내는 척도로써, 높을수록 좋다.
# r squared(상관계수)가 높아야 설명력이 높음, p value는 낮아야 설득력 있음 -> ***는 상관이 아주 많다는 뜻
# r-squared = 0.8이상, p-value = 0.05이하이면 좋은 회귀모델이라고 볼 수 있음



# predict(lm모형, 예측할 데이터): 만들어둔 lm모형을 가지고 새로운 데이터를 통해 회귀분석을 진행한다.
pre <- predict(fit, newdata = lp)
pre <- as.data.frame(pre)
head(pre)



# predict(lm모형, 예측할 데이터, interval): 예측범위를 구해주는 함수 -> 오차범위 포함 
pre <- predict(fit, newdata = lp, interval = "predict")
pre <- as.data.frame(pre)
head(pre)


# 잘 예측 되었는지 실제값과 비교
pre <- cbind(pre, lp$매출합)
head(pre)

# 'lwr <= price <= upr' 사이(예측범위 안)에 있으면 예측가격 안에 포함된 것
tf <- NA
pre <- cbind(pre, tf)
colnames(pre) <- c('fit', 'lwr', 'upr', 'price', 'tf')
View(pre)
pre$tf[pre$price>=pre$lwr & pre$price <=pre$upr] <-T

# 나머지 값은 F 처리
pre$tf[is.na(pre$tf)] <- F
head(pre)

# 예측률 = 예측성공 / 전체 행 개수
sum(pre$tf=="TRUE")/dim(pre)[1]
# ture는 1을 반환 

############################# 매점 매출데이터 회귀분석 ###################################


library(plyr)

# 회귀분석을 위해 일일 매출액을 도출
# ddply(data, 기준 변수, 수행할 함수): 기준 변수가 같은 데이터들에 함수를 적용
store<-read.csv("transaction_final.csv")
View(store)

sumsales<-NULL
sumsales <- ddply(store, c("판매일","날씨","최저온도", "최고온도", "주차", "요일","행사","행사유무","외부내부"),
                  function(df1)sum(df1$매출합))
colnames(sumsales) <- c('판매일', '날씨', '최저온도', '최고온도', '주차', '요일','행사','행사유무','외부내부' ,'장바구니매출')


# 회귀모형: 외부변수를 추가할 경우 '+외부변수'를 해주면 됨

#fit <- lm(장바구니매출~날씨+최저온도+최고온도+주차+요일+행사+행사유무+외부내부, data=sumsales)
fit <- lm(장바구니매출~날씨+최저온도+최고온도+주차+요일+행사, data=sumsales)
summary(fit)


# 예측

pre<-NULL
pre <- predict(fit, newdata = sumsales, interval = "predict")
pre <- as.data.frame(pre)

# 앞 내용과 동일
# 회귀모델 시각화는 생략


# 잘 예측 되었는지 실제값과 비교#####################error#####################
pre <- cbind(pre, lp$매출합)
head(pre)
nrow(pre)

# 'lwr <= price <= upr' 사이(예측범위 안)에 있으면 예측가격 안에 포함된 것
tf <- NA
pre <- cbind(pre, tf)
colnames(pre) <- c('fit', 'lwr', 'upr', 'tf')
View(pre)
pre$tf[pre$price>=pre$lwr & pre$price <=pre$upr] <-T

# 나머지 값은 F 처리
pre$tf[is.na(pre$tf)] <- F
head(pre)

# 예측률 = 예측성공 / 전체 행 개수
sum(pre$tf=="TRUE")/dim(pre)[1]
# ture는 1을 반환 

# plot(x=, y= ): 기본 형태
# xlab, ylab: x축, y축 제목
# type: 그래프의 출력 형태 -> "p":점으로 / "l":선으로 / "o":점과 선 동시에 / "h":히스토그램 / ...
# col: 색깔 -> 2: 빨강, 4:파랑
# format(data, scientific=F): 지수 표기를 숫자 표기로 바꿔준다.
# 여기서는 지수값 표기로 인해 실제 매출 그래프가 나타나지 않아서 쓰임 ex) 8e+06 -> 8000000

# 실제매출
plot(x = sumsales$판매일, y = format(sumsales$장바구니매출, scientific=F)
     , ylab = "매출액", xlab ="날짜", type = "o", cex = 1, col = 2)

# 예상매출
# points(x=, y= ): plot위에 그래프를 또 그려줌
points(x = sumsales$판매일, y = pre$fit, type = "o", cex = 1, col = 4)


# 과제
# 1) 주차(또는 요일)데이터 매점 매출데이터에 합치기 -> merge()
# 2) 선정한 외부변수 데이터 만들어서 매점 매출데이터에 합치기 -> merge()
# 3) 변수들 회귀분석하기(Adjusted r-squared 0.8이상 모델 목표로) -> lm(), predict()
# 외부변수 수집하는 방법은 크롤링 / ctrl+c ctrl+v 노가다 / R 내장 함수 / ... -> 방법에는 제한 없음
# 외부변수 데이터와 회귀분석 스크립트 카페에 올리기, 수집방법 글로 작성
# 매출액으로 
#adjusted r-squrared가 0.8 이상인거 찾아오기 

# 입주하는 날도 따로 해주기

write.csv(store,"store.csv")

### 3. 샤이니앱 만들기

library(arules)
library(arulesViz)
library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

install.packages("shiny")
library(shiny)

store <- store
store <- store[,-1]


store$판매일 <- as.Date(store$판매일)
store$판매시간 <- as.character(store$판매시간)

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
               checkboxGroupInput("event", 
                                  h3("event"), 
                                  choices = list('18-1 중간고사', 
                                                 '18-봄학기 축제', 
                                                 '18-1 기말고사',
                                                 '개강', 
                                                 '18-2 중간고사',
                                                 '18-가을학기 축제',
                                                 '면접고사',
                                                 '18-2 기말고사',
                                                 'HANst'),
                                  selected = c('18-1 중간고사', 
                                               '18-봄학기 축제', 
                                               '18-1 기말고사',
                                               '개강', 
                                               '18-2 중간고사',
                                               '18-가을학기 축제',
                                               '면접고사',
                                               '18-2 기말고사',
                                               'HANst')))
        
      ),
      fluidRow(
        
        column(6, 
               checkboxGroupInput("Yes event/No event", 
                                  h3("Yes event/No event"), 
                                  choices = list('행사있음','행사없음'),
                                  selected = c('행사있음','행사없음')))
        
      ),
      fluidRow(
        
        column(6, 
               checkboxGroupInput("event character", 
                                  h3("event character"), 
                                  choices = list('내부','외부'),
                                  selected = c('내부', '외부')))
        
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
      filter(요일 %in% input$weekdays) %>%
      filter(주차 %in% input$weeks) %>%
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
      filter(요일 %in% input$weekdays) %>%
      filter(주차 %in% input$weeks) %>%
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
      filter(요일 %in% input$weekdays) %>%
      filter(주차 %in% input$weeks) %>%
      filter(날씨 %in% input$weathers)
    
    sumsales <- ddply(store_filter, c("판매일","날씨","최저온도", "최고온도", "weeks", "weekdays"),
                      function(df1)sum(df1$매출합))
    colnames(sumsales) <- c('판매일', '날씨', '최저온도', '최고온도', 'weeks', 'weekdays', '장바구니매출')
    
    fit <- lm(장바구니매출~날씨+최저온도+최고온도+weeks+weekdays, data=sumsales)
    
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

