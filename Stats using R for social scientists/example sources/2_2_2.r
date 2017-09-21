# 2부. 데이터 관리
# 2장. 데이터 입력하기 및 불러오기
# 2. 외부 데이터 불러오기

# load() 명령문은 () 속 file이라고 명시된 곳에 저장된 R형식의 데이터를 불러오는 명령문이다.
# 명심할 것은 R의 경우 폴더의 구분은 폴더의 구분은 역슬래시(\)가 아니라 슬래시(/)를 이용한다.
# 만약 슬래시를 이용하고 싶지 않다면, 아래처럼 역슬래시를 두번 쓰면 된다
# ("C:\\data\\example\\WorldBank_LifeExpectancy_2010.rda").
life.world.rda <- load(file = "C:/data/example/WorldBank_LifeExpectancy_2010.rda")

# install.packages()은 괄호속의 라이브러리를 인스톨시키는 명령문이다.
install.packages('xlsx')
# ‘xlsx’ 라이브러리가 인스톨되어 있지 않을 경우 위의 명령문 실행해야 하지만,
# 이미 인스톨되어 있다면 실행할 필요가 없다.
# library()는 괄호속의 라이브러리를 활성화시키는 명령문이다. 인스톨이 되어 있어도
# library() 명령문이 없이는 xlsx 라이브러리 기능을 사용할 수 없다.
library('xlsx')
# read.xlsx() 명령문은 2007이후 버전의 엑셀화일(*.xlsx)을 불러오는 기능을 한다.
# read.xlsx() 의 file 옵션은 저장된 엑셀화일의 저장위치를 나타낸다. 앞서의 read.xls() 와 동일하다.
# read.xlsx() 의 sheetIndex 옵션은 저장된 엑셀화일의 탭(tab)의 순서를 나타낸다.
# sheetIndex = 1 이면 첫번째 탭이며 sheetIndex = 2라면 두번째 탭이다.
life.world.xlsx <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xlsx",
sheetIndex = 1)
# read.xlsx() 명령문은 2007 이전버전의 엑셀화일(*.xls)에서도 마찬가지로 작동한다.
life.world.xls <- read.xlsx(file = "C:/data/example/WorldBank_LifeExpectancy_2010.xls", sheetIndex = 1)

# read.table()은 괄호속의 파일을 도표(table)형태의 오브젝트로 불러오게 한다.
# read.table()의 sep은 입력값들의 분리된 방식을 명시한다. 따옴표 속에 콤마가 있으면
# 콤마로 구분되었음을 의미하고, 콜론이 들어가 있으면 콜론으로 구분되었음을 의미한다.
life.world.spc <- read.table(file = "C:/data/example/WorldBank_LifeExpectancy_2010.csv",
		sep = ',', header = TRUE)
# 만약 콤마로 구분된 것이 아니라 콜론으로 구분되었다면, 아래와 같이 명령문을 수정하면 된다.
life.world.cln <- read.table(file = "C:/data/example/WorldBank_LifeExpectancy_2010.txt",
		sep = ';', header = TRUE)
# CSV 형태의 파일은 광범위하게 쓰이기 때문에 read.csv()라는 별도의 명령문이 존재한다.
# 쓰임은 read.table과 유사하지만, header라는 옵션을 이용해서 가로줄 첫줄의 이름을
# 변수명으로 인식하게 할 수 있다. header = TRUE는 첫 가루줄의 입력값을 변수이름으로
# 입력하도록 하겠다는 의미이다. 만약 가로줄 첫줄이 변수명이 아니라면 header = FALSE 라고 명시하면 된다.
life.world.spc <- read.csv(file = "C:/data/example/WorldBank_LifeExpectancy_2010.csv", header = TRUE)

# foreign 라이브러리가 인스톨되어 있지 않을 경우 다음의 명령문 실행해야 한다.
# 그러나 이미 인스톨되어 있다면 실행할 필요가 없다. 다음 명령문을 실행하면 별도의 윈도우즈가 뜨면서
# 어디서 다운로드를 하기 원하는지를 묻는다. 사실 아무 곳에서 다운을 받아도 상관없으니 그냥 기본값(default)을
# 따르는 것이 가장 적절하다.
install.packages('foreign')
# 인스톨을 마친후 library()를 이용하여 foreign 라이브러리를 활성화시킨다.
library('foreign')
# read.spss() 명령문은 SPSS 버전의 파일을 불러오는 기능을 한다.
# read.spss()의 use.value.labels = FALSE 의 경우, SPSS의 데이터 입력값에 붙은 라벨의 값을
# 불러오지 않고 데이터에 기록된 그대로 불러온다는 것을 의미한다. 사회과학에서는 5점 혹은
# 7점 척도를 많이 이용한다. 이를테면 정책에 대한 선호를 “1 = 전혀 지지하지 않는다”
# “5 = 매우 지지한다”라고 한다면 use.value.labels = TRUE라고 되어 있는 경우 R 데이터에 입력값인
# 1 혹은 5가 입력되는 대신 ‘전혀 지지하지 않는다’ 혹은 ‘매우 지지한다’가 입력된다.
# use.value.labels = FALSE의 경우 데이터 입력값인 1∼5까지의 숫자가 R 데이터에 입력된다.
# 개인적으로는 use.value.labels = FALSE 가 데이터의 처리에 더 편리하다고 추천한다.
# read.spss()의 to.data.frame = TRUE 의 경우, 불러온 SPSS의 데이터를 data frame의 형태로
# 저장하는 것을 의미한다. 만약 to.data.frame = FALSE 라고 되어 있는 경우 데이터를
# data frame이 아닌 매트릭스 형태로 저장한다. 통계분석을 위해서는 data frame 형태의 데이터셋을 구성해야
# 하기 때문에, 가능하면 옵션을 TRUE라고 설정할 것을 권한다.
life.world.sps <- read.spss(file = "C:/data/example/WorldBank_LifeExpectancy_2010.sav", use.value.labels = FALSE, to.data.frame = TRUE)
# SAS 형태의 파일은 read.ssd라는 명령문으로 가능하다. ?read.ssd를 통해 자세한 이용법을 이용하기 바란다.
# STATA 형태의 파일은 read.dta라는 명령문으로 가능하다. ?read.dta를 통해 자세한 이용법을 이용하기 바란다.


