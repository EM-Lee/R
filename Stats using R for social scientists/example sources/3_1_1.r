# 3부. 데이터 사전처리
# 1장. 데이터 중 일부만 골라내기
# 1. 데이터 중 일부 변수 취하기 (selecting variables)

# 데이터중 일부의 변수만을 취하기: 원하는 변수만을 골라내기 
# TESS 데이터는 SPSS 포맷으로 저장되어 있다. 따라서 우선 foreign 라이브러리를 실행한다. 
# 만약 foreign 라이브러리가 없으면 install.packages('foreign')을 이용하여 인스톨한다. 
library('foreign')
# read.spss() 명령문을 이용하여 데이터를 불러온 후 tess라는 이름의 오브젝트로 저장한다. 
# read.spss()의 경우 앞에서 설명한 바 있다. 
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# colnames() 명령문은 tess 오브젝트의 세로줄 이름(즉 변수이름)이 어떤 것인지를 확인하기 위한 명령문이다. 
# rownames() 명령문의 경우는 오브젝트의 가로줄 이름을 확인하는 명령문이다. 
colnames(tess)
# c() 명령문을 통해서 원하는 이름의 변수들을 별도의 오브젝트 wanted.var로 저장한다. 
# 이 예시문에서는 개인아이디(CaseID), 성별(PPGENDER), 연령(PPAGE), 인종(PPETHM), 교육년수(PPEDUC),
# 소득(PPINCIMP), 혼인상태(PPMARIT)의 총 7개 변수들을 선택하기로 한다. 해당변수에 대한 설명은 
# 데이터에 첨부된 문서들을 참조하라. 
wanted.var <- c('CaseID','PPGENDER','PPAGE','PPETHM','PPEDUC','PPINCIMP','PPMARIT')
# ‘오브젝트[,]’의 [,]은 인덱싱(indexing)을 위해 데이터에서 원하는 부분이 어디인지 밝히는 역할을 한다. 
# 현재의 명령문은 tess라는 오브젝트에서 가로줄(row)은 별다른 인덱싱을 하지 않지만, 세로줄은 wanted.var이라는 
# 오브젝트에 해당되는 부분만을 인덱싱한다. 이렇게 인덱싱된 tess 오브젝트는 별도의 오브젝트인 
# tess.keep으로 저장하였다. 
tess.keep <- tess[,wanted.var]
# dim() 명령문을 통해서 해당 오브젝트의 가로줄과 세로줄의 숫자가 얼마인지를 확인하였다. 
# tess와 tess.keep의 경우 가로줄의 경우는 2160으로 동일하지만 tess의 경우는 49개의 세로줄, 
# tess.keep의 경우 7개의 세로줄이 있는 것을 확인할 수 있다. 
dim(tess)
dim(tess.keep)

# 데이터중 일부의 변수만을 취하기: 원하는 변수만을 골라내기 
# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav",
	use.value.labels = FALSE, to.data.frame = TRUE)
# tess 오브젝트의 가로줄과 세로줄의 숫자를 파악하였다. 변수(세로줄)의 경우 총 49개가 있다. 
dim(tess)
# 첫 번째 20개의 변수들은 다음과 같이 범위를 정한다. :의 경우 :의 왼쪽의 수부터 :의 오른쪽 수까지 
# 범위를 나타낸다. 해당 범위들은 var.first20 라는 이름의 오브젝트로 저장하였다. 
var.first20 <- 1:20
# 마찬가지로 처음 10개의 변수 범위를 설정하였다. 또한 마지막의 10개는 40:49라는 이름으로 설정하였다. 
# 두 개의 범위는 c() 명령문을 통해서 하나의 범위로 합친 후 
# var.fist10.last10이라는 이름의 오브젝트로 저장하였다. 
var.first10.last10 <- c(1:10,40:49)
# 위에서 소개한 인덱싱과 원리는 똑같다.  
tess.first20 <- tess[,var.first20]
tess.first10.last10 <- tess[,var.first10.last10]
# 사실 해당범위들의 경우 별도의 오브젝트로 만들지 않고 바로 인덱싱을 위한 정보로 이용할 수도 있다. 
tess.first20 <- tess[,c(1:20)]
tess.first10.last10 <- tess[,c(1:10,40:49)]
# dim() 명령문을 통해서 해당 오브젝트의 가로줄과 세로줄의 숫자가 얼마인지를 확인하였다.  
dim(tess)
dim(tess.first20)
dim(tess.first10.last10)

# 데이터 중 일부의 변수만을 취하기: 원하지 않는 변수 버리기
# 앞에서 반복된 부분의 경우 설명을 생략하였다. 
library('foreign')
tess <- read.spss(file = "C:/data/example/TESS2_089_Trawalter_Client.sav", 
	use.value.labels = FALSE, to.data.frame = TRUE)
# tess 오브젝트의 가로줄과 세로줄의 숫자를 파악하였다. 변수(세로줄)의 경우 총 49개가 있다. 
colnames(tess)
# 설문을 시작한 시간(tm_start), 설문을 종료한 시간(tm_finish), 
# 그리고 설문에 소요된 시간(duration)에 해당되는 목록을 만들어서 var.drop이라는 오브젝트를 생성하였다.  
var.drop <- c('tm_start','tm_finish','duration')
# 위에서 소개한 인덱싱과 원리는 똑같다. 하지만 변수를 버리는 경우 약간 프로그래밍이 복잡하다. 
# 우선 != 라는 기호는 좌변과 우변이 같지 않음을 의미한다. 즉 아래의 명령문은 tess 오브젝트의 
# 세로줄 이름이 var.drop의 목록과 같지 않으면 변수를 유지한다는 의미이다. 당연한 말이겠지만 
# colnames(tess) == var.drop 이면 var.drop 오브젝트에 목록으로 들어간 변수들만 유지(keep)되고 
# 나머지가 버려지게 된다. 
tess.drop <- tess[,colnames(tess) != var.drop]
colnames(tess.drop)
# 마찬가지로 변수가 몇 번째에 위치하는가에 따라 인덱싱하는 것이 편할 수 있다.
# colnames(tess)를 통해 tess 오브젝트에서 설문을 시작한 시간(tm_start), 설문을 종료한 시간(tm_finish), 
# 그리고 설문에 소요된 시간(duration) 변수가 4, 5, 6번째 위치하고 있음을 알 수 있다. 
tess.drop.number <- tess[,-c(4,5,6)]
colnames(tess.drop.number)
# 만약 변수들의 위치가 범위(range)라면 보다 짧게 R프로그래밍 명령문을 작성하는 것도 가능하다. 
# 아래의 경우 만약 -4:6라고 하는 경우 에러가 발생한다. 그 이유는 R프로그램이 해당범위를 4:6이 
# 아닌 것(-)으로 파악하는 것이 아니라 -4부터 6까지로 파악하기 때문이다. 
tess.drop.range <- tess[,-(4:6)]
colnames(tess.drop.range)




