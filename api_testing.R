mylocation = "C:/Users/Byeongjun Cho/Desktop/2020-1/데이터사이언스입문/data/open_api"
setwd(mylocation)

library(tidyverse)
library(httr)
library(XML)
library(xml2)
library(writexl)
library(tictoc)

# 응급의료기관 기본정보 조회 서비스
url = "http://openapi2.e-gen.or.kr/openapi/service/rest/ErmctInfoInqireService/"
operator = "getEmrrmRltmUsefulSckbdInfoInqire" # 응급실 실시간 가용병상정보 조회 오퍼레이터
Servicekey = "your_service_key"
STAGE1 = ""
STAGE2 = ""
pageNo = "1"
numOfRows = "99"

#  url_tmp = paste0(
#  url_1, paste0("?ServiceKey=", Servicekey), paste0("&STAGE1=", STAGE1), paste0("&STAGE2=", STAGE2), paste0("&pageNo=", pageNo),
#  paste0("&numOfRows=", numOfRows))

# Option1 지역별 크롤링
# 92번 지역은 오류가 나므로 1:91 93:228
region = read.csv("시군구명 활용_ 건강보험심사평가원_시군구별 요양기관 현황 2018.csv") %>% as_tibble()
region = region %>% rename(code1 = "시도.구분", code2 = "시군구.구분")
region.1 = region %>% mutate(code1.1 = 
                               case_when(code1 == "서울" ~ "서울특별시",
                                         code1 == "강원" ~ "강원도",
                                         code1 == "경기" ~ "경기도",
                                         code1 == "경남" ~ "경상남도",
                                         code1 == "경북" ~ "경상북도",
                                         code1 == "광주" ~ "광주광역시",
                                         code1 == "대구" ~ "대구광역시",
                                         code1 == "대전" ~ "대전광역시",
                                         code1 == "부산" ~ "부산광역시",
                                         code1 == "세종" ~ "세종특별자치시",
                                         code1 == "충남" ~ "충청남도",
                                         code1 == "전북" ~ "전라북도",
                                         code1 == "전남" ~ "전라남도",
                                         code1 == "경북" ~ "경상북도",
                                         code1 == "경남" ~ "경상남도",
                                         code1 == "제주" ~ "제주특별자치도",
                                         code1 == "충북" ~ "충청북도",
                                         code1 == "인천" ~ "인천광역시",
                                         code1 == "울산" ~ "울산광역시"))
region.2 = tibble()
for (i in 1:228){
  region.2[i,1] = unique(region.1$code1.1[region.1$code2 == unique(region.1$code2)[i]])[1]
  region.2[i,2] = unique(region.1$code2)[i]
}
region.2 = region.2 %>% mutate(...2 = as.character(...2))

result_table = tibble()

for (i in 93:228){
  STAGE1 = region.2[i,1]
  STAGE2 = region.2[i,2]
  queryParams = str_c("?serviceKey=", Servicekey, "&STAGE1=", STAGE1, "&STAGE2=", STAGE2)
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  names = rootNode[[2]][['items']][['item']] %>%
    names()
  tmp_tbl = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
    set_names(iconv(names, "UTF-8", "CP949") %>% unname()) %>%
    as_tibble()
  result_table = result_table %>% bind_rows(.,tmp_tbl)}

# Option2 지역 상관 없이 전부 긁어오기
result_table_1 = tibble()
for (i in 1:10){
  queryParams = str_c("?serviceKey=", Servicekey, "&pageNo=", as.character(i), "&numOfRows=", "50")
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  names = rootNode[[2]][['items']][['item']] %>%
    names()
  tmp_tbl = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
    set_names(iconv(names, "UTF-8", "CP949") %>% unname()) %>%
    as_tibble()
  result_table_1 = result_table_1 %>% bind_rows(.,tmp_tbl)}

which(result_table_1$dutyName == "의료법인명지의료재단명지병원")
result_table_1[c(23, 391),] # 이름은 같지만 지역이 다른 명지병원이므로 인정
# 응급의료기관 지정 병원 갯수가 대략 402개 나옵니다

write_xlsx(result_table_1, "응급의료기관 기본정보 조회 서비스_1.xlsx")
write_excel_csv(result_table_1, "result_0527_12_16.csv")
# "result_mmdd_hh_mm.csv"

# 건강보험심사평가원 병원정보서비스

url = "http://apis.data.go.kr/B551182/hospInfoService/"
operator = "getHospBasisList" # 기본정보 오퍼레이터
queryParams = str_c("?serviceKey=", Servicekey, "&numOfRows=","99","&zipCd=", "2030")
doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
rootNode = xmlRoot(doc)

#   tmp_tbl_2 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
# set_names(iconv(names, "UTF-8", "CP949") %>% unname()) %>%
#   as_tibble()
# result_table_2 = result_table_2 %>% bind_rows(.,tmp_tbl_2)}

names = rootNode[[2]][['items']][['item']] %>%
  names()
tmp_tbl3 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
  as_tibble()

tmp_tbl3$ykiho # 요양코드, 상세정보 서비스 검색에 필요

# 건강보험심사평가원 의료기관별 상세정보서비스

url = "http://apis.data.go.kr/B551182/medicInsttDetailInfoService/"
operator = "getDetailInfo" # 세부정보 오퍼레이터
queryParams = str_c("?serviceKey=", Servicekey, "&ykiho=", ykiho)
doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
rootNode = xmlRoot(doc)

names = rootNode[[2]][['items']][['item']] %>%
  names()
tmp_tbl4 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
  as_tibble()

## 응급의료기관 조회서비스 3번 오퍼레이터 - 좌표값 찾기
pageNo = "1"
numOfRows = "99" # "&pageNo=", pageNo, "&numOfRows=", numOfRows
operator = "getEgytListInfoInqire" # 응급의료기관 조회 서비스 3번 오퍼레이터 (xy좌표 찾기)

result_table_3 = tibble()
for (i in 1:402){
  QN = result_table_1[i,1]
  queryParams = str_c("?serviceKey=", Servicekey, "&QN=", QN)
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  tmp_tbl_2 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//items//hpid')) %>% as_tibble(.name_repair = "unique")
  tmp_tbl_3 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//items//dutyName')) %>% as_tibble(.name_repair = "unique")
  tmp_tbl_4 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//items//wgs84Lon')) %>% as_tibble(.name_repair = "unique")
  tmp_tbl_5 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//items//wgs84Lat')) %>% as_tibble(.name_repair = "unique")
  tmp_tbl_2 = tmp_tbl_2 %>% bind_cols(.,tmp_tbl_3) %>% bind_cols(.,tmp_tbl_4) %>% bind_cols(.,tmp_tbl_5)
  result_table_3 = result_table_3 %>% bind_rows(.,tmp_tbl_2)}

# which(result_table_3$text1 == "의료법인명지의료재단명지병원")
# result_table_3[c(21, 22, 389, 390),] # 이름은 같지만 지역이 다른 명지병원이므로 인정
# unique(result_table_3$text) # 고유 hpid값 371개
# result_table_3 = result_table_3 %>%
#  rename("hpid" = text, "dutyName" = text1, "wgs84Lon" = text2, "wgs84Lat" = text3)

write_xlsx(result_table_3, "응급의료기관 목록정보 조회 서비스_3.xlsx")

# tmp_join = subset(result_table_1, select = c(dutyName, hpid))
# full_join(result_table_3, tmp_join, by = c("text" = "hpid")) %>% count(duplicated(text3))

# (2) 중증질환자 수용가능 정보 오퍼레이터

operator = "getSrsillDissAceptncPosblInfoInqire"
result_table_2 = tibble()

for (i in 1:40){
  queryParams = str_c("?serviceKey=", Servicekey, "&pageNo=", as.character(i), "&numOfRows=", "14")
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  names = rootNode[[2]][['items']][['item']] %>%
    names()
  tmp_tbl_2 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//items')) %>%
    as_tibble(.name_repair = "unique")
  result_table_2 = result_table_2 %>% bind_rows(.,tmp_tbl_2)}

result_table_2.df = tibble()
for (i in 1:23){
  for (j in 1:14){
    result_table_2.df[j+14*(i-1),1] = str_extract(result_table_2[i,j], "[가-힣]+")
    result_table_2.df[j+14*(i-1),2] = str_extract(result_table_2[i,j], "[a-zA-Z][0-9]+")
    result_table_2.df[j+14*(i-1),3] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 1, 1)
    result_table_2.df[j+14*(i-1),4] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 2, 2)
    result_table_2.df[j+14*(i-1),5] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 3, 3)
    result_table_2.df[j+14*(i-1),6] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 4, 4)
    result_table_2.df[j+14*(i-1),7] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 5, 5)
    result_table_2.df[j+14*(i-1),8] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 6, 6)
    result_table_2.df[j+14*(i-1),9] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 7, 7)
    result_table_2.df[j+14*(i-1),10] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 8, 8)
    result_table_2.df[j+14*(i-1),11] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 9, 9)
    result_table_2.df[j+14*(i-1),12] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 10, 10)
    result_table_2.df[j+14*(i-1),13] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 11, 11)
    result_table_2.df[j+14*(i-1),14] = substr(str_extract(result_table_2[i,j], "[a-zA-Z]{12}"), 12, 12)}}
result_table_2.df = result_table_2.df[1:313,]

write_xlsx(result_table_2.df, "중증질환자 수용가능 정보_2.xlsx")

## (5) 응급의료기관 기본정보 조회 오퍼레이션

operator = "getEgytBassInfoInqire"
result_table_5 = tibble()

for (i in 1:2000){
  tic()
  queryParams = str_c("?serviceKey=", Servicekey, "&pageNo=", as.character(i), "&numOfRows=", "50")
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  tmp_tbl_2 = xmlToDataFrame(getNodeSet(rootNode, "//item")) %>% as_tibble()
  result_table_5 = result_table_5 %>% bind_rows(.,tmp_tbl_2)
  toc()}
write_xlsx(result_table_5, "응급의료기관 기본정보 조회_5_1.xlsx")

table(duplicated(result_table_5$dutyName))
# HPID = result_table_1[i,3]

## (8) 외상센터 기본정보 조회 오퍼레이션
operator = "getStrmBassInfoInqire"
result_table_8 = tibble()
for (i in 1:10){
  queryParams = str_c("?serviceKey=", Servicekey, "&pageNo=", as.character(i), "&numOfRows=", "50")
  doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
  rootNode = xmlRoot(doc)
  tmp_tbl_3 = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>% as_tibble()
  result_table_8 = result_table_8 %>% bind_rows(.,tmp_tbl_3)}

write_xlsx(result_table_8, "외상센터 기본정보 조회_8.xlsx")