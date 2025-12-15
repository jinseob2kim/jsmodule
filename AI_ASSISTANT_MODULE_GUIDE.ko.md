# AI Assistant 모듈

jsmodule 패키지를 위한 AI 기반 통계 분석 코드 생성 모듈입니다.

## 개요

AI Assistant 모듈은 통계 분석을 위한 R 코드를 생성하는 대화형 채팅 인터페이스를 제공합니다. jsmodule의 gadget들과 완벽하게 통합되며 여러 AI 제공자(Anthropic Claude, OpenAI GPT, Google Gemini)를 지원합니다.

## 빠른 시작

### 1. API 키 설정

`.Renviron` 파일에 API 키를 추가하세요:

```r
# .Renviron 파일 열기
usethis::edit_r_environ()

# 다음 중 하나를 추가:
ANTHROPIC_API_KEY=your_key_here
OPENAI_API_KEY=your_key_here
GOOGLE_API_KEY=your_key_here

# 저장 후 R 세션 재시작
```

### 2. 기본 사용법

#### 옵션 A: jsBasicGadget과 함께 사용

```r
library(jsmodule)

# AI Assistant가 포함된 gadget 실행
jsBasicGadget()

# "AI Assistant" 탭으로 이동
```

#### 옵션 B: 독립 실행형 Shiny 앱

```r
library(shiny)
library(jsmodule)
library(survival)

ui <- fluidPage(
  titlePanel("AI Statistical Assistant"),
  aiAssistantUI("ai")
)

server <- function(input, output, session) {
  data <- reactive(colon)
  data.label <- reactive(jstable::mk.lev(colon))

  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label
  )
}

shinyApp(ui, server)
```

## 주요 기능

### 코드 생성
- 통계 분석 코드 (회귀분석, 생존분석, 기술통계)
- 시각화 코드 (ggplot2, jskm, forestplot)
- 테이블 생성 (jstable, DT)
- jsmodule 규칙 및 모범 사례 준수

### 다양한 AI 제공자
- **Anthropic Claude** (기본값): claude-3-7-sonnet, claude-3-5-sonnet, claude-3-opus
- **OpenAI GPT**: gpt-4o, gpt-4-turbo, gpt-3.5-turbo
- **Google Gemini**: gemini-2.0-flash-exp, gemini-1.5-pro, gemini-1.5-flash

### 내보내기 옵션
- **Word (.docx)**: 서식이 적용된 테이블
- **PowerPoint (.pptx)**: 편집 가능한 벡터 그래픽 플롯
- **Excel (.xlsx)**: 데이터가 보존된 테이블
- **R Script (.R)**: 완전히 재현 가능한 코드

### 안전 기능
- 샌드박스 코드 실행 (허용된 패키지만 사용)
- 실행 전 코드 검토 및 편집
- AI 지원 오류 수정
- 파일 시스템 및 네트워크 접근 불가

## 중요 사항

### 데이터 접근
- AI는 `data` 파라미터를 통해 제공된 데이터만 접근 가능
- 생성된 코드에서 데이터는 `out`으로 참조됨
- 파일 업로드 데이터는 자동으로 reactive하게 처리됨

### 허용된 패키지
생성된 코드는 다음 패키지만 사용할 수 있습니다:
```
jstable, jskm, jsmodule, survival, ggplot2, ggpubr,
pROC, data.table, DT, gridExtra, GGally, forestploter,
MatchIt, timeROC
```

### 변수 구조
모듈은 변수 구조 정보를 자동으로 생성합니다:
- Factor 변수
- Numeric 변수
- 사용자 정의 구조 (`data_varStruct` 파라미터로 제공 시)

### API 키 우선순위
1. `callModule()`의 명시적 `api_key` 인자
2. UI 입력 (`show_api_config = TRUE`인 경우)
3. 환경 변수 (`.Renviron` 파일)

### API 설정 모드

`show_api_config` 파라미터는 API 키 관리 방식을 제어합니다:

#### `show_api_config = TRUE` (기본값)
- **사용 사례**: 개발, 개인 사용, 또는 사용자가 자신의 API 키를 제공하는 경우
- **동작**:
  - UI에 Settings 패널 표시
  - 사용자가 AI 제공자와 모델 선택 가능
  - 사용자가 인터페이스에서 직접 API 키 입력 가능
  - UI에 입력된 API 키가 `.Renviron` 파일보다 우선
- **보안 참고**: UI에 입력된 API 키는 브라우저 메모리에만 저장되며 디스크에 저장되지 않음
- **권장 사항**: 로컬 개발 및 단일 사용자 애플리케이션에 적합

```r
# 개발 모드 - 사용자가 UI에서 설정 가능
aiAssistantUI("ai", show_api_config = TRUE)  # 기본값

callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  show_api_config = TRUE
)
```

#### `show_api_config = FALSE`
- **사용 사례**: 프로덕션 배포, 공유 애플리케이션, 또는 사전 구성된 환경
- **동작**:
  - Settings 패널 완전히 숨김
  - `.Renviron` 파일 또는 명시적 `api_key` 인자만 사용
  - API 설정을 위한 UI 요소 없음
- **보안 참고**: 사용자가 API 키를 보거나 수정하는 것을 방지
- **권장 사항**: 공유 API 키를 사용하는 프로덕션 배포에 필수

```r
# 프로덕션 모드 - .Renviron에서만 API 키 읽기
aiAssistantUI("ai", show_api_config = FALSE)

callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  show_api_config = FALSE
)
```

## 고급 사용법

### 사용자 정의 변수 구조

```r
server <- function(input, output, session) {
  data <- reactive(lung)
  data.label <- reactive(jstable::mk.lev(lung))

  # 변수 역할 정의
  var_struct <- reactive({
    list(
      variable = names(lung),
      Base = c("age", "sex", "ph.ecog"),
      Event = "status",
      Time = "time"
    )
  })

  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label,
    data_varStruct = var_struct
  )
}
```

### 분석 컨텍스트

AI 응답을 개선하기 위해 배경 정보를 제공하세요:

```r
callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  analysis_context = reactive({
    "NCCTG 폐암 임상시험 데이터.
     주요 결과: 사망까지의 시간 (status/time).
     수행능력 점수(ph.ecog)를 예측 변수로 중점 분석."
  })
)
```

### 프로덕션 배포

프로덕션 환경에서는 API 설정 UI를 숨기세요:

```r
ui <- fluidPage(
  aiAssistantUI("ai", show_api_config = FALSE)
)

server <- function(input, output, session) {
  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label,
    show_api_config = FALSE  # .Renviron만 사용
  )
}
```

## 문제 해결

### API 키를 찾을 수 없음
**문제**: "API key not configured" 오류
**해결책**:
1. `.Renviron` 파일에 올바른 변수명이 있는지 확인
2. `.Renviron` 편집 후 R 세션 재시작
3. 키가 유효한지 확인 (터미널에서 테스트: `Sys.getenv("ANTHROPIC_API_KEY")`)

### 코드 실행 오류
**문제**: 생성된 코드가 실행 실패
**해결책**:
1. 자동 수정을 위해 "Ask AI to Fix" 버튼 클릭
2. 실행 전 에디터에서 코드 검토
3. 데이터에 필요한 변수가 있는지 확인
4. 패키지가 설치되어 있는지 확인

### Summary 결과가 너무 조각남
**문제**: `summary()` 결과가 여러 조각으로 나뉨
**해결책**: 최신 버전에서 수정되었습니다. jsmodule 패키지를 업데이트하세요.

### 텍스트 출력에 이스케이프 시퀀스 표시
**문제**: `\n`이 줄바꿈 대신 그대로 보임
**해결책**: 최신 버전에서 수정되었습니다. jsmodule 패키지를 업데이트하세요.

## 모범 사례

### 1. 구체적인 질문하기
❌ 나쁨: "이 데이터를 분석해줘"
✅ 좋음: "wt.loss를 결과변수로, age, sex, ph.ecog를 예측변수로 선형회귀분석 수행"

### 2. 생성된 코드 검토
"Run Code" 클릭 전에 항상 에디터에서 코드를 검토하세요

### 3. 컨텍스트 제공
`analysis_context` 파라미터를 사용하여 AI에게 데이터에 대한 배경 정보를 제공하세요

### 4. 적절한 모델 사용
- 간단한 작업: 빠른 모델 사용 (Sonnet, GPT-4o)
- 복잡한 분석: 고급 모델 사용 (Opus, GPT-4)

### 5. 반복적 개선
처음부터 다시 시작하기보다 후속 질문으로 코드를 개선하세요

## 제한사항

1. **외부 데이터 접근 불가**: 파일을 읽거나 데이터베이스에 연결할 수 없음
2. **제한된 패키지 범위**: 허용된 패키지만 사용 가능
3. **컨텍스트 윈도우**: 매우 긴 대화는 초기화가 필요할 수 있음
4. **시각화 미리보기**: 일부 복잡한 플롯은 즉시 렌더링되지 않을 수 있음
5. **통계 전문성**: AI는 코드를 제공하지 통계 컨설팅을 제공하지 않음

## 예제

### 예제 1: 기술통계
```
Q: "치료군(rx)별 기저 특성을 비교하는 Table 1을 만들어줘"
```

### 예제 2: 생존분석
```
Q: "time과 status를 생존 결과로, age, sex, ph.ecog를 보정하여
    Cox 회귀분석 수행"
```

### 예제 3: 시각화
```
Q: "치료군별로 층화한 Kaplan-Meier plot을 risk table과 함께 그려줘"
```

### 예제 4: 모델 진단
```
Q: "wt.loss ~ age + sex + ph.ecog 선형모델에서 VIF로 다중공선성 확인"
```

## 보안 고려사항

### 코드 실행 보안

#### 환경 인식 실행 (개발 vs 프로덕션)

AI Assistant 모듈은 보안과 사용성의 균형을 위해 **환경 인식 코드 실행**을 구현합니다:

**개발 모드** (기본값):
- 표준 `eval()` 함수로 코드 실행
- 디버깅과 개발이 용이
- 모든 콘솔 출력 표시
- 로컬, 신뢰할 수 있는 환경에 적합

**프로덕션 모드**:
- `RAppArmor::eval.secure()`로 샌드박스 실행 (Linux 전용)
- 향상된 보안 및 리소스 제한:
  - 1GB RAM 제한
  - 1MB 파일 크기 제한
  - 10초 타임아웃
  - 새 프로세스 생성 금지
- 시스템 명령 실행 방지
- 공개 배포 시 필수

**환경 감지**:
모듈은 다음을 통해 프로덕션 환경을 자동 감지합니다:
1. `DEPLOYMENT_ENV` 환경 변수 (`production` 또는 `development`)
2. shinyapps.io 배포 감지
3. RStudio Connect 감지
4. `.production` 마커 파일

**배포 모드 설정**:

로컬 개발용 (기본값):
```r
# 별도 설정 불필요 - 기본값이 개발 모드
# 또는 .Renviron에 명시적으로 설정:
DEPLOYMENT_ENV=development
```

프로덕션 배포용:
```r
# .Renviron 파일에 추가:
DEPLOYMENT_ENV=production
```

또는 마커 파일 생성:
```bash
# 앱 디렉토리에서
touch .production
```

**Linux 서버 설정** (RAppArmor용):
```bash
# AppArmor 설치
sudo apt-get install apparmor apparmor-utils libapparmor-dev

# R 패키지 설치
R -e "install.packages('RAppArmor')"
```

**플랫폼 지원**:
- ✅ **Linux**: 완전한 RAppArmor 샌드박싱 사용 가능
- ⚠️ **macOS/Windows**: 프로덕션 모드에서 경고와 함께 표준 eval로 폴백
- 권장사항: 최대 보안을 위해 Linux 서버에 배포

#### 기본 보안 기능
- **패키지 화이트리스트**: 승인된 패키지만 허용
- **실행 전 검토**: 실행 전 코드 편집 가능
- **오류 처리**: 시스템 정보 없는 안전한 오류 메시지

### API 키 보안

**⚠️ 중요: API 키 처리 방식**

**API 키 사용 방법**:
- API 키는 환경 변수(`.Renviron`) 또는 UI 입력에서 읽음
- UI에 입력된 경우, 키는 현재 R 세션 메모리에만 존재
- API 호출은 `httr` 패키지를 사용하여 AI 제공자 API로 전달

**오픈소스입니다**:
- 모든 코드는 https://github.com/jinseob2kim/jsmodule 에서 공개되어 감사 가능
- 숨겨진 API 키 저장이나 전송 없음
- 코드를 직접 검토할 수 있습니다

✅ **이 모듈이 API 키로 하지 않는 것**:
- 어디에도 저장하지 않음
- 로그에 기록하지 않음
- AI 제공자 외에는 전송하지 않음

✅ **AI 제공자로 전송되는 것**:
- 사용자의 질문과 프롬프트
- 데이터 구조 정보 (변수명, 타입, 요약 통계)
- 이전 대화 기록
- 생성된 코드 (오류 수정용)

**전송되지 않는 것**:
- 원시 데이터 값 (질문에 명시적으로 포함하지 않는 한)
- 파일 시스템 정보

#### 배포 유형별 모범 사례

**개인/데스크톱 사용** (권장):
```r
# .Renviron에 API 키 저장 (사용자 홈 디렉토리)
# 사용자 계정에만 비공개로 유지됩니다
ANTHROPIC_API_KEY=your_key_here
```

**팀/공유 사용**:
- 각 팀원이 `.Renviron`에 자신의 API 키를 사용해야 함
- 개별 설정을 허용하려면 `show_api_config = TRUE` 설정
- 사용자 간 API 키를 공유하지 마세요

**공개 웹 애플리케이션**:
- ⚠️ **권장하지 않음**: `show_api_config = TRUE`로 공개 배포하지 마세요
- 공개 배포가 필요한 경우 다음 대안을 고려하세요:
  1. 서버 측 API 프록시 구현 (커스텀 백엔드 필요)
  2. 접근을 제한하는 인증 사용
  3. 엄격한 사용량 할당 및 모니터링 설정

#### API 키 저장 위치

1. **`.Renviron` 파일** (개인 사용 권장):
   - 위치: `~/.Renviron` (사용자 홈 디렉토리)
   - 보안: 사용자 계정만 접근 가능
   - 지속성: R 세션 재시작 후에도 유지

2. **UI 입력** (개발용만):
   - 위치: 브라우저 메모리 (임시)
   - 보안: 브라우저 탭을 닫으면 사라짐
   - 지속성: 없음 - 매 세션마다 재입력 필요

3. **`api_key` 인자** (고급 사용):
   - 위치: R 스크립트 또는 코드
   - 보안: ⚠️ 피하세요 - 코드에 키가 노출됨
   - 지속성: 코드가 저장된 위치에 따라 다름

#### 규정 준수 고려사항

민감한 데이터로 작업하는 경우:
1. ✅ 데이터 구조 및 변수명이 AI 제공자로 전송됨
2. ✅ 통계 요약이 전송될 수 있음
3. ⚠️ 질문에 실제 데이터 값을 포함하지 마세요
4. ⚠️ 조직의 AI 사용 정책을 검토하세요
5. ⚠️ 분석 전 데이터 익명화를 고려하세요

### 권장 보안 설정

**최대 보안을 위해**:
```r
# 1. .Renviron에 API 키 저장 (코드에 절대 포함하지 않음)
usethis::edit_r_environ()
# 추가: ANTHROPIC_API_KEY=your_key

# 2. 프로덕션에서는 show_api_config = FALSE 사용
aiAssistantUI("ai", show_api_config = FALSE)

# 3. .Renviron을 버전 관리에 커밋하지 않음
# .gitignore에 추가:
# .Renviron
# .Renviron.local

# 4. API 키를 정기적으로 교체 (90일마다 권장)

# 5. 제공자의 대시보드를 통해 API 사용량 모니터링
```

## 지원

이슈나 기능 요청은 다음에서 제출해주세요:
https://github.com/jinseob2kim/jsmodule/issues

## 라이선스

jsmodule 패키지 라이선스와 동일합니다.
