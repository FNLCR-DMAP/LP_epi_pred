### creating custom theme object
customTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "black"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,254)"
  
  ### header
  ,logoBackColor = "white"
  
  ,headerButtonBackColor = "#E7E7E7"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "#D1D1D1"
  ,headerButtonIconColorHover = "rgb(207,57,92)"
  
  ,headerBackColor = "white"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  #,sidebarBackColor = "#F1F1F1"
  #,sidebarBackColor = "#bb1b3d"
  #,sidebarBackColor = "rgb(207,57,92)"
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#F1F1F1"
    ,colorMiddle = "white"
    ,colorEnd = "white"
    ,colorStartPos = 0
    ,colorMiddlePos = 90
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "#dfdfdf"
  ,sidebarShadowColor = "3px 5px 5px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(207,57,92)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "black"
  ,sidebarTabTextSize = 20
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(45,59,66)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(186,51,83)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(248,248,248)"
  ,boxBorderRadius = 0
  ,boxShadowSize = ""
  ,boxShadowColor = "none"
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(248,248,248)"
  ,boxPrimaryColor = "rgb(15,124,191)"
  ,boxInfoColor = "rgb(225,225,225)"
  ,boxSuccessColor = "rgb(59,133,95)"
  ,boxWarningColor = "rgb(178,83,149)"
  ,boxDangerColor = "rgb(207,57,92)"
  
  #,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabColor = "white"
  ,tabBoxTabTextSize = 17
  #,tabBoxTabTextColor = "rgb(42,102,98)"
  ,tabBoxTabTextColor = "black"
  ,tabBoxTabTextColorSelected = "rgb(207,57,92)"
  #,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxBackColor = "white"
  ,tabBoxHighlightColor = "rgb(207,57,92)"
  ,tabBoxBorderRadius = 0
  
  ### inputs
  ,buttonBackColor = "rgb(115,116,119)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(115,116,119)"
  ,buttonBorderRadius = 0
  
  ,buttonBackColorHover = "rgb(186,51,83)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(186,51,83)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(118,118,118)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
  
)
