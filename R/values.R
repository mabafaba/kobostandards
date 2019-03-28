legal_kobo_question_types<-function(){
  types<-matrix(c(
  "integer","Integer (i.e., whole number) input.",
  "decimal","Decimal input.",
  "range","Range input.",
  "text","Free text response.",
  "select_one","Multiple choice question; only one answer can be selected.",
  "select_multiple","Multiple choice question; multiple answers can be selected.",
  "rank","Rank question; order a list.",
  "note","Display a note on the screen, takes no input.",
  "geopoint","Collect a single GPS coordinate.",
  "geotrace","Record a line of two or more GPS coordinates.",
  "geoshape","Record a polygon of multiple GPS coordinates; the last point is the same as the first point.",
  "date","Date input.",
  "time","Time input.",
  "dateTime","Accepts a date and a time input.",
  "image","Take a picture or upload an image file.",
  "audio","Take an audio recording or upload an audio file.",
  "video","Take a video recording or upload a video file.",
  "file","Generic file input (txt, pdf, xls, xlsx, doc, docx, rtf, zip)",
  "barcode","Scan a barcode, requires the barcode scanner app to be installed.",
  "calculate","Perform a calculation; see the Calculation section below.",
  "acknowledge","Acknowledge prompt that sets value to “OK” if selected.",
  "hidden","A field with no associated UI element",
  "xml-external","",
  "calcualte","calculation",
  "deviceid","the id of the device",
  "start","start time",
  "end","end time"),ncol = 2,byrow = T)

  colnames(types)<-c("name","explanation")
  types
}

questionnaire_questions_is_type_legal<-function(types){
  lapply(types,function(x){
  type_split<-x %>% strsplit(" ") %>% unlist
  any(!is.na(match(type_split,legal_kobo_question_types()[,1])))
  }
  ) %>% unlist
}

