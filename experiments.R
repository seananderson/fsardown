library(officer)

# system("open templates/fsar-template.docx")

doc <- read_docx("templates/fsar-template.docx")
doc <- body_replace_text_at_bkm(doc, "region", "Pacific Region")

ctx <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce a quam consequat, tempus dui sit amet, consequat enim. Vestibulum scelerisque tincidunt augue. Proin facilisis nulla eu purus eleifend auctor. Sed laoreet magna eget vehicula malesuada. In at ipsum et metus eleifend interdum sit amet ac neque. Fusce est orci, elementum in dapibus ut, tempor pulvinar elit. Pellentesque fringilla ipsum ante, at aliquet nulla fringilla vitae. Maecenas pellentesque sed mi porttitor imperdiet. Donec sit amet orci vitae odio condimentum molestie id quis lacus. Aliquam tincidunt dolor non dui iaculis, nec viverra sapien accumsan. Praesent et quam quis elit consequat elementum. Sed id leo nec justo consequat viverra nec eu nulla. Praesent et efficitur mauris."

doc <- body_replace_all_text(doc, "<<context paragraph>>", ctx)

date_title <- "January 25, 2022 'Assessment of Quebec inshore waters softshell clam'"

doc <- body_replace_all_text(doc, "<<meeting date and title>>", date_title)

print(doc, target = "test.docx")

system("open test.docx")
