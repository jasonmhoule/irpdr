library(pdftools)
library(curl)

curl::curl_download("https://www.xcelenergy.com/staticfiles/xe-responsive/Company/Rates%20&%20Regulations/The-Resource-Plan-No-Appendices.pdf",
                    "xcel1.pdf")

info <- pdf_info("xcel1.pdf")
pda <- pdf_data("xcel1.pdf")
ptx <- pdf_text("xcel1.pdf")
ptoc <- pdf_toc("xcel1.pdf")

pda[[147]]
ptx[147]
ptoc
