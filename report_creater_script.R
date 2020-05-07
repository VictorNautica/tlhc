render_report = function(tlhc_ccg) {
  rmarkdown::render(
    "report_template.Rmd", params = list(
      tlhc_ccg = tlhc_ccg
    ),
    output_file = paste0("Report-", tlhc_ccg, ".pdf")
  )
}

render_report("Doncaster")
