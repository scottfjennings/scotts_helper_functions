the page-break.lua file allows insertion of page breaks in RMD .doc documents.

the file must be copied into the folder where the .rmd lives, and the initial chunk of the .rmd must include:

output:
  word_document:
    pandoc_args:
     '--lua-filter=page-break.lua'


Then, insert

\newpage

wherever a page break is wanted.