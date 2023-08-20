
# Preview.app で PDF を右綴じで表示したかった

PDF の情報を書き換えても無理でした...

````
$ python3 -m venv pdfenv
$ cd pdfenv
v. bin/activate
$ pip install pypdf

----
from pypdf import PdfWriter, PdfReader

input = "input.pdf"
output = "output.pdf"

reader = PdfReader(input)
writer = PdfWriter()

# writer.set_page_layout("/TwoColumnLeft")
# writer.set_page_layout("/TwoColumnRight")
# writer.set_page_layout("/TwoPageLeft")
# writer.set_page_layout("/TwoPageRight")
writer.set_page_layout("/TwoPageRight")

for i in reader.pages:
  writer.add_page(i)

with open(output, "wb") as fp:
    writer.write(fp)
----

$ open output.pdf
$ deactivate
````
