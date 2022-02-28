
SPEC    = spec.pdf

all: $(SPEC)

spec.pdf: spec.md firrtl.xml spec-template.tex firrtl.xml ebnf.xml
	pandoc $< --template spec-template.tex --syntax-definition firrtl.xml --syntax-definition ebnf.xml -r markdown+table_captions -o $@

clean:
	rm -f *.aux *.log *.out *.toc *.pdf
