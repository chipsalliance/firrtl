show:
vs:
	@rm -rf .bloop .metals project/.bloop

clean:
	@find . -name "target" | xargs rm -rf {} \;

rm:vs clean 
	@echo
