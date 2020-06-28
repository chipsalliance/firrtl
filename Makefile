
show:

clean:
	@find . -name "target" | xargs rm -rf {} \;
	@rm -f *.fir *.v

vs:
	@rm -rf .bloop .metals project/.bloop
