watch:
	dune build src/bench/main.exe @fmt --auto-promote --watch

test:
	dune runtest

bench:
	dune build src/bench/main.exe
	./_build/default/src/bench/main.exe benchmark

ui:
	dune build src/example/main.bc.js
	mkdir -p ./_static
	cp _build/default/src/example/main.bc.js ./_static/index.js
	chmod u+w ./_static/index.js
	cp ./src/example/static/index.html ./_static/index.html
	cp ./src/example/static/style.css ./_static/style.css
	pushd ./_static; python -m SimpleHTTPServer; popd
