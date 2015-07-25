# Makefile for multigrid
# Franklin Chen

# Use byte code compiler for debugging.
# Use native compiler for production.

CFLAGS = -Wall -O

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep

OCAMLFLAGS = -g
# Not much help; bytecode :-)
OCAMLFLAGS = -unsafe -noassert

OCAMLOPTFLAGS =
# For final production run.
OCAMLOPTFLAGS = -unsafe -inline 10 -noassert

BYTE_OBJS = cpu.cmo globals.cmo \
	grid.cmo relax.cmo multigrid.cmo options.cmo main.cmo
OPT_OBJS = cpu.cmx globals.cmx \
	grid.cmx relax.cmx multigrid.cmx options.cmx main.cmx

all: simp \
	multigrid \
#	multigrid.byte

simp: cpu.cmx simp.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ unix.cmxa $^ -cclib -lunix

multigrid.byte: $(BYTE_OBJS)
	$(OCAMLC) -custom $(OCAMLFLAGS) -o $@ unix.cma $^ -cclib -lunix

multigrid: $(OPT_OBJS)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ unix.cmxa $^ -cclib -lunix

command.tex: multigrid
	(echo '\begin{verbatim}'; \
	./multigrid -help; \
	echo '\end{verbatim}') 2>&1 | expand > $@

dataset.data: multigrid
	./multigrid -nu0 3 -nu1 3 -nu2 4 -size 33 -debug 1 > $@

dataset.wrl: dataset.data
	sed -n '/VRML/,$$p' $< > $@

readme.dvi: readme.tex command.tex multigrid.tex
	latex readme.tex

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.tex: %.plt
	gnuplot $< > $@

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f multigrid multigrid.byte
	rm -f *.cm[iox] *.[oa]

depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.PHONY: all clean

include .depend
