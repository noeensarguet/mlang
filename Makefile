# Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2018)
#
# This software is a computer program whose purpose is to compile and analyze
# programs written in the M langage, created by thge DGFiP.
#
# This software is governed by the CeCILL-C license under French law and
# abiding by the rules of distribution of free software.  You can  use,
# modify and/ or redistribute the software under the terms of the CeCILL-C
# license as circulated by CEA, CNRS and INRIA at the following URL
# http://www.cecill.info.
#
# As a counterpart to the access to the source code and  rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty  and the software's author,  the holder of the
# economic rights,  and the successive licensors  have only  limited
# liability.
#
# In this respect, the user's attention is drawn to the risks associated
# with loading,  using,  modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean  that it is complicated to manipulate,  and  that  also
# therefore means  that it is reserved for developers  and  experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and,  more generally, to use and operate it in the
# same conditions as regards security.
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL-C license and that you accept its terms.

SOURCE_DIR_2015=ir-calcul/sources2015m_4_6/
SOURCE_DIR_2016=ir-calcul/sources2016m_4_5/
SOURCE_DIR_2017=ir-calcul/sources2017m_6_10/

SOURCE_FILES=$(shell find $(SOURCE_DIR_2017) -name "*.m")

# export LD_LIBRARY_PATH=$(Z3_FOLDER)
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ocamlfind query z3)

deps:
	opam install ppx_deriving ANSITerminal re ocamlgraph z3 dune

build:
	dune build src/main.exe

test: build
		dune exec src/main.exe -- --debug test.m

simulateur_simplifie_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend python --output processing/ir_2018.py \
		--function_spec specs/simulateur_simplifie_2018.m_spec \
		$(SOURCE_FILES)

cas_basique_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend python --function_spec specs/cas_basique.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES) && \
	python processing/example_simple.py

autograd: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend autograd --function_spec specs/autograd.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES) && \
	python processing/example_autograd.py

verifisc_cas_basique_2018: build
	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend verifisc --function_spec specs/cas_basique.m_spec \
		--output processing/ir_2018.py $(SOURCE_FILES)

z3_basique: build
	OCAMLRUNPARAM=b	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec specs/cas_basique.m_spec \
		$(SOURCE_FILES)

z3_simulateur: build
	OCAMLRUNPARAM=b	dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --optimize \
		--backend z3 --function_spec specs/z3_simulateur.m_spec \
		$(SOURCE_FILES)

tests: build
	OCAMLRUNPARAM=b dune exec src/main.exe -- --application iliad \
	 	--display_time --debug --backend z3 \
		--function_spec specs/simulateur_simplifie_2018.m_spec \
		--run_all_tests \
		$(SOURCE_FILES)



doc:
	dune build @doc

graph:
	dot -Ksfdp -Goverlap=false -Goutputorder=edgesfirst -Nmargin=0.22,0.11 -Tsvg -Gratio=0.707106781 -o Graphe_IR.svg dep_graph.dot
	inkscape -z -e Graphe_IR.png -d 96 Graphe_IR.svg
	convert -resize 1980x1024 Graphe_IR.png Graphe_IR_Miniature.png
