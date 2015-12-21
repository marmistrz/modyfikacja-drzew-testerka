# Copyright Artur "mrowqa" Jamro 2015
TEST_BIN=iset_test
RANDOM_TEST_BIN=iset_random_test
OCAMLC=ocamlc
SRC=iSet.mli iSet.ml
TEST_SRC=$(SRC) iSet_test.ml
RANDOM_TEST_SRC=$(SRC) iSet_random_test.ml
OBJ_FILES=*.cmo *.cmi
ZIP_NAME=iset.zip

test: iset_test
	./$(TEST_BIN)

random_test: iset_random_test
	./$(RANDOM_TEST_BIN)

iset_test: $(TEST_SRC)
	$(OCAMLC) $(TEST_SRC) -o $(TEST_BIN)

iset_random_test: $(RANDOM_TEST_SRC)
	$(OCAMLC) $(RANDOM_TEST_SRC) -o $(RANDOM_TEST_BIN)

zip: $(TEST_SRC) $(RANDOM_TEST_SRC) Makefile
	rm -f $(ZIP_NAME)
	zip -r $(ZIP_NAME) $(TEST_SRC) $(RANDOM_TEST_SRC) Makefile

clean:
	rm -f $(TEST_BIN) $(RANDOM_TEST_BIN) $(ZIP_NAME) $(OBJ_FILES)

