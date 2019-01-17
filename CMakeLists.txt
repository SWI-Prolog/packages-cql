cmake_minimum_required(VERSION 3.5)
project(swipl-cql)

include("../cmake/PrologPackage.cmake")

swipl_plugin(cql
	     PL_LIB_SUBDIR cql
	     PL_LIBS cql.pl sql_parser.pl sql_tokenizer.pl sql_write.pl
		     sql_keywords.pl cql_database.pl cql_autoschema.pl
		     cql_hooks.pl)

swipl_examples(cql_demo.pl)

pkg_doc(
  cql
  SOURCE cql.pl cqldoc.tex "library('cql/cql')"
  DEPENDS cql chr)
