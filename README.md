Oredoc
======

Build documentation websites for some OCaml projects.

The `oredoc` command builds a website out of a project's repository, see
`oredoc --help`. The command line is parametrised only with environment
variables.

For example with Oredoc's website itself:

    INPUT=oredoc.ml INDEX=README.md TITLE_PREFIX="Oredoc: " OUTPUT_DIR=_doc \
          COMMAND_SUBSTITUTIONS=oredoc:_build/oredoc.native,some_command:gcc  \
          API=_apidoc \
          oredoc 

We can see:

- Oredoc also transforms ML files: like [`oredoc.ml`](./oredoc.ml).
- The output of `some_command --help` is grabbed and displayed also. 
- If the command accepts `--help=groff` (like with
[Cmdliner](http://erratique.ch/software/cmdliner)), and `groff` is present,
then the Man page will be converted to HTML, 
e.g. `opam --help`, or `opam install --help`.
- Links to MLI files, like (the not-even existent)
[`oredoc.mli`](./oredoc.mli) are converted into links to the
OCamlDoc-generated API
(the generation itself is not handled by Oredoc).

The meaning of the environment variables are:

- `INPUT=.,src/doc`: comma-separated list of files or directories (used to look
for files).
- `INDEX=README.md`: markdown file used to generate the `index.html`.
- `TITLE_PREFIX="Oredoc: "`: prefix pages' titles with this.
- `COMMAND_SUBSTITUTIONS=oredoc:_build/oredoc.native,some_command:gcc`:
when calling `--help` (or `--help=groff`) replace the prefix `some_command`
with `gcc` (comma-separated list of colon-separated substitutions).
- `API=_apidoc`: directory containing OCamldoc API documentation (will be
`rsync`ed to `api/` in the output directory).
- `CSS` is a comma-separated list of stylesheet URLs or paths.

