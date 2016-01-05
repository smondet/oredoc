Oredoc
======

Build documentation websites for some OCaml projects.

The `oredoc` command builds a website out of a project's repository, see
`oredoc --help`. The command line is parametrised only with environment
variables.

Install
-------

If you have `opam` up and running:

    opam pin add oredoc -k git https://github.com/smondet/oredoc.git


Example
-------

For example with Oredoc's website itself:

    INPUT=oredoc.ml \
      INDEX=README.md \
      TITLE_PREFIX="Oredoc: " \
      OUTPUT_DIR=_doc \
      COMMAND_SUBSTITUTIONS=oredoc:_build/oredoc.native,some_command:gcc  \
      API=_apidoc \
      CATCH_MODULE_PATHS=^Oredoc:,^Markdown:Oredoc. \
      TITLE_SUBSTITUTIONS="oredoc.ml:Literate Implementation" \
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
- Links to values (e.g. `Oredoc.Meta_result.bind`), 
types (e.g. `Oredoc.Meta_result.t`), or whole modules
(e.g. `Oredoc.Markdown`) are created too (note, for now oredoc is not that
clever: types are called `t`, values are anything else).
Prefixes can be passed to the argument and added later: c.f. `Markdown.to_html`.

Configuration
-------------

The meaning of the environment variables are:

- `INPUT=.,src/doc`: comma-separated list of files or directories (used to look
for files).
- `INDEX=README.md`: markdown file used to generate the `index.html`.
- `TITLE_PREFIX="Oredoc: "`: prefix pages' titles with this.
- `COMMAND_SUBSTITUTIONS=oredoc:_build/oredoc.native,some_command:gcc`:
when calling `--help` (or `--help=groff`) replace the prefix `some_command`
with `gcc` (comma-separated list of colon-separated substitutions).
- `TITLE_SUBSTITUTIONS="oredoc.ml:Literate Implementation"`: by default titles
for filenames are made by chopping off the extension and replacing
underscores with spaces; this settings overrides this behavior.
- `API=_apidoc`: directory containing OCamldoc API documentation (will be
`rsync`ed to `api/` in the output directory).
- `CATCH_MODULE_PATHS`: list of couples, POSIX regular expressions with prefix
string, used to convert module paths.
- `CSS` is a comma-separated list of stylesheet URLs or paths.

Warning
-------

Hoping this warning is useless, this is *not* a wiki engine; any piece of
documentation can execute arbitrary commands on the host that is building the
documentation:
`ordeoc $(wc -l /etc/passwd > /tmp/bouh) --help`
⇒
Do not run `oredoc` on files for which you would not run `bash` or `make`,
you've been warned.

License
-------

This is [ISC](http://en.wikipedia.org/wiki/ISC_license) licensed.
