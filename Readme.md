Readme
======

This is a very primitive integration of incredibuld command line with
the emacs "compile" interface. The package only includes 2 commands:

1. incredi-build: Which attempts to conform interactively the build
   command by asking to the user proper completion alternatives.
2. incredi-kill: Which cancels the a build in progress.

The package includes also a regex entry in
`compilation-error-regexp-alist-alist` to match the `VC` compilation
error lines and enable the jump-to-error feature in the compilation
buffer.

By default the packages makes:

`(setq compilation-error-regexp-alist '(msbuild cmake cmake-info))`

Installation
------------

Just load it... this is not an elaborated package yet.

