
Thanks for using the stack template `tasty-travis`! This file is here to guide
you through customizing the template files.

This template allows you to start a simple Haskell project, either to create a
library or an application. It offers you the choice to customize the source
directory while providing hints on the proposed hierarchy that the author uses
(inspired by other Haskell projects).

In the following sections, I will explain how to use the template.

1. Initial configurations
=========================

Before you get started, there are a few things that this template couldn't
provide for you. You should:

* Add a synopsis to `Genetic.cabal`. It should be a short, one sentence
  explanation of your project.

* Edit the description field in `Genetic.cabal` if you don't like having
  the description in the `README.md` file.

* In `Genetic.cabal`, the category of the project has been set as 'Test'.
  You might wish to change it to a more descriptive value. A list of
  categories that you can use for the project is available on Hackage at
  <http://hackage.haskell.org/packages>. Alternatively, you might prefer using
  a name from the shorter list at
  <https://byorgey.wordpress.com/2010/04/15/cabal-init/>.

* If you haven't provided the `author-email`, `author-name`, and
  `github-username` to the `config.yaml` global file, you will have to search
  for "TODO" markup and complete this information in `Genetic.cabal` and/or
  in `LICENSE`.

2. Creating the git repository
==============================

If this project is a subdirectory of a larger project with an existing version
control or you want to use another version control system or another setup,
then you can ignore this section.

From the root directory of the project (the directory of this file) you will
need to run the following three commands:

    git init
    git add .
    git commit -m "Initial commit"

Now you can create a repository on GitHub to publish the code.

Note that this file is excluded from the repository by being included in the
`.gitignore` file. If you want this file to be tracked, you can remove the
line `/tutorial.md` from that file.

3. Testing the initial code
===========================

These are the stack commands you will likely use the most:

``` sh
# Build the project.
stack build

# Run the binary
stack exec Genetic-exe

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

4. Customizing
==============

As you see, the template creates both a library and a binary and tests the
library using two test suites (doctests from comments and tests with Tasty).
Both test suites can test both properties and expected testcases. Finally,
the template also offers a way to benchmark the code.

Your project might differ significantly from this template. For example, you
might want to have a different number of executables. In that case, you should
remove/add more executable stanzas in `Genetic.cabal`.

Similarly, if you don't want both test suites, you can remove one of the
stanzas. You could do the same for the benchmarks.

*More importantly* you might want to change the contents of the library.
Rename `src/Lib` to whatever you want your top-module to be, usually the name
of your project but using `CamelCase`. Don't forget to change this name in all
places where it is referenced (executable(s), test(s) and benchmark(s)).

Thanks again, and happy hacking!