# litpro
literate program processor for SWI Prolog. You can write a markdown document and embed Prolog code using

        %% label "caption" tag1 tag2...
           .
           .
           .
        %%

The lit.pro program will transform that file into a .md file and use pandoc to format the markdown into a nicely formatted pdf (or html etc).  By default, the code chunks are evaluated as Prolog and any output is injected into the text.  Inline snippets of Prolog code (using backticks) are also evaluated.  This kind of "programmatic" generation of text, tables diagrams, etc. is very useful for tutorials, articles or reports, and is the primary motivation for the program.

# Installation
Ensure that you have SWI Prolog and pandoc installed.  Then, download lit.pro (or clone the repo) and compile it with:

        swipl -o lit -g main_4lit -c lit.pro

Note that the lit.pro file in the main repo directory is the development version.  For the latest stable release, go to the [releases](https://github.com/RdR1024/litpro/releases) tab and download the highest numbered release (e.g. [v1.0.0](https://github.com/RdR1024/litpro/archive/v1.0.0.zip)).

# Use
Assuming that you've written a literate Prolog document called "hello.pmd", turn it into a pdf with the following:

        ./lit hello.pmd

(If you compiled on Windows, then you don't need the initial ./ )

For further details on use and the code, see the `litdoc.pdf` file in the doc folder. Also see the examples in the examples folder.


# Pandoc
`lit.pro` uses the wonderful document converter `pandoc` as a post-processor.  You can find pandoc here: https://pandoc.org/

