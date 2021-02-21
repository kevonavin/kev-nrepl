# kev-nrepl

repo for added emacs functionality

## features

- goto usage or definition using clj-kondo


 TODO factor all the bs into an emacs package so others can use it
 TODO goto java source

## TO DO

gotos
- clojure source
  - git sources, and jar sources
  - see if you can just add this to :lint when passing to analysis at least for ns
  - clojure.tools.namespace should have stuff for clojure sources
  - clojure.java.classpath might help too
- java source
  - have that pdf for https://www.javadoc.io/doc/com.github.javaparser/javaparser-core/latest/index.html
  - that code in index.clj comment section for getting java source using deps.alpha
- howto install
- incremental update of index for specific files
  - would be an easy and huge win.. pass in in :paths to analysis
- elisp override ,gg with this
- navigation stack. need "go back" when u go to java sources
x fix buffer nav stack when going to defn

longer term
 - maybe implement formatting too since that sucks ass
 - inversion of control on the elisp side for flexible commands. offload as
 much as possible to clojure
