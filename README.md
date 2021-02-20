# kev-nrepl

repo for added emacs functionality

## features

- goto usage or definition using clj-kondo


 TODO factor all the bs into an emacs package so others can use it
 TODO goto java source? - will be more intense than kondo

## TO DO

gotos
- elisp override ,gg with this
- navigation stack. need "go back" when u go to java sources
- fix buffer nav stack when going to defn
- guava classpath might be the move
 https://stackoverflow.com/questions/15720822/how-to-get-names-of-classes-inside-a-jar-file
 for clojure sources though, tools.namespace should be fine
  - see if you can just add this to :lint when passing to analysis at least for ns
  - clojure.tools.namespace should have stuff for clojure sources
  - clojure.java.classpath might help too

longer term
 - maybe implement formatting too since that sucks ass
 - inversion of control on the elisp side for flexible commands. offload as
 much as possible to clojure
