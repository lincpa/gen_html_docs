# Clojure-clr version of gen_html_docs

I have started to do port of the clojure.contrib.gen_html_docs over to the CLR world.  It is by no means ready for prime time at the moment. My plan is to get the CLR version as possible to the original version.

## TODO
In no real order

<del> * Grab the namespace metadata and display the documentation like the function/var docs. </del>
* Link to the code for the function
* Add 'real' CLR only documentation support.  Currently it will only document functions that live in a file that have CLR in the path. Obviously this isn't the best way to go.  I'd like to add something to functions that flag them as clr only.
* A better way to construct the Namespace TOC on the left of the page. 