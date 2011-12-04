# Clojure-clr version of gen_html_docs

I have started to do port of the clojure.contrib.gen_html_docs over to the CLR world.  It is by no means ready for prime time at the moment. 
My plan is to get the CLR version as possible to the original version.  

## TODO
In no real order

* Generate an index page
* Generate an overview page
* Link to the code for the function
* A better way to construct the Namespace TOC on the left of the page. 

## Completed
* compiled a list of all namespaces that use anything from System.*
* Grab the namespace metadata and display the documentation like the function/var docs. 
