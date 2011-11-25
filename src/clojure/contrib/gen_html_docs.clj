(ns clojure.contrib.gen-html-docs
  (:use [clojure.contrib repl-utils prxml string])
  (require [clojure.core])
  (require [clojure.string :as s]))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc generation constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *script* " // <![CDATA[

function getElem(id)
{
  if( document.getElementById )
  {
    return document.getElementById( id )
  }
  else if ( document.all )
  {
    return eval( 'document.all.' + id )
  }
  else
    return false;
}

function setDisplayStyle(id,displayStyle)
{
  var elem = getElem (id)
  if (elem)
  {
    elem.style.display = displayStyle
  }

}

function setLinkToggleText (id, text)
{
 var elem = getElem (id)
 if (elem)
 {
   elem.innerHTML = text
 }
}

function collapse(id)
{
  setDisplayStyle (id, 'none')
}

function expand (id)
{
  setDisplayStyle (id, 'block')
}

function toggleSource( id )
{
  toggle(id, 'linkto-' + id, 'Hide Source', 'Show Source')
}

function toggle(targetid, linkid, textWhenOpen, textWhenClosed)
{
  var elem = getElem (targetid)
  var link = getElem (linkid)

  if (elem && link)
  {
    var isOpen = false
    if (elem.style.display == '')
    {
      isOpen = link.innerHTML == textWhenOpen
    }
    else if( elem.style.display == 'block' )
    {
      isOpen = true
    }
    
    if (isOpen)
    {
      elem.style.display = 'none'
      link.innerHTML = textWhenClosed
    }
    else
    {
      elem.style.display = 'block'
      link.innerHTML = textWhenOpen
    }
  }
}

      //]]>
")

(defn- extract-documentation 
  "Pulls the documentation for a var v out and turns it into HTML"
  [v]
  (if-let [docs (:doc (meta v))]
    (map 
     (fn [l] 
       [:div {:class "library-member-doc-line"} 
	(if (= 0 (count l)) 
	  [:span {:class "library-member-doc-whitespace"} " "] ; We need something here to make the blank line show up
	  l)]) 
     (s/split docs #"\n")) 
    ""))

(defn- member-type 
  "Figures out for a var x whether it's a macro, function, var or multifunction"
  [x]
  (try 
   (let [dx (deref x)] 
     (cond 
      (:macro (meta x)) :macro 
      (fn? dx) :fn 
      (= clojure.lang.MultiFn (:tag (meta x))) :multi 
      true :var))
   (catch Exception e
     :unknown)))

(defn- anchor-for-member 
  "Returns a suitable HTML anchor name given a library id and a member
  id" 
  [libid memberid]
  (str "member-" libid "/" memberid))

(defn- anchor-for-library 
  "Given a symbol id identifying a namespace, returns an identifier
suitable for use as the name attribute of an HTML anchor tag."
  [id]
  (str "library-" id))

(defn- id-for-member-source 
  "Returns a suitable HTML id for a source listing given a library and
  a member"
  [libid memberid]
  (str "membersource-" libid "-" memberid))

(defn- id-for-member-source-link 
  "Returns a suitable HTML id for a link to a source listing given a
  library and a member"
  [libid memberid]
  (str "linkto-membersource-" libid "-" memberid))

(defn- symbol-for 
  "Given a namespace object ns and a namespaceless symbol memberid
  naming a member of that namespace, returns a namespaced symbol that
  identifies that member."
  [ns memberid]
  (symbol (name (ns-name ns)) (name memberid)))

(defn- elide-to-one-line 
  "Elides a string down to one line."
  [s]
  (clojure.contrib.string/replace-re #"(\n.*)+" "..." s))

(defn- elide-string 
  "Returns a string that is at most the first limit characters of s"
  [s limit]
  (if (< (- limit 3) (count s))
    (str (subs s 0 (- limit 3)) "...")
    s))

(defn- doc-elided-src 
  "Returns the src with the docs elided."
  [docs src]
  (clojure.contrib.string/replace-re (re-pattern (str "\"" docs "\"")) 
	  (str "\""
		  (elide-to-one-line docs)
;; 	          (elide-string docs 10)
;;	          "..."
		  "\"")
	  src))

(defn- anchor-for-library-contents 
  "Returns an HTML ID that identifies the element that holds the
documentation contents for the specified library."
  [lib]
  (str "library-contents-" lib))

(defn- anchor-for-library-contents-toggle 
  "Returns an HTML ID that identifies the element that toggles the
visibility of the library contents."
  [lib]
  (str "library-contents-toggle-" lib))

(defn- format-source [libid memberid v]
  (try
   (let [docs (:doc (meta v)) 
	 src (if-let [ns (find-ns libid)]
	       (get-source (symbol-for ns memberid)))]
     (if (and src docs)
       (doc-elided-src docs src)
       src))
   (catch Exception ex
     nil)))

(defn- generate-lib-member [libid [n v]]
  [:div {:id "var-entry"}
   [:br][:hr]
   [:h2 {:id (anchor-for-member libid n)} (str n)]
   [:span {:id "var-type"} (name (member-type v))]
   [:br]
   [:pre {:id "var-usage"} (str (:arglists (meta v)))]
   [:pre {:id "var-docstr"} (extract-documentation v)]
   [:em "Need to figure out how to do the link to the source"]
  ])
;;   [:dt {:class "library-member-name"}
;;     (str n)]
;;    [:dd 
;;     [:div {:class "library-member-info"}
;;      [:span {:id "var-type"} (name (member-type v))]
;;      " "
;;      [:span {:class "library-member-arglists"} (str (:arglists (meta v)))]]
;;     (into [:div {:class "library-member-docs"}] (extract-documentation v))
;;       (let [member-source-id (id-for-member-source libid n)
;;	   member-source-link-id (id-for-member-source-link libid n)]
;;       (if-let [member-source (format-source libid n v)] 
;;	 [:div {:class "library-member-source-section"}
;;	  [:div {:class "library-member-source-toggle"}
;;	   "[ "
;;	   [:a {:href (format "javascript:toggleSource('%s')" member-source-id)
;;		:id member-source-link-id} "Show Source"]
;;	   " ]"]	  
;;	  [:div {:class "library-member-source" :id member-source-id}
;;	   [:pre member-source]]]))]]])

(defn- generate-lib-member-link 
  "Emits a hyperlink to a member of a namespace given libid (a symbol
identifying the namespace) and the vector [n v], where n is the symbol
naming the member in question and v is the var pointing to the
member." 
  [libid [n v]]
  [:a {:class "lib-member-link" 
       :href (str "#" (anchor-for-member libid n))} (name n)])

(defn- generate-lib-doc 
  "Emits the HTML that documents the namespace identified by the
symbol lib."
  [lib]
   (let [ns (find-ns lib)]
     (if ns 
       (let [lib-members (sort (ns-publics ns))]
	  (into [:div]
                 (map #(generate-lib-member lib %) lib-members)));;])
)))

(defn load-lib
  [lib]
  (try 
    (require lib)
    (catch System.Exception x nil)))

(defn- generate-function-link
  [fname]
   [:div {:style "margin-left: 1em;" :class "toc-entry"}
     [:a {:href (str "#" fname)} fname]])

(defn- generate-lib-link 
  "Generates a hyperlink to the documentation for a namespace given
lib, a symbol identifying that namespace."
  [lib]
    (map #(generate-function-link (key %)) (sort (ns-publics lib))))
;;  (let [ns (find-ns lib)]
;;    (if ns
;;      [:div {:style "margin-left: 1em;" :class "toc-entry"}
;;        [:a {:class "lib-link" :href (str "#" (anchor-for-library lib))} (str (ns-name ns))]]     
;;      )))

(defn- generate-lib-links 
  "Generates the list of hyperlinks to each namespace, given libs, a
vector of symbols naming namespaces."
  [libs]
    (map generate-lib-link libs))
;;  (into [:div {:class "lib-links"} 
;;	 [:div {:class "lib-link-header"} "Namespaces"
;;	  [:span {:class "all-libs-toggle"} 
;;	   " [ "
;;	   [:a {:href "javascript:expandAllNamespaces()"}
;;	    "Expand All"]
;;	   " ] [ "
;;	   [:a {:href "javascript:collapseAllNamespaces()"}
;;	    "Collapse All"]
;;	   " ]"]]] 
;;	(interpose " " (map generate-lib-link libs)))

(defn- anchor-for-library-contents 
  "Returns an HTML ID that identifies the element that holds the
documentation contents for the specified library."
  [lib]
  (str "library-contents-" lib))

(defn- anchor-for-library-contents-toggle 
  "Returns an HTML ID that identifies the element that toggles the
visibility of the library contents."
  [lib]
  (str "library-contents-toggle-" lib))


(defn generate-toggle-namespace-script 
  [action toggle-text lib]
  (str (format "%s('%s');\n" action (anchor-for-library-contents lib))
       (format "setLinkToggleText('%s', '%s');\n" (anchor-for-library-contents-toggle lib) toggle-text)))

(defn generate-all-namespaces-action-script 
  [action toggle-text libs]
  (str (format  "function %sAllNamespaces()" action)
       \newline
       "{"
       \newline
       (reduce str (map #(generate-toggle-namespace-script action toggle-text %) libs))
       \newline
       "}"))

(defn generate-documentation 
  [libs]
  (dorun (map load-lib libs))
  (let [writer (new System.IO.StringWriter)]
    (binding [*out* writer]
      (prxml
        [:html {:xmlns "http://www.w3.org/1999/xhtml"}
         [:head
           [:title "Clojure documentation browser"]
           [:link {:media "all" :type "text/css" :href "http://richhickey.github.com/clojure-contrib/static/clojure.css" :rel "stylesheet"}]
           [:link {:media "all" :type "text/css" :href "http://richhickey.github.com/clojure-contrib/static/wiki.css" :rel "stylesheet"}]
	   [:link {:media "all" :type "text/css" :href "http://richhickey.github.com/clojure-contrib/static/internal.css" :rel "stylesheet"}]]
          (let [lib-vec (sort libs)]
            [:body
                    [:div {:id "AllContentContainer"}
                     [:div {:id "Header"}
                       [:a {:id "Logo" :href "index.html"}
                         [:img {:alt "Clojure" :height "100" :width "100" :src "http://richhickey.github.com/clojure-contrib/static/clojure-icon.gif"}]]
                       [:h1 "Clojure (CLR) API Reference"]
                     ]
                     [:div {:id "leftcolumn"}
                       [:div {:style "text-align: center;"}]
                       [:div {:class "menu"}
                         [:div {:class "WikiCustomNav WikiElement wiki"}
                           [:div {:class "BranchTOC"}
                            [:a {:class "wiki_link" :href "#"} "Branches"]]]
                       ]]]
                     [:div {:id "rightcolumn"}
                       [:div {:id "Content"}
                         [:div {:class "contentBox"}
                           [:div {:class "innerContentBox"}
                             [:div {:id "content_view" :class "wiki wikiPage"}
                               [:div {:id "right-sidebar"}
                                 [:div {:id "toc"}
                                   [:h1 {:class "nopad"} "Table of Contents"]
                                   [:div {:style "margin-left: 1em;" :class "toc-section"}
                                     [:a {:href "#toc0"} "Overview"]
                                     (generate-lib-links lib-vec) 
                                ]]]
                                [:div {:id "content-tag"}
                                  [:h1 {:id "overview"} "API for "
                                     [:span {:id "namespace-name"} "Namespace name here"]
                                     [:span {:id "branch-name"} "(Branch name)"]]
                                  "by "
                                  [:span {:id "author"} "Author Name Here"]
                                  [:br]
                                  [:br]
                                  "Usage: 
                                  "
                                  [:pre 
                                    "(ns your-namespace
                                     (:require "
                                    [:span {:id "long-name"} "namespace name here"]
                                    "))
                                    "]
                                   [:pre]
                                   [:h2 "Overview"]
                                   [:pre {:id "namespace-docstr"} "Namespace doc string goes here" ]
				   [:br]
				   [:h2 "Public Variables and Functions"]
                                   (map generate-lib-doc lib-vec)
                                  
                                 ]]]]]]] )
                
        ]))
    (.ToString writer)))

(defn generate-documentation-to-file
  [path libs]
  (if (System.IO.File/Exists path)
     (System.IO.File/Delete path))
  (spit path (generate-documentation libs)))