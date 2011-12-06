(ns
  #^{ :author "Rob Rowe" :clr true
      :doc "Based on Craig Andera's gen-html-docs. This version Generates a 
an HTML page per namespace.  

THE PLAN:
  Have a flag that generates only CLR specific documentation that uses either
the path or the meta key :clr to indicate what should have docs generated for it."}
 
  clojure.contrib.gen-html-docs
  (:use [clojure.contrib repl-utils prxml string])
  (require [clojure.core])
  (require [clojure.repl :as repl])
  (require [clojure.string :as s]))  

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

;; (defn- is-clr-function
;;  [v]
;;  (or (and (not (nil? (:file (meta v))))
;;       (not (nil? (re-matches #".*clr.*" (:file (meta v))))))
;;      (= (:clr (meta v)) true)))

(defn- anchor-for-member 
  "Returns a suitable HTML anchor name given a library id and a member
  id" 
  [libid memberid]
  (str libid "/" memberid))

;; KEEP THESE TWO FUNCTIONS UNTIL I HAVE THE SOURCE LINK WORKING I
;; MAY USE THEM THEN
;; (defn- id-for-member-source 
;;  "Returns a suitable HTML id for a source listing given a library and
;;  a member"
;;  [libid memberid]
;;  (str "membersource-" libid "-" memberid))

;;(defn- id-for-member-source-link 
;;  "Returns a suitable HTML id for a link to a source listing given a
;;  library and a member"
;;  [libid memberid]
;;  (str "linkto-membersource-" libid "-" memberid))

 (defn- symbol-for 
  "Given a namespace object ns and a namespaceless symbol memberid
  naming a member of that namespace, returns a namespaced symbol that
  identifies that member."
  [ns memberid]
  (symbol (name (ns-name ns)) (name memberid)))

;; (defn- elide-to-one-line 
;;  "Elides a string down to one line."
;;  [s]
;;  (clojure.contrib.string/replace-re #"(\n.*)+" "..." s))

;; (defn- elide-string 
;;  "Returns a string that is at most the first limit characters of s"
;;  [s limit]
;;  (if (< (- limit 3) (count s))
;;    (str (subs s 0 (- limit 3)) "...")
;;    s))

;;(defn- doc-elided-src 
;;  "Returns the src with the docs elided."
;;  [docs src]
;;  (clojure.contrib.string/replace-re (re-pattern (str "\"" docs "\"")) 
;;	  (str "\""
;;		  (elide-to-one-line docs)
;;;; 	          (elide-string docs 10)
;;;;	          "..."
;;		  "\"")
;;	  src))

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

;; (defn- format-source [libid memberid v]
;;  (try
;;   (let [docs (:doc (meta v)) 
;;	 src (if-let [ns (find-ns libid)]
;;	       (get-source (symbol-for ns memberid)))]
;;     (if (and src docs)
;;       (doc-elided-src docs src)
;;       src))
;;   (catch Exception ex
;;     nil)))

;; (defn- is-clr-function
;;  [v]
;;  (and (not (nil? (:file (meta v))))
;;          (not (nil? (re-matches #".*clr.*" (:file (meta v)))))))

(defn- generate-lib-member [libid [n v]]
 ;;(if (is-clr-function v)
  [:div {:id "var-entry"}
   [:br][:hr]
   [:h2 {:id (anchor-for-member libid n)} (str n)]
   [:span {:id "var-type"} (name (member-type v))]
   [:pre {:id "var-usage"} (str (:arglists (meta v)))]
   [:pre {:id "var-docstr"} (extract-documentation v)]
   [:em "Need to figure out how to do the link to the source"]
  ]);;)

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

(defn- load-lib
  [lib]
  (try 
    (require lib)
    (catch System.Exception x nil)))

(defn- generate-function-link
  [lib fname]
   [:div {:style "margin-left: 1em;" :class "toc-entry"}
     [:a {:href (str  "#" lib "/" fname)} fname]])

(defn- generate-lib-link 
  "Generates a hyperlink to the documentation for a namespace given
lib, a symbol identifying that namespace."
  [lib]
   (let [lib-members (sort (ns-publics lib))]
    (map #(generate-function-link lib (key %)) lib-members)))

(defn- generate-lib-links 
  "Generates the list of hyperlinks to each namespace, given libs, a
   vector of symbols naming namespaces."
  [libs]
    (map generate-lib-link libs))

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

;; (defn generate-toggle-namespace-script 
;;  [action toggle-text lib]
;;  (str (format "%s('%s');\n" action (anchor-for-library-contents lib))
;;       (format "setLinkToggleText('%s', '%s');\n" (anchor-for-library-contents-toggle lib) toggle-text)))

;;(defn generate-all-namespaces-action-script 
;;  [action toggle-text libs]
;;  (str (format  "function %sAllNamespaces()" action)
;;       \newline
;;       "{"
;;       \newline
;;       (reduce str (map #(generate-toggle-namespace-script action toggle-text %) libs))
;;       \newline
;;       "}"))

(defn- get-version-number
 []
  (str (:major *clojure-version*) "." (:minor *clojure-version*)))

(defn- html-header
  [lib]
    [:head
           [:title (str (ns-name (first lib)) " - Clojure-clr v" (get-version-number) " API Documentation")]
           [:link {:href "http://clojure.github.com/clojure/static/favicon.png" :rel "shortcut icon"}]
           [:link {:media "all" :type "text/css" :href "http://clojure.github.com/clojure/static/clojure.css" :rel "stylesheet"}]
           [:link {:media "all" :type "text/css" :href "http://clojure.github.com/clojure/static/wiki.css" :rel "stylesheet"}]
	   [:link {:media "all" :type "text/css" :href "http://clojure.github.com/clojure/static/internal.css" :rel "stylesheet"}]
           ])

(defn- get-fn-src
  [func]
  (clojure.repl/source func))

(defn- get-ns-link
  [nspace]
  [:li [:a {:href (str (name nspace) ".html") :class "wiki_link"} (name nspace)]])

(defn- left-nav
  [all-libs]
  [:div {:id "leftcolumn"}
    [:div ];;{:style "text-align: center;"}]
     [:div {:class "menu"}
      [:div {:class "WikiCustomNav WikiElement wiki"}
       [:span {:class "toc-header"}
        [:span {:id "project-name"} "Clojure-clr v"]
        [:span {:id "version"} (get-version-number)]]
       [:br]
       [:ul 
         [:li [:a {:class "wiki_link" :href "index.html"} "Overview"]]
         [:li [:a {:class "wiki_link" :href "api-index.html"} "API Index"]]
       ]
       [:div {:class "NamespaceTOC"}
         [:span {:class "toc-header"} "Namespaces" ]
         [:ul {:id "left-sidebar-list"}
         
           (map get-ns-link all-libs)
         ]
       ]
      ]]])

(defn- get-ns-meta
  [lib]
  (meta (find-ns lib)))

(defn- page-header
  []
  [:div {:id "Header"}
   [:a {:id "Logo" :href "index.html"}
     [:img {:alt "Clojure-clr" :height "100" :width "100" :src "http://richhickey.github.com/clojure-contrib/static/clojure-icon.gif"}]]
   [:h1 
     [:a {:title "page header title" :id "page-header" :href "index.html"} "Clojure-clr API Reference"]]
   ])

(defn- get-body-heading
  [lib-name]
 [:h1 {:id "overview"} "API for "
     [:span {:id "namespace-name"} lib-name " - Clojure-clr v" (:major *clojure-version*) "." (:minor *clojure-version*) (:qualifier *clojure-version*)]])

(defn create-index-ns-funcs-listing
  [lib-name]
  [:div {:id "namespace-entry"}
    [:br][:hr]
    [:h2 (name lib-name)]
    [:a {:href (str (name lib-name) ".html")} "Detailed API documentation"]
    [:br]
    [:pre {:id "namespace-docstr"} (:doc (meta (find-ns lib-name)))]
    "Public variables and functions: "
    (map #(prxml [:span {:id "var-link"} [:a {:href (str (name lib-name) ".html#" (name lib-name) "/" (first %))} (str (first %))]] "  ") (ns-publics lib-name))
    ])

(defn generate-documentation 
  "Generates a single HMTL page for the provided namespace"
  #^{:clr true}
  [libs all-libs]
  (dorun (map load-lib libs))
  (load-lib libs)
  (let [writer (new System.IO.StringWriter)]
    (binding [*out* writer]
      (prxml
        [:html {:xmlns "http://www.w3.org/1999/xhtml"}
          (html-header libs)
          (let [lib-vec (sort libs)]
            [:body
                    [:div {:id "AllContentContainer"}
                     (page-header)
                     (left-nav all-libs)
                     ]
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
                                     [:span {:id "namespace-name"} (name (first libs)) " - Clojure-clr v" (:major *clojure-version*) "." (:minor *clojure-version*) (:qualifier *clojure-version*)]]
                                  (if (not (nil? (:author (get-ns-meta (first libs))))) 
                                      [:span {:id "author"} "by " (:author (get-ns-meta (first libs)))])
                                  
                                  [:H4 {:style "margin-top: 1em;"} "Usage: "] 
                                  
                                   [:pre
"(ns your-namespace
   (:require " (name (first libs)) "))"]
                                   [:h2 "Overview"]
                                   [:pre {:id "namespace-docstr"} (:doc (get-ns-meta (first libs)))]
				   [:br]
				   [:h2 "Public Variables and Functions"]
                                   (map generate-lib-doc lib-vec)
                                  
                                 ]]]]]]] )
                
        ]))
    (.ToString writer)))

(defn generate-index-file
  [libs]
  (let [writer (new System.IO.StringWriter)]
    (binding [*out* writer]
      (prxml
        [:html {:xmlns "http://www.w3.org/1999/xhtml"}
          (html-header libs)
           (let [lib-vec (sort libs)]
            [:body
              [:div {:id "AllContentContainer"}
                (page-header)
                (left-nav libs)
              ]
              [:div {:id "rightcolumn"}
                [:div {:id "Content"}
                  [:div {:class "contentBox"}
                    [:div {:class "innerContentBox"}
                      [:div {:id "content_view" :class "wiki wikiPage"}
                        [:div {:id "right-sidebar"}
                          [:div {:id "toc"}
                            [:h1 {:class "nopad"} "Table of Contents"]
                             [:div {:style "margin-left: 1em;" :class "toc-section"}
                               (map #(prxml [:div {:class "toc-section" } [:a {:href (str "#" (name %))} (name %)]]) libs)]
                               ;;(map get-ns-link libs)]
                             ]]
                      [:div {:id "content-tag"}
                        [:h1 {:id "overview"} "API for "
                          [:span {:id "namespace-name"} (name (first libs)) " - Clojure-clr v" (:major *clojure-version*) "." (:minor *clojure-version*) (:qualifier *clojure-version*)]]
			[:br]
                        [:div {:id "project-description"}
                          [:h3 "Important Clojure-clr resources"]
                          [:ul 
			    [:li "The official source code for Clojure-clr is on the "
                              [:a {:href "http://github.com/richhickey/clojure-clr"} "Clojure-clr GitHub source page"]]
                            [:li "Discussions among clojure developers take place in the "
                              [:a {:href "http://groups.google.com/group/clojure-dev"} "Clojure Dev Google group"]]
                            [:li "Issue tracking happens on the "
                              [:a {:href "https://github.com/richhickey/clojure-clr/issues"} "GitHub Project's Issues page"]]
                            [:li "This documentation is maintained in the gh-pages branch of clojure on GitHub and is always available online"
                              [:a {:href "http://rippinrobr.github.com/gen_html_docs/"} "at the Clojure-clr GitHub pages"]
                              "If you wish to have a version for off-line use you can use the download button on the "
                              [:a {:href "https://github.com/rippinrobr/gen_html_docs/tree/gh-pages"} "gh-pages branch page of GitHub"]]
                          ]]
                        [:br]
                        (map create-index-ns-funcs-listing libs)
                        ;; [:div {:id "namespace-entry"}
                        ;;  [:br]
                        ;;  [:hr]  
                        ;;]
                ]]]]]]])])
   (.ToString writer))))
    

(defn generate-documentation-to-file
  [path libs all-libs]
   (if (System.IO.File/Exists path)
     (System.IO.File/Delete path))
       (spit path (generate-documentation libs all-libs)))

(defn generate-documentation-to-files
  [base-path libs]
  (if (not (System.IO.Directory/Exists base-path))
      (System.IO.Directory/CreateDirectory base-path))

  (map #(generate-documentation-to-file (str base-path "\\" % ".html") [%] libs) libs)
  (spit (str base-path "\\index.html")  (generate-index-file libs)))

(defn generate-clr-docs
  [base-path]
  (generate-documentation-to-files base-path 
           ['clojure.clr.io
            'clojure.clr.shell
            'clojure.contrib.gen-html-docs
            'clojure.core
            'clojure.pprint
            'clojure.reflect
            'clojure.string
            'clojure.test-helper]))