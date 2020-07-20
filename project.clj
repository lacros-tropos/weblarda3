(defproject weblarda3 "0.1.0"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.516"]
                 [reagent "0.8.1"]
                 [cljs-http "0.1.46"]
                 [cljs-ajax "0.8.0"] ; another request library that might work better
                 [antizer "0.3.1"]
                 ;[cljsjs/d3 "4.12.0-0"]
                 [cljsjs/msgpack-lite "0.1.26-0"]
                 [cljsjs/d3 "5.7.0-0"]]

  :min-lein-version "2.5.3"
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.1.7"]]

  :clean-targets ^{:protect false} ["resources/public/js"
                                    "target"]

  :figwheel {:css-dirs ["resources/public/css"]}

  :profiles
  {:dev
   {:dependencies [[figwheel-sidecar "0.5.16"]
                   [cider/piggieback "0.3.1"]
                   [org.clojure/tools.nrepl "0.2.10"]]
    :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
    :plugins      [[lein-figwheel "0.5.18"]]}}
    

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src"]
     :figwheel     {:on-jsload "weblarda3.core/reload"}
     :compiler     {:main                 weblarda3.core
                    :optimizations        :none
                    :output-to            "resources/public/js/app.js"
                    :output-dir           "resources/public/js/dev"
                    :asset-path           "js/dev"
                    :foreign-libs [{:file "src/weblarda3/custom.js" :provides ["custom"]}
                                   {:file "src/weblarda3/colormaps.js" :provides ["colormaps"]}]
                    :source-map-timestamp true}}
    {:id           "min"
     :source-paths ["src"]
     :compiler     {:main            weblarda3.core
                    :optimizations   :advanced
                    ;:optimizations   :none
                    :output-to       "resources/public/js/app.js"
                    :output-dir      "resources/public/js/min"
                    :elide-asserts   true
                    :closure-defines {goog.DEBUG false}
                    :foreign-libs [{:file "src/weblarda3/custom.js" :provides ["custom"]}
                                   {:file "src/weblarda3/colormaps.js" :provides ["colormaps"]}]
                    :infer-externs   true
                    :pseudo-names    true
                    :source-map      "resources/public/js/app.js.map"
                    :pretty-print    false}}
    
    {:id           "dev_explorer"
     :source-paths ["src"]
     :figwheel     {:on-jsload "weblarda3.explorer/reload"}
                    ;:open-urls ["http://localhost:3449/explorer.html"]
     
     
     :compiler     {:main                 weblarda3.explorer
                    :optimizations        :none
                    :output-to            "resources/public/js/app_explorer.js"
                    :output-dir           "resources/public/js2/dev"
                    :asset-path           "js2/dev"
                    :foreign-libs [{:file "src/weblarda3/custom.js" :provides ["custom"]}
                                   {:file "src/weblarda3/colormaps.js" :provides ["colormaps"]}]
                    :source-map-timestamp true}}    
    
    {:id           "min_explorer"
     :source-paths ["src"]
     :compiler     {:main            weblarda3.explorer
                    :optimizations   :simple
                    ;:optimizations   :simple
                    ;:optimizations   :none
                    :output-to       "resources/public/js/app_explorer.js"
                    :output-dir      "resources/public/js/min_expl"
                    :foreign-libs [{:file "src/weblarda3/custom.js" :provides ["custom"]}
                                   {:file "src/weblarda3/colormaps.js" :provides ["colormaps"]}]
                    :elide-asserts   true
                    :closure-defines {goog.DEBUG false}
                    :infer-externs   true
                    :pseudo-names    true
                    :source-map      "resources/public/js/app_explorer.js.map"
                    :pretty-print    false}}]})
    
    


    
