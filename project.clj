(defproject caliban "0.1.2-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.562"]
                 [org.clojure/core.async "0.2.385"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [reagent "0.6.0-rc"]
                 [figwheel-sidecar "0.5.11"]
                 [cljsjs/d3 "4.2.2-0"]
                 [org.clojure/test.check "0.9.0"]
                 [ganymede "0.1.6-SNAPSHOT"]
                 [binaryage/devtools "0.8.2"]]

  :plugins [[lein-cljsbuild "1.1.1"]
            [s3-wagon-private "1.2.0"]]

  :repositories {"releases" {:url "s3p://nt-cljs/releases"
                             :username :env/NT_CLJS_ID
                             :passphrase :env/NT_CLJS_SECRET }
                 "snapshots" {:url "s3p://nt-cljs/snapshots/"
                              :username :env/NT_CLJS_ID
                              :passphrase :env/NT_CLJS_SECRET }}

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]

                :figwheel {:websocket-url "ws://localhost:3451/figwheel-ws"}

                :compiler {:main caliban.core
                           :preloads [devtools.preload]
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/caliban.js"
                           :output-dir "resources/public/js/compiled/out"}}]})
