(require 'figwheel-sidecar.repl-api)
(require 'figwheel-sidecar.system)
(require '[clojure.set :refer [rename-keys]])

(def master-config (figwheel-sidecar.system/fetch-config))

(def fe-config
  (as-> (:data master-config) $
        (merge $ {:figwheel-options {:server-port 3451 :css-dirs ["resources/public/css"]}
                  :build-ids ["dev"]})
        (assoc $ :all-builds (mapv #(rename-keys % {:build-options :compiler}) (:all-builds $)))))

(figwheel-sidecar.repl-api/start-figwheel! fe-config)
(figwheel-sidecar.repl-api/cljs-repl "dev")