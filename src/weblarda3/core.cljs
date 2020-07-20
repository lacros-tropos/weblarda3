(ns weblarda3.core
  (:require
   [reagent.core :as reagent]
   [cljs.core.async :as async]
   [cljs-http.client :as http]
   [antizer.reagent :as ant]
   [clojure.string :as string]
   [goog.iter]
   [weblarda3.vis :as vis]))
   

;; tiny helpers

(defn keys-to-str [list]
 ;(console.log (str list))
 (goog.iter/join (clj->js (map name list)) "|"))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

; dev or hard coded
;(def host "http://larda.tropos.de/larda3/")
;get from index.html <script> tag
(def host (first (string/split js/hostaddr #"\?")))
(println "set host " host)

(defonce app-state
  (reagent/atom {:c-list ["dummy"]
                 :c-selected "lacros_dacapo"
                 :c-info {}
                 :param-sel ["MIRA|Zg"]
                 :max-files 30
                 :host host}))

(swap! app-state assoc-in [:host] host)
(println (get @app-state :host))

(defonce modal1 (reagent/atom {:show false
                               :title ""
                               :content "loading.."}))


(defn set-initial-selection [c-info]
 (if (get-in c-info [:connectors :MIRA :params :Zg])
   (swap! app-state assoc-in [:param-sel] ["MIRA|Zg"])
   (let [system (first (keys (get-in c-info [:connectors])))
         param (first (keys (get-in c-info [:connectors system :params])))]
    (swap! app-state assoc-in [:param-sel] [(keys-to-str [system param])]))))

;; http request stuff

(defn fetch-c-info [c-name]
  (ant/notification-open {:message "loading data availability " :description c-name :duration 0 :key "retData"})
  (swap! app-state assoc-in [:param-sel] [])
  (async/go (let [response (async/<! (http/get (str host "api/" c-name "/") {:as :json :with-credentials? false}))]
             ;(println (:status response) (:body response))
              (println "fetched new c-info for " c-name (:status response))
              (ant/notification-close "retData")
              (ant/notification-info {:message "Hint" :description "right-click on parameter to display description text" :duration 40})
              (ant/notification-info {:message "Hint" :description "left-click on day to access explorer" :duration 40})
              (set-initial-selection (get-in response [:body]))
              (swap! app-state assoc-in [:c-info] (get-in response [:body])))))

(async/go (let [response (async/<! (http/get (str host "api/") {:as :json :with-credentials? false}))
                query-string (second (re-find #"camp=([^&]*)" (.. js/window -location -search)))]
            ;(println "found string " query-string (get-in response [:body :campaign_list]))
            (if-not (some #{(get-in @app-state [:c-selected])} (get-in response [:body :campaign_list]))
             (swap! app-state assoc-in [:c-selected] (first (get-in response [:body :campaign_list]))))
            (if (and (not (nil? query-string)) (some #{query-string} (get-in response [:body :campaign_list])))
              (swap! app-state assoc-in [:c-selected] query-string))
            ; at first check if Query_String provided campaign
            (println "fetch campaign list" (:status response) (:body response))
            (fetch-c-info (get @app-state :c-selected))
            (swap! app-state assoc-in [:c-list] (get-in response [:body :campaign_list]))))


(defn fetch-description [param_string]
  (let [c-name (get-in @app-state [:c-selected])
        system (first (string/split param_string #"\|")) 
        param (second (string/split param_string #"\|"))]
    (println "request string" (str host "description/" c-name "/" system "/" param))
    (swap! modal1 assoc-in [:title] (str system ": " param))
    (async/go (let [response (async/<! (http/get (str host "description/" c-name "/" system "/" param) {:as :raw :with-credentials? false}))]
                ;(println "retrieved" (:status response) (:body response))
                (println "fetched new description string " c-name system param (:status response))
                (swap! modal1 assoc-in [:content] (get-in response [:body]))))))

;; change functions

(defn max-no-files [sel-param-to-path]
  (let [; convert the selected paths form system|param to [[sys param] [sys param]]
        selected-path (mapv #(string/split % #"\|") (vals sel-param-to-path))
        ; extract the availability hashmaps into vector
        avail-all (mapv #(get-in @app-state [:c-info :connectors (keyword (first %)) :avail (keyword (second %))]) selected-path)
        ; take just the values
        just-no (mapv vals avail-all)
        max-per-day (apply max (flatten just-no))
        max-files-per-day (if (nil? max-per-day) 24 max-per-day)]
    ;(println "selected-pahts" selected-path)
    ;(println "max-files-per-day" max-files-per-day)
    (swap! app-state assoc-in [:max-files] max-files-per-day)))


(defn change-campaign [c-name]
  (let [query-string (.. js/window -location -search)]
    (if (count query-string)
     (js/window.history.replaceState (clj->js nil) (clj->js nil) (string/replace query-string #"camp=([^&]*)" (str "camp=" c-name)))))
 (swap! app-state assoc-in [:c-selected] (js->clj c-name))
 (fetch-c-info c-name))

(defn update-sel-params [list]
  (let [valid_list(filter #(string/includes? % "|") (js->clj list))]
   (println "update-selparams" valid_list)
   (max-no-files (vis/map-params-to-path valid_list (get-in @app-state [:c-info :connectors])))
   (swap! app-state assoc-in [:param-sel] (filterv #(not (string/includes? % ":")) valid_list))))


(defn regroup-param-path [param-path]
 (let [paths (vec (set (vals param-path)))
       path-param (mapv (fn [p] [p (mapv first (filter #(= p (val %)) param-path))]) paths)]
   (into {} path-param)))

(defn right-click-tree [inp]
  (let [param_string (-> inp .-node .-props .-eventKey)
        valid? (string/includes? param_string "|")]
    (if valid? 
     (do (fetch-description param_string) (swap! modal1 update-in [:show] not))
     (ant/notification-error {:message "Error" :description (str "Description only available for paramters, not systems")}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page

(defn select-campaign []
 [:div "campaign " [ant/select {:showSearch true
                                :placeholder "select campaign"
                                :value (get-in @app-state [:c-selected])
                                :onChange change-campaign
                                :style {:width "150px"} :size "small"}
                    (for [c-name (get-in @app-state [:c-list])] ^{:key c-name} [ant/select-option {:value c-name} c-name])]
  [:br]
  "max files   " [ant/input {:size "small" :style {:margin-left "6px" :margin-top "5px" :width "40px" :text-align "right"}
                             :value (get-in @app-state [:max-files])
                             :onChange #(swap! app-state assoc-in [:max-files] (js->clj (js/parseInt (.. % -target -value))))}]])


;(defn tree-nodes [system]
;(println "tree-nodes" system)
;(for [param (keys (get-in @app-state [:c-info :connectors (keyword system) :params]))] [ant/tree-tree-node {:title param}]))


(defn params-tree []
 [ant/tree {:checkable true :onCheck update-sel-params :onRightClick right-click-tree
            :checkedKeys (get @app-state :param-sel) :multiple false} 
  (doall (for [system (sort (keys (get-in @app-state [:c-info :connectors])))] 
          ^{:key system} [ant/tree-tree-node {:title system :selectable false :style {:font-size 12}} 
                          (for [param (sort (keys (get-in @app-state [:c-info :connectors (keyword system) :params])))] 
                            ^{:key (keys-to-str [system param])} [ant/tree-tree-node {:title param}])]))])



(defn line-in-info [k v] 
 [:tr [:td (first (string/split k #"\|"))] 
  [:td (second (string/split k #"\|"))] 
  [:td (goog.iter/join (clj->js (mapv #(second (string/split % #"\|")) v)) ",  ")]])

(defn info-area []
  (fn []
   (let [param-sel (get-in @app-state [:param-sel])
         connectors (get-in @app-state [:c-info :connectors])
         sel-param-to-path (vis/map-params-to-path param-sel connectors)
         regrouped (regroup-param-path sel-param-to-path)]
     (println "info-area" regrouped)
     (if (> (count (keys regrouped)) 4) 
      (ant/notification-error {:message "Error" :description (str "Too may file identifiers selected. Reduce to 4")}))
     [:div.info-area
      [:table [:thead [:tr [:th "System"] [:th "File identifier"] [:th "Parameters"]]]
       [:tbody (for [[k v] regrouped] ^{:key k} [line-in-info k v])]]])))
   

(defn calendar-component []
  (reagent/create-class
   {:reagent-render #(vis/calendar-render app-state)
    :component-did-mount #(vis/calendar-did-mount app-state)
    :component-did-update #(vis/calendar-did-mount app-state)}))


(defn display-modal []
    (fn []
       [ant/modal {:visible (get-in @modal1 [:show]) :title (str "Description " (get-in @modal1 [:title]))
                   :on-ok #(reset! modal1 {:show false :title "" :content "loading..."}) 
                   :on-cancel #(reset! modal1 {:show false :title "" :content "loading..."})}
        (reagent/as-element [:div {:dangerouslySetInnerHTML {:__html (string/replace (get-in @modal1 [:content]) #"\n" "<br />")}}])]))
       

(defn page []
  [:div
   [:h1 "Data availability"]
   [:div#contents 
    [:div#menu
     [select-campaign]
     [params-tree]]
    [:div#right
     (if-not (empty? (get @app-state :c-info)) [info-area])
     (if-not (empty? (get @app-state :c-info)) [calendar-component])]]
   ;[:br][ant/col {:span 24} [:div (str (get @app-state :param-sel))][:div (str @app-state)]]
   [display-modal]
   [:br]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

; (defn dev-setup []
;   (when ^boolean js/goog.DEBUG
;     (enable-console-print!)
;     (println "dev mode")))

(enable-console-print!)
    

(defn reload []
  (reagent/render [page]
                  (.getElementById js/document "app")))

(defn ^:export main []
  ;(dev-setup)
  (reload))

(comment 
   *ns*
  (in-ns 'weblarda3.core)
  (str/replace (.. js/window -location -search) #"camp=([^&]*)" "new")
  (require weblarda3.core :reload-all))
