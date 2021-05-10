(ns weblarda3.core
  (:require
   [reagent.core :as reagent]
   [cljs.core.async :as async]
   [cljs-http.client :as http]
   [antizer.reagent :as ant]
   [clojure.string :as string]
   [clojure.set :as set]
   [goog.iter]
   [weblarda3.vis :as vis]))
   

;; tiny helpers

;; (in-ns 'weblarda3.core)

(defn keys-to-str
 "combine the keys [:a :b] to a string separated with a|b"
 [list]
 ;(console.log (str list))
 (goog.iter/join (clj->js (map name list)) "|"))


(defn str-to-keys
 ""
 [str]
 (vec (string/split str #"\|")))

   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vars

; dev or hard coded
;(def host "http://larda.tropos.de/larda3/")
(def host "http://larda3.tropos.de/")
;get from index.html <script> tag
;(def host (first (string/split js/hostaddr #"\?")))
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

(defn join-sys-param
 "given a system and a vec of parameters return [sys|param1 sys|param2 ...]"
 [system params]
 (mapv #(keys-to-str [system %]) params))


(defn all-params
  [info]
  (let [systems (keys (get info :connectors))]
    (mapv #(join-sys-param % (keys (get-in info [:connectors % :params]))) systems)))
   
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
  

(defn set-initial-selection [c-info]
 (let [param-sel (get @app-state :param-sel)
       params (flatten (all-params c-info))
       valid-params (set/intersection (set param-sel) (set params))
       set-params (if (empty? valid-params) [(first params)] (vec valid-params))]
  ;(println "param-sel at set-initial" param-sel)
  ;(println "valid params " valid-params set-params)
  (swap! app-state assoc-in [:param-sel] set-params)))
  
    
(defn get-search []
; ?interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET|WIDTH
 (let [params (string/join "," (get-in @app-state [:param-sel]))] 
  (str "?camp=" (get @app-state :c-selected) "&params=" params)))
     

;; http request stuff
(defn hex-comma-and-split [str]
  (if str 
   (string/split (string/replace str #"%2C" ",") #",") []))

(defn fetch-c-info [c-name]
  (ant/notification-open {:message "loading data availability " :description c-name :duration 0 :key "retData"})
  ;(swap! app-state assoc-in [:param-sel] [])
  (async/go (let [response (async/<! (http/get (str host "api/" c-name "/") {:as :json :with-credentials? false}))]
             ;(println (:status response) (:body response))
              (println "fetched new c-info for " c-name (:status response))
              (ant/notification-close "retData")
              (ant/notification-info {:message "Hint" :description "right-click on parameter to display description text" :duration 40})
              (ant/notification-info {:message "Hint" :description "left-click on day to access explorer" :duration 40})
              (set-initial-selection (get-in response [:body]))
              (swap! app-state assoc-in [:c-info] (get-in response [:body])))

            (js/window.history.replaceState (clj->js nil) (clj->js nil) (get-search))
            (max-no-files (vis/map-params-to-path (get-in @app-state [:param-sel]) (get-in @app-state [:c-info :connectors])))
            ) )
              
              

(async/go (let [response (async/<! (http/get (str host "api/") {:as :json :with-credentials? false}))
                ;query-string (.. address -search)
                query-string (.. js/window -location -search)
                query-camp (second (re-find #"camp=([^&]*)" query-string))
                query-params (hex-comma-and-split (second (re-find #"params=([^&]*)" query-string)))]
            (println "found camp string " query-camp (get-in response [:body :campaign_list]))
            (println "found param string ", query-params)
            (if-not (some #{(get-in @app-state [:c-selected])} (get-in response [:body :campaign_list]))
             (swap! app-state assoc-in [:c-selected] (first (get-in response [:body :campaign_list]))))
            (if (and (not (nil? query-camp)) (some #{query-camp} (get-in response [:body :campaign_list])))
              (swap! app-state assoc-in [:c-selected] query-camp))
            (if-not (nil? query-params)
              (swap! app-state assoc-in [:param-sel] query-params))
            ; at first check if Query_String provided campaign
            (println "fetch campaign list" (:status response) (:body response))
            (fetch-c-info (get @app-state :c-selected))
            (swap! app-state assoc-in [:c-list] (get-in response [:body :campaign_list]))))


(defn fetch-description [param_string]
  (let [c-name (get-in @app-state [:c-selected])
        [system param] (str-to-keys param_string)]
    (println "request string" (str host "description/" c-name "/" system "/" param))
    (swap! modal1 assoc-in [:title] (str system ": " param))
    (async/go (let [response (async/<! (http/get (str host "description/" c-name "/" system "/" param) {:as :raw :with-credentials? false}))]
                ;(println "retrieved" (:status response) (:body response))
                (println "fetched new description string " c-name system param (:status response))
                (swap! modal1 assoc-in [:content] (get-in response [:body]))))))

;; change functions



(defn change-campaign [c-name]
  (let [query-string (.. js/window -location -search)]
    (if (count query-string)
     (js/window.history.replaceState (clj->js nil) (clj->js nil) (string/replace query-string #"camp=([^&]*)" (str "camp=" c-name)))))
 (swap! app-state assoc-in [:c-selected] (js->clj c-name))
 (fetch-c-info c-name))

(defn update-sel-params [list]
  (let [valid_list (filter #(string/includes? % "|") (js->clj list))]
   (println "update-selparams valid_list" valid_list)
   (max-no-files (vis/map-params-to-path valid_list (get-in @app-state [:c-info :connectors])))
   (swap! app-state assoc-in [:param-sel] (filterv #(not (string/includes? % ":")) valid_list))
   (js/window.history.replaceState (clj->js nil) (clj->js nil) (get-search))
   ))


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
