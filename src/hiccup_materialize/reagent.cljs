(ns hiccup-materialize.reagent
  (:require [reagent.core :as reagent]))

(defn tabs [[_ {{:keys [activeIndex]} :data-tabs}]]
  (let [get-instance #(js-invoke js/M.Tabs "getInstance" (reagent/dom-node %))
        destroy #(some-> % get-instance (js-invoke "destroy"))
        get-opts #(get-in (reagent/children %) [0 1 :data-tabs])
        initialize #(new M.Tabs (reagent/dom-node %) (clj->js (get-opts %)))]
    (reagent/create-class
      {:component-did-mount initialize
       :component-did-update
       (fn [this]
         (let [{:keys [activeIndex]} (get-opts this)]
           (when-not (= activeIndex (-> this get-instance (aget "index")))
             (doto this destroy initialize))))
       :reagent-render identity
       :component-will-unmount destroy})))

(defn- create-materialize-wrapper [plugin-name opts-attr]
  (let [instance (atom nil)]
    (reagent/create-class
      {:component-did-mount
       (fn [this]
         (->> (-> this reagent/children (get-in [0 1 opts-attr]) clj->js)
              (new (aget js/M plugin-name) (reagent/dom-node this))
              (reset! instance)))
       :component-will-unmount (fn [_]
                                 (when (aget @instance "close")
                                   (js-invoke @instance "close"))
                                 (js-invoke @instance "destroy"))
       :reagent-render identity})))

(def select (create-materialize-wrapper "Select" :data-select))
(def collapsible (create-materialize-wrapper "Collapsible" :data-collapsible))
(def modal (create-materialize-wrapper "Modal" :data-modal))
(def dropdown (create-materialize-wrapper "Dropdown" :data-dropdown))
