(ns hiccup-materialize.components
  (:require [clojure.string :as string]
            #?(:clj [hiccup.def :refer [defelem]]
               :cljs crate.element))
  #?(:cljs (:require-macros [crate.def-macros :refer [defelem]])))

(defn- classes [m]
  (->> m
       (filter second)
       (map (comp #(cond->> %
                     (not (vector? %)) vector
                     true (remove nil?)
                     true (map (fn [x]
                                 (try (name x)
                                      (catch #?(:clj Exception
                                                :cljs js/Object) e x))))
                     true (string/join "-"))
                  first))
       (string/join " ")))

(def merge-attrs
  (partial merge-with (fn [a b] (if (keyword? a) b (string/join " " [a b])))))

(defelem icon [icon] [:i.material-icons (some-> icon name)])
(defelem badge [text] [:span.badge text])

(defn collection [{:keys [header] :as attrs} & content]
  (into [:div.collection
         (merge-attrs
           {:class (classes {:with-header header})}
           (dissoc attrs :header))
         (when header [:div.collection-header header])]
        content))

(defn collection-item [{:keys [href title secondary avatar]
                        :as attrs} & content]
  [(if href :a :div)
   (merge-attrs {:class (classes {:collection-item true
                                  :avatar avatar})}
                (dissoc attrs :secondary :avatar))
   avatar
   (into (when avatar [:span.title]) [title])
   (into [:span] content)
   [:div.secondary-content secondary]])

(defn button [{:keys [href floating? size pulse? flat? waves]
               :or {waves :light}
               :as attrs} & content]
  (into
    [(if href :a :button)
     (merge-attrs {:class (classes {[:btn :floating] floating?
                                    :btn (not (or floating? size flat?))
                                    [:btn size] size
                                    [:btn :flat] flat?
                                    :waves-effect waves
                                    [:waves waves] waves
                                    :pulse pulse?})
                   :type (when-not href :button)}
                  (dissoc attrs :floating? :size :pulse? :flat? :waves))]
    content))

(defn card [{:keys [title actions image size reveal] :as attrs} & content]
  (let [card-content [:div.card-content
                      (when-not image
                        [:span.card-title {:class (classes {:activator reveal})}
                         title
                         (when reveal (icon {:class "right"} :more_vert))])]
        card-reveal [:div.card-reveal
                     [:span.card-title title (icon {:class "right"} :close)]
                     reveal]]
    [:div.card (merge-attrs
                 {:class (classes {size size})}
                 (dissoc attrs :actions :image :size :reveal))
     (when image [:div.card-image image [:span.card-title title]])
     (some->> content (into card-content))
     (when reveal card-reveal)
     (some->> actions (into [:div.card-action]))]))

(defelem collapsible [& content]
  (into [:ul.collapsible {:data-collapsible :accordion}]
        content))

(defelem tabs [& content]
  (into [:ul.tabs] content))

(defelem tab [& content]
  (into [:li.tab] content))

(defn collapsible-item [{:keys [header] :as attrs} & content]
  [:li (dissoc attrs :header)
   [:div.collapsible-header header]
   (into [:div.collapsible-body] content)])

(defn modal [{:keys [actions fixed-footer? bottom-sheet?]
              :or {actions [(button
                              {:class "modal-action modal-close"}
                              "Close")]}
              :as attrs} & content]
  [:div.modal (merge-attrs
                {:class (classes {:modal-fixed-footer fixed-footer?
                                  :bottom-sheet bottom-sheet?})}
                (dissoc attrs :actions :fixed-footer? :bottom-sheet?))
   (into [:div.modal-content] content)
   (into [:div.modal-footer] actions)])

(defn switch [{:keys [off-label on-label]
               :or {on-label "On", off-label "Off"}
               :as attrs}]
  [:div.switch
   [:label
    off-label
    [:input (merge-attrs {:type :checkbox}
                         (dissoc attrs :off-label :on-label))]
    [:span.lever]
    on-label]])

;TODO inline
;TODO custom error or success messages
;TODO file input, range, date, time
;TODO Break it off into multimethods
(defn input [{:keys [value type name label id inline?
                     validate? browser-default? placeholder label-class
                     helper-text prefix]
              :as attrs
              :or {label (or (some-> name clojure.core/name string/capitalize) "Label")
                   id (cond-> name
                        (#{:radio} type) (str "_" value))
                   validate? true
                   type :text}} & content]
  (let [{:keys [input-el input-type wrapper]
         :or {wrapper [:div {:class (classes {:input-field (not browser-default?)
                                              :inline inline?})}]
              input-el :input
              input-type type}}
        (get {:select {:input-el :select, :input-type nil}
              :checkbox {:wrapper [:p]}
              :radio {:wrapper [:p]}
              :textarea {:input-el :textarea, :input-type nil}}
             type)
        label-el (when label [:label {:for id :class label-class} label])]
    (if (= type :switch)
      (switch (merge-attrs {:id id} (dissoc attrs :type)))
      (conj wrapper
            (some->> :icon attrs (icon {:class "prefix"}))
            (when prefix [:span.prefix prefix])
            (when browser-default? label-el)
            (into [input-el
                   (merge-attrs
                     {:type input-type
                      :id id
                      :class (classes
                               {:validate validate?
                                :browser-default browser-default?
                                :materialize-textarea (= type :textarea)})}
                     (dissoc attrs
                             :label :icon :validate? :browser-default? :inline?))
                   (when (and (#{:select} type) placeholder)
                     [:option {:value ""} placeholder])]
              content)
            (when-not browser-default? label-el)
            (when helper-text [:span.helper-text helper-text])))))

(comment
  (input {:type :text, :label "First Name" :id :first_name})
  (input {:type :select, :name :country, :placeholder "Select Country"
          :browser-default? true})
  (input {:type :checkbox})
  (input {:type :radio :name :color :value :black})
  (input {:type :switch, :name :hej, :off-label "DOWN"
          :on-label "UP"})
  (input {:type :textarea :icon :account_circle})
  (input {:type :switch :name :notificationsEnabled :defaultChecked true}))
