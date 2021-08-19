(ns clj-list-selector.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    ["ink" :refer [render Text Box useInput useApp]]))



;; TODO: to work as :key later on - assert that :display-name is unique for user-input
;; Using a single ratom
;; accessing parts of it (and getting re-rendered on change) via cursors
;; --
;; it seems it is not idomatic to pass the state-ratom around but rather to access it globally?
;; https://stackoverflow.com/questions/38305198/clojurescript-reagent-pass-atoms-down-as-inputs-or-use-as-global-variables
;; it seems - passing ratoms around make the components mroe reusable (not that it matters here)
;; next step: re-frame
;; https://day8.github.io/re-frame/flow-mechanics/#how-flow-happens-in-reagent
(defonce state (r/atom (->> (iterate inc 1)
                         (map (fn [id] {:display-name (str "item " id) :selected false}))
                         (take 5)
                         (keep-indexed (fn [index item]
                                         (if (== index 0)
                                           (assoc item :highlighted true)
                                           (assoc item :highlighted false)
                                           )))
                         ;; use vec instead of lazyseq, so that update-in, etc., works
                         vec)))


(defn toggle-selection [state item-index]
  (swap! state update-in [item-index :selected] not))
(defn toggle-highlighted [state item-index]
  (swap! state update-in [item-index :highlighted] not))


;; change state to see effects...
(doseq [n (range 1 8)]
  (doseq [[i item] (keep-indexed vector @state)]
    (js/setTimeout #(toggle-selection state i) (* n i 33))))


;; giving an alias to the react-hook
(def <-input useInput)


(defn find-highlighted-index-item [state]
  (let [index-item-of-highlighted (fn [index item] (when (:highlighted item) [index item]))
        index-item (first (keep-indexed index-item-of-highlighted @state))]
    index-item))

(defn highlight-next-item [state]
  (let [[index item] (find-highlighted-index-item state)
        new-index (mod (inc index) (count @state))]
    (toggle-highlighted state index)
    (toggle-highlighted state new-index)))

(defn highlight-prev-item [state]
  (let [[index item] (find-highlighted-index-item state)
        new-index (mod (dec index) (count @state))]
    (toggle-highlighted state index)
    (toggle-highlighted state new-index)))

(defn toggle-selected-on-highlighted-item [state]
  (let [[index _] (find-highlighted-index-item state)]
    (toggle-selection state index)))


;; TODO use Form-2-Components:
;; https://github.com/reagent-project/reagent/blob/master/doc/CreatingReagentComponents.md#form-2--a-function-returning-a-function
;; but are they still necessary!? it works either way...?
(defn list-entry [state]
  (let [display-name (get @state :display-name)
        selected (get @state :selected)
        highlighted (get @state :highlighted)]
    [:> Box
     [:> Text (if highlighted {:color "green"}) "[" (if selected "x" " ") "] " display-name]]))

;; for interop with React-libraries: :> (r/create-class)
(defn app []
  (let [exit (useApp)]
    ;; the input-hook stands alone - no value - no assignment
    (<-input (fn [input key]
               (cond
                 ;; accessing JS-properties with `.-{property}` syntax
                 (.-escape key) ((.-exit exit))
                 (.-downArrow key) (highlight-next-item state)
                 (.-upArrow key) (highlight-prev-item state)
                 (.-return key) (toggle-selected-on-highlighted-item state))))
    [:> Box {:flexDirection "column" :borderStyle "round" :width 20 :paddingX 1}
     (doall
       (map-indexed
         (fn [index item-state]
           ^{:key (:display-name item-state)} [list-entry (r/cursor state [index])])
         ;; TODO get rid of the deref 'inside' the component.
         @state))]))



(defn main []
  ;; :f> is necessary, so that an React function component (not class component) is created.
  (render (r/as-element [:f> app])))

