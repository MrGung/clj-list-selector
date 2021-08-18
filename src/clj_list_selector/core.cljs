(ns clj-list-selector.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    ["ink" :refer [render Text Box useInput useApp]]))



;; TODO: to work as :key later on - assert that :display-name is unique for user-input
(defonce state (r/atom (->> (iterate inc 1)
                         (map (fn [id] {:display-name (str "item " id) :selected false}))
                         (take 5)
                         (keep-indexed (fn [index item]
                                         (if (== index 0)
                                           (assoc item :highlighted true)
                                           (assoc item :highlighted false)
                                           )))
                         #_(map (fn [item] [(:display-name item) item]))
                         #_(into {}))))

(defn update-selected [state item-key]
  (update-in state [item-key :selected] not))

(defn toggle-selection [state item-index]
  (println item-index)
  (swap! state #(update % item-index (comp not :selected))))
(defn toggle-highlighted [state item-index]
  (println item-index)
  (swap! state #(update % item-index (comp not :highlighted))))


;; change state to see effects...
(doseq [n (range 1 8)]
  (doseq [[i item] (keep-indexed vector @state)]
    (js/setTimeout #(toggle-selection state i) (* n i 33))))


(def <-input useInput)


(defn find-highlighted-index-item [state]
  (let [index-item (first (keep-indexed (fn [index item] (:hightlighted item)) @state))]
    index-item))

(defn highlight-next-item [state]
  (let [[index item] (find-highlighted-index-item state)
        new-index (mod (inc index) (count @state))]
    (toggle-highlighted state index)
    (toggle-highlighted state new-index)
    nil))

(defn highlight-prev-item [state]
  (let [[index item] (find-highlighted-index-item state)
        new-index (mod (dec index) (count @state))]
    (toggle-highlighted state index)
    (toggle-highlighted state new-index)
    nil))


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
    (<-input (fn [input key]
               (cond
                 (.-escape key) (println "exit")            ; (exit)
                 ;;(.-downArrow key) (println "down")         ;(highlight-next-item state)
                 ;;(.-upArrow key) (println "up")             ; (highlight-prev-item state)
                 (.-return key) (println "return") #_(swap! @state #(update % :selected not)))))
    [:> Box {:flexDirection "column" :borderStyle "round" :width 20 :paddingX 1}
     (doall
       (for [item-state @state
             :let [key (:display-name item-state)]]
         ^{:key key} [list-entry (r/cursor state [key])]))]))



(defn main []
  ;; :f> is necessary, so that an React function component (not class component) is created.
  (render (r/as-element [:f> app])))

