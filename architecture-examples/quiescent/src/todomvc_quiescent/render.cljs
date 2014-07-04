(ns todomvc-quiescent.render
  (:require [cljs.core.async :as a]
            [quiescent :as q :include-macros true]
            [quiescent.dom :as d])
  (:require-macros [cljs.core.async.macros :as am]))

(defn enter-key?
  "Return true if an event was the enter key"
  [evt]
  (= 13 (.-keyCode evt)))

(q/defcomponent Header
  "The page's header, which includes the primary input"
  [_ submit-ch]
  (d/header {:id "header"}
            (d/h1 {} "todos")
            (d/input {:id "new-todo"
                      :placeholder "What needs to be done?"
                      :onKeyDown
                      (fn [evt]
                        (when (enter-key? evt)
                          (let [v (.-value (.-target evt))]
                            (a/put! submit-ch v)
                            (set! (.-value (.-target evt)) ""))))
                      :autoFocus true})))

(q/defcomponent FilterItem
  "A filtering button"
  [selected? label href]
  (d/li {} (d/a {:className (when selected? "selected")
                 :href href}
                label)))

(q/defcomponent Filters
  "Buttons to filter the list"
  [filter]
  (d/ul {:id "filters"}
        (FilterItem (= :all filter) "All" "#/")
        (FilterItem (= :active filter) "Active" "#/active")
        (FilterItem (= :completed filter) "Completed" "#/completed")))

(q/defcomponent Footer
  "The footer at the bottom of the list"
  [app channels]
  (let [completed (count (filter :completed (:items app)))
        left (- (count (:items app)) completed)]
    (d/footer {:id "footer"}
              (d/span {:id "todo-count"}
                      (d/strong {} (str left " items left")))
              (Filters (:filter app))
              (when (< 0 completed)
                (d/button {:id "clear-completed"
                           :onClick #(a/put! (:clear-completed channels)
                                             :clear)}
                          (str "Clear completed (" completed ")"))))))

(defn class-name
  "Convenience function for creating class names from sets. Nils will
  not be included."
  [classes]
  (apply str (interpose " " (map identity classes))))

(defn hidden?
  "Given an item and the current application filter status, return
  true if the item should be hidden."
  [item filter]
  (or (and (= filter :active) (:completed item))
      (and (= filter :completed) (not (:completed item)))))

(q/defcomponent Item
  "An item in the todo list"
  [item channels]
  (let [done (boolean (:completed item))]
    (d/li {:key           (:id item)
           :className     (class-name #{(when done "completed")
                                        (when (:editing item) "editing")})
           :onDoubleClick #(a/put! (:start-edit channels)
                                   (:id item))}
          (d/div {:className "view"}
                 (d/input {:className "toggle"
                           :type      "checkbox"
                           :checked   done
                           :onClick
                           #(a/put! (:toggle channels)
                                    (:id item))})
                 (d/label {} (:text item))
                 (d/button {:className "destroy"
                            :onClick
                            #(a/put! (:destroy channels)
                                     (:id item))}))
          (q/on-render (d/input {:className    "edit"
                                 :defaultValue (:text item)
                                 :onKeyDown    (fn [evt] (when (enter-key? evt)
                                                           (.blur (.-target evt))))
                                 :onBlur       (fn [evt]
                                                 (let [v (.-value (.-target evt))]
                                                   (a/put! (:complete-edit channels)
                                                           [(:id item) v])))})
                       (fn [node]
                         (when (:editing item) (.focus node)))))))
(q/defcomponent TodoList
  "The primary todo list"
  [app channels]
  (apply d/ul {:id "todo-list"} (->> (:items app)
                                     (filter #(not (hidden? % (:filter app))))
                                     (map #(Item % channels)))))

(q/defcomponent App
  "The root of the application"
  [app channels]
  (let [some-items? (> (count (:items app)) 0)]
    (d/div {}
           (Header nil (:submit channels))
           (d/section {:id "main"}
                      (d/input {:id "toggle-all"
                                :type "checkbox"
                                :checked (and some-items?
                                              (every? :completed (:items app)))
                                :onClick #(a/put! (:toggle-all channels) :toggle-all)})
                      (d/label {:htmlFor "toggle-all"}
                               "Mark all as complete")
                      (TodoList app channels))
           (when some-items?
             (Footer app channels)))))

;; Here we use an atom to tell us if we already have a render queued
;; up; if so, requesting another render is a no-op
(let [render-pending? (atom false)]
  (defn request-render
    "Render the given application state tree."
    [app]
    (when (compare-and-set! render-pending? false true)
      (.requestAnimationFrame js/window
                              #(do (q/render (App @(:state app) (:channels app))
                                             (.getElementById js/document "todoapp"))
                                   (reset! render-pending? false))))))
