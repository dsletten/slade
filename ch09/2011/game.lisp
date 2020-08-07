(in-package :guess)

;;;
;;;    The CACHE is used for data integrity. It provides a quick lookup to check
;;;    whether or not a new answer already exists in the game.
;;;    
(defclass game ()
  ((tree :accessor tree :initarg :tree)
   (cache :reader cache :initform (make-hash-table :test #'equalp))
   (category :reader category :initarg :category)
   (determiner-flag :reader determiner-flag :initarg :determiner-flag :initform t)
   (back-up :accessor back-up :initform nil)))

(defmethod print-object ((g game) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~S" (category g))))

(defclass question ()
  ((text :reader text :initarg :text :type string)
   (yes :accessor yes :initarg :yes :type (or question answer))
   (no :accessor no :initarg :no :type (or question answer))))

(defmethod print-object ((q question) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~S" (text q))))

(defun questionp (obj)
  (typep obj 'question))

;;;
;;;    Each ANSWER maintains a reference to the QUESTION to which it corresponds.
;;;    
(defclass answer ()
  ((value :reader value :initarg :value :type string)
   (question :accessor question :initarg :question)))

(defmethod initialize-instance :after ((g game) &rest init-args)
  (declare (ignore init-args))
  (when (slot-boundp g 'tree)
;    (prep-cache g)))
     (cache-node g (tree g))))
;;      (cache-tree g (tree g))))

(defmethod initialize-instance :after ((a answer) &rest init-args)
  (declare (ignore init-args))
  (unless (slot-boundp a 'question)
    (setf (question a) a))) ; What does this accomplish?

(defmethod print-object ((a answer) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~S" (value a))))

;;;
;;;    Prepare the answer cache and also fix the QUESTION slots of the answers so that they
;;;    point to their parent question.
;;;    
;; (defun prep-cache (game)
;;   (labels ((prep-cache-aux (question)
;;              (let ((yes-branch (yes question))
;;                    (no-branch (no question)))
;;                (if (answerp yes-branch)
;;                    (progn (setf (question yes-branch) question)
;;                           (add-answer-to-cache game yes-branch))
;;                    (prep-cache-aux yes-branch))
;;                (if (answerp no-branch)
;;                    (progn (setf (question no-branch) question)
;;                           (add-answer-to-cache game no-branch))
;;                    (prep-cache-aux no-branch)))) )
;;     (prep-cache-aux (tree game))))

;; (defgeneric cache-tree (game node))
;; (defmethod cache-tree ((g game) (q question))
;;   (cache-tree g (yes q))
;;   (cache-tree g (no q)))

;; (defmethod cache-tree ((g game) (a answer))
;;   (add-answer-to-cache g a))

(defgeneric cache-node (game node))
(defmethod cache-node ((g game) (q question))
  (let ((yes-branch (yes q))
        (no-branch (no q)))
    (when (answerp yes-branch)
      (setf (question yes-branch) q))
    (when (answerp no-branch)
      (setf (question no-branch) q))
  (cache-node g yes-branch)
  (cache-node g no-branch)))

(defmethod cache-node ((g game) (a answer))
  (add-answer-to-cache g a))

(defun answerp (obj)
  (typep obj 'answer))

(defgeneric serialize (node))
(defmethod serialize ((q question))
  (list (text q) (serialize (yes q)) (serialize (no q))))

(defmethod serialize ((a answer))
  (value a))

(defgeneric process-node (game node))
(defmethod process-node ((g game) (q question))
  (if (query (format nil "~A " (text q)))
      (process-node g (yes q))
      (process-node g (no q))))

(defmethod process-node ((g game) (a answer))
  (if (query (format nil "~%Are you thinking of ~A? " (determiner-plus-word g (value a))))
      (brag)
      (add-new-entry g a)))

(defgeneric has-answer-p (game answer))
(defmethod has-answer-p ((g game) (a answer))
  (gethash (value a) (cache g)))

(defgeneric add-answer-to-cache (game answer))
(defmethod add-answer-to-cache ((g game) (a answer))
  (if (has-answer-p g a)
      (error "~A already exists as an answer." (value a))
      (setf (gethash (value a) (cache g)) a)))

(defgeneric add-node (game old-answer new-question new-answer new-side))
(defmethod add-node ((g game) (old-answer answer) (new-question question) (new-answer answer) new-side)
  (if (has-answer-p g new-answer)
      (error "~A already exists as an answer." (value new-answer))
      (let ((parent (question old-answer)))
        (cond ((eq old-answer parent) (setf (tree g) new-question))
              ((eq old-answer (yes parent)) (setf (yes parent) new-question))
              (t (setf (no parent) new-question)))
        (setf (question old-answer) new-question)
        (ccase new-side
          (yes (setf (yes new-question) new-answer
                     (no new-question) old-answer))
          (no (setf (no new-question) new-answer
                    (yes new-question) old-answer)))
        (add-answer-to-cache g new-answer)
        (setf (back-up g) t))))

;; (defmethod add-node ((g game) answer (new-question question) (new-answer answer) new-side)
;;   (if (has-answer-p g new-answer)
;;       (error "~A already exists as an answer." (value new-answer))
;;       (multiple-value-bind (parent old-side) (find-parent (tree g) answer)
;;         (let ((old-answer (ccase old-side
;;                             (yes (yes parent))
;;                             (no (no parent))
;;                             ((nil) parent))))
;;           (ccase old-side
;;             (yes (setf (yes parent) new-question))
;;             (no (setf (no parent) new-question))
;;             ((nil) (setf (tree g) new-question)))
;;           (ccase new-side
;;             (yes (setf (yes new-question) new-answer
;;                        (no new-question) old-answer))
;;             (no (setf (no new-question) new-answer
;;                       (yes new-question) old-answer)))
;;           (add-answer-to-cache g new-answer)
;;           (setf (back-up g) t)))) )

