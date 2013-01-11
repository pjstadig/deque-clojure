;;;; Copyright © 2012 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla
;;;; Public License, v. 2.0. If a copy of the MPL was not distributed
;;;; with this file, You can obtain one at
;;;; http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses",
;;;; as defined by the Mozilla Public License, v. 2.0.
(ns name.stadig.deque.protocol
  (:refer-clojure :exclude [first push last])
  (:require [clojure.core :as clj])
  (:import (clojure.lang Seqable)
           (java.util Arrays)))

(defprotocol IDeque
  (last [d])
  (inject [d v])
  (eject [d]))

(defprotocol INode
  (prefix ^objects [d])
  (child ^objects [d])
  (substack ^objects [d])
  (suffix ^objects [d]))
