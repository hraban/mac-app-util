#!/usr/bin/env sbcl --script

(require "asdf")
(asdf:load-system "alexandria")

(defpackage #:script
  (:use #:cl)
  (:local-nicknames (#:alex #:alexandria)))

(in-package #:script)

(format T "Hello: ~{~A~^, ~}~%" (alex:iota 9))
