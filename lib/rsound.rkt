#lang typed/racket

(require/typed rsound
               [#:struct rsound (
                [data : (any/c s16vector?)]
                [start : Positive-Integer]
                [stop : Positive-Integer]
                [sample-rate : Positive-Real])])