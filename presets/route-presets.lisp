(in-package :cl-orgelctl)

(setf *route-presets*
#((:preset 0 :routes nil)
  (:preset nil :routes
           (:all (:main (main 1)) :orgel01
            (:main (ccin 7) :bias-pos (ccin 21) :bias-bw (ccin 22) :level
             (bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
           (:orgel01
            (:level (apply-notch :bias-type (bias-cos :bias-pos :bias-bw)))))
  (:preset nil :routes
           (:orgel01
            (:level (apply-notch :bias-type (bias-wippe :bias-pos :bias-bw)))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                        1 0.5 1))))))
  (:preset nil :routes
           (:orgel01
            (:level
             (apply-notch :bias-type
                          (bias-wippe :bias-pos :bias-bw :levels
                                      #(0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                        1 0.5 1)))
             delay
             (apply-notch :bias-type
                          (bias-cos :bias-pos :bias-bw :levels
                                    #(1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5 1 0.5
                                      1 0.5))))))
  nil
  (:preset nil :routes
           (:all (:main (main 1)) :orgel01
            (:main (ccin 7) :bias-pos (ccin 21) :bias-bw (ccin 22) :osc-level
             (bias-cos :bias-pos :bias-bw))
            :orgel02 (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel03
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel04
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel05
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel06
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel07
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel08
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel09
            (:osc-level (bias-cos :bias-pos :bias-bw)) :orgel10
            (:osc-level (bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (bias-cos :bias-pos :bias-bw))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :level
             (permute (bias-cos :bias-pos :bias-bw)
                      #(16 10 15 11 2 12 3 13 5 14 9 4 7 8 6 1)))))
  (:preset nil :routes
           (:orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :main (ccin 2) :level
             (permute (bias-cos :bias-pos :bias-bw)
                      #(16 10 15 11 2 12 3 13 5 14 9 4 7 8 6 1)))))
  nil
  (:preset nil :routes
           (:all (:main (main 1)) :orgel01
            (:bias-pos (ccin 0) :bias-bw (ccin 1) :global
             ((apply-notch :bias-type
                           (bias-cos :bias-pos :bias-bw :targets
                                     *global-targets* :levels *global-amps*))
              *global-targets*))))
  nil nil nil nil nil (:preset nil :routes (:all (:main (ccin 0)))) nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
  nil nil nil nil nil nil nil nil))