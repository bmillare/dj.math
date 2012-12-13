(ns dj.math.linearalgebra
  (:require [dj.math :as dm]
            [dj.math.matrix :as dmm]
            [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]))

(defn qr-decomp-step
  [A I I-size Q R]
  (let [x (first (dmm/col-seq A))
        x-size (count x)
        idx (- I-size x-size)]
    (if (= 1 x-size)
      {:Q Q
       :R (dmm/assoc-minor R
                           [idx idx]
                           x)}
      (let [
            ;; computes reflection matrix given column vector x and e is a
            ;; column vector that is part of a basis of orthonormal vectors,
            ;; usually [1,0...0]^T
            e (dmm/t (dmm/v [(vec (concat [1]
                                          (repeat (dec x-size)
                                                  0)))]))
            ;; the sign of alpha should be opposite of kth coordinate of x, since
            ;; we want u to be as small as possible
            ;; you can use Math/copySign or in cuda copysignf
            alpha (dm/copy-sign (dmm/vnorm x)
                                (first (seq x)))
            I-step (dmm/minor I
                              [idx idx])
            ;; if u is the zero vector, Q should just be the identity (BUG)
            u (dm/- x (dm/* alpha e))
            ;; the next two lines are exploding in code size
            ;; detected reuse since its u over norm u
            v (dm/d u (dmm/vnorm u))
            ;; detected reuse since v times v
            Q-step (dm/- I-step (dm/* 2 (dm/* v (dmm/t v))))
            ;; ideas for fixing
            ;; 1. do optimized substitutions
            ;; 2. matrix operated with a scalar is a place for reuse
            ;; 3. create new forms that do special things when emitted
            ;; 4. maybe a let form?
            ;; 5. precomputing values?
            ;; 6. at each qr-decomp-step evaluate expression?

            ;; this is to improve accuracy
            Q-step-1A (dmm/assoc-minor (dm/* Q-step
                                             A)
                                       [0 0]
                                       (dm/* e
                                             alpha))
            A' (dmm/minor Q-step-1A
                          [1 1])]
        (qr-decomp-step A'
                        I
                        I-size
                        (dm/* Q (dmm/assoc-minor I
                                                 [idx idx]
                                                 (dmm/t Q-step)))
                        (dmm/assoc-minor R
                                         [idx idx]
                                         Q-step-1A))))))

;; Will have to figure out how to fix the extra lets later
;; Also probably want a macro
;; (letm [internal binding, exp that will be auto-let'd] body)

(defn qr-decomp [A]
  (let [height (dmm/height A)
        I (dmm/identity-m height)]
    (qr-decomp-step A I height I I)))

(defn qr-decomp-step'
  [A I I-size Q R]
  (let [x (first (dmm/col-seq A))
        x-size (count x)
        idx (- I-size x-size)]
    (if (= 1 x-size)
      {:Q Q
       :R (dmm/assoc-minor R
                           [idx idx]
                           x)}
      (let [;; computes reflection matrix given column vector x and e is a
            ;; column vector that is part of a basis of orthonormal vectors,
            ;; usually [1,0...0]^T
            e (dmm/t (dmm/v [(vec (concat [1]
                                          (repeat (dec x-size)
                                                  0)))]))
            ;; the sign of alpha should be opposite of kth coordinate of x, since
            ;; we want u to be as small as possible
            ;; you can use Math/copySign or in cuda copysignf
            alpha (dm/copy-sign (dmm/vnorm x)
                                (first (seq x)))
            I-step (dmm/minor I
                              [idx idx])]
        (dm/letm [;; if u is the zero vector, Q should just be the identity (BUG)
                  u (dm/- x (dm/* alpha e))]
          (dm/letm [v (dm/d u (dmm/vnorm u))]
            (let [Q-step (dm/- I-step (dm/* 2 (dm/* v (dmm/t v))))
                  ;; ideas for fixing
                  ;; 1. do optimized substitutions
                  ;; 2. matrix operated with a scalar is a place for reuse
                  ;; 3. create new forms that do special things when emitted
                  ;; 4. maybe a let form?
                  ;; 5. precomputing values?
                  ;; 6. at each qr-decomp-step evaluate expression?

                  ;; this is to improve accuracy
                  Q-step-1A (dmm/assoc-minor (dm/* Q-step
                                                   A)
                                             [0 0]
                                             (dm/* e
                                                   alpha))
                  A' (dmm/minor Q-step-1A
                                [1 1])]
              (dm/letm [A A'
                        Q (dm/* Q (dmm/assoc-minor I
                                                 [idx idx]
                                                 (dmm/t Q-step)))
                        R (dmm/assoc-minor R
                                           [idx idx]
                                           Q-step-1A)]
                       (qr-decomp-step' A
                                        I
                                        I-size
                                        Q
                                        R)))))))))

(defn qr-decomp' [A]
  (let [height (dmm/height A)
        I (dmm/identity-m height)]
    (qr-decomp-step' A I height I I)))

(defn back-substitution
  "R must be in reduced row echelon form"
  [R b]
  (let [rows (vec (map (comp vec seq)
                       (dmm/row-seq R)))
        b (vec (seq b))]
    (dmm/v [(vec (rseq (reduce (fn [v i]
                                 (let [bi (b i)
                                       ri (rows i)]
                                   (conj v
                                         (dm/d (dm/- bi
                                                     (reduce dm/+
                                                             0
                                                             (map dm/*
                                                                  (reverse
                                                                   (subvec ri
                                                                           (inc i)))
                                                                  v)))
                                               (ri i)))))
                               []
                               (reverse (range (count rows))))))])))

(defn back-substitution'
  "R must be in reduced row echelon form"
  [R b]
  (let [rows (vec (map (comp vec seq)
                       (dmm/row-seq R)))
        b (vec (seq b))
        syms (mapv (fn [n]
                     (dmp/s {:variable (dm/gensym (str "v" n))}))
                   (range (count rows)))
        es (reduce (fn [bs i]
                     (let [bi (b i)
                           ri (rows i)]
                       (conj bs
                             (dm/d (dm/- bi
                                         (reduce dm/+
                                                 0
                                                 (map dm/*
                                                      (reverse
                                                       (subvec ri
                                                               (inc i)))
                                                      (rseq syms))))
                                   (ri i)))))
                   []
                   (reverse (range (count rows))))]
    ;; Weird bug here, can't have vec inside children???
    ;; probably has to do with letm
    (dmp/s {:op "let"
            :bindings (dmb/zip->bindings (rseq syms)
                                         es)
            :children [(vec syms)]})))

(defn solve [A b]
  (let [{:keys [Q R]} (qr-decomp A)]
    (back-substitution R (dm/* (dmm/t Q)
                               b))))

(defn solve' [A b]
  (dm/letm [results (qr-decomp' A)]
           (let [{:keys [Q R]} results]
             (back-substitution' R (dm/* (dmm/t Q)
                                         b)))))

(defn test-run []
  (let [[a b c d e f g h i] (map (fn [n]
                                   (dmp/s {:variable (str "A" n)}))
                                 (range 9))
        [x y z] (map (fn [n]
                       (dmp/s {:variable (str "b" n)}))
                     (range 3))]
    (-> #_ (qr-decomp' (dmm/v [[a b c]
                            [d e f]
                            [g h i]]))
        #_ (qr-decomp' (dmm/v [[a b]
                               [c d]]))
        (solve' (dmm/v [[a b c]
                        [d e f]
                        [g h i]])
                (dmm/t (dmm/v [[x y z]])))
        #_ dj.math.cemit/emit
        dj.math.parser/emit
        user/re)))

;; solution [5 3 -2]
#_ (let [[A0 A1 A2 A3 A4 A5 A6 A7 A8 A9] [1.0 1.0 1.0 0.0 2.0 5.0 2.0 5.0 -1.0]
         [b0 b1 b2] [6.0 -4.0 27.0]]
     (let [m0_00522 (- A0 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0)) m1_00523 A3 m2_00524 A6 m0_00525 (/ m0_00522 (Math/sqrt (+ (+ (Math/pow m0_00522 2) (Math/pow m1_00523 2)) (Math/pow m2_00524 2)))) m1_00526 (/ m1_00523 (Math/sqrt (+ (+ (Math/pow m0_00522 2) (Math/pow m1_00523 2)) (Math/pow m2_00524 2)))) m2_00527 (/ m2_00524 (Math/sqrt (+ (+ (Math/pow m0_00522 2) (Math/pow m1_00523 2)) (Math/pow m2_00524 2)))) m0_00528 (+ (+ (* (- (* (* m1_00526 m0_00525) 2)) A1) (* (- 1 (* (* m1_00526 m1_00526) 2)) A4)) (* (- (* (* m1_00526 m2_00527) 2)) A7)) m1_00529 (+ (+ (* (- (* (* m1_00526 m0_00525) 2)) A2) (* (- 1 (* (* m1_00526 m1_00526) 2)) A5)) (* (- (* (* m1_00526 m2_00527) 2)) A8)) m2_0052A (+ (+ (* (- (* (* m2_00527 m0_00525) 2)) A1) (* (- (* (* m2_00527 m1_00526) 2)) A4)) (* (- 1 (* (* m2_00527 m2_00527) 2)) A7)) m3_0052B (+ (+ (* (- (* (* m2_00527 m0_00525) 2)) A2) (* (- (* (* m2_00527 m1_00526) 2)) A5)) (* (- 1 (* (* m2_00527 m2_00527) 2)) A8)) m0_0052C (- 1 (* (* m0_00525 m0_00525) 2)) m1_0052D (- (* (* m1_00526 m0_00525) 2)) m2_0052E (- (* (* m2_00527 m0_00525) 2)) m3_0052F (- (* (* m0_00525 m1_00526) 2)) m4_00530 (- 1 (* (* m1_00526 m1_00526) 2)) m5_00531 (- (* (* m2_00527 m1_00526) 2)) m6_00532 (- (* (* m0_00525 m2_00527) 2)) m7_00533 (- (* (* m1_00526 m2_00527) 2)) m8_00534 (- 1 (* (* m2_00527 m2_00527) 2)) m0_00535 (Math/copySign (Math/sqrt (+ (+ (Math/pow A0 2) (Math/pow A3 2)) (Math/pow A6 2))) A0) m1_00536 (+ (+ (* (- 1 (* (* m0_00525 m0_00525) 2)) A1) (* (- (* (* m0_00525 m1_00526) 2)) A4)) (* (- (* (* m0_00525 m2_00527) 2)) A7)) m2_00537 (+ (+ (* (- 1 (* (* m0_00525 m0_00525) 2)) A2) (* (- (* (* m0_00525 m1_00526) 2)) A5)) (* (- (* (* m0_00525 m2_00527) 2)) A8)) m4_00539 (+ (+ (* (- (* (* m1_00526 m0_00525) 2)) A1) (* (- 1 (* (* m1_00526 m1_00526) 2)) A4)) (* (- (* (* m1_00526 m2_00527) 2)) A7)) m5_0053A (+ (+ (* (- (* (* m1_00526 m0_00525) 2)) A2) (* (- 1 (* (* m1_00526 m1_00526) 2)) A5)) (* (- (* (* m1_00526 m2_00527) 2)) A8)) m7_0053C (+ (+ (* (- (* (* m2_00527 m0_00525) 2)) A1) (* (- (* (* m2_00527 m1_00526) 2)) A4)) (* (- 1 (* (* m2_00527 m2_00527) 2)) A7)) m8_0053D (+ (+ (* (- (* (* m2_00527 m0_00525) 2)) A2) (* (- (* (* m2_00527 m1_00526) 2)) A5)) (* (- 1 (* (* m2_00527 m2_00527) 2)) A8)) m0_0053E (- m0_00528 (Math/copySign (Math/sqrt (+ (Math/pow m0_00528 2) (Math/pow m2_0052A 2))) m0_00528)) m1_0053F m2_0052A m0_00540 (/ m0_0053E (Math/sqrt (+ (Math/pow m0_0053E 2) (Math/pow m1_0053F 2)))) m1_00541 (/ m1_0053F (Math/sqrt (+ (Math/pow m0_0053E 2) (Math/pow m1_0053F 2)))) m0_00542 (+ (* (- (* (* m1_00541 m0_00540) 2)) m1_00529) (* (- 1 (* (* m1_00541 m1_00541) 2)) m3_0052B)) m0_00543 m0_0052C m1_00544 (+ (* m1_0052D (- 1 (* (* m0_00540 m0_00540) 2))) (* m2_0052E (- (* (* m0_00540 m1_00541) 2)))) m2_00545 (+ (* m1_0052D (- (* (* m1_00541 m0_00540) 2))) (* m2_0052E (- 1 (* (* m1_00541 m1_00541) 2)))) m3_00546 m3_0052F m4_00547 (+ (* m4_00530 (- 1 (* (* m0_00540 m0_00540) 2))) (* m5_00531 (- (* (* m0_00540 m1_00541) 2)))) m5_00548 (+ (* m4_00530 (- (* (* m1_00541 m0_00540) 2))) (* m5_00531 (- 1 (* (* m1_00541 m1_00541) 2)))) m6_00549 m6_00532 m7_0054A (+ (* m7_00533 (- 1 (* (* m0_00540 m0_00540) 2))) (* m8_00534 (- (* (* m0_00540 m1_00541) 2)))) m8_0054B (+ (* m7_00533 (- (* (* m1_00541 m0_00540) 2))) (* m8_00534 (- 1 (* (* m1_00541 m1_00541) 2)))) m0_0054C m0_00535 m1_0054D m1_00536 m2_0054E m2_00537 m4_00550 (Math/copySign (Math/sqrt (+ (Math/pow m0_00528 2) (Math/pow m2_0052A 2))) m0_00528) m5_00551 (+ (* (- 1 (* (* m0_00540 m0_00540) 2)) m1_00529) (* (- (* (* m0_00540 m1_00541) 2)) m3_0052B)) m8_00554 (+ (* (- (* (* m1_00541 m0_00540) 2)) m1_00529) (* (- 1 (* (* m1_00541 m1_00541) 2)) m3_0052B)) v2_00558 (/ (+ (+ (* m2_00545 b0) (* m5_00548 b1)) (* m8_0054B b2)) m0_00542) v1_00557 (/ (- (+ (+ (* m1_00544 b0) (* m4_00547 b1)) (* m7_0054A b2)) (* m5_00551 v2_00558)) m4_00550) v0_00556 (/ (- (+ (+ (* m0_00543 b0) (* m3_00546 b1)) (* m6_00549 b2)) (+ (* m2_0054E v2_00558) (* m1_0054D v1_00557))) m0_0054C)] [v0_00556 v1_00557 v2_00558]))
