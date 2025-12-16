(ns thermos.opt.net.pipes-test
  (:require [clojure.test :refer [deftest is testing]]
            [thermos.opt.net.pipes :as sut]
            [thermos.opt.test :refer [≈]]))

(deftest dia->kw-range-test
  (testing "looking up diameter from kw rounds up to next least dia"
    (let [curve {:dia->kw [[0.01 50] [0.02 100] [0.03 200]]}]
      (is (= [101 200] (sut/dia->kw-range curve 30)))
      (is (= [101 200] (sut/dia->kw-range curve 29)))
      (is (= [51 100] (sut/dia->kw-range curve 20)))
      (is (= [0 50] (sut/dia->kw-range curve 5))))))

(deftest velocity-m-per-s-test
  (testing "correctly estimates water velocity for a pipe"
    (is (≈ 0.6 (sut/velocity-m-per-s 0.020) 6.0))
    (is (≈ 0.8 (sut/velocity-m-per-s 0.032) 6.5))
    (is (≈ 1.1 (sut/velocity-m-per-s 0.050) 2.0))
    (is (≈ 1.4 (sut/velocity-m-per-s 0.080) 1.0))
    (is (≈ 1.7 (sut/velocity-m-per-s 0.125) 2.0))
    (is (≈ 2.1 (sut/velocity-m-per-s 0.200) 2.0))
    (is (≈ 2.6 (sut/velocity-m-per-s 0.300) 2.0))
    (is (≈ 2.9 (sut/velocity-m-per-s 0.400) 1.0))
    (is (= 3.0 (sut/velocity-m-per-s 0.500)))
    (is (= 3.0 (sut/velocity-m-per-s 0.700)))
    (is (= 3.0 (sut/velocity-m-per-s 0.900)))
    (is (= 3.0 (sut/velocity-m-per-s 1.100)))))

(deftest ^:optimiser linear-approximate-test
  (let [linear-approximate #'sut/linear-approximate]
    (testing "finds intercept on line"
      (let [[c m] (linear-approximate [[0 5] [10 15]] 0 10)]
        (is (< 4.999 c 5.001))
        (is (< 0.999 m 1.001)))

      (let [[c m] (linear-approximate [[0 5] [10 15]] 5 10)]
        (is (< 4.999 c 5.001))
        (is (< 0.999 m 1.001)))

      (let [[c m] (linear-approximate [[0 5] [10 15]] 0 10)]
        (is (< 4.999 c 5.001))
        (is (< 0.999 m 1.001))))

    (testing "approximates several segments"
      (let [[c m] (linear-approximate [[0 0] [1 1] [2 1] [3 2]] 0 1)]
        (is (< -0.001 c 0.001))
        (is (< 0.999 m 1.001)))
      (let [[c m] (linear-approximate [[0 0] [1 1] [2 1] [3 2]] 1 2)]
        (is (< 0.999 c 1.001))
        (is (< -0.001 m 0.001)))
      (let [[c m] (linear-approximate [[0 0] [1 1] [2 1] [3 2]] 2 3)]
        (is (< -1.001 c -0.999))
        (is (< 0.999 m 1.001)))
      (let [[c m] (linear-approximate [[0 0] [1 1] [2 1] [3 2]] 0 3)]
        (is (< 0.2 c 0.3))
        (is (< 0.5 m 0.6))))))
