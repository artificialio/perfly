module Perf.Types.ExternalSpec where

import Test.Hspec
import Perf.Types.External
import Data.Aeson (decode)
import Data.Aeson (Value(..))

spec :: Spec
spec = do
  describe "Decoding" do
    it "Metric" $
      shouldBe (decode "{ \"metric\": \"foo\", \"rangeLower\": 1, \"rangeUpper\": 2, \"mean\": 1.5, \"stddev\": 0.1 }")
       $ Just $ Metric {
           metric = "foo",
           rangeLower = 1.0,
           rangeUpper = 2.0,
           mean = 1.5,
           stddev = 0.1
         }
    it "Factor" do
      shouldBe (decode "{ \"factor\": \"foo\", \"value\": \"123\" }")
       $ Just $ Factor {factor = "foo", value = "123"}
      shouldBe (decode "{ \"factor\": \"foo\", \"value\": \"abc\" }")
       $ Just $ Factor {factor = "foo", value = "abc"}
    it "Test" $
      shouldBe (decode "{ \"factors\": [{ \"factor\": \"foo\", \"value\": \"123\" }], \"metrics\": [{ \"metric\": \"foo\", \"rangeLower\": 1, \"rangeUpper\": 2, \"mean\": 1.5, \"stddev\": 0.1 }] }")
       $ Just $ Test {
          factors = [
           Factor {
             factor = "foo",
             value = "123"
           }
          ],
          metrics = [
           Metric {
             metric = "foo",
             rangeLower = 1.0,
             rangeUpper = 2.0,
             mean = 1.5,
             stddev = 0.1
           }
          ]
        }
    it "Benchmark" $
      shouldBe (decode "{ \"subject\": \"foo\", \"tests\": [ { \"factors\": [{ \"factor\": \"foo\", \"value\": \"123\" }], \"metrics\": [{ \"metric\": \"foo\", \"rangeLower\": 1, \"rangeUpper\": 2, \"mean\": 1.5, \"stddev\": 0.1 }] }] }")
       $ Just $ Benchmark {
         subject = "foo",
         tests = [
          Test {
            factors = [
             Factor {
               factor = "foo",
               value = "123"
             }
            ],
            metrics = [
             Metric {
               metric = "foo",
               rangeLower = 1.0,
               rangeUpper = 2.0,
               mean = 1.5,
               stddev = 0.1
             }
            ]
          }
         ]
       }

  describe "Golden test from 997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc" do
    it "Comit" $
      shouldBe (decode "{\"branch\":\"master\",\"commit\":\"997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc\",\"result\":{\"benchmarks\":[{\"subject\":\"policy addition benchmark\",\"tests\":[{\"factors\":[{\"factor\":\"policies\",\"value\":\"2\"}],\"metrics\":[{\"mean\":0.1,\"metric\":\"time\",\"rangeLower\":0.1,\"rangeUpper\":1.1,\"stddev\":0.1},{\"mean\":610.1,\"metric\":\"allocated MiB\",\"rangeLower\":260.1,\"rangeUpper\":1660.1,\"stddev\":699.1},{\"mean\":433.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":433.1,\"rangeUpper\":433.1,\"stddev\":0.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"20\"}],\"metrics\":[{\"mean\":4.1,\"metric\":\"time\",\"rangeLower\":4.1,\"rangeUpper\":4.1,\"stddev\":0.1},{\"mean\":2605.1,\"metric\":\"allocated MiB\",\"rangeLower\":2604.1,\"rangeUpper\":2606.1,\"stddev\":0.1},{\"mean\":471.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":444.1,\"rangeUpper\":489.1,\"stddev\":19.1}]},{\"factors\":[{\"factor\":\"policies\",\"value\":\"200\"}],\"metrics\":[{\"mean\":41.1,\"metric\":\"time\",\"rangeLower\":41.1,\"rangeUpper\":41.1,\"stddev\":0.1},{\"mean\":26074.1,\"metric\":\"allocated MiB\",\"rangeLower\":26074.1,\"rangeUpper\":26074.1,\"stddev\":0.1},{\"mean\":499.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":493.1,\"rangeUpper\":505.1,\"stddev\":5.1}]}]},{\"subject\":\"Marine Hull: transactPolicy\",\"tests\":[{\"factors\":[{\"factor\":\"iterations\",\"value\":\"20\"},{\"factor\":\"datapoints\",\"value\":\"1\"}],\"metrics\":[{\"mean\":2.1,\"metric\":\"time\",\"rangeLower\":2.1,\"rangeUpper\":2.1,\"stddev\":0.1},{\"mean\":2037.1,\"metric\":\"allocated MiB\",\"rangeLower\":2036.1,\"rangeUpper\":2038.1,\"stddev\":0.1},{\"mean\":1674.1,\"metric\":\"peak allocated MiB\",\"rangeLower\":688.1,\"rangeUpper\":2700.1,\"stddev\":633.1}]}]}]}}")
       $ Just $ Commit {
         branch = "master",
         commit = "997d6b8c3cf3c5ea14c40ec8c0f55f9c574a51cc",
         result = Result {
           benchmarks = [
            Benchmark {
              subject = "policy addition benchmark",
              tests = [
               Test {
                 factors = [
                  Factor {
                    factor = "policies",
                    value = "2"
                  }
                 ],
                 metrics = [
                  Metric {
                    metric = "time",
                    rangeLower = 0.1,
                    rangeUpper = 1.1,
                    mean = 0.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "allocated MiB",
                    rangeLower = 260.1,
                    rangeUpper = 1660.1,
                    mean = 610.1,
                    stddev = 699.1
                  },
                  Metric {
                    metric = "peak allocated MiB",
                    rangeLower = 433.1,
                    rangeUpper = 433.1,
                    mean = 433.1,
                    stddev = 0.1
                  }
                 ]
               },
               Test {
                 factors = [
                  Factor {
                    factor = "policies",
                    value = "20"
                  }
                 ],
                 metrics = [
                  Metric {
                    metric = "time",
                    rangeLower = 4.1,
                    rangeUpper = 4.1,
                    mean = 4.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "allocated MiB",
                    rangeLower = 2604.1,
                    rangeUpper = 2606.1,
                    mean = 2605.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "peak allocated MiB",
                    rangeLower = 444.1,
                    rangeUpper = 489.1,
                    mean = 471.1,
                    stddev = 19.1
                  }
                 ]
               },
               Test {
                 factors = [
                  Factor {
                    factor = "policies",
                    value = "200"
                  }
                 ],
                 metrics = [
                  Metric {
                    metric = "time",
                    rangeLower = 41.1,
                    rangeUpper = 41.1,
                    mean = 41.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "allocated MiB",
                    rangeLower = 26074.1,
                    rangeUpper = 26074.1,
                    mean = 26074.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "peak allocated MiB",
                    rangeLower = 493.1,
                    rangeUpper = 505.1,
                    mean = 499.1,
                    stddev = 5.1
                  }
                 ]
               }
              ]
            },
            Benchmark {
              subject = "Marine Hull: transactPolicy",
              tests = [
               Test {
                 factors = [
                  Factor {
                    factor = "iterations",
                    value = "20"
                  },
                  Factor {
                    factor = "datapoints",
                    value = "1"
                  }
                 ],
                 metrics = [
                  Metric {
                    metric = "time",
                    rangeLower = 2.1,
                    rangeUpper = 2.1,
                    mean = 2.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "allocated MiB",
                    rangeLower = 2036.1,
                    rangeUpper = 2038.1,
                    mean = 2037.1,
                    stddev = 0.1
                  },
                  Metric {
                    metric = "peak allocated MiB",
                    rangeLower = 688.1,
                    rangeUpper = 2700.1,
                    mean = 1674.1,
                    stddev = 633.1
                  }
                 ]
               }
              ]
            }
           ]
         }
       }
