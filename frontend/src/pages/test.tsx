import React from "react";
import { trpc } from "../utils/trpc";

function TestPage() {

    const code = `module SolutionSession6 where
data Unary = I Unary | Z

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I n) = 1 + unary2int n`;

    const test = `module Session6Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import SolutionSession6

main :: IO()
main = hspec $ do
    describe "Session6.unary2int" $ do
        it "from Unary IIIIZ get 4" $ do
            unary2int (I(I(I(I Z)))) \`shouldBe\` 4`;

  const req = trpc.useQuery([
    "code.spam",
    {
      count: 10,
      code,
      test,
    },
  ]);

  if (!req.isSuccess) return <div>loading...</div>;

  console.table(req.data.responses);

  return (
    <div>
      Sent {req.data.requestCount} requests to the server.
      <br />
      Received {req.data.fulfilled} responses.

      This took {req.data.duration}ms.
    </div>
  );
}

export default TestPage;
