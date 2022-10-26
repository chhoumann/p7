import React from "react";
import { trpc } from "../utils/trpc";

function TestPage() {
  const code = `
module Code where
add x y = x + y
`;

  const test = `
import Test.Hspec
import Test.QuickCheck
import Code (add)
import Control.Exception (evaluate)
main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "should evaluate 2 + 2 = 4" $ do
      add 2 2 \`shouldBe\` (4 :: Int)
`;

  const req = trpc.useQuery(
    [
      "code.spam",
      {
        count: 10,
        code,
        test,
      },
    ],
    {
      enabled: false,
      refetchOnWindowFocus: false,
      refetchOnMount: false,
      refetchOnReconnect: false,
    }
  );

  if (req?.data?.responses) console.table(req.data.responses);

  return (
    <div>
      {!req.isSuccess && req.data && (
        <div>
          Sent {req.data.requestCount} requests to the server.
          <br />
          Received {req.data.fulfilled} responses. This took {req.data.duration}
          ms.
        </div>
      )}

      <button
        className="rounded px-4 py-2 bg-gray-300"
        onClick={() => req.refetch()}
      >
        Run
      </button>
    </div>
  );
}

export default TestPage;
