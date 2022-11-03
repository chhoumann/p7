import React, { useState } from "react";
import { trpc } from "../utils/trpc";

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

function TestPage() {
    const [count, setCount] = useState(10);
    const [countError, setCountError] = useState(false);

    const req = trpc.useQuery(
        [
            "code.spam",
            {
                count,
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

    return (
        <div>
            {req.isSuccess && req.data && (
                <div>
                    Sent {req.data.requestCount} requests to the server.
                    <br />
                    Received {req.data.fulfilled} responses. This took{" "}
                    {req.data.duration}
                    ms.
                </div>
            )}

            {req.isLoading && <div>Loading...</div>}

            {countError && <div>Count is invalid.</div>}

            <div className="flex gap-4">
                <button
                    className="rounded px-4 py-2 bg-gray-300"
                    onClick={() => req.refetch()}
                >
                    Run
                </button>

                <input
                    onInput={(input) => {
                        const count = parseInt(input.currentTarget.value);

                        if (!count) {
                            setCountError(true);
                            return;
                        }

                        setCountError(false);
                        setCount(count);
                    }}
                    className="border-2"
                    type="number"
                    name="count"
                    placeholder="10"
                    defaultValue={10}
                />
            </div>
        </div>
    );
}

export default TestPage;
