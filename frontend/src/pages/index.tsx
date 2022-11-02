import type { NextPage } from "next";
import Head from "next/head";
import { useRef, useState } from "react";
import { trpc } from "../utils/trpc";

enum TabState {
  Instructions,
  Result,
}

const Home: NextPage = () => {
  const [tab, setTab] = useState<TabState>(TabState.Instructions);
  const codebox = useRef<HTMLTextAreaElement>(null);

  const mutation = trpc.useMutation("code.haskell", {
    onSuccess: () => setTab(TabState.Result),
  });

  // Ensure user isn't on results tab in an invalid state (no results).
  if (mutation.isError && tab === TabState.Result) {
    setTab(TabState.Instructions);
  }

  return (
    <>
      <Head>
        <title>P7</title>
        <meta name="description" content="Generated by create-t3-app" />
        <link rel="icon" type="image/x-icon" href="favicon.ico" />
      </Head>

      <div className="h-screen flex flex-col">
        <nav>
          <div className="border-b-2 p-4">
            <span className="font-bold">Home</span>
          </div>
        </nav>

        <main className="mx-auto h-full w-full">
          <div className="flex flex-row gap-8 items-center justify-center h-full w-full px-10">
            <div className="border-2 w-full h-3/4 flex flex-col rounded-lg">
              <textarea
                ref={codebox}
                className="h-5/6 px-2 py-1 font-mono resize-none rounded-lg outline-0"
                data-cy="code-text-input-file"
              />
              <div className="h-1/6 w-full p-4 gap-2 items-center justify-end flex flex-row border-t">
                <button className="rounded-lg bg-sky-500 hover:bg-sky-400 px-4 py-2 text-white font-semibold">
                  Attempt
                </button>
                <button
                  className="rounded-lg bg-green-500 hover:bg-green-400 px-4 py-2 text-white font-semibold"
                  data-cy="submit-code"
                  onClick={() =>
                    mutation.mutate({
                      code: codebox.current?.value ?? "test",
                    })
                  }
                >
                  Submit
                </button>
              </div>
            </div>
            <div className="border-2 w-full h-3/4 flex flex-col rounded-lg">
              <div className="w-full items-center justify-around flex">
                <Tab
                  text="Instructions"
                  selected={tab === TabState.Instructions}
                  onClick={() => setTab(TabState.Instructions)}
                />
                <Tab
                  text="Results"
                  selected={tab === TabState.Result}
                  onClick={() => setTab(TabState.Result)}
                  disabled={!mutation.isSuccess}
                />
              </div>
              <div className="px-2 py-1 overflow-y-auto overflow-x-clip">
                {tab === TabState.Instructions && <Instructions />}
                {tab === TabState.Result && mutation.isSuccess && (
                  <Results
                    result={mutation.data?.result}
                    success={mutation.data?.success}
                  />
                )}
              </div>
            </div>
          </div>
        </main>
      </div>
    </>
  );
};

function Tab({
  text,
  selected,
  disabled,
  onClick,
}: {
  text: string;
  selected: boolean;
  disabled?: boolean;
  onClick: () => void;
}) {
  return (
    <span
      className={`w-1/2 text-center p-4 font-semibold ${
        selected ? "text-[#005CC5]" : "text-gray-500 bg-gray-200"
      } ${disabled ? "" : "cursor-pointer"}`}
      onClick={disabled ? () => false : onClick}
    >
      {text}
    </span>
  );
}

function Instructions() {
  return (
    <>
      <strong>SHORT INTRODUCTION</strong>
      <p>
        Understanding general language structure and syntax is important.
        Therefore, we begin with a short example of invalid code.
        <br />
        <br />
        PROBLEM FORMULATION HERE
        <br />
        Find the issue in the code on the left.
      </p>
      <br />
      <strong>ADDITIONAL TIPS</strong>
      <br />
      This is not Rust.
      <br />
    </>
  );
}

function Results({ result, success }: { result?: string; success?: boolean }) {
  if (!result || !success) return <></>;

  return (
    <div className="flex flex-col w-full">
      <span className="w-full text-center text-3xl">
        {success ? "Success!" : "Code failed to run"}
      </span>

      <div className="my-8" />

      <span className="text-xl">Output</span>
      <code className="bg-gray-100 p-2 rounded-lg">{result}</code>
    </div>
  );
}

export default Home;
