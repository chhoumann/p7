import type { NextPage } from "next";
import Head from "next/head";
import { useState } from "react";

enum TabState {
  Instructions,
  Result,
}

const Home: NextPage = () => {
  const [tab, setTab] = useState<TabState>(TabState.Instructions);

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

        <main className="container mx-auto h-full">
          <div className="flex flex-row gap-4 items-center justify-center h-full">
            <div className="border-2 w-1/2 h-3/4 flex flex-col rounded-lg">
              <textarea className="h-5/6 px-2 py-1 font-mono resize-none rounded-lg outline-0" />
              <div className="h-1/6 w-full p-4 gap-2 items-center justify-end flex flex-row border-t">
                <button className="rounded-lg bg-sky-500 hover:bg-sky-400 px-4 py-2 text-white font-semibold">
                  Attempt
                </button>
                <button className="rounded-lg bg-green-500 hover:bg-green-400 px-4 py-2 text-white font-semibold">
                  Submit
                </button>
              </div>
            </div>
            <div className="border-2 w-1/2 h-3/4 flex flex-col rounded-lg">
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
                />
              </div>
              <div className="px-2 py-1 overflow-y-auto overflow-x-clip">
                {tab === TabState.Instructions && <Instructions />}
                {tab === TabState.Result && <Results />}
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
  onClick,
}: {
  text: string;
  selected: boolean;
  onClick: () => void;
}) {
  return (
    <span
      className={`w-1/2 text-center p-4 font-semibold ${
        selected ? "text-[#005CC5]" : "text-gray-500 bg-gray-200"
      } cursor-pointer`}
      onClick={onClick}
    >
      Instructions
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
      <br />
      <br />
      <br />
      <br />
      <br />
      <br />
      <br />
      <br />
      <br />
      hej
    </>
  );
}

function Results() {
  return <>0 tests passed.</>;
}

export default Home;
