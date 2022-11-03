import type { NextPage } from "next";
import { useRef, useState } from "react";
import { trpc } from "../../../utils/trpc";
import { useRouter } from "next/router";
import Layout from "../../../components/layout";

enum TabState {
    Instructions,
    Result,
}

const SolveProblem: NextPage = () => {
    const [tab, setTab] = useState<TabState>(TabState.Instructions);
    const codebox = useRef<HTMLTextAreaElement>(null);
    const router = useRouter();
    const { problemId } = router.query;

    const problem = trpc.useQuery(["problem.byId", problemId as string], {
        enabled: router.isReady,
    });
    const test = trpc.useQuery(["test.byId", problemId as string], {
        enabled: router.isReady,
    });

    const mutation = trpc.useMutation("code.haskell", {
        onSuccess: () => setTab(TabState.Result),
    });

    // Ensure user isn't on results tab in an invalid state (no results).
    if (mutation.isError && tab === TabState.Result) {
        setTab(TabState.Instructions);
    }

    if (problem.isLoading || test.isLoading) {
        return (
            <div className="flex flex-col justify-center h-screen">
                <div className="mx-auto my-auto">Loading...</div>
            </div>
        );
    }

    if (!problem.isSuccess || !problem.data) {
        return <div>Could not find problem ☹</div>;
    }

    if (!test.isSuccess || !test.data) {
        return <div>Could not find test ☹</div>;
    }

    return (
        <Layout title={problem.data.name}>
            <main className="mx-auto h-full w-full">
                <div className="flex flex-row gap-8 items-center justify-center h-full w-full px-10">
                    <div className="border-2 w-full h-3/4 flex flex-col rounded-lg">
                        <textarea
                            ref={codebox}
                            className="h-5/6 px-2 py-1 font-mono resize-none rounded-lg outline-0"
                            defaultValue={problem.data.template}
                        />
                        <div className="h-1/6 w-full p-4 gap-2 items-center justify-end flex flex-row border-t">
                            <button className="rounded-lg bg-sky-500 hover:bg-sky-400 px-4 py-2 text-white font-semibold">
                                Attempt
                            </button>
                            <button
                                className="rounded-lg bg-green-500 hover:bg-green-400 px-4 py-2 text-white font-semibold"
                                onClick={() =>
                                    mutation.mutate({
                                        code: codebox.current?.value ?? "test",
                                        test: test.data.code,
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
                            {tab === TabState.Instructions && (
                                <Instructions text={problem.data.description} />
                            )}
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
        </Layout>
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

function Instructions({ text }: { text: string }) {
    return (
        <>
            <p>{text}</p>
        </>
    );
}

function Results({ result, success }: { result?: string; success?: boolean }) {
    if (!result) return <></>;
    const formattedResult = result
        .trim()
        .replaceAll("[v]", "✅")
        .replaceAll("[x]", "❌");

    return (
        <div className="flex flex-col w-full">
            <span className="w-full text-center text-3xl">
                {success ? "Success!" : "Code failed to run"}
            </span>

            <div className="my-8" />

            <span className="text-xl">Output</span>
            <pre
                className={`bg-gray-100 p-2 rounded-lg text-white ${
                    success ? `bg-green-600 ` : `bg-red-700`
                }`}
            >
                {formattedResult}
            </pre>
        </div>
    );
}

export default SolveProblem;
