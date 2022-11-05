import type { GetServerSideProps, NextPage } from "next";
import { useState } from "react";
import { trpc } from "../../../utils/trpc";
import { useRouter } from "next/router";
import Layout from "../../../components/layout";
import CodeEditor from "../../../components/codeEditor";
import dayjs from "dayjs";
import { getServerAuthSession } from "../../../server/common/get-server-auth-session";
import { PrismaClient } from "@prisma/client";

enum TabState {
    Instructions,
    Result,
}

const SolveProblem: NextPage<{template: string}> = ({ template }) => {
    const [tab, setTab] = useState<TabState>(TabState.Instructions);
    const [code, setCode] = useState<string>(template);
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
            <main className="mx-auto h-full w-full overflow-hidden">
                <h2 className="text-3xl w-full text-center p-8">
                    {problem.data.name}
                </h2>
                <div className="flex flex-row gap-8 justify-center my-auto h-full w-full px-10">
                    <div className="border-2 w-1/2 h-3/4 flex flex-col rounded-lg">
                        <CodeEditor
                            value={code}
                            height="100%"
                            className="h-full px-2 py-1 font-mono resize-none rounded-lg outline-0"
                            setCode={setCode}
                        />
                        <div className="w-full p-4 gap-2 items-center justify-end flex flex-row border-t">
                            <AttemptSelect
                                problemId={problem.data.id}
                                setCode={setCode}
                                defaultCode={template}
                            />
                            <button
                                className="rounded-lg bg-green-500 hover:bg-green-400 px-4 py-2 text-white font-semibold"
                                onClick={() =>
                                    mutation.mutate({
                                        code,
                                        test: test.data.code,
                                        problemId: problem.data.id,
                                    })
                                }
                            >
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
            <span className="w-full text-center text-3xl p-2">
                {success ? "Success!" : "Code failed to run"}
            </span>

            <div className="my-8" />

            <span className="text-xl">Output</span>
            <pre
                className={`bg-gray-100 p-2 rounded-lg text-white overflow-auto ${
                    success ? `bg-green-600 ` : `bg-red-700`
                }`}
            >
                {formattedResult}
            </pre>
        </div>
    );
}

function AttemptSelect({
    setCode,
    problemId,
    defaultCode,
}: {
    problemId: string;
    setCode: (code: string) => void;
    defaultCode: string;
}) {
    const [selected, setSelected] = useState<string>("");

    const submissions = trpc.useQuery(
        ["submission.byProblemId", { problemId }],
        {
            enabled: true,
        }
    );

    if (submissions.isLoading) {
        return null;
    }

    if (!submissions.isSuccess || submissions.data === undefined) {
        return null;
    }

    return (
        <select
            className="w-full p-2 rounded-lg"
            value={selected}
            onChange={(e) => {
                setSelected(e.target.value);
                setCode(e.target.value);
            }}
        >
            <option value={defaultCode}>Select a submission</option>
            {submissions.data.map((submission, i) => (
                <option key={submission.id} value={submission.code}>
                    #{submissions.data.length - i}{" "}
                    {dayjs(submission.createdAt).format("MMM D, YYYY HH:mm")} -{" "}
                    {submission.success ? "✅" : "❌"}
                </option>
            ))}
        </select>
    );
}

export default SolveProblem;

export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getServerAuthSession(ctx);

    if (!session) {
        return {
            redirect: {
                destination: "/login",
                permanent: false,
            },
        };
    }

    const prisma = new PrismaClient();
    const { problemId } = ctx.query;

    const problem = await prisma.problem.findUnique({
        where: {
            id: problemId as string,
        },
        select: {
            template: true,
        }
    });

    return {
        props: {
            template: problem?.template
        },
    };
};
