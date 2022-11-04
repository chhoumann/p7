import { NextPage } from "next";
import { trpc } from "../../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";
import { useRouter } from "next/router";

const ProblemSetPage: NextPage = () => {
    const router = useRouter();
    const { problemSetId } = router.query as { problemSetId: string };
    const [selectedId, setSelectedId] = useState<string>();

    const problemSet = trpc.useQuery(
        ["problemSets.getByProblemSetId", problemSetId],
        {}
    );

    const problems = trpc.useQuery(
        ["problem.getByProblemSetId", problemSetId],
        {
            enabled: router.isReady,
        }
    );

    const deleteProblemMutation = trpc.useMutation(["problem.delete"]);

    async function deleteProblemHandler() {
        if (!selectedId) return;

        await deleteProblemMutation.mutateAsync(selectedId);
        problems.refetch();
    }

    return (
        <div className="container mx-auto flex justify-center items-center w-full h-[75vh]">
            {problemSet.isSuccess ? (
                <h2 className="absolute top-10 text-2xl">
                    Problems for {problemSet.data.topic}
                </h2>
            ) : null}
            <div className="flex flex-col mt-40 w-3/5 h-full border-solid border-2 border-gray-500 overflow-auto">
                {problems.isSuccess &&
                    problems.data.map((problemSet) => (
                        <React.Fragment key={problemSet.id}>
                            <SessionRow
                                {...problemSet}
                                onClick={() => setSelectedId(problemSet.id)}
                                isSelected={problemSet.id === selectedId}
                            />
                        </React.Fragment>
                    ))}
                <div className="mb-auto" />
                <div className="flex flex-row-reverse justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                    <Link href={`/problemsets/${problemSetId}/create`}>
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Create new problem
                        </button>
                    </Link>

                    <Link
                        href={
                            selectedId ? `/problem/${selectedId}/edit` : `#`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Edit
                        </button>
                    </Link>
                    <Link
                        href={selectedId ? `/problem/${selectedId}` : `#`}
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            View
                        </button>
                    </Link>
                    <button
                        className="bg-red-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black"
                        onClick={deleteProblemHandler}
                    >
                        Delete
                    </button>
                </div>
            </div>
        </div>
    );
};

export default ProblemSetPage;

function SessionRow({
    name,
    isSelected,
    onClick,
}: {
    name: string;
    isSelected: boolean;
    onClick: () => void;
}) {
    return (
        <div
            className={`p-3 m-3 border-b-2 ${
                isSelected ? "bg-gray-200" : "hover:bg-gray-200"
            }`}
            onClick={onClick}
        >
            <span>{name}</span>
        </div>
    );
}
