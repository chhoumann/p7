import { NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";
import { useRouter } from "next/router";

const ProblemSets: NextPage = () => {
    const router = useRouter();
    const { id: syllabusId } = router.query;
    const [selectedId, setSelectedId] = useState<string>();

    if (typeof syllabusId !== "string") return null;

    const problemSets = trpc.useQuery(
        ["problemSets.getBySyllabusId", syllabusId],
        { enabled: router.isReady }
    );
    const deleteSyllabus = trpc.useMutation(["problemSets.deleteProblemSet"], {
        onSuccess: () => problemSets.refetch(),
    });

    function handleDeleteSyllabus() {
        if (!selectedId) return;

        deleteSyllabus.mutate(selectedId);
    }

    return (
        <div className="container flex justify-center items-center w-full h-[75vh]">
            <h2 className="absolute top-10">List of problem sets</h2>
            <div className="flex flex-col mt-40 w-3/5 h-full border-solid border-2 border-gray-500 overflow-auto">
                {problemSets.isSuccess &&
                    problemSets.data.map((problemSet) => (
                        <React.Fragment key={problemSet.id}>
                            <SessionRow
                                {...problemSet}
                                onClick={() => setSelectedId(problemSet.id)}
                                isSelected={problemSet.id === selectedId}
                            />
                        </React.Fragment>
                    ))}
                <div className="mb-auto" />
                <div className="flex flex-row justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                    <Link href={`/problemsets/create`}>
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Create new problem set
                        </button>
                    </Link>

                    <Link
                        href={
                            selectedId
                                ? `/problemsets/edit/${selectedId}`
                                : `/${syllabusId}`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Edit
                        </button>
                    </Link>
                    <Link
                        href={
                            selectedId
                                ? `/problemsets/${selectedId}`
                                : `/${syllabusId}`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            View
                        </button>
                    </Link>
                    <button
                        className="bg-red-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black"
                        onClick={handleDeleteSyllabus}
                    >
                        Delete
                    </button>
                </div>
            </div>
        </div>
    );
};

export default ProblemSets;

function SessionRow({
    topic,
    isSelected,
    onClick,
}: {
    topic: string;
    isSelected: boolean;
    onClick: () => void;
}) {
    return (
        <div
            className={`p-3 m-3 ${
                isSelected ? "bg-gray-400" : "bg-gray-300 hover:bg-gray-400"
            }`}
            onClick={onClick}
        >
            <span>{topic}</span>
        </div>
    );
}
