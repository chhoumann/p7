import { NextPage } from "next";
import { useRouter } from "next/router";
import { trpc } from "../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";

const SyllabusId: NextPage = () => {
    const [selectedId, setSelectedId] = useState<string>();
    const router = useRouter();
    const { id } = router.query;

    const problemSets = trpc.useQuery(
        ["problemSets.getBySyllabusId", (id as string) ?? ""],
        {
            enabled: router.isReady,
        }
    );

    const deleteSetMutation = trpc.useMutation(["problemSets.deleteProblemSet"], {
        onSuccess: () => problemSets.refetch()
    });
    
    function deleteSetHandler() {
        if (!selectedId) return;

        deleteSetMutation.mutate(selectedId)
    }

    return (
        <div className="container flex justify-center items-center mx-auto w-full h-[75vh]">
            <h2 className="absolute top-10">Problem sets</h2>
            <div className="flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500 overflow-auto">
                {problemSets.isSuccess &&
                    problemSets.data.map((problemsets) => (
                        <React.Fragment key={problemsets.id}>
                            <ExerciseRow
                                {...problemsets}
                                onClick={() => setSelectedId(problemsets.id)}
                                isSelected={problemsets.id === selectedId}
                            />
                        </React.Fragment>
                    ))}
                <div className="mb-auto" />
                <div className="flex flex-row-reverse gap-4 mx-3 my-3 pt-3 pb-3 justify-center sticky bottom-0 bg-white">
                    <Link href={`/problemsets/create`}>
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Create new
                        </button>
                    </Link>
                    <Link
                        href={
                            selectedId
                                ? `/problemsets/${selectedId}`
                                : `/syllabi/${id}`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            View
                        </button>
                    </Link>
                    <Link
                        href={
                            selectedId
                                ? `/problemsets/edit/${selectedId}`
                                : `/syllabi/${id}`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Edit
                        </button>
                    </Link>
                        <button onClick={deleteSetHandler} className="bg-red-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Delete
                        </button>
                </div>
            </div>
        </div>
    );
};

export default SyllabusId;

function ExerciseRow({
    id,
    topic,
    isSelected,
    onClick,
}: {
    id: string;
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
