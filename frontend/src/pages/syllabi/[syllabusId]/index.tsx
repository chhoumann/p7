import { NextPage } from "next";
import { useRouter } from "next/router";
import { trpc } from "../../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";
import Layout from "../../../components/layout";

const SyllabusId: NextPage = () => {
    const [selectedId, setSelectedId] = useState<string>();
    const router = useRouter();
    const { syllabusId } = router.query as { syllabusId: string };

    const syllabus = trpc.useQuery(["syllabus.getById", syllabusId], {});

    const problemSets = trpc.useQuery(
        ["problemSets.getBySyllabusId", syllabusId],
        {
            enabled: router.isReady,
        }
    );

    const deleteSetMutation = trpc.useMutation(
        ["problemSets.deleteProblemSet"],
        {
            onSuccess: () => problemSets.refetch(),
        }
    );

    async function deleteSetHandler() {
        if (!selectedId) return;

        await deleteSetMutation.mutateAsync(selectedId);
        problemSets.refetch();
    }

    if (!syllabus.isSuccess) {
        return null;
    }

    return (
        <Layout title={syllabus.data?.syllabus.name}>
            <div className="container flex justify-center items-center mx-auto w-full h-[75vh]">
                {syllabus.isSuccess ? (
                    <h2 className="absolute top-10 text-2xl">
                        Problem sets for {syllabus.data.syllabus.name}
                    </h2>
                ) : null}
                <div className="flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500 overflow-auto">
                    {problemSets.isSuccess &&
                        problemSets.data.map((problemsets) => (
                            <React.Fragment key={problemsets.id}>
                                <ExerciseRow
                                    {...problemsets}
                                    onClick={() =>
                                        setSelectedId(problemsets.id)
                                    }
                                    isSelected={problemsets.id === selectedId}
                                />
                            </React.Fragment>
                        ))}
                    <div className="mb-auto" />
                    <div className="flex flex-row-reverse gap-4 mx-3 my-3 pt-3 pb-3 justify-center sticky bottom-0 bg-white">
                        <Link href={`/syllabi/${syllabusId}/create`}>
                            <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                                Create new
                            </button>
                        </Link>
                        <Link
                            href={
                                selectedId
                                    ? `/problemsets/${selectedId}`
                                    : `/syllabi/${syllabusId}`
                            }
                        >
                            <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                                View
                            </button>
                        </Link>
                        <Link
                            href={
                                selectedId
                                    ? `/problemsets/${selectedId}/edit`
                                    : `/syllabi/${syllabusId}`
                            }
                        >
                            <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                                Edit
                            </button>
                        </Link>
                        <button
                            onClick={deleteSetHandler}
                            className="bg-red-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black"
                        >
                            Delete
                        </button>
                    </div>
                </div>
            </div>
        </Layout>
    );
};

export default SyllabusId;

function ExerciseRow({
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
            className={`p-3 m-3 border-b-2 ${
                isSelected ? "bg-gray-200" : "hover:bg-gray-200"
            }`}
            onClick={onClick}
        >
            <span>{topic}</span>
        </div>
    );
}
