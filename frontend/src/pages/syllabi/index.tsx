import { NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";

const Syllabi: NextPage = () => {
    const [selectedName, setSelectedName] = useState<string>();

    const syllabi = trpc.useQuery(["syllabus.findAll"]);
    const deleteSyllabus = trpc.useMutation(["syllabus.deleteSyllabus"], {
        onSuccess: () => syllabi.refetch()
    });

    function handleDeleteSyllabus() {
        if (!selectedName) return;

        deleteSyllabus.mutate(selectedName);
    }

    return (
        <div className="container flex justify-center items-center mx-auto w-full h-[75vh]">
            <h2 className="absolute top-10">List of syllabi</h2>
            <div className="flex flex-col mt-40 w-3/5 h-full border-solid border-2 border-gray-500 overflow-auto">
                {syllabi.isSuccess &&
                    syllabi.data.map((syllabus) => (
                        <React.Fragment key={syllabus.name}>
                            <SessionRow
                                {...syllabus}
                                onClick={() => setSelectedName(syllabus.name)}
                                isSelected={syllabus.name === selectedName}
                            />
                        </React.Fragment>
                    ))}
                <div className="mb-auto" />
                <div className="flex flex-row justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                    <Link href={`/syllabi/create`}>
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Create new syllabus
                        </button>
                    </Link>

                    <Link
                        href={
                            selectedName
                                ? `/syllabi/edit/${selectedName}`
                                : `/syllabi`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Edit
                        </button>
                    </Link>
                    <Link
                        href={
                            selectedName ? `/syllabi/${selectedName}` : `/syllabi`
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

export default Syllabi;

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
