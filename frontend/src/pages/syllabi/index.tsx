import { NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";

const Syllabi: NextPage = () => {
    const [selectedId, setSelectedId] = useState<string>();

    const session = trpc.useQuery(["syllabus.findAll"]);

    return (
        <div className="container flex justify-center items-center w-full h-[75vh]">
            <h2 className="absolute top-10">List of syllabi</h2>
            <div className="flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500 overflow-auto">
                {session.isSuccess &&
                    session.data.map((session) => (
                        <React.Fragment key={session.id}>
                            <SessionRow
                                {...session}
                                onClick={() => setSelectedId(session.id)}
                                isSelected={session.id === selectedId}
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
                            selectedId
                                ? `/syllabi/edit/${selectedId}`
                                : `/syllabi`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            Edit
                        </button>
                    </Link>
                    <Link
                        href={
                            selectedId ? `/syllabi/${selectedId}` : `/syllabi`
                        }
                    >
                        <button className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black">
                            View
                        </button>
                    </Link>
                </div>
            </div>
        </div>
    );
};

export default Syllabi;

function SessionRow({
    id,
    name,
    isSelected,
    onClick,
}: {
    id: string;
    name: string;
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
            <span>{name}</span>
        </div>
    );
}
