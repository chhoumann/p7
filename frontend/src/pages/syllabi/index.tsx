import { GetServerSideProps, NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { useState } from "react";
import Link from "next/link";
import Layout from "../../components/layout";
import { getSession } from "next-auth/react";
import { ArrowRightCircle } from "react-feather";

const Syllabi: NextPage<{ role: string }> = ({ role }) => {
    const [selectedName, setSelectedName] = useState<string>();

    const syllabi = trpc.useQuery(["syllabus.findAll"]);
    const deleteSyllabus = trpc.useMutation(["syllabus.deleteSyllabus"], {
        onSuccess: () => syllabi.refetch(),
    });

    async function handleDeleteSyllabus() {
        if (!selectedName) return;

        await deleteSyllabus.mutateAsync(selectedName);
        syllabi.refetch();
    }

    return (
        <Layout title="Syllabi">
            <h1 className="text-4xl mt-20 text-center">Your courses</h1>

            <div className="my-8 mx-auto border-b w-1/2 shadow-xs" />
            
            <div className="container flex flex-col justify-center items-center mx-auto w-full h-3/4">
                <div className="flex flex-col w-full max-w-3xl h-full overflow-auto">
                    {syllabi.isSuccess &&
                        syllabi.data.map((syllabus) => (
                            <React.Fragment key={syllabus.name}>
                                {role === "teacher" ? (
                                    <SessionRowSelect
                                        {...syllabus}
                                        onClick={() =>
                                            setSelectedName(syllabus.name)
                                        }
                                        isSelected={
                                            syllabus.name === selectedName
                                        }
                                    />
                                ) : (
                                    <SessionRow name={syllabus.name} />
                                )}
                            </React.Fragment>
                        ))}
                    <div className="mb-auto" />
                    {role === "teacher" ? (
                        <div className="flex flex-row-reverse justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
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
                                    selectedName
                                        ? `/syllabi/${selectedName}`
                                        : `/syllabi`
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
                    ) : null}
                </div>
            </div>
        </Layout>
    );
};

export default Syllabi;

function SessionRowSelect({
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
            className={`p-3 m-3 text-xl border-b-2 ${
                isSelected ? "bg-gray-200" : "hover:bg-gray-200"
            }`}
            onClick={onClick}
        >
            <span>{name}</span>
        </div>
    );
}

function SessionRow({ name }: { name: string }) {
    return (
        <div className="p-3 m-3 text-xl group hover:bg-slate-50 rounded-lg flex flex-row align-middle justify-between transition-all duration-500 ease-in-out">
            <span>{name}</span>
            <Link href={`/syllabi/${name}`}>
                <ArrowRightCircle size={30} className="cursor-pointer group-hover:scale-125 transition-transform duration-500 ease-in-out" />
            </Link>
        </div>
    );
}

export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getSession(ctx);

    if (!session) {
        return {
            redirect: {
                destination: "/",
                permanent: false,
            },
        };
    }

    const userRole = await prisma?.role.findFirst({
        where: {
            users: {
                some: {
                    id: session?.user?.id,
                },
            },
        },
    });

    return {
        props: {
            role: userRole?.name,
        },
    };
};
