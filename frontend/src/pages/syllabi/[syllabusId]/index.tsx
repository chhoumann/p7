import { GetServerSideProps, NextPage } from "next";
import { useRouter } from "next/router";
import { trpc } from "../../../utils/trpc";
import React, { Fragment, useState } from "react";
import Layout from "../../../components/layout";
import { TeacherRowItem } from "../../../components/TeacherRowItem";
import { StudentRowItem } from "../../../components/StudentRowItem";
import { getServerAuthSession } from "../../../server/common/get-server-auth-session";
import { PrismaClient } from "@prisma/client";
import { SubmitHandler, useForm } from "react-hook-form";
import { Transition, Dialog } from "@headlessui/react";

const SyllabusId: NextPage<{ role: string }> = ({ role }) => {
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

    const [isOpen, setIsOpen] = useState(false);

    async function deleteSetHandler(id: string) {
        await deleteSetMutation.mutateAsync(id);
        problemSets.refetch();
    }

    if (!syllabus.isSuccess || !problemSets.isSuccess) {
        return null;
    }

    return (
        <React.Fragment>
            <CreateProblemSetModal
                isOpen={isOpen}
                setIsOpen={setIsOpen}
                onSuccess={problemSets.refetch}
                syllabusName={syllabus.data.syllabus.name}
            />
            <Layout title={syllabus.data?.syllabus.name}>
                <h1 className="text-4xl mt-20 text-center">
                    {syllabus.data.syllabus.name}
                </h1>

                <div className="my-8 mx-auto border-b w-1/2 shadow-xs" />

                <div className="container flex flex-col justify-center items-center mx-auto w-full h-3/4">
                    <div className="flex flex-col w-full max-w-3xl h-full overflow-auto">
                        {problemSets.data.map((problemSet) => (
                            <React.Fragment key={problemSet.id}>
                                {role === "teacher" ? (
                                    <TeacherRowItem
                                        name={problemSet.topic}
                                        url={`/problemsets/${problemSet.id}`}
                                        editUrl={`/problemsets/${problemSet.id}/edit`}
                                        onClickTrash={() => {
                                            deleteSetHandler(problemSet.id);
                                        }}
                                    />
                                ) : (
                                    <StudentRowItem
                                        url={`/problemsets/${problemSet.id}`}
                                        name={problemSet.topic}
                                    />
                                )}
                            </React.Fragment>
                        ))}
                        <div className="mb-auto" />
                        {role === "teacher" ? (
                            <div className="flex flex-row-reverse justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                                <button
                                    className="px-3 py-2 outline outline-1 outline-gray-400 rounded-lg hover:ring-1 hover:ring-offset-white hover:ring-gray-400 hover:ring-offset-2 transition duration-300 ease-in-out"
                                    onClick={() => setIsOpen(true)}
                                >
                                    Create new problem set
                                </button>
                            </div>
                        ) : null}
                    </div>
                </div>
            </Layout>
        </React.Fragment>
    );
};

interface Inputs {
    title: string;
    date: string;
}

export default SyllabusId;

function CreateProblemSetModal({
    isOpen,
    syllabusName,
    setIsOpen,
    onSuccess,
}: {
    isOpen: boolean;
    syllabusName: string;
    setIsOpen: (isOpen: boolean) => void;
    onSuccess: () => void;
}) {
    const {
        register,
        handleSubmit,
        formState: { errors },
        reset,
    } = useForm<Inputs>();

    const mutation = trpc.useMutation(["problemSets.postProblemSet"], {
        onSuccess: () => {
            setIsOpen(false);
            onSuccess();
            reset();
        },
    });

    const onSubmit: SubmitHandler<Inputs> = async (data) => {
        try {
            await mutation.mutateAsync({
                topic: data.title,
                syllabusName,
                date: new Date(data.date).toISOString(),
            });
        } catch (error) {} // Handled by form library
    };

    return (
        <Transition appear show={isOpen} as={Fragment}>
            <Dialog
                as="div"
                className="relative z-10"
                onClose={() => setIsOpen(false)}
            >
                <Transition.Child
                    as={Fragment}
                    enter="ease-out duration-300"
                    enterFrom="opacity-0"
                    enterTo="opacity-100"
                    leave="ease-in duration-200"
                    leaveFrom="opacity-100"
                    leaveTo="opacity-0"
                >
                    <div className="fixed inset-0 bg-black bg-opacity-25" />
                </Transition.Child>

                <div className="fixed inset-0 overflow-y-auto">
                    <div className="flex min-h-full items-center justify-center p-4 text-center">
                        <Transition.Child
                            as={Fragment}
                            enter="ease-out duration-300"
                            enterFrom="opacity-0 scale-95"
                            enterTo="opacity-100 scale-100"
                            leave="ease-in duration-200"
                            leaveFrom="opacity-100 scale-100"
                            leaveTo="opacity-0 scale-95"
                        >
                            <Dialog.Panel className="w-full max-w-md transform overflow-hidden rounded-2xl bg-white p-6 text-left align-middle shadow-xl transition-all">
                                <Dialog.Title
                                    as="h3"
                                    className="text-lg font-medium leading-6 text-gray-900"
                                >
                                    New problem set
                                </Dialog.Title>
                                <div className="container flex justify-center items-center w-full">
                                    <form
                                        className="mt-6 w-full"
                                        onSubmit={handleSubmit(onSubmit)}
                                    >
                                        <div className="">
                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                Title
                                            </label>
                                            <input
                                                type="text"
                                                {...register("title", {
                                                    required: true,
                                                })}
                                                className="w-full border-2 rounded-lg border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                            />
                                            {!!errors.title ? (
                                                <span>
                                                    `Invalid input: $
                                                    {errors.title.type}`
                                                </span>
                                            ) : null}

                                            <div className="my-4" />

                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                Due date
                                            </label>
                                            <input
                                                type="date"
                                                {...register("date", {
                                                    required: true,
                                                })}
                                                className="w-full rounded-lg border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                            />

                                            {!!errors.date ? (
                                                <span>
                                                    `Invalid input: $
                                                    {errors.date.type}`
                                                </span>
                                            ) : null}

                                            {mutation.isError ? (
                                                <span>
                                                    {mutation.error.message}
                                                </span>
                                            ) : null}
                                        </div>
                                        <div className="mb-auto mt-4" />
                                        <div className="flex flex-row justify-end gap-4 pt-3 pb-3 sticky bottom-0 bg-white">
                                            <input
                                                type="submit"
                                                className="px-3 py-2 outline outline-1 outline-gray-400 rounded-lg hover:ring-1 cursor-pointer hover:ring-offset-white hover:ring-gray-400 hover:ring-offset-2 transition duration-300 ease-in-out"
                                            />
                                        </div>
                                    </form>
                                </div>
                            </Dialog.Panel>
                        </Transition.Child>
                    </div>
                </div>
            </Dialog>
        </Transition>
    );
}

export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getServerAuthSession(ctx);
    const prisma = new PrismaClient();

    if (!session) {
        return {
            redirect: {
                destination: "/",
                permanent: false,
            },
        };
    }

    const userRole = await prisma.role.findFirst({
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
