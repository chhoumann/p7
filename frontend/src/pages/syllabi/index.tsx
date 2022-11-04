import { GetServerSideProps, NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { Fragment, useState } from "react";
import Layout from "../../components/layout";
import { Dialog, Transition } from "@headlessui/react";
import { SubmitHandler, useForm } from "react-hook-form";
import { getServerAuthSession } from "../../server/common/get-server-auth-session";
import { PrismaClient } from "@prisma/client";
import { StudentRowItem } from "../../components/StudentRowItem";
import { TeacherRowItem } from "../../components/TeacherRowItem";

const Syllabi: NextPage<{ role: string }> = ({ role }) => {
    const syllabi = trpc.useQuery(["syllabus.findAll"]);
    const deleteSyllabus = trpc.useMutation(["syllabus.deleteSyllabus"], {
        onSuccess: () => syllabi.refetch(),
    });

    async function handleDeleteSyllabus(name: string) {
        await deleteSyllabus.mutateAsync(name);

        syllabi.refetch();
    }

    const [isOpen, setIsOpen] = useState(false);

    return (
        <React.Fragment>
            <CreateSyllabusModal
                isOpen={isOpen}
                setIsOpen={(b) => setIsOpen(b)}
                onSuccess={() => syllabi.refetch()}
            />
            <Layout title="Syllabi">
                <h1 className="text-4xl mt-20 text-center">Your courses</h1>

                <div className="my-8 mx-auto border-b w-1/2 shadow-xs" />

                <div className="container flex flex-col justify-center items-center mx-auto w-full h-3/4">
                    <div className="flex flex-col w-full max-w-3xl h-full overflow-auto">
                        {syllabi.isSuccess &&
                            syllabi.data.map((syllabus) => (
                                <React.Fragment key={syllabus.name}>
                                    {role === "teacher" ? (
                                        <TeacherRowItem
                                            {...syllabus}
                                            url={`/syllabi/${syllabus.name}`}
                                            editUrl={`/syllabi/edit/${syllabus.name}`}
                                            onClickTrash={() => {
                                                handleDeleteSyllabus(
                                                    syllabus.name
                                                );
                                            }}
                                        />
                                    ) : (
                                        <StudentRowItem
                                            url={`/syllabi/${syllabus.name}`}
                                            name={syllabus.name}
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
                                    Create new syllabus
                                </button>
                            </div>
                        ) : null}
                    </div>
                </div>
            </Layout>
        </React.Fragment>
    );
};

export default Syllabi;

function CreateSyllabusModal({
    isOpen,
    setIsOpen,
    onSuccess,
}: {
    isOpen: boolean;
    setIsOpen: (isOpen: boolean) => void;
    onSuccess: () => void;
}) {
    const {
        register,
        handleSubmit,
        formState: { errors },
        reset,
    } = useForm<{ title: string }>();

    const mutation = trpc.useMutation(["syllabus.postSyllabus"], {
        onSuccess: () => {
            setIsOpen(false);
            onSuccess();
            reset();
        },
    });

    const onSubmit: SubmitHandler<{ title: string }> = (data) => {
        try {
            mutation.mutate({
                name: data.title,
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
                                    New syllabus
                                </Dialog.Title>
                                <div className="container flex justify-center items-center w-full">
                                    <form
                                        onSubmit={handleSubmit(onSubmit)}
                                        className="w-full"
                                    >
                                        <div className="mt-4">
                                            <input
                                                type="text"
                                                placeholder="Title"
                                                {...register("title", {
                                                    required: true,
                                                })}
                                                className="w-full rounded-lg border border-gray-500 py-2 px-3 text-grey-darkest"
                                            />
                                            {!!errors.title ? (
                                                <span>
                                                    Invalid input:{" "}
                                                    {errors.title.type}
                                                </span>
                                            ) : null}

                                            {mutation.isError ? (
                                                <span>
                                                    {mutation.error.message}
                                                </span>
                                            ) : null}
                                        </div>
                                        <div className="mb-auto" />
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
