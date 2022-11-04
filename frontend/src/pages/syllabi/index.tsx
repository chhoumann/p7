import { GetServerSideProps, NextPage } from "next";
import { trpc } from "../../utils/trpc";
import React, { Fragment, useState } from "react";
import Link from "next/link";
import Layout from "../../components/layout";
import { ArrowRightCircle, Edit, Trash } from "react-feather";
import clsx from "clsx";
import { Dialog, Transition } from "@headlessui/react";
import { SubmitHandler, useForm } from "react-hook-form";
import { getServerAuthSession } from "../../server/common/get-server-auth-session";
import { PrismaClient } from "@prisma/client";

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
                                        <SyllabusRowSelect
                                            {...syllabus}
                                            onClickTrash={() => {
                                                handleDeleteSyllabus(
                                                    syllabus.name
                                                );
                                            }}
                                        />
                                    ) : (
                                        <SyllabusRow name={syllabus.name} />
                                    )}
                                </React.Fragment>
                            ))}
                        <div className="mb-auto" />
                        {role === "teacher" ? (
                            <div className="flex flex-row-reverse justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                                {/* <Link href={`/syllabi/create`}> */}
                                <button
                                    className="px-3 py-2 outline outline-1 outline-gray-400 rounded-lg hover:ring-1 hover:ring-offset-white hover:ring-gray-400 hover:ring-offset-2 transition duration-300 ease-in-out"
                                    onClick={() => setIsOpen(true)}
                                >
                                    Create new syllabus
                                </button>
                                {/* </Link> */}
                            </div>
                        ) : null}
                    </div>
                </div>
            </Layout>
        </React.Fragment>
    );
};

export default Syllabi;

function SyllabusRowSelect({
    name,
    onClickTrash,
}: {
    name: string;
    onClickTrash: () => void;
}) {
    return (
        <div
            className={clsx(
                "p-3 m-3 text-xl group hover:bg-zinc-100 rounded-lg flex flex-row align-middle justify-between transition-all duration-500 ease-in-out"
            )}
        >
            <span>{name}</span>
            <div className="flex flex-row gap-3">
                <Trash
                    size={30}
                    className="cursor-pointer"
                    onClick={onClickTrash}
                />
                <Link href={`/syllabi/edit/${name}`}>
                    <Edit size={30} className="cursor-pointer" />
                </Link>
                <Link href={`/syllabi/${name}`}>
                    <ArrowRightCircle size={30} className="cursor-pointer" />
                </Link>
            </div>
        </div>
    );
}

function SyllabusRow({ name }: { name: string }) {
    return (
        <Link href={`/syllabi/${name}`}>
            <div
                className={
                    "p-3 m-3 text-xl group hover:bg-zinc-100 rounded-lg flex flex-row align-middle cursor-pointer justify-between transition-all duration-500 ease-in-out"
                }
            >
                <span>{name}</span>
                <ArrowRightCircle
                    size={30}
                    className="cursor-pointer group-hover:scale-125 transition-transform duration-500 ease-in-out"
                />
            </div>
        </Link>
    );
}

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
