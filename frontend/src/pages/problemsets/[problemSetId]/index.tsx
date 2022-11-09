import { GetServerSideProps, NextPage } from "next";
import { trpc } from "../../../utils/trpc";
import React, { Fragment, useState } from "react";
import { useRouter } from "next/router";
import Layout from "../../../components/layout";
import { StudentRowItem } from "../../../components/StudentRowItem";
import { TeacherRowItem } from "../../../components/TeacherRowItem";
import { PrismaClient } from "@prisma/client";
import { getServerAuthSession } from "../../../server/common/get-server-auth-session";
import { Transition, Dialog } from "@headlessui/react";
import { useForm, SubmitHandler } from "react-hook-form";

const ProblemSetPage: NextPage<{ role: string }> = ({ role }) => {
    const router = useRouter();
    const { problemSetId } = router.query as { problemSetId: string };

    const problemSet = trpc.useQuery([
        "problemSets.getByProblemSetId",
        problemSetId,
    ]);

    const problems = trpc.useQuery(
        ["problem.getByProblemSetId", problemSetId],
        {
            enabled: router.isReady,
        }
    );

    const solvedProblems = trpc.useQuery(
        ["submission.solvedByUserInSet", problemSetId],
        {
            enabled: router.isReady,
        }
    );

    const deleteProblemMutation = trpc.useMutation(["problem.delete"]);

    const [isOpen, setIsOpen] = useState(false);

    async function deleteProblemHandler(id: string) {
        await deleteProblemMutation.mutateAsync(id);
        problems.refetch();
    }

    if (
        !problemSet.isSuccess ||
        !problems.isSuccess ||
        !solvedProblems.isSuccess
    ) {
        return null;
    }

    return (
        <React.Fragment>
            <CreateProblemModal
                isOpen={isOpen}
                problemSetId={problemSetId}
                setIsOpen={(b) => setIsOpen(b)}
                onSuccess={() => problems.refetch()}
            />
            <Layout title={problemSet.data.topic}>
                <h1 className="text-4xl mt-20 text-center">
                    {problemSet.data.topic} ({Object.keys(solvedProblems.data).length}/{problems.data.length})
                </h1>

                <div className="my-8 mx-auto border-b w-1/2 shadow-xs" />

                <div className="container flex flex-col justify-center items-center mx-auto w-full h-3/4">
                    <div className="flex flex-col w-full max-w-3xl h-full overflow-auto">
                        {problems.isSuccess &&
                            problems.data.map((problem) => (
                                <React.Fragment key={problem.id}>
                                    {role === "teacher" ? (
                                        <TeacherRowItem
                                            name={problem.name}
                                            url={`/problem/${problem.id}`}
                                            editUrl={`/problem/${problem.id}/edit`}
                                            onClickTrash={() =>
                                                deleteProblemHandler(problem.id)
                                            }
                                        />
                                    ) : (
                                        <StudentRowItem
                                            url={`/problem/${problem.id}`}
                                            name={problem.name}
                                            showCheckmark={solvedProblems.data[problem.id]}
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
                                    Create new problem
                                </button>
                            </div>
                        ) : null}
                    </div>
                </div>
            </Layout>
        </React.Fragment>
    );
};

export default ProblemSetPage;

const TEST_CODE_TEMPLATE = `module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "..." $ do
`;

interface Inputs {
    title: string;
    template: string;
    test: string;
    description: string;
}

function CreateProblemModal({
    isOpen,
    problemSetId,
    setIsOpen,
    onSuccess,
}: {
    isOpen: boolean;
    problemSetId: string;
    setIsOpen: (isOpen: boolean) => void;
    onSuccess: () => void;
}) {
    const {
        register,
        handleSubmit,
        formState: { errors },
        reset,
    } = useForm<Inputs>();

    const mutation = trpc.useMutation(["problem.newProblem"], {
        onSuccess: () => {
            setIsOpen(false);
            onSuccess();
            reset();
        },
    });

    const onSubmit: SubmitHandler<Inputs> = async (data) => {
        try {
            await mutation.mutateAsync({
                name: data.title,
                template: data.template,
                description: data.description,
                problemSetId,
                test: data.test,
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
                            <Dialog.Panel className="w-full max-w-2xl transform overflow-hidden rounded-2xl bg-white p-6 text-left align-middle shadow-xl transition-all">
                                <Dialog.Title
                                    as="h3"
                                    className="text-lg font-medium leading-6 text-gray-900"
                                >
                                    New problem
                                </Dialog.Title>
                                <div className="container flex justify-center items-center w-full">
                                    <form
                                        className="mb-6 w-full mt-6"
                                        onSubmit={handleSubmit(onSubmit)}
                                    >
                                        <div className="flex flex-col gap-4">
                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                Title
                                                <input
                                                    type="text"
                                                    {...register("title", {
                                                        required: true,
                                                    })}
                                                    className="w-full rounded-lg border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                                />
                                            </label>
                                            {!!errors.title ? (
                                                <span>
                                                    `Invalid input: $
                                                    {errors.title.type}`
                                                </span>
                                            ) : null}

                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                Function template
                                                <textarea
                                                    spellCheck="false"
                                                    {...register("template")}
                                                    className="w-full h-48 rounded-lg border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                                />
                                            </label>

                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                <span>
                                                    Test code (
                                                    <a
                                                        className="text-blue-500"
                                                        href="https://hspec.github.io/"
                                                        target={"_blank"}
                                                        rel={"noreferrer"}
                                                    >
                                                        Hspec documentation
                                                    </a>
                                                    )
                                                </span>
                                                <textarea
                                                    {...register("test", {
                                                        required: true,
                                                    })}
                                                    spellCheck="false"
                                                    className="w-full h-48 rounded-lg border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                                    defaultValue={
                                                        TEST_CODE_TEMPLATE
                                                    }
                                                />
                                            </label>
                                            {!!errors.test ? (
                                                <span>
                                                    `Invalid input: $
                                                    {errors.test.type}`
                                                </span>
                                            ) : null}

                                            <label className="flex flex-col mb-2 font-semibold text-lg text-grey-darkest">
                                                Instructions
                                                <textarea
                                                    {...register(
                                                        "description",
                                                        { required: true }
                                                    )}
                                                    className="w-full h-48 rounded-lg border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                                />
                                            </label>
                                            {!!errors.description ? (
                                                <span>
                                                    `Invalid input: $
                                                    {errors.description.type}`
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
