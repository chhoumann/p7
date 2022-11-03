import { useRouter } from "next/router";
import { trpc } from "../../../utils/trpc";
import { SubmitHandler, useForm } from "react-hook-form";

interface Inputs {
    title: string;
    template: string;
    test: string;
    description: string;
}

const TEST_CODE_TEMPLATE = `module SolutionSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Code # Imports solution code

main :: IO()
main = hspec $ do
    describe "Solution" $ do
        it "..." $ do
`;

export default function CreateProblemPage() {
    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm<Inputs>();

    const mutation = trpc.useMutation(["problem.newProblem"]);
    const router = useRouter();
    const { problemSetId } = router.query as { problemSetId: string };

    const onSubmit: SubmitHandler<Inputs> = async (data) => {
        try {
            await mutation.mutateAsync({
              name: data.title,
              template: data.template,
              description: data.description,
              problemSetId,
              test: data.test,
            });

            router.push(`/problemsets/${router.query.problemSetId}`);
        } catch (error) {} // Handled by form library
    };

    return (
        <div className="container flex justify-center items-center w-full h-[75vh]">
            <div className="flex flex-col mt-40 w-[60vh] h-full">
                <form className="mb-6" onSubmit={handleSubmit(onSubmit)}>
                    <div className="flex flex-col gap-4">
                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Title
                            <input
                                type="text"
                                {...register("title", { required: true })}
                                className="w-full border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                            />
                        </label>
                        {!!errors.title ? (
                            <span>`Invalid input: ${errors.title.type}`</span>
                        ) : null}

                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Function template
                            <textarea
                                spellCheck="false"
                                {...register("template")}
                                className="w-full h-48 border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                            />
                        </label>

                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
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
                                {...register("test", { required: true })}
                                spellCheck="false"
                                className="w-full h-48 border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                                defaultValue={TEST_CODE_TEMPLATE}
                            />
                        </label>
                        {!!errors.test ? (
                            <span>`Invalid input: ${errors.test.type}`</span>
                        ) : null}

                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Instructions
                            <textarea
                                {...register("description", { required: true })}
                                className="w-full h-48 border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                            />
                        </label>
                        {!!errors.description ? (
                            <span>
                                `Invalid input: ${errors.description.type}`
                            </span>
                        ) : null}

                        {mutation.isError ? (
                            <span>{mutation.error.message}</span>
                        ) : null}
                    </div>
                    <div className="mb-auto" />
                    <div className="flex flex-row justify-center gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
                        <input
                            type="submit"
                            className="bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 cursor-pointer hover:outline-black"
                        />
                    </div>
                </form>
            </div>
        </div>
    );
}
