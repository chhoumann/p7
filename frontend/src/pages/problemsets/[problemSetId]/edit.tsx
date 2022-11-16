import { useRouter } from "next/router";
import { trpc } from "../../../utils/trpc";
import { SubmitHandler, useForm } from "react-hook-form";
import { GetServerSideProps } from "next";
import { getServerAuthSession } from "../../../server/common/get-server-auth-session";

interface Inputs {
    title: string;
    date: string;
}

export default function EditProblemSetPage() {
    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm<Inputs>();

    const mutation = trpc.useMutation(["problemSets.update"]);
    const router = useRouter();
    const { problemSetId } = router.query as { problemSetId: string; };
    
    const problemSet = trpc.useQuery(["problemSets.getByProblemSetId", problemSetId], {
        enabled: router.isReady,
    });

    const onSubmit: SubmitHandler<Inputs> = async (data) => {
        try {
            await mutation.mutateAsync({
                id: problemSetId,
                topic: data.title,
                date: new Date(data.date).toISOString(),
            });

            router.push(`/syllabi/${problemSet.data?.syllabusName}`);
        } catch (error) {} // Handled by form library
    };

    if (problemSet.isLoading) {
        return (
            <div className="flex flex-col justify-center h-screen">
                <div className="mx-auto my-auto">Loading...</div>
            </div>
        );
    }

    if (!problemSet.isSuccess || !problemSet.data) {
        return <div>Could not find problem set ☹</div>;
    }

    return (
        <div className="container flex justify-center items-center w-full h-[75vh]">
            <div className="flex flex-col mt-40 w-[60vh] h-full">
                <form className="mb-6" onSubmit={handleSubmit(onSubmit)}>
                    <div className="">
                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Title
                        </label>
                        <input
                            type="text"
                            defaultValue={problemSet.data.topic}
                            {...register("title", { required: true })}
                            className="w-full border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                        />
                        {!!errors.title ? (
                            <span>`Invalid input: ${errors.title.type}`</span>
                        ) : null}

                        <div className="my-4" />

                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Due date
                        </label>
                        <input
                            // https://stackoverflow.com/a/13052187
                            defaultValue={new Date(problemSet.data.date).toJSON().slice(0, 10)}
                            type="date"
                            {...register("date", { required: true })}
                            className="w-full border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                        />

                        {!!errors.date ? (
                            <span>`Invalid input: ${errors.date.type}`</span>
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


export const getServerSideProps: GetServerSideProps = async (ctx) => {
    const session = await getServerAuthSession(ctx);

    if (!session || !session.user || session.user.role !== 'teacher') {
        return {
            redirect: {
                destination: "/",
                permanent: false,
            },
        };
    }

    return {
        props: {}
    }
}