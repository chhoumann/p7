import { useRouter } from "next/router";
import { trpc } from "../../utils/trpc";
import { SubmitHandler, useForm } from "react-hook-form";

interface Inputs {
    title: string;
}

export default function CreateSyllabusPage() {
    const {
        register,
        handleSubmit,
        formState: { errors },
    } = useForm<Inputs>();

    const mutation = trpc.useMutation(["syllabus.postSyllabus"]);
    const router = useRouter();

    const onSubmit: SubmitHandler<Inputs> = async (data) => {
        try {
            await mutation.mutateAsync({
                name: data.title,
            });

            router.push("/syllabi");
        } catch (error) {} // Handled by form library
    };

    return (
        <div className="container flex justify-center items-center w-full h-[75vh]">
            <div className="flex flex-col mt-40 w-[60vh] h-full">
                <form className="mb-6" onSubmit={handleSubmit(onSubmit)}>
                    <div className="">
                        <label className="flex flex-col mb-2 font-bold text-lg text-grey-darkest">
                            Enter Title
                        </label>
                        <input
                            type="text"
                            {...register("title", { required: true })}
                            className="w-full border-2 border-solid border-gray-500 py-2 px-3 text-grey-darkest"
                        />
                            {!!errors.title
                                ? <span>`Invalid input: ${errors.title.type}`</span>
                                : null}

                                {mutation.isError ? <span>{mutation.error.message}</span> : null}
                    </div>
                    <div className="mb-auto" />
                    <div className="flex flex-row gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white">
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
