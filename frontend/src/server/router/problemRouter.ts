import {createRouter} from "./context";
import {input, z} from "zod";
import * as trpc from "@trpc/server";
import {prisma} from "../db/client";
import {env} from "../../env/server.mjs";
import {Problem} from "@prisma/client";
import ky from "ky";


const postInput = z.object({
    id: z.string(),
    name: z.string(),
    template: z.string(),
    description: z.string(),
    problemSetId: z.string()
})

const postOutput = z.object({
    success: z.boolean(),
    result: z.string()
});


export const problemRouter = createRouter().query("getByProblemSetId", {
    input: z.string(),
    async resolve({input: id}) {
        const problems: Problem[] | null = await prisma.problem.findMany({
            where: {problemSetId: id},
        });

        if (!problems || problems.length === 0) {
            throw new trpc.TRPCError({
                code: "NOT_FOUND",
                message: `Problem with problemSetId ${id} not found`,
            });
        }

        return {problems};
    },
}).mutation("postProblem", {
    input: postInput,
    output: postOutput,
    async resolve({input}) {
        try {
            const webserverResponse = await ky
                .post(`${env.WEBSERVER_ADDRESS}/problem`, {
                    json: {
                        ...input
                    }
                }).json();

            const parsedResponse = z
                .object({
                    result: z.string(),
                    success: z.boolean(),
                })
                .safeParse(webserverResponse);

            if (!parsedResponse.success) {
                return {success: false, result: parsedResponse.error.message};
            }
            return parsedResponse.data;

        } catch (error) {
            throw new trpc.TRPCError({
                code: "INTERNAL_SERVER_ERROR",
                message: "Could not post problem to database.",
                cause: error
            })
        }
    }
});


