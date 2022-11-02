import {createRouter} from "./context";
import {z} from "zod";
import * as trpc from "@trpc/server";
import {prisma} from "../db/client";
import {Problem} from "@prisma/client";

export const zObjProblem = z.object({
    name: z.string(),
    template: z.string(),
    description: z.string(),
    problemSetId: z.string(),
})



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
}).mutation("newProblem", {
    input: zObjProblem,
    async resolve({input}) {
        try {
            await prisma.problem.create({data: input})
            return {success: true, result: "Successfully created problem!"}

        } catch (error) {
            throw new trpc.TRPCError({
                code: "INTERNAL_SERVER_ERROR",
                message: "Could not post problem to database.",
                cause: error
            })
        }
    }
});


