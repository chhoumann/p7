import {createRouter} from "./context";
import {z} from "zod";
import * as trpc from "@trpc/server";
import {prisma} from "../db/client";
import {Syllabus} from "@prisma/client";

const postInput = z.object({
    id: z.string(),
    name: z.string()
})

const postOutput = z.object({
    success: z.boolean(),
    result: z.string()
});

export const syllabusRouter = createRouter().query("getById", {
    input: z.string(),
    async resolve({input: id}) {
        const syllabus: Syllabus | null = await prisma.syllabus.findFirst({
            where: {id},
        });

        if (!syllabus) {
            throw new trpc.TRPCError({
                code: "NOT_FOUND",
                message: `Syllabus with id ${id} not found`,
            });
        }

        return {syllabus};
    },
})
    .mutation("postSyllabus", {
        input: postInput, output: postOutput, async resolve({input}) {
            try {
                await prisma.syllabus.create({data: input})
                return {success: true, result: "Successfully created test!"}
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not post test to database.",
                    cause: error
                })
            }
        }
    });
