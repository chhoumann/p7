import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import {Test} from "@prisma/client";


const zodObjTest = z.object({
    code: z.string(),
    problemId: z.string(),
})


export const TestRouter = createRouter().query("getTestById", {
    input: z.string(),
    async resolve({ input: id }) {
        const tests: Test[] | null = await prisma.test.findMany({
            where: { problemId: id},
        });

        if (!tests || tests.length === 0) {
            throw new trpc.TRPCError({
                code: "NOT_FOUND",
                message: `Tests for problem with id ${id} not found.`,
            });
        }

        return { tests };
    },
}).mutation("postTest", {
    input: zodObjTest,
    async resolve({input}) {
        try {
            await prisma.test.create({data: input})
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
