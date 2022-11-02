import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Test } from "@prisma/client";

const zodObjTest = z.object({
    code: z.string(),
    problemId: z.string(),
});

export const TestRouter = createRouter()
    .query("byId", {
        input: z.string(),

        async resolve({ input: problemId }) {
            const test: Test | null = await prisma.test.findFirst({
                where: { problemId: problemId },
            });

            if (!test) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Problem with test with ${problemId} not found`,
                });
            }

            return test;
        },
    })
    .mutation("postTest", {
        input: zodObjTest,
        async resolve({ input }) {
            try {
                await prisma.test.create({ data: input });
                return { success: true, result: "Successfully created test!" };
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not post test to database.",
                    cause: error,
                });
            }
        },
    });
