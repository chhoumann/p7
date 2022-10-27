import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Problem, Test } from "@prisma/client";

export const testRouter = createRouter().query("byId", {
    input: z.string(),

    async resolve({input: problemId}) {
        const test: Test | null = await prisma.test.findFirst({
            where: {problemId: problemId}
        });

        if (!test) {
            throw new trpc.TRPCError({
                code: "NOT_FOUND",
                message: `Problem with test with ${problemId} not found`,
            });
        }

        return test;
    }
})