import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { ProblemSet } from "@prisma/client";

const zodObjProblemSet = z.object({
    topic: z.string(),
    date: z.string(),
    syllabusName: z.string(),
});

export const problemSetsRouter = createRouter()
    .query("getBySyllabusId", {
        input: z.string(),
        async resolve({ input: name }) {
            const problemSets: ProblemSet[] | null =
                await prisma.problemSet.findMany({
                    where: { syllabusName: name },
                });

            if (!problemSets || problemSets.length === 0) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `ProblemSets with syllabusId ${name} not found`,
                });
            }

            return problemSets;
        },
    })
    .query("getByProblemSetId", {
        input: z.string(),
        async resolve({ input: id }) {
            const problemSet: ProblemSet | null =
                await prisma.problemSet.findUnique({
                    where: { id },
                });

            if (!problemSet) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `ProblemSet with id ${id} not found`,
                });
            }

            return problemSet;
        }
    })
    .mutation("postProblemSet", {
        input: zodObjProblemSet,
        async resolve({ input }) {
            try {
                await prisma.problemSet.create({ data: input });
                return {
                    success: true,
                    result: "Successfully created problem!",
                };
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not post problemset to database.",
                    cause: error,
                });
            }
        },
    })
    .mutation("deleteProblemSet", {
        input: z.string(),
        async resolve({ input: id }) {
            try {
                await prisma.problemSet.delete({
                    where: {
                        id,
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: `Could not delete problem set with ID ${id}`,
                    cause: error,
                });
            }
        },
    })
    .mutation("update", {
        input: z.object({
            id: z.string(),
            topic: z.string(),
            date: z.string(),
        }),
        async resolve({ input }) {
            try {
                await prisma.problemSet.update({
                    where: {
                        id: input.id,
                    },
                    data: {
                        topic: input.topic,
                        date: input.date,
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: `Could not update problem set with ID ${input.id}`,
                    cause: error,
                });
            }
        }
    })
