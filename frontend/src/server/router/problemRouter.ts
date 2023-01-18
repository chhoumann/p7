import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Problem } from "@prisma/client";

export const problemRouter = createRouter()
    .query("getByProblemSetId", {
        input: z.string(),
        async resolve({ input: id }) {
            const problems: Problem[] | null = await prisma.problem.findMany({
                where: { problemSetId: id },
            });

            if (!problems) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Problem with problemSetId ${id} not found`,
                });
            }

            return problems;
        },
    })
    .query("byId", {
        input: z.string(),
        async resolve({ input }) {
            const problem: Problem | null = await prisma.problem.findFirst({
                where: { id: input },
            });

            if (!problem) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Problem with id ${input} not found`,
                });
            }

            return problem;
        },
    })
    .mutation("newProblem", {
        input: z.object({
            problemSetId: z.string(),
            name: z.string(),
            description: z.string(),
            template: z.string(),
            test: z.string(),
        }),
        async resolve({ input, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== "teacher") {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            try {
                await prisma.problem.create({
                    data: {
                        problemSetId: input.problemSetId,
                        name: input.name,
                        description: input.description,
                        template: input.template,
                        Tests: {
                            create: {
                                code: input.test,
                            },
                        },
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not post problem to database.",
                    cause: error,
                });
            }
        },
    })
    .mutation("delete", {
        input: z.string(),
        async resolve({ input, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== "teacher") {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            try {
                await prisma.problem.delete({
                    where: {
                        id: input,
                    },
                });
                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not delete problem from database.",
                    cause: error,
                });
            }
        },
    })
    .mutation("update", {
        input: z.object({
            id: z.string(),
            name: z.string(),
            description: z.string(),
            template: z.string(),
            test: z.string(),
        }),
        async resolve({ input, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== "teacher") {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            try {
                await prisma.problem.update({
                    where: {
                        id: input.id,
                    },
                    data: {
                        name: input.name,
                        description: input.description,
                        template: input.template,
                        Tests: {
                            update: {
                                where: {
                                    problemId: input.id,
                                },
                                data: {
                                    code: input.test,
                                },
                            },
                        },
                    },
                });
                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not update problem in database.",
                    cause: error,
                });
            }
        },
    });
