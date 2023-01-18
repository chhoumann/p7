import { createRouter } from "./context";
import { z } from "zod";
import * as trpc from "@trpc/server";
import { prisma } from "../db/client";
import { Prisma, Syllabus } from "@prisma/client";

const postInput = z.object({
    name: z.string(),
});

const postOutput = z.object({
    success: z.boolean(),
    result: z.string(),
});

export const syllabusRouter = createRouter()
    .query("getById", {
        input: z.string(),
        async resolve({ input: name }) {
            const syllabus: Syllabus | null = await prisma.syllabus.findFirst({
                where: { name },
            });

            if (!syllabus) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Syllabus with name ${name} not found`,
                });
            }

            return { syllabus };
        },
    })
    .query("findAll", {
        async resolve() {
            const syllabi: Syllabus[] | null = await prisma.syllabus.findMany();

            if (!syllabi) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Syllabi cannot be found`,
                });
            }

            return syllabi;
        },
    })
    .mutation("postSyllabus", {
        input: postInput,
        async resolve({ input: { name }, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== 'teacher') {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized"
                })
            }

            try {
                await prisma.syllabus.create({
                    data: {
                        name: name,
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: "Could not post syllabus to database.",
                    cause: error,
                });
            }
        },
    })
    .mutation("editSyllabus", {
        input: z.object({
            old: z.string(),
            new: z.string(),
        }),
        async resolve({ input, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== 'teacher') {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized"
                })
            }

            try {
                await prisma.syllabus.update({
                    where: {
                        name: input.old,
                    },
                    data: {
                        name: input.new,
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: `Could not update syllabus with name ${input.old}`,
                    cause: error,
                });
            }
        },
    })
    .mutation("deleteSyllabus", {
        input: z.string(),
        async resolve({ input: name, ctx }) {
            const { session } = ctx;

            if (!session || !session.user || session.user.role !== 'teacher') {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized"
                })
            }

            try {
                await prisma.syllabus.delete({
                    where: {
                        name,
                    },
                });

                return true;
            } catch (error) {
                throw new trpc.TRPCError({
                    code: "INTERNAL_SERVER_ERROR",
                    message: `Could not delete syllabus with name ${name}`,
                    cause: error,
                });
            }
        },
    });
