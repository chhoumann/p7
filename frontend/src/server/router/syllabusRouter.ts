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
        async resolve({ input: id }) {
            const syllabus: Syllabus | null = await prisma.syllabus.findFirst({
                where: { id },
            });

            if (!syllabus) {
                throw new trpc.TRPCError({
                    code: "NOT_FOUND",
                    message: `Syllabus with id ${id} not found`,
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
        async resolve({ input: { name } }) {
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
    });
