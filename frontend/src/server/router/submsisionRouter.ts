import trpc from "@trpc/server";
import { z } from "zod";
import { createRouter } from "./context";

export const submissionRouter = createRouter()
    .query("byProblemId", {
        input: z.object({
            problemId: z.string(),
        }),
        async resolve({ input, ctx }) {
            const { prisma, session } = ctx;
            const { problemId } = input;

            if (!session || !session.user?.id) {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            const submissions = await prisma.submission.findMany({
                where: {
                    problemId,
                    userId: session?.user?.id,
                },
                orderBy: {
                    createdAt: "desc",
                },
            });

            return submissions;
        },
    })
    .query("solvedByUserInSet", {
        input: z.string(),
        async resolve({ input: problemSetId, ctx }) {
            const { prisma, session } = ctx;

            if (!session || !session.user?.id) {
                throw new trpc.TRPCError({
                    code: "UNAUTHORIZED",
                    message: "Unauthorized",
                });
            }

            const solvedProblemIds = await prisma.submission.findMany({
                where: {
                    problem: {
                        problemSetId,
                    },
                    userId: session.user.id,
                    success: true,
                },
                select: {
                    problemId: true,
                },
            });

            return solvedProblemIds
                .map((solvedProblemId) => solvedProblemId.problemId)
                .reduce((obj: Record<string, boolean>, id) => {
                    obj[id] = true;
                    return obj;
                }, {});
        },
    });
