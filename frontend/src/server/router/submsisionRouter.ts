import trpc from "@trpc/server";
import { z } from "zod";
import { createRouter } from "./context";

export const submissionRouter = createRouter().query("byProblemId", {
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
});
